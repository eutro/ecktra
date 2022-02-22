#lang racket/base

(require racket/cmdline
         racket/port
         racket/system
         racket/list
         racket/runtime-path
         racket/match
         ffi/unsafe
         ffi/vector
         ;; file/convertible
         "../util/fps.rkt"
         "../util/ringbuf.rkt"
         ;; "../foreign/decode.rkt"
         "signal.rkt")

(provide parse-cl)

(struct vconfig (rate fps input deplete))

(define (parse-cl)
  (define rate* 44100)
  (define fps* 30)
  (define input* #f)
  (define deplete* #f)
  (define input-handler (ffmpeg-input-handler null))
  (define output-handler (gui-output-handler))

  (match-define (cons cc argv) ;; loopback point
    (let/cc cc (cons cc (current-command-line-arguments))))
  (define (recur args) (cc (cons cc args)))
  (command-line
   #:argv argv
   #:once-each
   [("-c" "--config")
    =>
    (lambda (flag first-arg . args)
      (command-line
       #:program flag
       #:argv (cons first-arg args)
       #:once-each
       [("-r" "--rate") rate "Set the sample rate" (set! rate* (string->number rate))]
       [("--fps" "--max-fps") fps "Set the maximum visualizer framerate" (set! fps* (string->number fps))]
       #:args rest-args
       (recur rest-args)))
    (list "Configure the audio visualizer" "option")]
   [("-i" "--input")
    =>
    (lambda (flag first-arg . args)
      (command-line
       #:program flag
       #:argv (cons first-arg args)
       #:once-each
       [("--deplete" "--sync")
        "Deplete extra samples from the input when necessary to keep up with it. Useful for live input."
        (set! deplete* #t)]
       #:once-any
       [("-f" "--ffmpeg")
        =>
        (lambda (_flag . args)
          (define-values (ffmpeg-args rest-args)
            (splitf-at args (lambda (s) (not (equal? s "--")))))
          (set! input-handler (ffmpeg-input-handler ffmpeg-args))
          (recur (cdr rest-args)))
        (list "Add arguments to ffmpeg calls, these will be inserted before -i and the input argument. End with --"
              "arguments")]
       #:args rest-args
       (recur rest-args)))
    (list "Configure audio input" "option")]
   [("-o" "--output")
    =>
    (lambda (flag first-arg . args)
      (command-line
       #:program flag
       #:argv (cons first-arg args)
       #:once-each
       [("--save")
        =>
        (lambda (flag out-file . args)
          (command-line
           #:program flag
           #:argv args
           #:args rest-args
           (recur rest-args)))
        (list "Save to a file" "out-file" "option")]
       [("--gui")
        =>
        (lambda (flag . args)
          (define no-play* #false)
          (command-line
           #:program flag
           #:argv args
           #:once-each
           [("--no-play") "Don't play audio, useful when using audio capture" (set! no-play* #true)]
           #:args rest-args
           (begin
             (set! output-handler (gui-output-handler (not no-play*)))
             (recur rest-args))))
        (list "Show a GUI (default)" "option")]
       #:args rest-args
       (recur rest-args)))
    (list "Configure video output" "option")]
   #:args (input)
   (set! input* input))

  (current-sample-rate rate*)
  (output-handler (vconfig rate* fps* (input-handler input*) deplete*)))

(define sample-size-bytes 8)

(begin ;; ffmpeg input
  (define (((ffmpeg-input-handler args) input) vcfg audio-out-port)
    (match-define
      (list stdout
            #f ;; stdin
            _ ;; pid
            #f ;; stderr
            handle ;; handle
            )
      (apply
       process*/ports
       #f ;; stdout
       (open-input-bytes #"") ;; stdin
       (current-error-port) ;; stderr
       (find-executable-path "ffmpeg")
       `[,@args
         "-i"
         ,input

         "-af" ,(format "asetrate=~a" (vconfig-rate vcfg))
         "-map" "0"
         "-ac" "1"
         "-c" ,(if (system-big-endian?) "pcm_f64be" "pcm_f64le")
         "-f" "data"
         "pipe:1"]))
    (define ports-to-close (list stdout))
    (file-stream-buffer-mode stdout 'block)
    (when audio-out-port
      (define old-stdout stdout)
      (define-values (new-stdout new-stdout-input) (make-pipe))
      (thread
       (lambda ()
         (copy-port old-stdout
                    audio-out-port
                    new-stdout-input)))
      (set! stdout new-stdout)
      (set! ports-to-close (list* new-stdout new-stdout-input ports-to-close)))
    (values (read-next-from-port stdout (vconfig-deplete vcfg))
            (lambda ()
              (handle 'kill)
              (for ([port (in-list ports-to-close)])
                (if (input-port? port)
                    (close-input-port port)
                    (close-output-port port))))))

  (define (read-next-from-port port deplete)
    (define (push-from-bytes buf sample-bytes byte-count)
      (define bytes-ptr (u8vector->cpointer sample-bytes))
      (define (ref i) (ptr-ref bytes-ptr _double i))
      (define sample-count (quotient byte-count sample-size-bytes))
      (ring-buffer-push-all! buf sample-count ref)
      (port-commit-peeked
       (* sample-count sample-size-bytes)
       (port-progress-evt port)
       always-evt
       port))
    (define (normal-read buf n)
      (cond
        [(port-closed? port) eof]
        [else
         (define expected-byte-count (* n sample-size-bytes))
         (define sample-bytes (make-bytes expected-byte-count))
         (define bytes-read (peek-bytes-avail! sample-bytes 0 #f port))
         (cond
           [(integer? bytes-read)
            (push-from-bytes buf sample-bytes bytes-read)
            #t]
           [else eof])]))
    (define (depleting-read)
      (define running-buffer #f)
      (define buffer-thread #f)
      (define (start-buffering)
        (define sample-bytes (make-bytes (* sample-size-bytes 4096)))
        (let loop ()
          (define bytes-read (peek-bytes-avail! sample-bytes 0 #f port))
          (when (integer? bytes-read)
            (push-from-bytes running-buffer sample-bytes bytes-read)
            (loop))))
      (lambda (buf n)
        (unless running-buffer
          (set! running-buffer (make-sliding-buffer (make-ring-buffer (vector-length (ring-buffer-buf buf)) 0.0)))
          (set! buffer-thread (thread start-buffering)))
        (cond
          [(or (port-closed? port)
               (not (thread-running? buffer-thread)))
           eof]
          [else
           (define sbuf (make-vector n))
           (sliding-buffer-pop!
            running-buffer n
            (lambda (i s) (vector-set! sbuf i s)))
           (ring-buffer-push-all! buf n (lambda (i) (vector-ref sbuf i)))
           #t])))
    (if deplete
        (depleting-read)
        normal-read)))

#;
(begin ;; portaudio input
  (define ((portaudio-input-handler input) vcfg audio-out-port)
    (dynamic-require
     'portaudio
     #f
     (lambda ()
       (raise-user-error "portaudio package not installed")))
    (define (portaudio-require sym)
      (dynamic-require 'portaudio sym))
    (void)))

#;
(begin ;; file output TODO
  (define ((save-output out-file) vcfg)
    (define max-fps (vconfig-fps vcfg))
    (define counter 0)
    (define encoder-thread
      (thread
       (lambda ()
         (match-define (list #f ;; stdout
                             ffmpeg-stdin
                             _ ;; pid
                             #f ;; stderr
                             _ ;; ffmpeg-control
                             )
           (apply
            process*/ports
            (current-output-port)
            #f
            (current-error-port)
            (find-executable-path "ffmpeg")

            `[,@(format-ffmpeg-args vcfg)
              "-r" ,(format "~a" max-fps)
              "-f" "image2pipe"
              "-s" "1024x400"
              "-i" "-"
              "-vcodec" "libx264"
              "-crf" "25"
              "-pix_fmt" "yuv420p"
              ,out-file]))
         (let loop ()
           (define frame (thread-receive))
           (when frame
             (define png-bytes (convert frame 'png-bytes))
             (write-bytes png-bytes ffmpeg-stdin)
             (set! counter (add1 counter))
             (loop)))
         (close-output-port ffmpeg-stdin))))
    (values
     ((vconfig-input vcfg) vcfg)
     (lambda (frame)
       (thread-send encoder-thread frame)
       (void))
     (lambda () ;; finish
       (close-input-port ffmpeg-port)
       (thread-send encoder-thread #f)
       (thread-wait encoder-thread))
     (wrap-evt always-evt (lambda (_) (/ 1000 max-fps)))
     thread-wait)))

(begin ;; GUI output
  (define-runtime-module-path gui-path "gui.rkt")
  (define ((gui-output-handler [play #t]) vcfg)
    (define (gui-require s) (dynamic-require gui-path s))
    ((gui-require 'init-gui))
    (define ffplay-in* #f)
    (define handle* #f)
    (when play
      (match-define
        (list #f ;; out
              ffplay-in ;; in
              _ ;; pid
              #f ;; err
              handle ;; handle
              )
        (process*/ports
         (current-output-port) ;; out
         #f ;; in
         (current-error-port) ;; err
         (find-executable-path "ffplay")
         "-nodisp"
         "-f"
         (if (system-big-endian?) "f64be" "f64le")
         "-i" "pipe:0"))
      (set! ffplay-in* ffplay-in)
      (set! handle* handle))
    (define fps (fps-event (vconfig-fps vcfg)))
    (sync fps)
    (define-values (read-next! close-next) ((vconfig-input vcfg) vcfg ffplay-in*))
    (values
     read-next!
     (gui-require 'put-frame!)
     (lambda ()
       (close-next)
       (when ffplay-in*
         (handle* 'kill)
         (close-output-port ffplay-in*)))
     fps
     (lambda (_)
       (void)))))
