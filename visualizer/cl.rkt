#lang racket/base

(require racket/cmdline
         racket/port
         racket/list
         racket/runtime-path
         racket/match
         racket/contract
         racket/tcp
         (except-in ffi/unsafe ->)
         ffi/vector
         ;; file/convertible
         "../util/fps.rkt"
         "../util/ringbuf.rkt"
         "signal.rkt")

(provide parse-cl)

(struct vconfig
  (rate ;; integer?
   fps ;; integer?
   input ;; (-> input-stream? #;"mpegts")
   deplete ;; boolean?
   ))

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
  (define (ffmpeg* args
                   #:command [command "ffmpeg"]
                   #:stdin [in #f]
                   #:stdout [out #f])
    (define-values (proc-in proc-out) (values #f #f))
    (when (and in (file-stream-port? in)) (set! proc-in in) (set! in #f))
    (when (and out (file-stream-port? out)) (set! proc-out out) (set! out #f))
    (define-values
      (proc
       stdout
       stdin
       _stderr)
      (apply
       subprocess
       proc-out
       proc-in
       (current-error-port) ;; stderr
       (find-executable-path command)
       "-hide_banner"
       args))
    (when in
      (thread
       (lambda ()
         (copy-port in stdin)
         (close-output-port stdin))))
    (when out
      (thread
       (lambda ()
         (copy-port stdout out))))
    (thread
     (lambda ()
       (sync (port-closed-evt stdout))
       (close-input-port stdout)
       (close-output-port stdin)
       (subprocess-kill proc #true #;"Maybe we could be nicer?")))
    (values stdin stdout))

  (define (((ffmpeg-input-handler args) input) . outs)
    (-> list? (-> string? (-> input-port?)))
    (define sockets
      (for/list ([_ outs]
                 [port (range 54323 55000)])
        (list "127.0.0.1" port)))
    (define out-args
      (append*
       (for/list ([args outs]
                  [socket sockets])
         `("-map" "0" ,@args ,(apply format "tcp://~a:~a?listen" socket)))))
    (define-values (stdin stdout)
      (ffmpeg*
       `[,@args "-i" ,input ,@out-args]))

    (apply values stdout sockets)))

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
       (define bytes-read (read-bytes! sample-bytes port))
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
      normal-read))

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

    (define pcm-args
      `[
        "-af" ,(format "aresample=~a" (vconfig-rate vcfg))
        "-ac" "1"
        "-f" "data"
        "-c" ,(if (system-big-endian?) "pcm_f64be" "pcm_f64le")])
    (define-values (read-next! close)
      (cond
        [play
         (define-values (ffmpeg-port
                         mpeg-socket
                         pcm-socket)
           ((vconfig-input vcfg)
            `["-f" "mpegts"]
            pcm-args))
         ;; cross our fingers and hope that they remain sufficiently in sync...
         (define-values (ffplay-in ffplay-out)
           (ffmpeg*
            #:command "ffplay"
            `["-nodisp" "-f" "mpegts" ,(apply format "tcp://~a:~a" mpeg-socket)]))
         (sleep 3) ;; TODO do better
         (define-values (pcm-port tcp-out) (apply tcp-connect pcm-socket))
         (file-stream-buffer-mode pcm-port 'block)
         (define read-next! (read-next-from-port pcm-port (vconfig-deplete vcfg)))
         (define (close)
           (close-input-port ffmpeg-port)
           (close-input-port ffplay-out)

           (close-input-port pcm-port)
           (close-output-port tcp-out)
           (close-output-port ffplay-in))
         (values read-next! close)]
        [else
         (define-values (ffmpeg-port pcm-socket) ((vconfig-input vcfg) pcm-args))
         (sleep 3) ;; TODO do better
         (define-values (pcm-port tcp-out) (apply tcp-connect pcm-socket))
         (define read-next! (read-next-from-port pcm-port (vconfig-deplete vcfg)))
         (define (close)
           (close-input-port ffmpeg-port)
           (close-input-port pcm-port)
           (close-output-port tcp-out))
         (values read-next! close)]))

    (define fps (fps-event (vconfig-fps vcfg)))
    (sync fps)
    (values
     read-next!
     (gui-require 'put-frame!)
     (lambda () (close))
     fps
     (lambda (_) (void)))))
