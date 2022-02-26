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
         file/convertible
         "../util/fps.rkt"
         "../util/ringbuf.rkt"
         "signal.rkt")

(provide parse-cl)

(struct vconfig
  (rate ;; integer?
   fps ;; integer?
   input ;;
   deplete ;; boolean?
   delayc ;; number?
   ))

(define (parse-cl)
  (define rate* 44100)
  (define fps* 30)
  (define input* #f)
  (define deplete* #f)
  (define delay* 0)
  (define input-handler (ffmpeg-input-handler null))
  (define output-handler (gui-output-handler))

  (match-define (cons cc argv) ;; loopback point
    (let/cc cc (cons cc (current-command-line-arguments))))
  (define (recur args) (cc (cons cc args)))
  (command-line
   #:program "ecktra"
   #:argv argv
   #:once-each
   [("-c" "--config")
    =>
    (lambda (flag first-arg . args)
      (command-line
       #:program flag
       #:argv (cons first-arg args)
       #:once-each
       ;; doesn't do anything [("-r" "--rate") rate "Set the sample rate" (set! rate* (string->number rate))]
       [("--fps" "--max-fps") fps "Set the maximum visualizer framerate" (set! fps* (string->number fps))]
       [("--delay")
        seconds "How long to wait before syncing with framerate, on top of any existing latency"
        (set! delay* (string->number seconds))]
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
        "Deplete extra samples from the input when necessary to keep up with it."
        (set! deplete* #t)]
       #:once-any
       [("-F" "--ffmpeg")
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
           (begin
             (set! output-handler (save-output out-file))
             (recur rest-args))))
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
   #:args (input . program-args)
   (begin (set! input* input)
          (current-command-line-arguments (list->vector program-args))))

  (current-sample-rate rate*)
  (current-subprocess-custodian-mode 'kill) ;; just let them die
  (output-handler (vconfig rate* fps* (input-handler input*) deplete* delay*)))

(define sample-size-bytes 8)

(define loopback-addr "127.0.0.1")

(define (make-ephemeral-address) ;; "ephemeral"
  (list loopback-addr (or (getenv "ECKTRA_MPEGTS_PORT") "40167")))

(define (listener->addr+port listener)
  (define-values (addr port _oa _op)
    (tcp-addresses listener #t))
  (list addr port))

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
       (current-error-port)
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
    (values proc stdin stdout))

  (define (((ffmpeg-input-handler args) input) . outs)
    (-> list? (-> string? (-> input-port?)))
    (define out-args
      (append*
       (for/list ([args outs])
         `("-map" "0" ,@args))))
    (define-values (_proc _stdin stdout)
      (ffmpeg*
       `[,@args "-i" ,input ,@out-args]))

    stdout))

(define (read-next-from-port port vcfg)
  (define deplete (vconfig-deplete vcfg))
  (define (push-from-bytes buf sample-bytes byte-count peeked?)
    (define bytes-ptr (u8vector->cpointer sample-bytes))
    (define (ref i) (ptr-ref bytes-ptr _double i))
    (define sample-count (quotient byte-count sample-size-bytes))
    (ring-buffer-push-all! buf sample-count ref)
    (when peeked?
      (port-commit-peeked
       (* sample-count sample-size-bytes)
       (port-progress-evt port)
       always-evt
       port)))
  (define (normal-read buf n)
    (cond
      [(port-closed? port) eof]
      [else
       (define expected-byte-count (* n sample-size-bytes))
       (define sample-bytes (make-bytes expected-byte-count))
       (define bytes-read (read-bytes! sample-bytes port))
       (cond
         [(integer? bytes-read)
          (push-from-bytes buf sample-bytes bytes-read #f)
          #t]
         [else eof])]))
  (define (depleting-read)
    (define missed-bytes 0)
    (define _peeker-thread
      (thread
       (lambda ()
         (define peek-buf (make-bytes (* sample-size-bytes 4096)))
         (let loop ()
           (unless (port-closed? port)
             (peek-bytes! peek-buf 0 port)
             (sync (port-progress-evt port))
             (loop))))))
    (lambda (buf n)
      (cond
        [(port-closed? port) eof]
        [else
         (define expected-byte-count (+ (* n sample-size-bytes) missed-bytes))
         (define sample-bytes (make-bytes expected-byte-count))
         (define bytes-read (peek-bytes-avail! sample-bytes 0 #f port))
         (cond
           [(integer? bytes-read)
            (push-from-bytes buf sample-bytes bytes-read #t)
            (set! missed-bytes (- expected-byte-count bytes-read))
            #t]
           [else eof])])))
  (if deplete
      (depleting-read)
      normal-read))

(define/contract (pcm-args vcfg listener)
  (-> vconfig? tcp-listener? list?)
  `[
    "-af" ,(format "aresample=~a" (vconfig-rate vcfg))
    "-ac" "1"
    "-f" "data"
    "-c" ,(if (system-big-endian?) "pcm_f64be" "pcm_f64le")
    ,(apply format "tcp://~a:~a?" (listener->addr+port listener))])

(define (get-pcm-ports ffmpeg-port listener)
  (define ffmpeg-port-closed (port-closed-evt ffmpeg-port))
  (define pcm-ports-or-closed-port
    (sync (tcp-accept-evt listener)
          ffmpeg-port-closed))
  (when (eq? ffmpeg-port-closed pcm-ports-or-closed-port)
    (exit 1))
  (match-define (list pcm-port tcp-out) pcm-ports-or-closed-port)
  (values pcm-port tcp-out))

(define (format-tcp port+addr [args ""])
  (format "tcp://~a:~a~a" (first port+addr) (second port+addr) args))

(begin ;; GUI output
  (define-runtime-module-path gui-path "gui.rkt")
  (define ((gui-output-handler [play #t]) vcfg)
    (define (gui-require s) (dynamic-require gui-path s))
    ((gui-require 'init-gui))

    (define pcm-listener (tcp-listen 0 1 #f loopback-addr))

    (define-values (read-next! close)
      (cond
        [play
         (define mpegts-port (make-ephemeral-address))
         (define ffmpeg-port
           ((vconfig-input vcfg)
            `["-f" "mpegts" ,(format-tcp mpegts-port "?listen")]
            (pcm-args vcfg pcm-listener)))

         ;; cross our fingers and hope that they remain sufficiently in sync...
         (sleep 3) ;; sleep 3 seconds so the port is awake >:(
         (define-values (_proc ffplay-in ffplay-out)
           (ffmpeg*
            #:command "ffplay"
            `["-nodisp" "-autoexit" "-f" "mpegts" "-i" ,(format-tcp mpegts-port)]))

         (define-values (pcm-port tcp-out) (get-pcm-ports ffmpeg-port pcm-listener))

         (define read-next! (read-next-from-port pcm-port vcfg))
         (define (close)
           (close-input-port ffmpeg-port)
           (close-input-port ffplay-out)

           (close-input-port pcm-port)
           (close-output-port tcp-out)
           (close-output-port ffplay-in))
         (values read-next! close)]
        [else
         (define ffmpeg-port ((vconfig-input vcfg) (pcm-args vcfg pcm-listener)))
         (define-values (pcm-port tcp-out) (get-pcm-ports ffmpeg-port pcm-listener))
         (define read-next! (read-next-from-port pcm-port vcfg))
         (define (close)
           (close-input-port ffmpeg-port)
           (close-input-port pcm-port)
           (close-output-port tcp-out))
         (values read-next! close)]))

    (sleep (vconfig-delayc vcfg))
    (define fps (fps-event (vconfig-fps vcfg)))
    (sync fps)
    (values
     read-next!
     (gui-require 'put-frame!)
     (lambda () (close))
     fps
     (lambda (_)
       #;
       (parameterize ([current-namespace (make-base-namespace)])
         (read-eval-print-loop))
       (void)))))

(begin ;; file output
  (define ((save-output out-file) vcfg)
    (define max-fps (vconfig-fps vcfg))
    (define pcm-listener (tcp-listen 0 1 #f loopback-addr))
    (define mpegts-listener (tcp-listen 0 1 #f loopback-addr))
    (define ffmpeg-port
      ((vconfig-input vcfg)
       (pcm-args vcfg pcm-listener)
       `["-f" "mpegts" ,(format-tcp (listener->addr+port mpegts-listener))]))
    (define encoder-thread
      (thread
       (lambda ()
         (define mpegts-out-listener (tcp-listen 0 1 #f loopback-addr))
         (define stdin* #f)
         (define proc* #f)
         (define (get-stdin width height)
           (unless stdin*
             (define-values (ffmpeg-proc ffmpeg-stdin _ffmpeg-stdout)
               (ffmpeg*
                `[
                  "-f" "mpegts" "-i" ,(format-tcp (listener->addr+port mpegts-out-listener))

                  "-r" ,(format "~a" max-fps)
                  "-f" "image2pipe"
                  "-s" ,(format "~ax~a"
                                (inexact->exact (floor width))
                                (inexact->exact (floor height)))
                  "-i" "pipe:0"

                  "-vf" "pad=ceil(iw/2)*2:ceil(ih/2)*2:color=white"

                  "-vcodec" "libx264"
                  "-crf" "25"
                  "-pix_fmt" "yuv420p"
                  ,out-file]))
             (set! stdin* ffmpeg-stdin)
             (set! proc* ffmpeg-proc))
           stdin*)
         (thread
          (lambda ()
            (define-values (mpegts-in mpegts-tcp-out) (tcp-accept mpegts-listener))
            (define-values (mpegts-tcp-in mpegts-out) (tcp-accept mpegts-out-listener))
            (copy-port mpegts-in mpegts-out)
            (close-output-port mpegts-out)))
         (let loop ()
           (define frame (thread-receive))
           (when frame
             (match-define (list bs width height _ _) (convert frame 'png-bytes+bounds))
             (write-bytes bs (get-stdin width height))
             (loop)))
         (when stdin*
           (close-output-port stdin*)
           (subprocess-wait proc*)))))
    (define-values (pcm-port tcp-out) (get-pcm-ports ffmpeg-port pcm-listener))
    (define read-next! (read-next-from-port pcm-port vcfg))
    (values
     read-next!
     (lambda (frame)
       (thread-send encoder-thread frame)
       (void))
     (lambda () ;; finish
       (close-input-port ffmpeg-port)
       (close-output-port tcp-out)
       (close-input-port pcm-port)
       (thread-send encoder-thread #f)
       (thread-wait encoder-thread))
     (wrap-evt always-evt (lambda (_) (/ 1000 max-fps)))
     thread-wait)))
