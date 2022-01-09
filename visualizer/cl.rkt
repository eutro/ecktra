#lang racket/base

(require racket/cmdline
         racket/system
         racket/flonum
         racket/runtime-path
         racket/match
         ffi/unsafe
         file/convertible
         "../util/fps.rkt"
         "../util/ringbuf.rkt"
         "../foreign/decode.rkt"
         "signal.rkt")

(provide parse-cl)

(struct parsed-cl
  (file-name output-file max-fps))

(define (get-parsed-cl)
  (define output-file* (box #f))
  (define max-fps* (box 30))
  (define file-name
    (command-line
     #:once-each
     [("-r" "--rate")
      sample-rate
      "Set sample rate (default 44100)"
      (current-sample-rate (string->number sample-rate))]
     [("-o" "--out" "--output")
      output-file
      "Set output file"
      (set-box! output-file* output-file)]
     [("--fps" "--max-fps")
      max-fps
      "Set the maximum FPS"
      (set-box! max-fps* (string->number max-fps))]
     #:args (file-name)
     file-name))
  (parsed-cl
   file-name
   (unbox output-file*)
   (unbox max-fps*)))

(define (parse-cl)
  (define parse (get-parsed-cl))
  (define finish-hooks (box null))
  (define (finish)
    (for ([f (in-list (unbox finish-hooks))])
      (f)))

  (define read-next! (start-buffering parse finish-hooks))
  (define-values (put-frame! consume-thread fps) (init-output parse finish-hooks))
  (values read-next! put-frame! finish fps consume-thread))

(define-runtime-module-path gui-path "gui.rkt")

(define (init-output parse finish-hooks)
  (match-define (parsed-cl file-name out-file max-fps) parse)
  (cond
    [out-file
     (define counter 0)
     (define encoder-thread
       (thread
        (lambda ()
          (match-define (list #f ; stdout
                              ffmpeg-stdin
                              _ ; pid
                              #f ; stderr
                              _ ; ffmpeg-control
                              )
            (process*/ports
             (current-output-port)
             #f
             (current-error-port)
             (find-executable-path "ffmpeg")
             "-i" file-name
             "-r" (format "~a" max-fps)
             "-f" "image2pipe"
             "-s" "1024x400"
             "-i" "-"
             "-vcodec" "libx264"
             "-crf" "25"
             "-pix_fmt" "yuv420p"
             out-file))
          (let loop ()
            (define frame (thread-receive))
            (when frame
              (define png-bytes (convert frame 'png-bytes))
              (write-bytes png-bytes ffmpeg-stdin)
              (set! counter (add1 counter))
              (loop)))
          (close-output-port ffmpeg-stdin))))
     (define (finish)
       (thread-send encoder-thread #f)
       (thread-wait encoder-thread))
     (set-box! finish-hooks (cons finish (unbox finish-hooks)))
     (values
      (lambda (frame)
        (thread-send encoder-thread frame)
        (void))
      thread-wait
      (wrap-evt always-evt (lambda (_) (/ 1000 max-fps))))]
    [else
     (define (gui-require s)
       (dynamic-require gui-path s))
     ((gui-require 'init-gui))
     (values
      (gui-require 'put-frame!)
      (lambda (_)
        (process* (find-executable-path "ffplay") "-nodisp" (parsed-cl-file-name parse))
        (void))
      (fps-event max-fps))]))

(define (start-buffering parse finish-hooks)
  (define astream (ecktra-decode-audio-file (current-sample-rate) (parsed-cl-file-name parse)))
  (define bastream (ecktra-start-buffering astream))
  (define rate (current-sample-rate))
  (stream-close! astream)
  (define (read-next! buf n)
    (define cbuf (ecktra-buffered-stream-current-buffered bastream))
    (when (< cbuf n) (ecktra-buffered-stream-ensure-buffered bastream (* rate 5)))
    (define flv (make-flvector n))
    (define unread (ecktra-buffered-stream-read bastream (flvector->cpointer flv) n 1))
    (define (ref i) (flvector-ref flv i))
    (ring-buffer-push-all! buf n ref)
    unread)
  read-next!)
