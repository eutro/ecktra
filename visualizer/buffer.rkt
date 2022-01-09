#lang racket/base

(require racket/cmdline
         "../foreign/decode.rkt"
         "../util/ringbuf.rkt"
         "signal.rkt"
         ffi/unsafe
         racket/flonum
         racket/system)
(provide start-buffering)

(define (parse-cl)
  (define astream
    (command-line
     #:once-any
     [("-r" "--rate")
      sample-rate
      "Set sample rate (default 441000)"
      (current-sample-rate (string->number sample-rate))]
     #:args (filename)
     (begin
       (process* (find-executable-path "ffplay") "-nodisp" filename)
       (ecktra-decode-audio-file (current-sample-rate) filename))))
  astream)

(define (real->s16 x)
  (inexact->exact (round (* 32767 x))))

(define (start-buffering buf)
  (define astream (parse-cl))
  (define bastream (ecktra-start-buffering astream))
  (define rate (current-sample-rate))
  (stream-close! astream)

  (define (read-next n)
    (define cbuf (ecktra-buffered-stream-current-buffered bastream))
    (when (< cbuf n) (ecktra-buffered-stream-ensure-buffered bastream (* rate 5)))
    (define flv (make-flvector n))
    (define unread (ecktra-buffered-stream-read bastream (flvector->cpointer flv) n 1))
    (define (ref i) (flvector-ref flv i))
    (ring-buffer-push-all! buf n ref)
    unread)
  read-next)
