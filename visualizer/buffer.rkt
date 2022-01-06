#lang racket/base

(require racket/cmdline
         "../foreign/decode.rkt"
         "ringbuf.rkt"
         "signal.rkt"
         ffi/unsafe)
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
     (ecktra-decode-audio-file (current-sample-rate) filename)))
  astream)

(define (start-buffering buf)
  (define astream (parse-cl))
  (define left #f)
  (define (read-next!) (ecktra-stream-read astream))
  (define (read-next n)
    (define (loop n nxt)
      (cond
        [(eof-object? nxt) n]
        [else
         (define nbuf (car nxt))
         (define nsz (cdr nxt))
         (define (ref-nxt i) (ptr-ref nbuf _double i))
         (cond
           [(> n nsz)
            (ring-buffer-push-all! buf nsz ref-nxt)
            (loop (- n nsz) (read-next!))]
           [(< n nsz)
            (ring-buffer-push-all! buf n ref-nxt)
            (define remaining-nxt (- nsz n))
            (set! left (cons (ptr-add nbuf n) nsz))
            0]
           [else
            (ring-buffer-push-all! buf n ref-nxt)
            (set! left #f)
            0])]))
    (loop n (or left (read-next!))))
  read-next)
