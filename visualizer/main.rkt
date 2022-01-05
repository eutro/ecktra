#lang typed/racket/base

(require "signal.rkt"
         "lang.rkt"
         "ringbuf.rkt")

(require/typed "buffer.rkt"
  [start-buffering (-> (RingBuffer Flonum) (-> Integer Integer Void))])

(provide start-with)

(: start-with (-> (-> (Signal Any)) Void))
(define (start-with f)
  (define out (f))
  (: ringbuf RingBuffer)
  (: start StartFn)
  (define (start backbuf latency)
    (define bufsz (* 2 (+ backbuf latency)))
    (set! rb (make-ring-buffer bufsz))
    (start-buffering)
    (pure 1.0))
  (define produce (signal-produce out))
  (for ([i (in-naturals)])
    (writeln (produce i))))
