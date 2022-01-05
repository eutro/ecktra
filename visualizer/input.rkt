#lang typed/racket/base

(require "signal.rkt")
(provide process-input)

(: process-input (-> (-> (-> Time Time (Signal Flonum)) (Signal Any)) Void))
(define (process-input f)
  (: make (-> Time Time (Signal Flonum)))
  (define (make b l)
    (: do-sin (-> Time Flonum))
    (define (do-sin t)
      (real->double-flonum (sin (* 2 3.14 t 100 1/441000))))
    (make-signal do-sin (signal-meta l b)))
  (define out (f make))
  (define produce (signal-produce out))
  (for ([i (in-naturals)])
    (writeln (produce i))))
