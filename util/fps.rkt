#lang racket/base

(provide fps-event)

(define (fps-event fps)
  (define evt-box (box always-evt))
  (define (maker) (unbox evt-box))
  (define last-t (current-inexact-milliseconds))
  (define (wrap res)
    (define t (current-inexact-milliseconds))
    (set-box! evt-box (alarm-evt (+ t (/ 1000 fps))))
    (define ret (- t last-t))
    (set! last-t t)
    ret)
  (define ret (wrap-evt (guard-evt maker) wrap))
  (sync ret)
  ret)

(module* typed typed/racket/base
  (require/typed/provide (submod "..")
    [fps-event (-> Positive-Exact-Rational (Evtof Real))]))
