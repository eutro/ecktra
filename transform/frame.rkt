#lang racket/base

(require racket/sequence racket/stream "../seq.rkt")

(provide transform-mean
         transform-sum
         transform-rms
         transform-map
         transform-drop
         transform-take
         transform-decimate)

(define (transform-sum samples)
  (for/sum ([x samples]) x))

(define (transform-mean samples)
  (define-values (len total)
    (for/fold ([len 0]
               [total 0])
              ([s samples])
      (values (add1 len)
              (+ total s))))
  (/ total len))

(define (transform-rms samples)
  (define (square x) (expt x 2))
  (sqrt (transform-mean (sequence-map square samples))))

(define ((transform-map transform) sample-frames)
  (sequence-map transform sample-frames))

(define ((transform-drop n) samples)
  (sdrop samples n))

(define ((transform-take n) samples)
  (stake samples n))

(define ((transform-decimate factor) samples)
  (let loop ([samples samples])
    (define taken (stake samples factor))
    (cond
      [(uncons taken)
       =>
       (lambda (x) (stream-cons (car x) (loop (sdrop samples factor))))]
      [else empty-stream])))
