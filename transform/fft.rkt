#lang racket/base

(require racket/flonum racket/sequence "../fft.rkt")
(provide transform-dft/magnitude
         transform-dft-single/magnitude
         transform-inverse-dft/magnitude
         transform-inverse-dft-single/magnitude)

(define (->flvector ss)
  (if (vector? ss)
      (for/flvector #:length (vector-length ss) ([x (in-vector ss)]) (exact->inexact x))
      (for/flvector ([x ss]) (exact->inexact x))))

(define (transform-inverse-dft-single/magnitude samples)
  (ecktra-fft-mag (->flvector samples) #t))

(define (transform-inverse-dft/magnitude sample-frames)
  (sequence-map transform-inverse-dft-single/magnitude sample-frames))

(define (transform-dft-single/magnitude samples)
  (ecktra-fft-mag (->flvector samples) #f))

(define (transform-dft/magnitude sample-frames)
  (sequence-map transform-dft-single/magnitude sample-frames))
