#lang typed/racket/base

(require typed/racket/unsafe
         racket/flonum)
(unsafe-require/typed
 "../foreign/fft.rkt"
 [fft! (->* (FlVector
             Boolean)
            ((Option FlVector)
             (Option FlVector)
             (Option FlVector))
            Void)])

(provide flvector-dft-mag!
         flvector-dft-mag*
         flvector-idft-mag!
         flvector-idft-mag*)

(: flvector-dft-mag! (-> FlVector FlVector))
(define (flvector-dft-mag! v)
  (fft! v #false #f v)
  v)

(: flvector-idft-mag! (-> FlVector FlVector))
(define (flvector-idft-mag! v)
  (fft! v #true #f v)
  v)

(: flvector-dft-mag* (-> FlVector FlVector))
(define (flvector-dft-mag* v)
  (flvector-dft-mag! (flvector-copy v)))

(: flvector-idft-mag* (-> FlVector FlVector))
(define (flvector-idft-mag* v)
  (flvector-idft-mag! (flvector-copy v)))
