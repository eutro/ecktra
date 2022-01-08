#lang typed/racket/base

(require typed/racket/unsafe
         racket/flonum)
(unsafe-require/typed
 "../foreign/fft.rkt"
 [fft! (->* (Boolean
             FlVector)
            ((Option FlVector)
             (Option FlVector)
             (Option FlVector))
            Void)])

(provide flvector-dft-mag!
         flvector-dft-mag*
         flvector-idft-mag!
         flvector-idft-mag*
         flvector-map!)

(: flvector-dft-mag! (-> FlVector FlVector))
(define (flvector-dft-mag! v)
  (fft! #false v #f v)
  v)

(: flvector-idft-mag! (-> FlVector FlVector))
(define (flvector-idft-mag! v)
  (fft! #true v #f v)
  v)

(: flvector-dft-mag* (-> FlVector FlVector))
(define (flvector-dft-mag* v)
  (flvector-dft-mag! (flvector-copy v)))

(: flvector-idft-mag* (-> FlVector FlVector))
(define (flvector-idft-mag* v)
  (flvector-idft-mag! (flvector-copy v)))

(: flvector-map! (-> (-> Flonum Flonum) FlVector FlVector))
(define (flvector-map! f v)
  (for ([(x i) (in-indexed (in-flvector v))])
    (flvector-set! v i (f x)))
  v)
