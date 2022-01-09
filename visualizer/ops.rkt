#lang typed/racket/base

(require typed/racket/unsafe
         racket/math
         racket/flonum)
(unsafe-require/typed
 "../foreign/fft.rkt"
 [fft! (->* (Boolean
             FlVector)
            ((Option FlVector)
             (Option FlVector)
             (Option FlVector))
            Void)])

(provide (all-defined-out))

(module untyped racket/base
  (provide (all-defined-out))
  (define (unsafe-cast x) x))
(unsafe-require/typed
 'untyped
 [unsafe-cast (All (A B) (-> A B))])
(provide unsafe-cast)

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

(: flvector-map!
   (case->
    [(-> Flonum Flonum) FlVector -> FlVector]
    [(-> Flonum Flonum Flonum) FlVector FlVector -> FlVector]
    [(-> Flonum Flonum Flonum Flonum) FlVector FlVector FlVector -> FlVector]))
(define flvector-map!
  (case-lambda
    [(f v)
     (for ([(x i) (in-indexed (in-flvector v))])
       (flvector-set! v i (f x)))
     v]
    [(f v1 v2)
     (for ([i (in-naturals)]
           [x1 (in-flvector v1)]
           [x2 (in-flvector v2)])
       (flvector-set! v1 i (f x1 x2)))
     v1]
    [(f v1 v2 v3)
     (for ([i (in-naturals)]
           [x1 (in-flvector v1)]
           [x2 (in-flvector v2)]
           [x3 (in-flvector v3)])
       (flvector-set! v1 i (f x1 x2 x3)))
     v1]))

(: flvector-map-to-vector (All (T) (-> (-> Flonum T) FlVector (Vectorof T))))
(define (flvector-map-to-vector f v)
  (for/vector : (Vectorof T)
      #:length (flvector-length v)
      ([x (in-flvector v)])
      (f x)))

(: fllerp
   (case->
    [Flonum Flonum Flonum -> Flonum]
    [Flonum Flonum -> (-> Flonum Flonum)]))
(define fllerp
  (case-lambda
    [(x minv maxv)
     (+ minv (* x (- maxv minv)))]
    [(minv maxv)
     (λ (x) (fllerp x minv maxv))]))

(: flunlerp
   (case->
    [Flonum Flonum Flonum -> Flonum]
    [Flonum Flonum -> (-> Flonum Flonum)]))
(define flunlerp
  (case-lambda
    [(x minv maxv)
     (/ (- x minv) (- maxv minv))]
    [(minv maxv)
     (λ (x) (flunlerp x minv maxv))]))

(: flremap
   (case->
    [Flonum Flonum Flonum Flonum Flonum -> Flonum]
    [Flonum Flonum Flonum Flonum -> (-> Flonum Flonum)]))
(define flremap
  (case-lambda
    [(x min-in max-in min-out max-out)
     (fllerp (flunlerp x min-in max-in) min-out max-out)]
    [(min-in max-in min-out max-out)
     (λ (x) (flremap x min-in max-in min-out max-out))]))

(: lerp
   (case->
    [Real Real Real -> Real]
    [Real Real -> (-> Real Real)]))
(define lerp
  (case-lambda
    [(x minv maxv)
     (+ minv (* x (- maxv minv)))]
    [(minv maxv)
     (λ (x) (lerp x minv maxv))]))

(: unlerp
   (case->
    [Real Real Real -> Real]
    [Real Real -> (-> Real Real)]))
(define unlerp
  (case-lambda
    [(x minv maxv)
     (/ (- x minv) (- maxv minv))]
    [(minv maxv)
     (λ (x) (unlerp x minv maxv))]))

(: remap
   (case->
    [Real Real Real Real Real -> Real]
    [Real Real Real Real -> (-> Real Real)]))
(define remap
  (case-lambda
    [(x min-in max-in min-out max-out)
     (lerp (unlerp x min-in max-in) min-out max-out)]
    [(min-in max-in min-out max-out)
     (λ (x) (remap x min-in max-in min-out max-out))]))

(: make-hann-window (-> Integer FlVector))
(define (make-hann-window N)
  (for/flvector #:length N ([i (in-range N)])
    (cast (exact->inexact (expt (sin (/ (* pi i) N)) 2.0)) Flonum)))

(: frequency-resolution
   (-> Nonnegative-Integer
       Nonnegative-Integer
       Nonnegative-Exact-Rational))
(define (frequency-resolution sample-rate bufsz)
  (/ sample-rate bufsz))

(: frequency->bucket
   (-> Nonnegative-Exact-Rational
       Nonnegative-Integer
       Nonnegative-Integer
       Nonnegative-Integer))
(define (frequency->bucket f sample-rate bufsz)
  (exact-round (/ (* f bufsz) sample-rate)))

(: bucket->frequency
   (-> Nonnegative-Integer
       Nonnegative-Integer
       Nonnegative-Integer
       Nonnegative-Exact-Rational))
(define (bucket->frequency b sample-rate bufsz)
  (/ (* b sample-rate) bufsz))

(: flvector-rms (-> FlVector Flonum))
(define (flvector-rms flv)
  (/ (for/fold : Flonum ([s 0.0]) ([x (in-flvector flv)])
       (+ s (expt x 2)))
     (flvector-length flv)))
