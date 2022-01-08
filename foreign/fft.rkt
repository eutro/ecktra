#lang racket/base

(require ffi/unsafe "foreign.rkt" racket/flonum)
(provide fft!)

(define-ecktra ecktra-fft
  (_fun [real : _pointer]
        [imag : _pointer]
        [len : _int]
        [inverse : _int]
        [mag : _pointer]
        [arg : _pointer]
        -> [ret : _int]
        ->
        (cond
          [(zero? ret) (void)]
          [else (raise-argument-error 'ecktra-fft-mag "power-of-two?" len)])))

(define (fft! inverse real [imag #f] [mag #f] [arg #f])
  (unless (flvector? real)
    (raise-argument-error 'fft! "flvector?" real))
  (define len (flvector-length real))
  (let ()
    (define (flvector-or-null? x)
      (or (flvector? x) (not x)))
    (define (correct-length? v)
      (or (not v) (= (flvector-length v) len)))
    (define to-check (list imag mag arg))
    (unless (andmap flvector-or-null? to-check)
      (raise-arguments-error 'fft! "expected flvector? or #f"
                             "real" real
                             "imag" imag
                             "mag" mag
                             "arg" arg))
    (unless (andmap correct-length? to-check)
      (raise-arguments-error 'fft! "expected flvectors of same length"
                             "real" real
                             "imag" imag
                             "mag" mag
                             "arg" arg)))
  (define rp (flvector->cpointer real))
  (define ip (and imag (flvector->cpointer imag)))
  (define mp (and mag (flvector->cpointer mag)))
  (define ap (and arg (flvector->cpointer arg)))
  (ecktra-fft rp ip len (if inverse 1 0) mp ap))
