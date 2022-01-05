#lang racket/base

(require ffi/unsafe "foreign.rkt" racket/flonum)

(define-ecktra ecktra-fft-mag
  (_fun (flv inverse) ::
        [(flvector->cpointer flv) : _pointer]
        [(flvector-length flv) : _int]
        [(if inverse 1 0) : _int]
        -> [ret : _int]
        ->
        (cond
          [(zero? ret) flv]
          [else (raise-argument-error 'ecktra-fft-mag "power-of-two?" (flvector-length flv))])))
