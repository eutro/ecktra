#lang typed/racket/base

(require/typed "../foreign/fft.rkt"
  [ecktra-fft-mag (-> FlVector Boolean FlVector)])

;; TODO (i)dft, bulk flvector ops, etc.
