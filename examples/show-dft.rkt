#!/usr/bin/env racket
#lang ecktra/visualizer

(require racket/flonum racket/math)

(define bufsz (expt 2 16))
(define halfbuf (floor (/ bufsz 2)))
(define minfreq 1)
(define maxfreq 500)
(define (zero-if-nan [x : Flonum]) : Flonum
  (if (nan? x) 0.0 x))
(define (upscale [x : Flonum]) : Flonum
  (* 4 (zero-if-nan x)))

#:backbuf halfbuf
#:latency halfbuf

(bind frame : FlVector (time-travel-backward halfbuf (sliding-window-flvector bufsz (samples))))
(define view (flvector-map! upscale (flvector-copy (flvector-idft-mag! frame) minfreq maxfreq)))
(pure (plot-2d-x-y 1024 400 view))
