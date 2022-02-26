#!/usr/bin/env racket
#lang ecktra/visualizer

(require racket/math racket/flonum threading)

(define bufsz (expt 2 12))
(define halfbuf (quotient bufsz 2))
(define window-size 512)
(define angles
  (for/vector : (Vectorof Complex) #:length bufsz
      ([i (in-range bufsz)])
      (make-polar (/ window-size 4) (* 2 pi (/ i bufsz)))))

#:backbuf halfbuf
#:latency halfbuf

(bind frame : FlVector
      (~> (samples)
          (biquad-filter _ 'lowpass 400.0 0.0 0.0)
          (signal-buffer _ 0.0 bufsz)
          (sliding-window-flvector bufsz _)
          (time-travel-backward halfbuf _)))
(define points
  (for/list : (Listof (Pair Real Real))
      ([s (in-flvector frame)]
       [a (in-vector angles)])
    (define z (* (+ 1 (/ s 2)) a))
    (cons (+ (/ window-size 2) (real-part z))
          (- (/ window-size 2) (imag-part z)))))
(pure (plot-2d-points window-size window-size points))
