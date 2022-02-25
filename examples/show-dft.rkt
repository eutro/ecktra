#!/usr/bin/env racket
#lang ecktra/visualizer

(require math pict racket/function)

(define sample-rate (current-sample-rate))
(define bufsz (expt 2 16))
(define halfbuf (floor (/ bufsz 2)))
(define minfreq 30)
(define maxfreq 4000)
(define minidx (frequency->bucket minfreq sample-rate bufsz))
(define maxidx (frequency->bucket maxfreq sample-rate bufsz))
(define window-width 1024)
(define window-height 400)
(define hann-window (make-hann-window bufsz))
(define tilt-window
  (for/flvector #:length (- maxidx minidx) ([i (in-range minidx maxidx)])
    (fllog (exact->inexact i))))
(define bucketise (bucketise-with (logarithmic-posns minidx maxidx 0 window-width) min))

#:backbuf halfbuf
#:latency halfbuf

(bind frame : FlVector (time-travel-backward halfbuf (sliding-window-flvector bufsz (samples))))
(void (flvector-map! * frame hann-window))
(define view : FlVector (flvector-copy (flvector-idft-mag! frame) minidx maxidx))
(void (flvector-map! * view tilt-window)
      (flvector-map! (fllerp (* 0.66 window-height) 0.0) view))
(define points : (Listof (Pair Real Real))
  (for/list ([y (in-flvector (bucketise view))]
             [x (in-range window-width)])
    (cons x y)))
(pure (vl-append
       (text (format "FPS: ~a" (real->decimal-string (current-fps) 2)))
       (plot-2d-points window-width window-height points)))
