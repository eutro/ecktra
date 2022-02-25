#!/usr/bin/env racket
#lang ecktra/visualizer

(require math
         racket/runtime-path
         typed/pict)
(begin
  (require/typed rsvg
    [svg-file->pict (->* (Path-String) (Real) pict)])
  (require/typed pict
    [translate (-> pict Real Real pict)]))

(define sample-rate (current-sample-rate))
(define bufsz (expt 2 16))
(define halfbuf (quotient bufsz 2))
(define minfreq 30)
(define maxfreq 4000)
(define minidx (frequency->bucket minfreq sample-rate bufsz))
(define maxidx (frequency->bucket maxfreq sample-rate bufsz))
(define window-width 1024)
(define window-height 400)
(define padding 32)
(define hann-window (make-hann-window bufsz))
(define tilt-window
  (for/flvector #:length (- maxidx minidx) ([i (in-range minidx maxidx)])
    (fllog (exact->inexact i))))

(begin (define-runtime-path star-path "star.svg"))
(define star (svg-file->pict star-path))

(define star-count 128)
(define star-posns
  (for/vector : (Vectorof (Pairof Real Real))
      #:length star-count
      ([_ (in-range star-count)])
    (cons (lerp (random) padding (- window-width padding))
          (lerp (random) padding (- window-height padding)))))

(define bucketise (bucketise-with (logarithmic-posns minidx maxidx 0 star-count) min star-count))

#:backbuf halfbuf
#:latency halfbuf

(bind frame : FlVector (time-travel-backward halfbuf (sliding-window-flvector bufsz (samples))))
(void (flvector-map! * frame hann-window))
(define view : FlVector (flvector-copy (flvector-idft-mag! frame) minidx maxidx))
(void (flvector-map! * view tilt-window))

(define image
  (for/fold : pict
      ([cvs (filled-rectangle
             window-width window-height
             #:color "black")])
      ([pos (in-vector star-posns)]
       [a (in-flvector (bucketise view))])
    (define size : Flonum
      (flremap (- (/ (fllog (+ 0.05 (magnitude a)))))
               0.0 1.0
               0.1 8.0))
    (pin-over
     cvs
     (car pos) (cdr pos)
     (freeze (scale star size)))))

(pure image)
