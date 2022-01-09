#!/usr/bin/env racket
#lang ecktra/visualizer

(require racket/flonum racket/math
         math
         racket/vector
         pict)

(define sample-rate (current-sample-rate))
(define bufsz (expt 2 16))
(define halfbuf (floor (/ bufsz 2)))
(define freq-res (cast (/ sample-rate bufsz) Nonnegative-Exact-Rational)) ;; Δf
(define (idx->freq [x : Nonnegative-Integer]) (* x freq-res)) ;; f = i*Δf
(define (freq->idx [f : Nonnegative-Integer]) (/ f freq-res)) ;; i = f/Δf
(define minfreq 30)
(define maxfreq 1000)
(define minidx (floor (freq->idx minfreq)))
(define maxidx (ceiling (freq->idx maxfreq)))
(define (offset-idx->freq [x : Nonnegative-Integer]) (idx->freq (+ minidx x)))
(define log-minidx (log minidx))
(define log-maxidx (log maxidx))
(define log-window (- log-maxidx log-minidx))
(define window-width 1024)
(define window-height 400)
(define (spread-idx [x : Nonnegative-Integer]) : Real
  (define logged (log x))
  (define pos (/ (- logged log-minidx) log-window))
  (* pos window-width))
(define hann-window
  (for/flvector #:length bufsz ([i (in-range bufsz)])
    (cast (exact->inexact (expt (sin (/ (* pi i) bufsz)) 2.0)) Flonum)))
(define tilt-window
  (for/flvector #:length (- maxidx minidx) ([i (in-range minidx maxidx)])
    (cast (log i) Flonum)))

#:backbuf halfbuf
#:latency halfbuf

(bind frame : FlVector (time-travel-backward halfbuf (sliding-window-flvector bufsz (samples))))
(void ;; windowing function
 (for ([(s i) (in-indexed (in-flvector frame))]
       [h (in-flvector hann-window)])
   (flvector-set! frame i (* s h))))
(define view : FlVector (flvector-copy (flvector-idft-mag! frame) minidx maxidx))
(define points : (Listof (Pair Real Real))
  (for/list ([(y x) (in-indexed (in-flvector view))]
             [t (in-flvector tilt-window)])
    (cons (spread-idx (cast (+ minidx x) Nonnegative-Integer))
          (- (* 2/3 window-height) (* t y window-height)))))
(pure (vl-append
       (text (format "FPS: ~a" (real->decimal-string (current-fps) 2)))
       (plot-2d-points window-width window-height points)))
