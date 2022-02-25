#!/usr/bin/env racket
#lang ecktra/visualizer

(require racket/flonum racket/math racket/vector math pict)

(define sample-rate (current-sample-rate))
(define bufsz (expt 2 16))
(define halfbuf (floor (/ bufsz 2)))
(define minfreq 30)
(define maxfreq 1000)
(define minidx (frequency->bucket minfreq sample-rate bufsz))
(define maxidx (frequency->bucket maxfreq sample-rate bufsz))
(define log-minidx (log minidx))
(define log-maxidx (log maxidx))
(define window-size 512)
(define hann-window (make-hann-window bufsz))
(define tilt-window
  (for/flvector #:length (- maxidx minidx) ([i (in-range minidx maxidx)])
    (cast (log i) Flonum)))
(define uvs
  (for/vector : (Vectorof Complex)
      #:length (- maxidx minidx)
      ([i (in-range minidx maxidx)])
      (make-polar 1 (remap i minidx maxidx 0 (* -2 pi)))))
(define bucketise (bucketise-with (logarithmic-posns minidx maxidx 0 (- maxidx minidx)) min))

#:backbuf halfbuf
#:latency halfbuf

(bind ct : Time current-time)
(define offset-angle (make-polar 1 (/ ct sample-rate pi)))
(bind frame : FlVector (time-travel-backward halfbuf (sliding-window-flvector bufsz (samples))))
(void (flvector-map! * frame hann-window))
(define scale (- 0.8 (/ (log (flvector-rms frame)))))
(define view : FlVector (flvector-copy (flvector-idft-mag! frame) minidx maxidx))
(void (flvector-map! * view tilt-window)
      (set! view (bucketise view))
      (flvector-set! view (- maxidx minidx 1) (flvector-ref view 0)) ;; ensure continuity
      (flvector-map! (fllerp 100.0 (* 0.5 window-size)) view))
(define points : (Listof (Pair Real Real))
  (for/list ([y (in-flvector view)]
             [v (in-vector uvs)])
    (define mv (* offset-angle scale y v))
    (cons (+ (/ window-size 2) (real-part mv))
          (- (/ window-size 2) (imag-part mv)))))
(pure (vl-append
       (text (format "FPS: ~a" (real->decimal-string (current-fps) 2)))
       (plot-2d-points window-size window-size points)))
