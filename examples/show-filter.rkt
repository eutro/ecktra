#!/usr/bin/env racket
#lang ecktra/visualizer

(require math pict racket/cmdline threading)

(define sample-rate (current-sample-rate))
(define bufsz (expt 2 16))
(define halfbuf (floor (/ bufsz 2)))
(define hann-window (make-hann-window bufsz))
(define window-width 1024)
(define window-height 400)

(define minfreq 20)
(define maxfreq 20000)
(define freq 200.0)
(define q 1.0)
(define gain 0.0)
(define type : FilterType 'lowpass)

#:backbuf halfbuf
#:latency halfbuf

(define (string->positive-flonum x)
  (real->double-flonum
   (magnitude
    (or (string->number (cast x String))
        (raise-user-error "Not a number")))))
(void
 (command-line
  #:once-each
  ("--freq" freqv "Frequency" (set! freq (string->positive-flonum freqv)))
  ("--quality" qv "Quality" (set! q (string->positive-flonum qv)))
  ("--gain" gainv "Gain" (set! gain (string->positive-flonum gainv)))
  ("--type" typev "Type" (set! type (cast (string->symbol (cast typev String)) FilterType)))))

(define minidx (frequency->bucket minfreq sample-rate bufsz))
(define maxidx (frequency->bucket maxfreq sample-rate bufsz))
(define bucketise (bucketise-with (logarithmic-posns minidx maxidx 0 window-width) min))

(bind frame : FlVector
      (~> (samples)
          (biquad-filter _ type freq q gain)
          (signal-buffer 0.0 bufsz)
          (sliding-window-flvector bufsz _)
          (time-travel-backward halfbuf _)))
(void (flvector-map! * frame hann-window))
(define view : FlVector (flvector-copy (flvector-idft-mag! frame) minidx maxidx))
(void (flvector-map! (fllerp (* 0.66 window-height) 0.0) view))
(define points : (Listof (Pair Real Real))
  (for/list ([y (in-flvector (bucketise view))]
             [x (in-range window-width)])
    (cons x y)))
(pure (vl-append
       (text (format "FPS: ~a" (real->decimal-string (current-fps) 2)))
       (plot-2d-points window-width window-height points)))
