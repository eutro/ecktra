#lang typed/racket/base

(require "../ringbuf.rkt"
         math/array
         math/base
         racket/sequence
         racket/math)

(provide sine-window
         power-of-sine-window
         hann-window
         rect-window

         transform-window

         transform-inverse-dft-single
         transform-inverse-dft

         transform-dft-single
         transform-dft)

(: sine-window (-> Number Number))
(define (sine-window t)
  (cos (* t 1/2 pi)))

(: power-of-sine-window (-> Number (-> Number Number)))
(define ((power-of-sine-window a) t)
  (expt (sine-window t) a))

(: hann-window (-> Number Number))
(define hann-window (procedure-rename (power-of-sine-window 2) 'hann-window))

(: rect-window (-> Any Number))
(define (rect-window _) 1)

(: transform-window
   (->* (Nonnegative-Integer
         Nonnegative-Integer)
        ((-> Real Number))
        (-> (Sequenceof Number)
            (Sequenceof (Vectorof Number)))))
(define ((transform-window window-size samples-per-frame [window-fn rect-window]) samples)
  (make-do-sequence
   (lambda ()
     (define-values (has-sample? next-sample!) (sequence-generate samples))
     (: rbuf (RingBuffer Number))
     (define rbuf (make-ring-buffer window-size window-size 0))
     (define window-arr
       (parameterize ([array-strictness #t])
         (define half-window (/ window-size 2))
         (for/array #:shape (vector window-size)
             ([i (in-range window-size)])
           : Number (window-fn (sub1 (/ i half-window))))))
     (define windowed-sample-arr
       (parameterize ([array-strictness #f])
         (inline-array-map * (ring-buffer-array rbuf) window-arr)))
     (define remaining-in-window window-size)
     (define (step!)
       (define out-of-range
         (for/fold : Integer
             ([out-of-range 0])
             ([_ (in-range samples-per-frame)])
           (define-values (sample oor)
             (if (and (zero? out-of-range) (has-sample?))
                 (values (next-sample!) 0)
                 (values 0 (add1 out-of-range))))
           (ring-buffer-push! rbuf sample)
           oor))
       (unless (zero? out-of-range)
         (set! remaining-in-window (- remaining-in-window out-of-range)))
       (void))
     (: get-dft (-> (Vectorof Number)))
     (define (get-dft) (array->vector windowed-sample-arr))
     (define (is-finished?)
       (< remaining-in-window 0))
     (values
      (lambda ([x : (Vectorof Number)]) x)
      (lambda (_) (begin (step!) (get-dft)))
      (get-dft)
      (lambda (_) (not (is-finished?)))
      #f
      #f))))

(: transform-inverse-dft-single (-> (Sequenceof Number) (Sequenceof Number)))
(define (transform-inverse-dft-single samples)
  (array->vector (array-axis-inverse-fft (for/array ([s samples]) : Number s) 0)))

(: transform-inverse-dft (-> (Sequenceof (Sequenceof Number)) (Sequenceof (Sequenceof Number))))
(define (transform-inverse-dft sample-frames)
  (sequence-map transform-inverse-dft-single sample-frames))

(: transform-dft-single (-> (Sequenceof Number) (Sequenceof Number)))
(define (transform-dft-single samples)
  (array->vector (array-axis-fft (for/array ([s samples]) : Number s) 0)))

(: transform-dft (-> (Sequenceof (Sequenceof Number)) (Sequenceof (Sequenceof Number))))
(define (transform-dft sample-frames)
  (sequence-map transform-dft-single sample-frames))
