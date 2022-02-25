#lang typed/racket/base

(require "signal.rkt"
         racket/math
         racket/flonum
         math/flonum
         racket/set
         "../util/ringbuf.rkt"
         (for-syntax racket/base))
(provide (all-defined-out))

(: current-time (Signal Time))
(define current-time
  (let ()
    (: id (-> Time Time))
    (define (id t) t)
    (make-signal id pure-signal-meta)))

(: sliding-window-by
   (All (T R)
        (-> (-> Integer (-> Integer T) R)
            Time
            (Signal T)
            (Signal R))))
(define (sliding-window-by f len signal)
  (cond
    [(negative? len) (time-travel-forward len (sliding-window-by f (- len) signal))]
    [(zero? len) (pure (f 0 (λ (_) (raise #f))))]
    [else
     (define old-prod (signal-produce signal))
     (: new-prod (-> Time R))
     (define (new-prod t)
       (: at (-> Time T))
       (define (at i) (old-prod (+ t i)))
       (f len at))
     (swap-produce new-prod signal)]))

(: sliding-window-vector (All (T) (-> Time (Signal T) (Signal (Vectorof T)))))
(define (sliding-window-vector len signal)
  (sliding-window-by
   (ann build-vector (-> Integer (-> Integer T) (Vectorof T)))
   len signal))

(: sliding-window-flvector (-> Time (Signal Flonum) (Signal FlVector)))
(define (sliding-window-flvector len signal)
  (: build-flvector (-> Integer (-> Integer Flonum) FlVector))
  (define (build-flvector len f)
    (define flv (make-flvector len))
    (for ([i (in-range len)])
      (flvector-set! flv i (f i)))
    flv)
  (sliding-window-by build-flvector len signal))

(: time-travel-forward (All (T) (-> Time (Signal T) (Signal T))))
(define (time-travel-forward by signal)
  (cond
    [(zero? by) signal]
    [else
     (define old-prod (signal-produce signal))
     (: new-prod (-> Time T))
     (define (new-prod t) (old-prod (+ t by)))
     (swap-produce new-prod signal)]))

(: time-travel-backward (All (T) (-> Time (Signal T) (Signal T))))
(define (time-travel-backward by signal)
  (time-travel-forward (- by) signal))

(: signal-fold (All (A B) (->* [(Signal A) (-> B A B) B] [Positive-Integer] (Signal B))))
(define (signal-fold signal proc dflt [backbuf 1])
  (define buf (make-ring-buffer backbuf dflt))
  (define buftime 0)
  (define old-prod (signal-produce signal))
  (: new-prod (-> Time B))
  (define (new-prod t)
    (cond
      [(<= t buftime)
       (define idx (+ backbuf (- t buftime 1)))
       (when (negative? idx)
         (raise (exn:fail:signal:out-of-range "signal-fold: sample out of range" (current-continuation-marks))))
       (ring-buffer-nth buf idx)]
      [else
       (let loop ()
         (set! buftime (add1 buftime))
         (define acc (ring-buffer-nth buf (sub1 backbuf)))
         (define next-val
           (with-handlers ([exn:fail:signal:out-of-range? (lambda (_) dflt)])
             (proc acc (old-prod buftime))))
         (ring-buffer-push! buf next-val)
         (if (>= buftime t)
             next-val
             (loop)))]))
  (swap-produce new-prod signal))

(: signal-buffer (All (A) (-> (Signal A) A Positive-Integer (Signal A))))
(define (signal-buffer signal dflt bufsz)
  (define buf (make-ring-buffer bufsz dflt))
  (define buftime 0)
  (define old-prod (signal-produce signal))
  (: new-prod (-> Time A))
  (define (new-prod t)
    (cond
      [(<= t buftime)
       (define idx (+ bufsz (- t buftime 1)))
       (when (negative? idx)
         (raise-arguments-error 'signal-buffer
                                "sample out of range"
                                "t" t))
       (ring-buffer-nth buf idx)]
      [else
       (for ([i (in-range (min bufsz (- t buftime)))])
         (ring-buffer-push! buf (old-prod (+ buftime i))))
       (set! buftime t)
       (ring-buffer-nth buf (sub1 bufsz))]))
  (swap-produce new-prod signal))

(define nan-warned : (Boxof (Setof Syntax)) (box (ann (set) (Setof Syntax))))

(: iir-filter* (-> Syntax (-> (Signal Flonum) FlVector FlVector (Signal Flonum))))
(define ((iir-filter* stx) signal feedfw feedbck)
  (define ysz (flvector-length feedbck))
  (define xsz (flvector-length feedfw))

  (define-type AccTy (Pair FlVector FlVector))

  (: fold (-> AccTy Flonum AccTy))
  (define (fold acc in)
    (define oxs (car acc))
    (define oys (cdr acc))
    (define xs (make-flvector xsz))
    (flvector-copy! xs 1 oxs 0 (sub1 xsz))
    (flvector-set! xs 0 in)
    (define ys (make-flvector ysz))
    (flvector-copy! ys 1 oys 0 (sub1 ysz))
    (define out
      (/
       (+
        (for/fold : Flonum
            ([x 0.0])
            ([i (in-range xsz)])
          (+ x
             (* (flvector-ref xs i)
                (flvector-ref feedfw i))))
        (for/fold : Flonum
            ([x 0.0])
            ([i (in-range 1 ysz)])
          (+ x
             (* -1.0
                (flvector-ref ys i)
                (flvector-ref feedbck i)))))
       (flvector-ref feedbck 0)))
    (when (and (nan? out) (not (set-member? (unbox nan-warned) stx)))
      ;; not atomic but the risk is just a repeated warning
      (set-box! nan-warned (set-add (unbox nan-warned) stx))
      (fprintf (current-error-port)
               "Warning: IIR filter at ~a:~a:~a produced NaN\n"
               (syntax-source stx)
               (syntax-line stx)
               (syntax-column stx)))
    (flvector-set! ys 0 out)
    (cons xs ys))

  (: acc AccTy)
  (define acc (cons (make-flvector xsz) (make-flvector ysz)))

  (: last-out (-> AccTy Flonum))
  (define (last-out acc) (flvector-ref (cdr acc) 0))

  (fmap last-out (signal-fold signal fold acc)))

(define-type FilterType (U 'lowpass 'highpass 'bandpass 'lowshelf 'highshelf 'peaking 'notch 'allpass))
(: biquad-filter* (-> Syntax (-> (Signal Flonum) FilterType Flonum Flonum Flonum (Signal Flonum))))
(define ((biquad-filter* stx) signal type freq q gain)
  ;; see https://webaudio.github.io/web-audio-api/#BiquadFilterNode
  (define Fs : Flonum (real->double-flonum (current-sample-rate)))
  (define f0 : Flonum freq)
  (define G : Flonum gain)
  (define Q : Flonum q)
  (define A : Flonum (expt 10.0 (/ G 40.0)))
  (define sqrtA : Flonum (flsqrt A))
  (define ω0 : Flonum (* 2.0 pi (/ f0 Fs)))

  (define sinω0 : Flonum (sin ω0))
  (define cosω0 : Flonum (cos ω0))

  (define αQ : Flonum (/ sinω0 (* 2.0 Q)))
  (define αQdB : Flonum (/ sinω0 (* 2.0 (expt 10.0 (/ Q 20.0)))))
  (define S : Flonum 1.0)
  (define αS : Flonum (* (/ sinω0 2.0) (flsqrt (+ (* (+ A (/ A)) (- (/ S) 1.0)) 2.0))))

  (: b0 Flonum) (: b1 Flonum) (: b2 Flonum)
  (: a0 Flonum) (: a1 Flonum) (: a2 Flonum)
  (define-values (b0 b1 b2 a0 a1 a2)
    (case type
      [(lowpass)
       (values (/ (- 1.0 cosω0) 2.0)
               (- 1.0 cosω0)
               (/ (- 1.0 cosω0) 2.0)
               (+ 1.0 αQdB)
               (* -2.0 cosω0)
               (- 1.0 αQdB))]
      [(highpass)
       (values (/ (+ 1.0 cosω0) 2.0)
               (- (+ 1.0 cosω0))
               (/ (+ 1.0 cosω0) 2.0)
               (+ 1.0 αQdB)
               (* -2.0 cosω0)
               (- 1.0 αQdB))]
      [(bandpass)
       (values αQ
               0.0
               (- αQ)
               (+ 1.0 αQ)
               (* -2.0 cosω0)
               (- 1.0 αQ))]
      [(notch)
       (values 1.0
               (* -2.0 cosω0)
               1.0
               (+ 1.0 αQ)
               (* -2.0 cosω0)
               (- 1.0 αQ))]
      [(allpass)
       (values (- 1.0 αQ)
               (* -2.0 cosω0)
               (+ 1.0 αQ)
               (+ 1.0 αQ)
               (* -2.0 cosω0)
               (- 1.0 αQ))]
      [(peaking)
       (values (+ 1.0 (* αQ A))
               (* -2.0 cosω0)
               (- 1.0 (* αQ A))
               (+ 1.0 (/ αQ A))
               (* -2.0 cosω0)
               (- 1.0 (/ αQ A)))]
      [(lowshelf)
       (values (* A (+ (+ A 1.0) (* -1.0 (- A 1.0) cosω0) (* 2.0 αS sqrtA)))
               (* 2.0 A (- (- A 1.0) (* (+ A 1.0) cosω0)))
               (* A (+ (+ A 1.0) (* -1.0 (- A 1.0) cosω0) (* -2.0 αS sqrtA)))
               (+ (+ A 1.0) (* (- A 1.0) cosω0) (* 2.0 αS sqrtA))
               (* -2.0 (+ (- A 1.0) (* (+ A 1.0) cosω0)))
               (+ (+ A 1.0) (* (- A 1.0) cosω0) (* -2.0 αS sqrtA)))]
      [(highshelf)
       (values (* A (+ (+ A 1.0) (* (- A 1.0) cosω0) (* 2.0 αS sqrtA)))
               (* -2.0 A (+ (- A 1.0) (* (+ A 1.0) cosω0)))
               (* A (+ (+ A 1.0) (* (- A 1.0) cosω0) (* -2.0 αS sqrtA)))
               (+ (+ A 1.0) (* -1.0 (- A 1.0) cosω0) (* 2.0 αS sqrtA))
               (* 2.0 (- (- A 1.0) (* (+ A 1.0) cosω0)))
               (+ (+ A 1.0) (* -1.0 (- A 1.0) cosω0) (* -2.0 αS sqrtA)))]))
  ((iir-filter* stx)
   signal
   (flvector b0 b1 b2)
   (flvector a0 a1 a2)))

(define-syntax-rule (with-srcloc name name*)
  (define-syntax (name stx)
    (syntax-case stx ()
      [(_ . args) (quasisyntax/loc stx ((name* #'#,stx) . args))]
      [_ (quasisyntax/loc stx (name* #'#,stx))])))

(with-srcloc iir-filter iir-filter*)
(with-srcloc biquad-filter biquad-filter*)
