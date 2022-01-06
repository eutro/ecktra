#lang typed/racket/base

(require racket/string)
(provide (all-defined-out))

(define-type Time Integer)

(struct signal-meta
  ([unused : Void])
  #:transparent)

(: pure-signal-meta signal-meta)
(define pure-signal-meta (signal-meta (void)))
(: current-sample-rate (Parameterof Integer))
(define current-sample-rate (make-parameter 441000))
(: current-latency (Parameterof Time))
(define current-latency (make-parameter 1))
(: current-backbuf (Parameterof Time))
(define current-backbuf (make-parameter 0))

(struct (T) signal
  ([produce : (-> Time T)]
   [metadata : signal-meta])
  #:type-name Signal
  #:constructor-name make-signal)

(: swap-produce (All (A B) (-> (-> Time B) (Signal A) (Signal B))))
(define (swap-produce f signal)
  (make-signal
   f
   (signal-metadata signal)))

(: fmap (All (A B) (-> (-> A B) (Signal A) (Signal B))))
(define (fmap f signal)
  (: old-prod (-> Time A))
  (define old-prod (signal-produce signal))
  (: new-prod (-> Time B))
  (define (new-prod t) (f (old-prod t)))
  (swap-produce new-prod signal))

(: sequence_ (All (A ...) (-> (List (Signal A) ... A) (Signal Void))))
(define (sequence_ signals)
  (pure (void)))

(: sequence (All (A ...) (-> (List (Signal A) ... A) (Signal (List A ... A)))))
(define (sequence signals)
  (define produces (map signal-produce signals))
  (: produce (-> Time (List A ... A)))
  (define (produce t)
    (: $ (All (T) (-> (-> Time T) T)))
    (define ($ f) (f t))
    (map $ produces))
  (make-signal produce pure-signal-meta))

(: liftA (All (R A ...) (-> (-> A ... A R) (Signal A) ... A (Signal R))))
(define (liftA f . signals)
  (: app (-> (List A ... A) R))
  (define (app ls) (apply f ls))
  (: sequenced (Signal (List A ... A)))
  (define sequenced (sequence signals))
  (fmap app sequenced))

(: join (All (A) (-> (Signal (Signal A)) (Signal A))))
(define (join signal)
  (: old-prod (-> Time (Signal A)))
  (define old-prod (signal-produce signal))
  (: new-prod (-> Time A))
  (define (new-prod t)
    (define new-sig (old-prod t))
    (define prod (signal-produce new-sig))
    (prod t))
  (swap-produce new-prod signal))

(: >>= (All (A B) (-> (Signal A) (-> A (Signal B)) (Signal B))))
(define (>>= signal f)
  (join (fmap f signal)))

(: pure (All (A) (-> A (Signal A))))
(define (pure x)
  (define (prod _) x)
  (make-signal prod pure-signal-meta))

(: >> (All (A B) (-> (Signal A) (Signal B) (Signal B))))
(define (>> lhs rhs)
  rhs)
