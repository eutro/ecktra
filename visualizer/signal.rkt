#lang typed/racket/base

(provide (all-defined-out))

(define-type Time Real)

(struct (T) signal
  ([produce : (-> Time T)]
   [latency : Time]
   [backlog : Time]
   [sample-freq : (Option Integer)])
  #:type-name Signal
  #:constructor-name make-signal)

(: swap-produce (All (A B) (-> (-> Time B) (Signal A) (Signal B))))
(define (swap-produce f signal)
  (make-signal
   f
   (signal-latency signal)
   (signal-backlog signal)
   (signal-sample-freq signal)))

(: fmap (All (A B) (-> (-> A B) (Signal A) (Signal B))))
(define (fmap f signal)
  (: old-prod (-> Time A))
  (define old-prod (signal-produce signal))
  (: new-prod (-> Time B))
  (define (new-prod t) (f (old-prod t)))
  (swap-produce new-prod signal))

(: sequence_ (All (A ...) (-> (List (Signal A) ... A) (Signal Void))))
(define (sequence_ signals)
  (if (null? signals)
      (pure (void))
      (>> (car signals)
          (sequence_ (cdr signals)))))

(: sequence (All (A ...) (-> (List (Signal A) ... A) (Signal (List A ... A)))))
(define (sequence signals)
  (define produces (map signal-produce signals))
  (: produce (-> Time (List A ... A)))
  (define (produce t)
    (: $ (All (T) (-> (-> Time T) T)))
    (define ($ f) (f t))
    (map $ produces))
  (>> (sequence_ signals)
      (make-signal produce 0 0 #f)))

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
    (define prod (signal-produce (old-prod t)))
    (prod t))
  (>> signal (swap-produce new-prod signal)))

(: >>= (All (A B) (-> (Signal A) (-> A (Signal B)) (Signal B))))
(define (>>= signal f)
  (join (fmap f signal)))

(: pure (All (A) (-> A (Signal A))))
(define (pure x)
  (define (prod _) x)
  (make-signal prod 0 0 #f))

(: >> (All (A B) (-> (Signal A) (Signal B) (Signal B))))
(define (>> lhs rhs)
  (make-signal
   (signal-produce rhs)
   (max (signal-latency lhs)
        (signal-latency rhs))
   (max (signal-backlog lhs)
        (signal-backlog rhs))
   (let ([lf (signal-sample-freq lhs)]
         [rf (signal-sample-freq rhs)])
     (if (and lf rf)
         (max lf rf) ;; maybe resample?
         (or lf rf)))))
