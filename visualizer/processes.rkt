#lang typed/racket/base

(require "signal.rkt" racket/flonum "../util/ringbuf.rkt")
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
(define (signal-fold signal proc acc [backbuf 1])
  (define buf (make-ring-buffer backbuf acc))
  (define buftime 0)
  (define old-prod (signal-produce signal))
  (: new-prod (-> Time B))
  (define (new-prod t)
    (cond
      [(<= t buftime)
       (define idx (+ backbuf (- t buftime 1)))
       (when (negative? idx)
         (raise-arguments-error 'signal-fold
                                "sample out of range"
                                "t" t))
       (ring-buffer-nth buf idx)]
      [else
       (let loop ()
         (set! buftime (add1 buftime))
         (define acc (ring-buffer-nth buf (sub1 backbuf)))
         (define next-val (proc acc (old-prod buftime)))
         (ring-buffer-push! buf next-val)
         (if (>= buftime t)
             next-val
             (loop)))]))
  (swap-produce new-prod signal))
