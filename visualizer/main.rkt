#lang typed/racket/base

(require "signal.rkt"
         "../util/ringbuf.rkt"
         (submod "../util/fps.rkt" typed)
         typed/racket/unsafe)
(require/typed "buffer.rkt"
  [start-buffering (-> (RingBuffer Flonum) (-> Integer Integer))])
(unsafe-require/typed ;; safety: the procedures would throw on violation anyway
 "gui.rkt"
 [init-gui (-> Void)]
 [put-frame! (-> Any Void)])

(provide start-with)

(: start-with
   (-> (-> (Signal Any))
       (Parameterof (Signal Flonum))
       Void))
(define (start-with f current-start)
  (define time-offset (current-backbuf))
  (define latency (current-latency))
  (define bufsz (max 1 (+ time-offset latency)))

  (define buf (make-ring-buffer bufsz 0.0))
  (define read-samples! (start-buffering buf))

  (define t0 0)
  (: get-sample (-> Time Flonum))
  (define (get-sample t)
    (define i (- t t0))
    (unless (and (<= 0 i) (< i bufsz))
      (raise-arguments-error 'get-sample
                              "sample out of range"
                              "t" t))
    (ring-buffer-nth buf i))
  (define samples (make-signal get-sample pure-signal-meta))

  (parameterize ([current-start samples])
    (define out (f))
    (define produce (signal-produce out))

    (read-samples! latency)
    (define max-fps 30)
    (define fps (fps-event max-fps))
    (init-gui)
    (thread
     (lambda ()
       (let loop ([t 0])
         (set! t0 (- t time-offset))
         (define frame (produce t))
         (define dt-ms (sync fps))
         (displayln (/ 1000 dt-ms))
         (put-frame! frame)
         (define dt
           (floor
            (* (/ max-fps)
               (current-sample-rate))))
         (when (zero? (read-samples! dt))
           (loop (+ t dt))))))
    (void)))
