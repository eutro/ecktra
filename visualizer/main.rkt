#lang typed/racket/base

(require "signal.rkt"
         "../util/ringbuf.rkt"
         typed/racket/unsafe)
(unsafe-require/typed "cl.rkt"
  [parse-cl (-> (Values (-> (RingBuffer Flonum) Integer Integer)
                        (-> Any Void)
                        (-> Void)
                        (Evtof Real)
                        (-> Thread Void)))])

(provide start-with)

(: start-with
   (-> (-> (Signal Any))
       (Parameterof (Signal Flonum))
       Void))
(define (start-with f current-start)
  (define-values (read-next! put-frame! finish fps consume-thread) (parse-cl))

  (define time-offset (current-backbuf))
  (define latency (current-latency))
  (define bufsz (max 1 (+ time-offset latency)))

  (define buf (make-ring-buffer bufsz 0.0))
  (define (read-samples! [n : Time]) (read-next! buf n))

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
    (consume-thread
     (thread
      (lambda ()
        (let loop ([t 0])
          (set! t0 (- t time-offset))
          (define frame (produce t))
          (define dt-ms (sync fps))
          (current-fps (/ 1000 dt-ms))
          (put-frame! frame)
          (define dt
            (floor
             (inexact->exact
              (floor
               (* (/ dt-ms 1000)
                  (current-sample-rate))))))
          (when (zero? (read-samples! dt))
            (loop (+ t dt))))
        (finish))))))
