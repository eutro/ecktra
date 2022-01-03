#lang typed/racket/base

(require math)
(provide (struct-out ring-buffer)
         ring-buffer-push!
         ring-buffer-nth
         ring-buffer-array
         make-ring-buffer
         RingBuffer)

(struct (T) ring-buffer
  ([buf : (Mutable-Vectorof T)]
   [start : Exact-Nonnegative-Integer]
   [end : Exact-Nonnegative-Integer]
   [len : Exact-Nonnegative-Integer])
  #:type-name RingBuffer
  #:mutable)

(: ring-buffer-push! (All (T) (-> (RingBuffer T) T Void)))
(define (ring-buffer-push! ringbuf x)
  (define buf (ring-buffer-buf ringbuf))
  (define start (ring-buffer-start ringbuf))
  (define end (ring-buffer-end ringbuf))
  (define size (vector-length buf))
  (vector-set! buf end x)
  (define (incwrap [n : Exact-Nonnegative-Integer])
    (modulo (add1 n) size))
  (set-ring-buffer-start! ringbuf (incwrap start))
  (set-ring-buffer-end! ringbuf (incwrap end))
  (void))

(: ring-buffer-nth (All (T) (-> (RingBuffer T) Index T)))
(define (ring-buffer-nth ringbuf i)
  (define buf (ring-buffer-buf ringbuf))
  (define size (vector-length buf))
  (define bufi (modulo (+ i (ring-buffer-start ringbuf)) size))
  (vector-ref buf bufi))

(: make-ring-buffer (All (T) (-> Integer Integer T (RingBuffer T))))
(define (make-ring-buffer size len default)
  (when (> len size)
    (raise-argument-error 'make-ring-buffer
                          "(len <= size)"
                          `(not (,len <= ,size))))
  (ring-buffer
   (make-vector size default)
   0
   (cast (modulo len size) Index)
   (cast len Index)))

(: ring-buffer-array (All (T) (-> (RingBuffer T) (Array T))))
(define (ring-buffer-array ringbuf)
  (build-array
   (vector-immutable (ring-buffer-len ringbuf))
   (lambda ([i : Indexes]) (ring-buffer-nth ringbuf (vector-ref i 0)))))

(: ring-buffer->vector (All (T) (-> (RingBuffer T) (Mutable-Vectorof T))))
(define (ring-buffer->vector ringbuf)
  (build-vector (ring-buffer-len ringbuf)
                (lambda ([i : Index]) (ring-buffer-nth ringbuf i))))

(module+ test
  (require typed/rackunit)
  (test-case "ring-buffer->vector"
    (check-equal?
     (ring-buffer->vector (make-ring-buffer 10 10 0))
     (make-vector 10 0)))

  (test-case "ring-buffer-push!"
    (let ()
      (define rb (make-ring-buffer 2 2 0))
      (check-equal? (ring-buffer->vector rb) (make-vector 2 0))
      (ring-buffer-push! rb 1)
      (check-equal? (ring-buffer->vector rb) #[0 1])
      (ring-buffer-push! rb 2)
      (check-equal? (ring-buffer->vector rb) #[1 2]))
    (let ()
      (define rb (make-ring-buffer 10 4 0))
      (check-equal? (ring-buffer->vector rb) (make-vector 4 0))
      (ring-buffer-push! rb 1)
      (check-equal? (ring-buffer->vector rb) #[0 0 0 1])
      (ring-buffer-push! rb 2)
      (check-equal? (ring-buffer->vector rb) #[0 0 1 2])
      (ring-buffer-push! rb 3)
      (check-equal? (ring-buffer->vector rb) #[0 1 2 3])
      (ring-buffer-push! rb 4)
      (check-equal? (ring-buffer->vector rb) #[1 2 3 4])
      (ring-buffer-push! rb 5)
      (check-equal? (ring-buffer->vector rb) #[2 3 4 5])
      (ring-buffer-push! rb 6)
      (check-equal? (ring-buffer->vector rb) #[3 4 5 6]))))
