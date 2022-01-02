#lang typed/racket/base

(require math)
(provide (struct-out ring-buffer)
         ring-buffer-push!
         ring-buffer-nth
         ring-buffer-array
         make-ring-buffer)

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

(: make-ring-buffer (All (T) (-> Index Index T (RingBuffer T))))
(define (make-ring-buffer size len default)
  (ring-buffer
   (make-vector size default)
   0 (modulo len size) len))

(: ring-buffer-array (All (T) (-> (RingBuffer T) (Array T))))
(define (ring-buffer-array ringbuf)
  (build-array
   (vector-immutable (ring-buffer-len ringbuf))
   (lambda ([i : Indexes]) (ring-buffer-nth ringbuf (vector-ref i 0)))))
