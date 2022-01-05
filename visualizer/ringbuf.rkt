#lang typed/racket/base

(require math)
(provide (struct-out ring-buffer)
         ring-buffer-push!
         ring-buffer-push-all!
         ring-buffer-nth
         ring-buffer-array
         make-ring-buffer
         RingBuffer)

(struct (T) ring-buffer
  ([buf : (Mutable-Vectorof T)]
   [offset : Integer])
  #:type-name RingBuffer
  #:mutable)

(: ring-buffer-push! (All (T) (-> (RingBuffer T) T Void)))
(define (ring-buffer-push! ringbuf x)
  (define buf (ring-buffer-buf ringbuf))
  (define offset (ring-buffer-offset ringbuf))
  (define size (vector-length buf))
  (vector-set! buf offset x)
  (set-ring-buffer-offset! ringbuf (modulo (add1 offset) size))
  (void))

(: ring-buffer-push-all! (All (T) (-> (RingBuffer T) Integer (-> Integer T) Void)))
(define (ring-buffer-push-all! ringbuf n f)
  (define buf (ring-buffer-buf ringbuf))
  (define off (ring-buffer-offset ringbuf))
  (define size (vector-length buf))
  (cond
    [(>= n size)
     (for ([i (in-range 1 (add1 size))])
       (vector-set! buf (- size i) (f (- n i))))
     (set-ring-buffer-offset! ringbuf 0)]
    [(<= (+ off n) size)
     (for ([i (in-range n)])
       (vector-set! buf (+ off i) (f i)))
     (set-ring-buffer-offset! ringbuf (+ off n))]
    [else
     (define pre-end (- size off))
     (define post-end (- n pre-end))
     (for ([i (in-range pre-end)])
       (vector-set! buf (+ off i) (f i)))
     (for ([j (in-range post-end)])
       (vector-set! buf j (f (+ pre-end j))))
     (set-ring-buffer-offset! ringbuf post-end)]))

(: ring-buffer-nth (All (T) (-> (RingBuffer T) Index T)))
(define (ring-buffer-nth ringbuf i)
  (define buf (ring-buffer-buf ringbuf))
  (define size (vector-length buf))
  (define bufi (modulo (+ i (ring-buffer-offset ringbuf)) size))
  (vector-ref buf bufi))

(: make-ring-buffer (All (T) (-> Integer T (RingBuffer T))))
(define (make-ring-buffer size default)
  (ring-buffer (make-vector size default) 0))

(: ring-buffer-array (All (T) (-> (RingBuffer T) (Array T))))
(define (ring-buffer-array ringbuf)
  (build-array
   (vector-immutable (vector-length (ring-buffer-buf ringbuf)))
   (lambda ([i : Indexes]) (ring-buffer-nth ringbuf (vector-ref i 0)))))

(: ring-buffer->vector (All (T) (-> (RingBuffer T) (Mutable-Vectorof T))))
(define (ring-buffer->vector ringbuf)
  (build-vector (vector-length (ring-buffer-buf ringbuf))
                (lambda ([i : Index]) (ring-buffer-nth ringbuf i))))

(module+ test
  (require typed/rackunit)
  (test-case "ring-buffer->vector"
    (check-equal?
     (ring-buffer->vector (make-ring-buffer 10 0))
     (make-vector 10 0)))

  (test-case "ring-buffer-push!"
    (let ()
      (define rb (make-ring-buffer 2 0))
      (check-equal? (ring-buffer->vector rb) (make-vector 2 0))
      (ring-buffer-push! rb 1)
      (check-equal? (ring-buffer->vector rb) #[0 1])
      (ring-buffer-push! rb 2)
      (check-equal? (ring-buffer->vector rb) #[1 2]))
    (let ()
      (define rb (make-ring-buffer 4 0))
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
      (check-equal? (ring-buffer->vector rb) #[3 4 5 6])))

  (test-case "ring-buffer-push-all!"
    (let ()
      (define rb (make-ring-buffer 10 0))
      (ring-buffer-push-all! rb 10 (λ ([x : Integer]) x))
      (check-equal? (ring-buffer->vector rb) #[0 1 2 3 4 5 6 7 8 9])
      (ring-buffer-push-all! rb 2 (λ ([x : Integer]) (+ x 10)))
      (check-equal? (ring-buffer->vector rb) #[2 3 4 5 6 7 8 9 10 11])
      (ring-buffer-push-all! rb 4 (λ ([x : Integer]) (+ x 12)))
      (check-equal? (ring-buffer->vector rb) #[6 7 8 9 10 11 12 13 14 15])
      (ring-buffer-push-all! rb 6 (λ ([x : Integer]) (+ x 16)))
      (check-equal? (ring-buffer->vector rb) #[12 13 14 15 16 17 18 19 20 21])
      (ring-buffer-push-all! rb 20 (λ ([x : Integer]) x))
      (check-equal? (ring-buffer->vector rb) #[10 11 12 13 14 15 16 17 18 19]))))
