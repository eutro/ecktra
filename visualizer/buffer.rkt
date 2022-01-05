#lang racket/base

(require racket/cmdline
         "../foreign/decode.rkt"
         "ringbuf.rkt"
         ffi/unsafe)
(provide start-buffering)

(define (parse-cl)
  (define rate (box 441000))
  (define astream
    (command-line
     #:once-any
     [("-r" "--rate") sample-rate "Sample rate (default 441000)"
                      (set-box! rate (string->number sample-rate))]
     #:args (filename)
     (ecktra-decode-audio-file (unbox rate) filename)))
  astream)

(define (start-buffering buf)
  (define astream (parse-cl))
  (define left #f)
  (define (read-next!) (ecktra-stream-read astream))
  (define (read-next skip n)
    (define (loop1 skip nxt)
      (cond
        [(> skip (car nxt)) (loop1 (- skip (car nxt)) (read-next!))]
        [(< skip (car nxt))
         (ptr-add! (cdr nxt) skip)
         (cons (- (car nxt) skip) (cdr nxt))]
        [else (read-next!)]))
    (define (loop2 n nxt)
      (define (ref-nxt i) (ptr-ref (cdr nxt) i))
      (cond
        [(> n (car nxt))
         (ring-buffer-push-all! buf nxt ref-nxt)
         (loop2 (- n (car nxt)) (read-next!))]
        [(< n (car nxt))
         (ring-buffer-push-all! buf n ref-nxt)
         (define remaining-nxt (- (car nxt) n))
         (ptr-add! (cdr nxt) n)
         (set! left (cons remaining-nxt (cdr nxt)))
         (void)]
        [else
         (ring-buffer-push-all! buf n ref-nxt)
         (void)]))
    (define packet (or left (read-next!)))
    (loop2
     n
     (if (zero? skip)
         packet
         (loop1 skip packet))))
  read-next)
