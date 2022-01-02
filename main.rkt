#lang racket/base

(require math
         racket/sequence
         "ringbuf.rkt"
         "decode.rkt")

(define (spectrograph samples samplec samples/frame)
  (define-values (has-next? next-sample!) (sequence-generate samples))
  (define ringbuf (make-ring-buffer samplec samplec 0.0))
  (define arr (ring-buffer-array ringbuf))
  (let loop ([i 0])
    (cond
      [(or (>= i samplec) (not (has-next?))) (void)]
      [else
       (ring-buffer-push! ringbuf (next-sample!))
       (loop (add1 i))]))
  (define (next-spectrum!)
    (define fft (array-axis-fft arr 0))
    (for ([_ (in-range samples/frame)])
      #:break (not (has-next?))
      (ring-buffer-push! ringbuf (next-sample!)))
    fft)
  (stop-after (in-producer next-spectrum!) (lambda (_) (not (has-next?)))))

(module+ main
  (require racket/cmdline
           "visualize.rkt"
           "gui.rkt")
  (define rate (box 441000))
  (define samplec (box 262144))
  (define sample/frame (box 441000/30))
  (define astream
    (command-line
     #:once-any
     [("-r" "--rate") sample-rate "Sample rate (default 441000)"
                      (set-box! rate (string->number sample-rate))]
     [("-s" "--samples") sample-frame-rate
                         "Samples in output, must be an integer power of 2 (default 262144)"
                         (set-box! samplec (string->number sample-frame-rate))]
     [("-f" "--frame") samples-per-frame "How many samples to read per frame"
                       (set-box! sample/frame (string->number samples-per-frame))]
     #:args (filename)
     (ecktra-decode-audio-file (unbox rate) filename)))
  (define ani ((visualize-graph/2d/t-y 100 100)
               (in-cycle (in-audio-stream astream))))
  (thread-wait (show-animation ani))
  #;
  (for ([spectrum (spectrograph (in-audio-stream astream) (unbox samplec) (unbox sample/frame))])
    (displayln spectrum))
  (ecktra-stream-close astream))
