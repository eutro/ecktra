#lang racket/base

(module+ main
  (require racket/cmdline
           "seq.rkt"
           "decode.rkt"
           "transform.rkt"
           "visualize.rkt"
           "gui.rkt")
  (define rate (box 441000))
  (define sample/frame (box 441000/30))
  (define astream
    (command-line
     #:once-any
     [("-r" "--rate") sample-rate "Sample rate (default 441000)"
                      (set-box! rate (string->number sample-rate))]
     [("-f" "--frame") samples-per-frame "How many samples to read per frame"
                       (set-box! sample/frame (string->number samples-per-frame))]
     #:args (filename)
     (ecktra-decode-audio-file (unbox rate) filename)))
  (define close-stream!
    (let ()
      (define closed (box #f))
      (lambda ()
        (when (box-cas! closed #f #t)
          (ecktra-stream-close astream)))))
  (define sample-stream (sequence->stream (in-audio-stream astream)))
  (define decoder-thread
    (thread
     (lambda ()
       ;; force the stream
       ;; TODO buffer off-thread (or rather, off-place)
       (for ([_ sample-stream]) (sleep))
       (close-stream!))))
  (define ani
    ((compose1
      (compose1
       (visualize-resize 800 400)
       (visualize-graph/2d/t-x-y 200))
      #;(visualize-graph/2d/t-y 1000 400 441000/300)
      #;(transform-map (transform-decimate 2))
      (compose1
       (transform-map
        (compose1
         #;transform-ln
         (transform-mul 1/10000)
         (transform-take 400)))
       transform-inverse-dft/magnitude)
      (transform-window (expt 2 16) 441000/30 hann-window)
      #;transform-dft-single
      )
     (in-cycle sample-stream)
     #;
     (for/stream ([t (in-naturals)])
       (+ (* 1 (sin (* 2 3.14 100 t)))
          (* 2 (sin (* 2 3.14 250 t)))))))
  (define ani-thread (show-animation ani))
  (void
   (thread
    (lambda ()
      (thread-wait ani-thread)
      (kill-thread decoder-thread)
      (close-stream!)))))
