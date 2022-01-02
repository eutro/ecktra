#lang racket/base

(require (prefix-in pict/ typed/pict)
         (prefix-in draw/ typed/racket/draw)
         racket/class
         racket/sequence
         "seq.rkt")

(provide visualize-graph/2d/x-y
         visualize-graph/2d/t-y)

(define ((visualize-graph/2d/x-y height) samples)
  (define width (sequence-length samples))
  (define path (new draw/dc-path%))
  (define points
    (for/list ([(yv x) (in-indexed samples)])
      (cons x (/ (+ height (* yv (- height 2))) 2))))
  (send path move-to (caar points) (cdar points))
  (send path lines (cdr points) 0 0)
  (define (draw dc dx dy)
    (define pen (send dc get-pen))
    (send dc set-pen "black" 1 'solid)
    (define brush (send dc get-brush))
    (send dc set-brush "white" 'transparent)
    (send dc draw-path path dx dy)
    (send dc set-brush brush)
    (send dc set-pen pen))
  (pict/dc draw width height))

(define ((visualize-graph/2d/t-y width height [s/frame 1]) samples)
  (smap (visualize-graph/2d/x-y height)
        (scons (stake samples width) (spartition samples width s/frame))))

(module+ main
  (require "gui.rkt")
  (show-animation ((visualize-graph/2d/t-y 100 100)
                   (smap (λ (x) (sin (/ x 60)))
                         (in-naturals)))))
