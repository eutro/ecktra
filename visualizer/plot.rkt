#lang racket/base

(require (prefix-in pict/ pict)
         (prefix-in draw/ racket/draw)
         racket/class
         racket/sequence)

(provide plot-2d-x-y)

(define (plot-2d-x-y width height frame)
  (define frame-width (sub1 (sequence-length frame)))
  (define path (new draw/dc-path%))
  (define points
    (for/list ([(yv x) (in-indexed frame)])
      (cons (* width (/ x frame-width))
            (/ (- height (* yv (- height 2))) 2))))
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
