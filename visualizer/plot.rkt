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
  (define pen
    (send draw/the-pen-list
          find-or-create-pen
          "black" 1 'solid))
  (define brush
    (send draw/the-brush-list
          find-or-create-brush
          "white" 'transparent))
  (define (draw dc dx dy)
    (send dc set-pen pen)
    (send dc set-brush brush)
    (send dc draw-path path dx dy))
  (pict/unsafe-dc draw width height))

(module* typed typed/racket/base
  (require typed/pict)
  (require/typed (submod "..")
    [plot-2d-x-y (-> Integer Integer
                     (U FlVector (Sequenceof Real))
                     pict)])
  (provide plot-2d-x-y))
