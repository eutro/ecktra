#lang racket/base

(require (except-in pict show-pict)
         racket/gui
         "seq.rkt")
(provide show-animation)

;; (: show-animation (-> (Sequenceof Frame) Void))
(define (show-animation frames)
  (when (sempty? frames)
    (raise-argument-error 'show-animation
                          "non-empty seq"
                          frames))
  (define pict-frame (show-pict (sfirst frames)))
  (thread
   (lambda ()
     (for ([frame frames])
       (send pict-frame set-pict frame)
       (sleep 1/30)))))

;; show-pict from pict but with the crucial difference that we did not
;; forget to actually return the frame
(define (show-pict p
                   [w #f] [h #f]
                   #:frame-style [frame-style '()]
                   #:frame-x [frame-x #f]
                   #:frame-y [frame-y #f])
  (define the-pict p)
  (define pict-drawer (make-pict-drawer the-pict))
  (define no-redraw? #f)
  (define pict-frame%
    (class frame%
      (define/public (set-pict p)
        (set! the-pict p)
        (set! pict-drawer (make-pict-drawer the-pict))
        (set! no-redraw? #t)
        (let ([pw (inexact->exact (floor (pict-width the-pict)))]
              [ph (inexact->exact (floor (pict-height the-pict)))])
          (send c min-width (if w (max w pw) pw))
          (send c min-height (if h (max h ph) ph)))
        (set! no-redraw? #f)
        (send c on-paint))
      (super-instantiate ())))
  (define pict-canvas%
    (class canvas%
      (inherit get-dc)
      (define/override (on-paint)
        (unless no-redraw?
          (let ([dc (get-dc)])
            (send dc clear)
            (let* ([pw (pict-width the-pict)]
                   [ph (pict-height the-pict)]
                   [xo (if (and w
                                (pw . < . w))
                           (- (/ w 2) (/ pw 2))
                           0)]
                   [yo (if (and h
                                (ph . < . h))
                           (- (/ h 2) (/ ph 2))
                           0)])
              (pict-drawer dc xo yo)))))
      (super-instantiate ())))
  (define f (new pict-frame% 
                 [label "sound"] 
                 [style frame-style] 
                 [x frame-x]
                 [y frame-y]))
  (define c (make-object pict-canvas% f))
  (send (send c get-dc) set-smoothing 'aligned)
  (send f set-pict p)
  (send f show #t)
  f)
