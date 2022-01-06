#lang typed/racket/base

(require typed/racket/gui
         typed/pict
         racket/match)
(provide (struct-out gui-frame)
         ->gui-frame)

(struct gui-frame
  ([draw : (-> (Instance Frame%) (Instance Canvas%) Void)])
  #:type-name GuiFrame
  #:constructor-name make-gui-frame)

(: ->gui-frame (-> Any GuiFrame))
(define (->gui-frame o)
  (match o
    [(? gui-frame?) o]
    [(? pict?)
     (define pd (make-pict-drawer o))
     (make-gui-frame
      (lambda ([frm : (Instance Frame%)]
               [cvs : (Instance Canvas%)])
        (define dc (send cvs get-dc))
        (send cvs min-height (floor (inexact->exact (floor (pict-height o)))))
        (send cvs min-width (floor (inexact->exact (floor (pict-width o)))))
        (pd dc 0 0)
        (void)))]
    [_
     (define str (format "~a" o))
     (->gui-frame (text str))]))
