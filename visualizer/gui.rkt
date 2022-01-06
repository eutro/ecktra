#lang racket/base

(require racket/draw
         racket/gui
         racket/class)
(provide init-gui
         put-frame!)

(define current-gui (make-parameter #f))

(define (gui-loop)
  (define window-frame
    (new frame%
         [label "Ecktra"]))
  (send window-frame show #t)
  (let loop ()
    (define frame (thread-receive))
    (displayln frame)
    (loop)))

(define (init-gui)
  (current-gui (thread gui-loop))
  (void))

(define (put-frame! frame)
  (define gui (current-gui))
  (unless gui
    (raise (exn:fail "gui uninitialised" (current-continuation-marks))))
  (thread-send gui frame)
  (void))
