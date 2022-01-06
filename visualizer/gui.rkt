#lang racket/base

(require racket/draw
         racket/gui
         racket/class
         "gui-frame.rkt")
(provide init-gui
         put-frame!)

(define current-gui (box #f))

(define (make-gui)
  (define in-chan (make-channel))
  (define out-chan (make-channel))
  (thread
   (lambda ()
     (define current-frame #f)
     (define (set-frame! frame)
       (set! current-frame frame))

     (define (draw! . _)
       (when current-frame
         (send window-cvs suspend-flush)
         (send (send window-cvs get-dc) clear)
         (define draw (gui-frame-draw current-frame))
         (draw window-frame window-cvs)
         (send window-cvs resume-flush)))

     (define window-frame
       (new frame%
            [label "Ecktra"]))
     (define window-cvs
       (new canvas%
            [parent window-frame]
            [paint-callback draw!]))
     (send window-frame show #t)

     (define (put-frame! frame)
       (set-frame! (->gui-frame frame))
       (send window-cvs on-paint))

     (channel-put out-chan #t)
     (let loop ()
       (define frame (sync in-chan))
       (put-frame! frame)
       (channel-put out-chan #t)
       (loop))))

  (sync out-chan)
  (lambda (frame)
    (channel-put in-chan frame)
    (sync out-chan)
    (void)))

(define (init-gui)
  (set-box! current-gui (make-gui))
  (void))

(define (put-frame! frame)
  (define gui (unbox current-gui))
  (unless gui (raise (exn:fail "gui uninitialised" (current-continuation-marks))))
  (gui frame)
  (void))
