#!/usr/bin/env racket
#lang ecktra/visualizer

(require racket/runtime-path
         racket/cmdline
         racket/path
         racket/port
         typed/racket/draw
         typed/pict
         typed/net/url)
(begin
  (require/typed net/head ;; typed wrong
    [extract-field (-> String String (Option String))])
  (require/typed rsvg
    [svg-file->pict (-> Path-String pict)]
    [svg-port->pict (-> Input-Port pict)])
  (define-runtime-path logo-path "../logo.svg")
  (define-namespace-anchor anchor))

(define bufsz (expt 2 14))
(define halfbuf (quotient bufsz 2))
(define hann-window (make-hann-window bufsz))

#:backbuf halfbuf
#:latency halfbuf

(define background-thunk (lambda () (blank window-width* window-height*)))
(define window-width* : Positive-Integer 512)
(define window-height* : Positive-Integer window-width*)
(define image-thunk : (-> pict) (lambda () (svg-file->pict logo-path)))
(: decoder-for (-> String (-> pict)))
(define (decoder-for path)
  (define url (string->url path))
  (cond
    [(url-scheme url)
     (lambda ()
       (define-values (port headers) (get-pure-port/headers url))
       (case (extract-field "Content-Type" headers)
         [("image/svg+xml") (svg-port->pict port)]
         [else (bitmap (read-bitmap port))]))]
    [else
     (define ext (path-get-extension path))
     (case ext
       [(#".svg") (lambda () (svg-file->pict path))]
       [else (lambda () (bitmap (read-bitmap path)))])]))
(void
 (command-line
  #:once-any
  [("--image")
   image-url
   "Set an image path to use for the visualization."
   (set! image-thunk (decoder-for (cast image-url String)))]
  [("--expr")
   image-expr
   "Set a Racket expression to use as the image for the visualization, must return a pict."
   (let ([stx (with-input-from-string (cast image-expr String) read-syntax)])
     (set! image-thunk
           (lambda ()
             (call-with-values
              (lambda () (eval stx (namespace-anchor->namespace anchor)))
              (lambda vals (car (cast vals (List pict))))))))]
  #:once-each
  [("--size")
   window-size
   "Set the window size (width and height) of the output."
   (set! window-width* (cast (string->number (cast window-size String)) Positive-Integer))
   (set! window-height* (cast (string->number (cast window-size String)) Positive-Integer))]
  [("--width")
   window-width
   "Set the window width of the output."
   (set! window-width* (cast (string->number (cast window-width String)) Positive-Integer))]
  [("--height")
   window-height
   "Set the window height of the output."
   (set! window-height* (cast (string->number (cast window-height String)) Positive-Integer))]
  #:once-any
  [("--background-colour" "--background-color" #;":)")
   colour
   "Set the background colour of the window, a colour as accepted by `filled-rectangle'."
   (set! background-thunk (lambda () (filled-rectangle window-width* window-height* #:color (cast colour String))))]
  [("--background-image")
   image-url
   "Set an image path to use for the background. This will override window dimensions."
   (let ()
     (define image-thunk (decoder-for (cast image-url String)))
     (set! background-thunk
           (lambda ()
             (define img (image-thunk))
             (set! window-width* (cast (inexact->exact (floor (pict-width img))) Positive-Integer))
             (set! window-height* (cast (inexact->exact (floor (pict-height img))) Positive-Integer))
             img)))]))

(define image* (image-thunk))
(define cvs (background-thunk))
(define image
  (let ()
    (define height-ratio (/ (pict-height cvs) (pict-height image*)))
    (define width-ratio (/ (pict-width cvs) (pict-width image*)))
    (define sf (* 0.5 (min width-ratio height-ratio)))
    (scale image* sf)))

(bind frame : FlVector (time-travel-backward halfbuf (sliding-window-flvector bufsz (samples))))
(void (flvector-map! * frame hann-window))
(define size (- 0.8 (/ (log (flvector-rms frame)))))
(define scaled (scale image size))
(pure (pin-over
       cvs
       (- (/ (pict-width cvs) 2) (/ (pict-width scaled) 2))
       (- (/ (pict-height cvs) 2) (/ (pict-height scaled) 2))
       scaled))
