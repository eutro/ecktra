#lang info
(define collection "ecktra")
(define deps '("base" "draw-lib" "gui-lib" "math-lib" "pict-lib" "typed-racket-lib" "typed-racket-more" "rsvg"))
(define build-deps '("scribble-lib" "rackunit-lib" "rackunit-typed"))
(define scribblings '(("scribblings/ecktra.scrbl" ())))
(define compile-omit-paths '("examples"))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(eutro))
