#lang racket/base

(require racket/sequence)

(provide transform-numeric
         transform-binary

         transform-magnitude
         transform-angle
         transform-real
         transform-imag

         transform-mul
         transform-add)

(define (transform-numeric* f name)
  (define (app x)
    (cond
      [(number? x) (f x)]
      [(sequence? x) (sequence-map app x)]
      [else (raise-argument-error name "(or/c number? sequence?)" x)]))
  (procedure-rename app name))
(define-syntax-rule (define/transform-numeric name op)
  (define name (transform-numeric* op 'name)))

(define (transform-numeric f) (transform-numeric* f 'transform-numeric))
(define/transform-numeric transform-magnitude magnitude)
(define/transform-numeric transform-angle angle)
(define/transform-numeric transform-real real-part)
(define/transform-numeric transform-imag imag-part)

(define (transform-binary* op name)
  (define (app n) (transform-numeric* (lambda (x) (op x n)) name))
  (procedure-rename app name))
(define-syntax-rule (define/transform-binary name op)
  (define name (transform-binary* op 'name)))

(define (transform-binary op n) ((transform-binary* op 'transform-binary) n))
(define/transform-binary transform-mul *)
(define/transform-binary transform-add +)
