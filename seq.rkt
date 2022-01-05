#lang racket/base

(require racket/stream
         racket/sequence)

(provide uncons
         sfirst
         sempty?
         snext
         stake
         sdrop
         scons
         smap
         vec
         scount
         spartition
         siterate)

(define (uncons ls)
  (cond
    [(list? ls)
     (if (null? ls)
         #f
         ls)]
    [(stream? ls)
     (if (stream-empty? ls)
         #f
         (cons (stream-first ls)
               (stream-rest ls)))]
    [(sequence? ls) (uncons (sequence->stream ls))]
    [else (raise-argument-error 'sfirst "sequence?" ls)]))

(define (sfirst ls)
  (cond
    [(list? ls) (if (null? ls) #f (car ls))]
    [(stream? ls) (if (stream-empty? ls) #f (stream-first ls))]
    [(sequence? ls)
     (define-values (gen _) (sequence-generate* ls))
     (and gen (sfirst gen))]
    [else (raise-argument-error 'sfirst "sequence?" ls)]))

(define (snext ls)
  (cond
    [(list? ls) (if (null? ls) null (cdr ls))]
    [(stream? ls) (if (stream-empty? ls) null (stream-rest ls))]
    [(sequence? ls) (snext (sequence->stream ls))]
    [else (raise-argument-error 'sfirst "sequence?" ls)]))

(define (sempty? ls)
  (cond
    [(list? ls) (null? ls)]
    [(stream? ls) (stream-empty? ls)]
    [(sequence? ls)
     (define-values (gen _) (sequence-generate* ls))
     (eq? gen #f)]
    [else (raise-argument-error 'sfirst "sequence?" ls)]))

(define (stake ls n)
    (cond
    [(<= n 0) null]
    [(uncons ls)
     =>
     (lambda (x) (stream-cons (car x) (stake (cdr x) (sub1 n))))]
    [else null]))

(define (sdrop ls n)
  (cond
    [(<= n 0) ls]
    [else
     (define nx (snext ls))
     (if (null? nx) null (sdrop nx (sub1 n)))]))

(define (vec ls)
  (for/vector ([x ls]) x))

(define scount sequence-length)

(define (spartition seq n [step n])
  (cond
    [(sempty? seq) null]
    [else
     (define p (vec (stake seq n)))
     (if (= n (scount p))
         (stream-cons p (spartition (sdrop seq step) n step))
         null)]))

(define (siterate f x)
  (stream-cons x (siterate f (f x))))

(define (scons lhs rhs)
  (cond
    [(or (pair? rhs) (null? rhs)) (cons lhs rhs)]
    [(stream? rhs) (stream-cons lhs rhs)]
    [(sequence? rhs) (cons lhs (sequence->stream rhs))]
    [else (raise-argument-error 'scons "sequence?" rhs)]))

(define (smap f l . ls)
  (if (null? ls)
      (stream-map f (sequence->stream l))
      (let loop ([ls (cons l ls)])
        (if (andmap pair? ls)
            (stream-cons (apply f (map sfirst ls))
                         (loop (map snext ls)))
            empty-stream))))

(module+ test
  (require rackunit)
  (test-case "sdrop"
    (check-equal?
     (sdrop null 10)
     null)
    (check-equal?
     (sdrop '(1 2 3) 1)
     '(2 3))
    (check-equal?
     (sdrop '(1 2 3) 2)
     '(3)))

  (test-case "stake"
    (check-equal?
     (stream->list (stake '(1 2 3) 1))
     '(1))
    (check-equal?
     (stream->list (stake '(1 2 3) 2))
     '(1 2)))

  (test-case "vec"
    (check-equal?
     (vec '(1 2 3))
     #[1 2 3]))

  (test-case "scount"
    (check-equal?
     (scount '(1 2 3))
     3))

  (test-case "spartition"
    (check-equal?
     (stream->list (spartition '(1 2 3 4 5 6) 2))
     '(#[1 2] #[3 4] #[5 6]))
    (check-equal?
     (stream->list (spartition '(1 2 3 4 5 6) 2 1))
     '(#[1 2] #[2 3] #[3 4] #[4 5] #[5 6]))
    (check-equal?
     (stream->list (stake (spartition (in-cycle '(1 2 3)) 2) 4))
     '(#[1 2] #[3 1] #[2 3] #[1 2]))))
