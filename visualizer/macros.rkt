#lang racket/base

(require "signal.rkt"
         (only-in typed/racket/base :)
         (rename-in typed/racket/base
                    [lambda typed-lambda]
                    [define typed-define]
                    [begin typed-begin]
                    [void typed-void])
         (for-syntax
          syntax/parse
          racket/base))
(provide bind seq)

(define-syntax (bind stx)
  (raise-syntax-error 'bind "cannot be used outside of (seq ...) or top level" stx))

(begin-for-syntax
  (define-syntax-class bind-stmt
    #:literals (bind :)
    (pattern (bind id:id : ty val:expr)))
  (define-syntax-class top-stmt
    #:literals (: typed-void typed-define)
    (pattern (: _ ...))
    (pattern (typed-void _ ...))
    (pattern (typed-define _ ...))))

(define-syntax (seq stx)
  (syntax-parse stx
    #:track-literals
    #:literals (typed-define)
    [(_ bind:bind-stmt tail ...+)
     (syntax/loc #'bind.id
       (>>= bind.val (typed-lambda ([bind.id : bind.ty]) (seq tail ...))))]
    [(_ stmt:top-stmt tail ...+)
     (syntax/loc stx
       (begin
         stmt
         (seq tail ...)))]
    [(_ stmt:expr tail:expr ...+)
     (syntax/loc #'stmt
       (>> stmt (seq tail ...)))]
    [(_ ret:expr) #'ret]))
