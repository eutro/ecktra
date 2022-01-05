#lang racket/base

(require ecktra/visualizer/macros
         ecktra/visualizer/ops
         ecktra/visualizer/processes
         ecktra/visualizer/signal
         ecktra/visualizer/plot
         typed/racket/base
         (for-syntax racket/base syntax/parse))
(provide (rename-out [module-begin #%module-begin])
         (except-out (all-from-out typed/racket/base) #%module-begin)
         (all-from-out ecktra/visualizer/macros
                       ecktra/visualizer/ops
                       ecktra/visualizer/processes
                       ecktra/visualizer/signal
                       ecktra/visualizer/plot))

(module typed typed/racket/base
  (require ecktra/visualizer/signal)
  (provide current-samples samples StartFn)
  (define-type StartFn (-> Time Time (Signal Flonum)))
  (: current-samples (Parameterof StartFn))
  (define current-samples (make-parameter (lambda _ (raise #f))))
  (: samples StartFn)
  (define (samples backlog latency) ((current-samples) backlog latency)))

(require 'typed)
(provide samples StartFn)

(define-syntax (module-begin stx)
  (define-syntax-class require-form
    #:literals (require #%require)
    (pattern (require _ ...))
    (pattern (#%require _ ...)))
  (syntax-parse stx
    [(_ requires:require-form ... body ...+)
     (syntax/loc stx
       (#%module-begin
        requires ...
        (: main (-> (Signal Any)))
        (define (main)
          (seq body ...))
        (module+ main
          (require ecktra/visualizer/main)
          (start-with main))))]))
