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
  (provide current-samples samples)
  (: current-samples (Parameterof (-> Time Time (Signal Flonum))))
  (define current-samples (make-parameter (lambda _ (raise #f))))
  (: samples (-> Time Time (Signal Flonum)))
  (define (samples backlog latency) ((current-samples) backlog latency)))

(require 'typed)
(provide samples)

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
        (: main (-> (-> Time Time (Signal Flonum)) (Signal Any)))
        (define (main samples)
          (parameterize ([current-samples samples])
            (seq body ...)))
        (module+ main
          (require ecktra/visualizer/input)
          (process-input main))))]))
