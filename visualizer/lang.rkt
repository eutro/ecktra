#lang racket/base

(require ecktra/visualizer/macros ;; macros
         ecktra/visualizer/ops ;; typed
         ecktra/visualizer/processes ;; typed
         ecktra/visualizer/signal ;; typed
         ecktra/visualizer/main ;; typed
         (submod ecktra/visualizer/plot typed) ;; typed
         typed/racket/base
         (prefix-in pict/ typed/pict)
         (for-syntax racket/base syntax/parse))
(provide (rename-out [module-begin #%module-begin])
         (except-out (all-from-out typed/racket/base) #%module-begin)
         (all-from-out ecktra/visualizer/macros
                       ecktra/visualizer/ops
                       ecktra/visualizer/processes
                       ecktra/visualizer/signal
                       (submod ecktra/visualizer/plot typed)))

(module typed typed/racket/base
  (require ecktra/visualizer/signal)
  (provide current-samples samples)

  (: current-samples (Parameterof (Signal Flonum)))
  (define current-samples (make-parameter (pure 0.0)))
  (: samples (-> (Signal Flonum)))
  (define (samples) (current-samples)))

(require 'typed)
(provide samples)

(define-syntax (module-begin stx)
  (define-syntax-class pre-form
    #:literals (require #%require define define-values void begin)
    (pattern ({~or require #%require define define-values void begin} _ ...)))
  (define-splicing-syntax-class option
    #:attributes (opt val)
    (pattern {~seq #:latency val:expr} #:attr opt #'current-latency)
    (pattern {~seq #:backbuf val:expr} #:attr opt #'current-backbuf)
    (pattern {~seq #:sample-rate val:expr} #:attr opt #'current-sample-rate))
  (syntax-parse stx
    [(_ pres:pre-form ...
        option:option ...
        body ...+)
     (with-syntax ([main-def
                    (syntax/loc stx
                      (define (main)
                        (seq body ...)))])
       (syntax/loc stx
         (#%module-begin
          pres ...
          (: main (-> (Signal Any)))
          main-def
          (module+ main
            (require ecktra/visualizer/main)
            (parameterize ([option.opt option.val]
                           ...)
              (start-with main current-samples))))))]))
