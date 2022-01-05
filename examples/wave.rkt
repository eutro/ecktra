#lang ecktra/visualizer

(require racket/flonum)

(bind frame : FlVector (time-travel-backward 1 (sliding-window-flvector 1 (samples 1 1))))
(define maxv (for/fold : Number ([m -inf.0]) ([x (in-flvector frame)]) (max m x)))
(pure maxv)
