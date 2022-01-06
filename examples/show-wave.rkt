#!/usr/bin/env racket
#lang ecktra/visualizer

#:backbuf 1024
#:latency 1024

(bind frame : FlVector (time-travel-backward 1024 (sliding-window-flvector 2048 (samples))))
(pure (plot-2d-x-y 1024 400 frame))
