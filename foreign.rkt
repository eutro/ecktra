#lang racket/base

(require ffi/unsafe ffi/unsafe/define
         ffi/unsafe/define/conventions
         racket/runtime-path)

(provide (protect-out define-ecktra))

(define-runtime-path lib-dir "lib")
(define-ffi-definer define-ecktra (ffi-lib (build-path lib-dir "libecktra"))
  #:make-c-id convention:hyphen->underscore
  #:provide provide-protected)
