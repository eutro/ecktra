#lang racket/base

(require ffi/unsafe
         "foreign.rkt"
         ffi/unsafe/custodian)

(provide _AudioStream
         AudioStream?
         stream-closed?
         stream-close!
         buffered-stream-close!)

(define-cpointer-type _AudioStream)
(define-cpointer-type _BufferedAudioStream)

(define (raise-errno err)
  (raise (exn:fail (format "error\n  errno: ~a" err)
                   (current-continuation-marks))))

(define-ecktra ecktra-decode-audio-file
  (_fun
   _int ; sample-rate
   _path ; url
   [ret : (_ptr o _AudioStream)]
   -> [err : _int]
   -> (cond
        [(< err 0)
         (raise-errno err)]
        [else
         (register-custodian-shutdown ret stream-close! #:at-exit? #t)
         ret])))

(define-ecktra ecktra-stream-close
  (_fun _AudioStream -> _void))

(define-ecktra ecktra-start-buffering
  (_fun
   _AudioStream
   -> [ret :  _BufferedAudioStream]
   -> (begin
        (register-custodian-shutdown ret buffered-stream-close! #:at-exit? #t)
        ret)))

(define-ecktra ecktra-buffered-stream-close
  (_fun _BufferedAudioStream -> _void))

(define (stream-closed? ptr)
  (cpointer-has-tag? ptr 'closed))

(define (stream-close! ptr)
  (unless (stream-closed? ptr)
    (ecktra-stream-close ptr)
    (cpointer-push-tag! ptr 'closed))
  (void))

(define (buffered-stream-close! ptr)
  (unless (stream-closed? ptr)
    (ecktra-buffered-stream-close ptr)
    (cpointer-push-tag! ptr 'closed))
  (void))

(define-ecktra ecktra-eof _int)

(define-ecktra ecktra-stream-read
  (_fun _AudioStream
        [buf : (_ptr o _pointer)]
        [bufsz : (_ptr o _int)]
        -> [err : _int]
        -> (cond
             [(= err ecktra-eof) eof]
             [(< err 0) (raise-errno err)]
             [else (cons buf bufsz)])))

(define-ecktra ecktra-buffered-stream-fork
  (_fun _BufferedAudioStream -> _BufferedAudioStream))

(define-ecktra ecktra-buffered-stream-read
  (_fun [as : _BufferedAudioStream]
        [buf : _pointer]
        [bufsz : (_ptr io _int)]
        [destroy : _int]
        -> [err : _int]
        -> (cond
             [(= err ecktra-eof) bufsz]
             [(< err 0) (raise-errno err)]
             [else 0])))

(define-ecktra ecktra-buffered-stream-current-buffered
  (_fun _BufferedAudioStream
        -> _int))

(define-ecktra ecktra-buffered-stream-ensure-buffered
  (_fun _BufferedAudioStream
        [n : (_ptr io _int)]
        -> [err : _int]
        -> (cond
             [(= err ecktra-eof) n]
             [(< err 0) (raise-errno err)]
             [else 0])))
