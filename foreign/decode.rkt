#lang racket/base

(require ffi/unsafe
         "foreign.rkt"
         racket/generator)

(provide _AudioStream
         AudioStream?
         in-audio-stream)

(define-cpointer-type _AudioStream)

(define (raise-errno err)
  (raise (exn:fail (format "error\n  errno: ~a" err)
                   (current-continuation-marks))))

(define-ecktra ecktra-decode-audio-file
  (_fun
   _int ; sample-rate
   _path ; url
   [ret : (_ptr o _AudioStream)]
   -> [err : _int]
   -> (if (< err 0)
          (raise-errno err)
          ret)))

(define-ecktra ecktra-stream-close
  (_fun _AudioStream -> _void))

(define-ecktra ecktra-eof _int)

(define-ecktra ecktra-stream-read
  (_fun _AudioStream
        [buf : (_ptr o _pointer)]
        [bufsz : (_ptr o _int)]
        -> [err : _int]
        -> (cond
             [(= err ecktra-eof) eof]
             [(< err 0) (raise-errno err)]
             [else (cblock->vector buf _double bufsz)])))

(define (sequence-append* seqs)
  (in-generator
   (for ([seq seqs])
     (for ([el seq])
       (yield el)))))

(define (in-audio-stream astream)
  (sequence-append* (in-producer ecktra-stream-read eof astream)))

(module+ main
  (require racket/cmdline)
  (define astream
    (let ()
      (define rate (box 441000))
      (command-line
       #:once-any
       [("-r" "--rate") sample-rate "Sample rate (default 441000)"
                        (set-box! rate (string->number sample-rate))]
       #:args (filename)
       (ecktra-decode-audio-file (unbox rate) filename))))
  (for ([sample (in-audio-stream astream)])
    (displayln sample))
  (ecktra-stream-close astream)
  ;
  )
