#lang typed/racket/base

(require typed/racket/unsafe
         racket/sequence
         math/base
         math/flonum
         racket/flonum
         racket/unsafe/ops)

(provide (all-defined-out))

(module untyped racket/base
  (provide (all-defined-out))
  (define (unsafe-cast x) x))
(unsafe-require/typed
 'untyped
 [unsafe-cast (All (A B) (-> A B))])
(provide unsafe-cast)

(: fft! (->* (Boolean FlVector)
             [(Option FlVector)
              (Option FlVector)
              (Option FlVector)]
             Void))
(define (fft! inverse real [imag #f] [mag #f] [arg #f])
  (define len : Fixnum (flvector-length real))
  (unless (power-of-two? len)
    (raise-argument-error 'fft "power-of-two?" len))
  (let ()
    (define (correct-length? [v : (Option FlVector)])
      (or (not v) (= (flvector-length v) len)))
    (define to-check (list imag mag arg))
    (unless (andmap correct-length? to-check)
      (raise-arguments-error 'fft! "expected flvectors of same length"
                             "real" real
                             "imag" imag
                             "mag" mag
                             "arg" arg)))
  (define x real)
  (define y (or imag (make-flvector len)))

  (: log2 (-> Integer Integer))
  (define (log2 n)
    (let loop
        ([n n]
         [l2 0])
      (if (= 1 n)
          l2
          (loop (quotient n 2) (add1 l2)))))

  (define m (log2 len))

  ;; do the bit reversal
  (define i2 : Fixnum (unsafe-fxrshift len 1))
  (define j : Fixnum 0)
  (for ([i (in-range (sub1 len))])
    (when (unsafe-fx< i j)
      (define tx (unsafe-flvector-ref x i))
      (define ty (unsafe-flvector-ref y i))
      (unsafe-flvector-set! x i (unsafe-flvector-ref x j))
      (unsafe-flvector-set! y i (unsafe-flvector-ref y j))
      (unsafe-flvector-set! x j tx)
      (unsafe-flvector-set! y j ty))
    (let loop ([k i2])
      (cond
        [(unsafe-fx<= k j)
         (set! j (unsafe-fx- j k))
         (loop (unsafe-fxrshift k 1))]
        [else
         (set! j (unsafe-fx+ j k))])))

  ;; Compute the FFT
  (define c1 : Flonum -1.0)
  (define c2 : Flonum 0.0)
  (define l2 : Fixnum 1)
  (for ([l (in-range m)])
    (define l1 : Fixnum l2)
    (set! l2 (unsafe-fxlshift l2 1))
    (define u1 : Flonum 1.0)
    (define u2 : Flonum 0.0)
    (for ([j (in-range l1)])
      (for ([i (in-range j len l2)])
        (define i1 (unsafe-fx+ i l1))
        (define t1
          (unsafe-fl- (unsafe-fl* u1 (unsafe-flvector-ref x i1))
                      (unsafe-fl* u2 (unsafe-flvector-ref y i1))))
        (define t2
          (unsafe-fl+ (unsafe-fl* u1 (unsafe-flvector-ref y i1))
                      (unsafe-fl* u2 (unsafe-flvector-ref x i1))))
        (unsafe-flvector-set!
         x i1 (unsafe-fl- (unsafe-flvector-ref x i) t1))
        (unsafe-flvector-set!
         y i1 (unsafe-fl- (unsafe-flvector-ref y i) t2))
        (unsafe-flvector-set!
         x i (unsafe-fl+ (unsafe-flvector-ref x i) t1))
        (unsafe-flvector-set!
         y i (unsafe-fl+ (unsafe-flvector-ref y i) t2)))
      (define z
        (unsafe-fl- (unsafe-fl* u1 c1)
                    (unsafe-fl* u2 c2)))
      (set! u2
        (unsafe-fl+ (unsafe-fl* u1 c2)
                    (unsafe-fl* u2 c1)))
      (set! u1 z))
    (set! c2 (unsafe-flsqrt (unsafe-fl/ (unsafe-fl- 1.0 c1) 2.0)))
    (unless inverse
      (set! c2 (unsafe-fl- 0.0 c2)))
    (set! c1 (unsafe-flsqrt (unsafe-fl/ (unsafe-fl+ 1.0 c1) 2.0))))

  ;; scale for inverse transform
  (when inverse
    (define fllen : Flonum (exact->inexact len))
    (for ([i (in-range len)])
      (unsafe-flvector-set!
       x i (unsafe-fl/ (unsafe-flvector-ref x i) fllen))
      (unsafe-flvector-set!
       y i (unsafe-fl/ (unsafe-flvector-ref y i) fllen))))

  (when (or arg mag)
    (for ([i (in-range len)])
      (define re : Flonum (unsafe-flvector-ref x i))
      (define im : Flonum (unsafe-flvector-ref y i))
      (when arg (unsafe-flvector-set! arg i (unsafe-flatan (unsafe-fl/ re im))))
      (when mag (unsafe-flvector-set! mag i (flhypot re im)))))

  (void))

(: flvector-dft-mag! (-> FlVector FlVector))
(define (flvector-dft-mag! v)
  (fft! #false v #f v)
  v)

(: flvector-idft-mag! (-> FlVector FlVector))
(define (flvector-idft-mag! v)
  (fft! #true v #f v)
  v)

(: flvector-dft-mag* (-> FlVector FlVector))
(define (flvector-dft-mag* v)
  (flvector-dft-mag! (flvector-copy v)))

(: flvector-idft-mag* (-> FlVector FlVector))
(define (flvector-idft-mag* v)
  (flvector-idft-mag! (flvector-copy v)))

(: flvector-foldl (All (A) (-> (-> A Flonum A) A FlVector A)))
(define (flvector-foldl f init flv)
  (for/fold : A
      ([x init])
      ([s (in-flvector flv)])
    (f x s)))

(: flvector-map!
   (case->
    [(-> Flonum Flonum) FlVector -> FlVector]
    [(-> Flonum Flonum Flonum) FlVector FlVector -> FlVector]
    [(-> Flonum Flonum Flonum Flonum) FlVector FlVector FlVector -> FlVector]))
(define flvector-map!
  (case-lambda
    [(f v)
     (for ([(x i) (in-indexed (in-flvector v))])
       (flvector-set! v i (f x)))
     v]
    [(f v1 v2)
     (for ([i (in-naturals)]
           [x1 (in-flvector v1)]
           [x2 (in-flvector v2)])
       (flvector-set! v1 i (f x1 x2)))
     v1]
    [(f v1 v2 v3)
     (for ([i (in-naturals)]
           [x1 (in-flvector v1)]
           [x2 (in-flvector v2)]
           [x3 (in-flvector v3)])
       (flvector-set! v1 i (f x1 x2 x3)))
     v1]))

(: flvector-map-to-vector (All (T) (-> (-> Flonum T) FlVector (Vectorof T))))
(define (flvector-map-to-vector f v)
  (for/vector : (Vectorof T)
      #:length (flvector-length v)
      ([x (in-flvector v)])
      (f x)))

(: fllerp
   (case->
    [Flonum Flonum Flonum -> Flonum]
    [Flonum Flonum -> (-> Flonum Flonum)]))
(define fllerp
  (case-lambda
    [(x minv maxv)
     (+ minv (* x (- maxv minv)))]
    [(minv maxv)
     (λ (x) (fllerp x minv maxv))]))

(: flunlerp
   (case->
    [Flonum Flonum Flonum -> Flonum]
    [Flonum Flonum -> (-> Flonum Flonum)]))
(define flunlerp
  (case-lambda
    [(x minv maxv)
     (/ (- x minv) (- maxv minv))]
    [(minv maxv)
     (λ (x) (flunlerp x minv maxv))]))

(: flremap
   (case->
    [Flonum Flonum Flonum Flonum Flonum -> Flonum]
    [Flonum Flonum Flonum Flonum -> (-> Flonum Flonum)]))
(define flremap
  (case-lambda
    [(x min-in max-in min-out max-out)
     (fllerp (flunlerp x min-in max-in) min-out max-out)]
    [(min-in max-in min-out max-out)
     (λ (x) (flremap x min-in max-in min-out max-out))]))

(: lerp
   (case->
    [Real Real Real -> Real]
    [Real Real -> (-> Real Real)]))
(define lerp
  (case-lambda
    [(x minv maxv)
     (+ minv (* x (- maxv minv)))]
    [(minv maxv)
     (λ (x) (lerp x minv maxv))]))

(: unlerp
   (case->
    [Real Real Real -> Real]
    [Real Real -> (-> Real Real)]))
(define unlerp
  (case-lambda
    [(x minv maxv)
     (/ (- x minv) (- maxv minv))]
    [(minv maxv)
     (λ (x) (unlerp x minv maxv))]))

(: remap
   (case->
    [Real Real Real Real Real -> Real]
    [Real Real Real Real -> (-> Real Real)]))
(define remap
  (case-lambda
    [(x min-in max-in min-out max-out)
     (lerp (unlerp x min-in max-in) min-out max-out)]
    [(min-in max-in min-out max-out)
     (λ (x) (remap x min-in max-in min-out max-out))]))

(: make-hann-window (-> Integer FlVector))
(define (make-hann-window N)
  (for/flvector #:length N ([i (in-range N)])
    (cast (exact->inexact (expt (sin (/ (* pi i) N)) 2.0)) Flonum)))

(: frequency-resolution
   (-> Nonnegative-Integer
       Nonnegative-Integer
       Nonnegative-Exact-Rational))
(define (frequency-resolution sample-rate bufsz)
  (/ sample-rate bufsz))

(: frequency->bucket
   (-> Nonnegative-Exact-Rational
       Nonnegative-Integer
       Nonnegative-Integer
       Nonnegative-Integer))
(define (frequency->bucket f sample-rate bufsz)
  (exact-round (/ (* f bufsz) sample-rate)))

(: bucket->frequency
   (-> Nonnegative-Integer
       Nonnegative-Integer
       Nonnegative-Integer
       Nonnegative-Exact-Rational))
(define (bucket->frequency b sample-rate bufsz)
  (/ (* b sample-rate) bufsz))

(: flvector-rms (-> FlVector Flonum))
(define (flvector-rms flv)
  (/ (for/fold : Flonum ([s 0.0]) ([x (in-flvector flv)])
       (+ s (expt x 2)))
     (flvector-length flv)))

(: bucketise-with (->* (FlVector (-> Flonum Flonum Flonum)) (Integer) (-> FlVector FlVector)))
(define (bucketise-with xs merge [ln #f])
  (define len (or ln (flvector-length xs)))
  (define bucket-fns
    : (Vectorof (U (Listof Integer)
                   (Vector Real Integer)
                   Integer
                   False))
    (make-vector len #f))
  (for ([x (in-flvector xs)]
        [lx (sequence-append (list 0) (in-flvector xs))]
        [i (in-range len)])
    (define ix (floor (inexact->exact (floor x))))
    (define ilx (floor (inexact->exact (floor lx))))
    (let ()
      (define old-bucket (vector-ref bucket-fns ix))
      (unless (or (vector? old-bucket) (integer? old-bucket))
        (vector-set! bucket-fns ix (cons i (or old-bucket null)))))
    (for ([ii (in-range ilx ix)])
      (unless (vector-ref bucket-fns ii)
        (vector-set! bucket-fns ii (vector (unlerp ii ilx ix) i)))))
  (for ([i (in-range len)])
    (define old-bucket (vector-ref bucket-fns i))
    (when (and (list? old-bucket) (null? (cdr old-bucket)))
      (vector-set! bucket-fns i (car old-bucket))))
  (define (bucketise [flv : FlVector])
    (for/flvector #:length len
        ([f bucket-fns])
      (cond
        [(vector? f)
         (define i (vector-ref f 1))
         (cast
          (lerp (vector-ref f 0)
                (flvector-ref flv (sub1 i))
                (flvector-ref flv i))
          Flonum)]
        [(integer? f) (flvector-ref flv f)]
        [(list? f)
         (for/fold : Flonum
             ([x (flvector-ref flv (car f))])
             ([i (in-list (cdr f))])
           (merge x (flvector-ref flv i)))]
        [else 0.0])))
  bucketise)

(: logarithmic-posns (-> Nonnegative-Integer Nonnegative-Integer
                         Real Real
                         FlVector))
(define (logarithmic-posns minidx maxidx minout maxout)
  (define log-minidx (real->double-flonum (log minidx)))
  (define log-maxidx (real->double-flonum (log maxidx)))
  (for/flvector
      #:length (- maxidx minidx)
      ([i (in-range minidx maxidx)])
      (flremap (unsafe-cast (log i))
               log-minidx
               log-maxidx
               (real->double-flonum minout)
               (real->double-flonum maxout))))
