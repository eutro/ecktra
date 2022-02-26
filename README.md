# Ecktra

![logo](logo.svg)

Easily extensible[^fn], expressive[^fn] audio visualization in Racket.

[^fn]: Your experience may vary.

# Usage

Ecktra is currently in development (and may remain so indefinitely),
thus documentation and such is severely lacking (read: there is none).
If you wish to use this, you can try the [examples](examples/),
and maybe even try to understand them.

To install, you should clone this repository and run

```sh
raco pkg install
```

in it.

[ffmpeg](https://ffmpeg.org/) is required to run, and it must be on
the PATH so that `find-executable-path` can find it. ffplay is also
required to play the sounds.

To use an audio visualizer (such as the [examples](examples/)),
run it on the command line with `racket`:

```sh
racket ./examples/dft-circle.rkt /audio/path/or/url/
```

Run with `--help` to see what options are available to all Ecktra audio visualizers.

Some visualizers may also take extra arguments after the audio file:

```
racket ./examples/amplitude-image.rkt /audio/path/or/url/ --image https://racket-lang.org/img/racket-logo.svg
```

which can also be accessed by `--help`, provided it is put after the audio file.

If you are stuck for music to try it on, I've downloaded a few songs into [audio](audio/) c:

## The Visualizer Language

Ecktra uses a domain specific language based on typed Racket.  Each
program in this represents an audio visualizer. Examples can be found
in the [examples](examples/) directory (in case you didn't know already).

A typical audio visualizer may look something like this:

```racket
#lang ecktra/visualizer

(require ...) ;; some requires

(define ...) ;; some top level definitions

;; the DSL only recognises a few top-level forms before the options below,
;; if you need to use otherwise unsupported forms, you can use a `begin'
(begin ...)

#:backbuf ... ;; latency and back-buffer options,
#:latency ... ;; to declare how far into the future and past we can take samples from

(command-line
 ...) ;; possibly parsing command line options

(bind ...) ;; a sequence of steps consuming the signal...
(define ...)
(void ...)
(pure ...) ;; ...returning a pict at the end
```

To be more general, an audio visualizer takes the following form:

1. `#lang ecktra/visualizer`
2. A series of top-level statements, which will be put inline in the module body:
   - These can be any of `require`, `#%require`, `define`, `define-values`, `void` or `begin`,
     with `begin` letting you use anything else.
3. Some options specified with `#:keyword value`:
   - `#:latency` (default 1): how far "into the future" samples can be read from,
     which in practice translates to the (absolute minimum) latency
     that Ecktra can even begin decoding with.
   - `#:backbuf` (default 0): how many samples are stored after their time has passed.
4. A sequence of statements defining what should be done to the signal.
   These are wrapped in a [`seq` block](#Signals_and_Seq_Blocks),
   and must return a `(Signal T)`, with the constraints on `T` being output-dependent.
   Notably, `(Signal pict)` is always good.
   - When rendering to a GUI, if `T` is `pict-convertible?`, it will be
     drawn to the screen, otherwise `T` will be converted to a string
     (by `display`), and that will be rendered instead.
   - When rendering to a file, `T` must be `file/convertible` to `png-bytes+bounds`.

Everything in `typed/racket/base` is available, as well as various
signal-related functions. See
[ecktra/visualizer/lang](visualizer/lang.rkt) for which modules are
provided. Anything else can be `require`d as usual, including
`typed/racket` if you don't mind the namespace pollution.

## Signals and Seq Blocks

The `(Signal T)` type represents a "signal" of `T`s. That is, a
function from time to `T`, sampled at discrete intervals. If you are
familiar with
[monads](https://en.wikipedia.org/wiki/Monad_(functional_programming)),
that is what these are[^fn1], and some familiar functions are defined
for signals (see [ecktra/visualizer/signal](visualizer/signal.rkt)).
The most important such function is `pure : (-> T (Signal T))`, which
takes any `T` and produces a "pure" signal of it: one which returns
that value when sampled at any time.

[^fn1]: There may be one or two laws broken by the use of interior
    mutability by IIR (including Biquad) filters, and anything else
    using `signal-fold`. To be safe, make sure to define any such
    signals before your first `bind` or `>>=`, otherwise you might get
    unexpected results.

A `seq` block (which is implicit at the top of a visualizer program),
represents a sequencing of signal transformations. It is like a
`begin` block, but you must return a `(Signal T)` at the end, need to
wrap normal statements in `void`, and you can use `bind`. If you are
familiar with Haskell, this is a `do` block, and `bind` is `<-`.

A `bind` statement is of the form (the type hint is necessary):

```racket
(bind name : Type signal)
```

where `signal` is of type `(Signal Type)`.

If you don't want to think about it, this is just like a `define`
statement, but it lets you take the `T` out of a `(Signal T)`.

If you *do* want to think about it, this produces a new signal that,
when sampled at a given time, samples `signal` at that time, and
evaluates the rest of the `seq` block with the sample bound to `name`.

[Samples](https://en.wikipedia.org/wiki/Sampling_(signal_processing))
are provided to audio visualizers through the `(samples)` parameter,
of type `(Signal Flonum)`. Additionally, there are many useful signal
operations defined in [ecktra/visualizer/processes](visualizer/processes.rkt).
