#lang s-exp syntax/module-reader

;; The reader for `#lang evm-redex/sim`.  Like the asm reader, it hands the entire
;; scenario text to the module language (evm-redex/sim/runtime) as one string;
;; parsing and execution happen there.  `#:read` / `#:read-syntax` are called
;; repeatedly until eof, so the whole source is returned once and then eof (a
;; single non-eof list would loop forever).

evm-redex/sim/runtime

#:read        sim-read
#:read-syntax sim-read-syntax

(require racket/port)

(define (sim-read in)
  (if (eof-object? (peek-char in)) eof (port->string in)))
(define (sim-read-syntax src in)
  (if (eof-object? (peek-char in)) eof (datum->syntax #f (port->string in))))
