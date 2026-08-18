#lang s-exp syntax/module-reader

;; The reader for `#lang evm-redex/asm`.  It hands the entire source text to the
;; module language (evm-redex/asm/runtime) as a single string; the assembling and
;; running happen there.  Keeping the reader this thin means the one parser
;; (asm/parse.rkt, reached through `assemble`) is the only place that understands
;; the assembly syntax, whether reached via `#lang` or via `(require
;; evm-redex/asm)` + `assemble`.

evm-redex/asm/runtime

#:read        evm-read
#:read-syntax evm-read-syntax

(require racket/port)

;; `#:read` / `#:read-syntax` are called REPEATEDLY until they return eof (like
;; `read`).  The whole source is one datum — the assembly text as a string — so
;; return it once and then eof, or module-reader loops forever appending "".
(define (evm-read in)
  (if (eof-object? (peek-char in)) eof (port->string in)))
(define (evm-read-syntax src in)
  (if (eof-object? (peek-char in)) eof (datum->syntax #f (port->string in))))
