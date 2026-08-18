#lang racket/base

;; ======================================================================
;; The module language for `#lang evm-redex/asm`.
;;
;; The reader (asm/lang/reader.rkt) hands the whole assembly source to this
;; module as a single string; our `#%module-begin` assembles it, runs it on the
;; interpreter, prints a summary when the module is instantiated, and provides
;; the artefacts for programmatic use / tests:
;;
;;   program-bytes  : the assembled bytecode (list of bytes)
;;   result         : the evm-result (outcome, gas, stack, returndata, machine)
;;   final-machine  : the final machine term
;; ======================================================================

(require racket/format
         (only-in "assemble.rkt" assemble)
         (only-in "execute.rkt" run-source
                  evm-result-outcome evm-result-gas-used evm-result-stack
                  evm-result-returndata evm-result-machine))

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [asm-module-begin #%module-begin]))

(define-syntax-rule (asm-module-begin src)
  (#%module-begin
   (define-values (program-bytes the-config) (assemble src))
   (define result (run-source program-bytes the-config))
   (define final-machine (evm-result-machine result))
   (provide program-bytes result final-machine)
   ;; the summary is printed on instantiation (i.e. when you Run the file)
   (report result)))

;; --- the printed summary ----------------------------------------------
(define (report r)
  (printf "outcome:     ~a\n" (evm-result-outcome r))
  (printf "gas used:    ~a\n" (evm-result-gas-used r))
  (printf "stack:       ~a\n" (fmt-stack (evm-result-stack r)))
  (printf "returndata:  ~a\n" (fmt-bytes (evm-result-returndata r))))

;; stack top first, each word as 0x-hex
(define (fmt-stack s)
  (if (null? s) "[]"
      (string-append "[" (fmt-join (map word->hex s) ", ") "]")))

(define (word->hex n) (string-append "0x" (number->string n 16)))

(define (fmt-bytes bs)
  (string-append "0x" (apply string-append (map (lambda (b) (~r b #:base 16 #:min-width 2 #:pad-string "0")) bs))))

(define (fmt-join xs sep)
  (cond [(null? xs) ""]
        [(null? (cdr xs)) (car xs)]
        [else (string-append (car xs) sep (fmt-join (cdr xs) sep))]))
