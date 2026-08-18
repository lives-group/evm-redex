#lang racket/base

;; ======================================================================
;; Run assembled bytecode on the library's interpreter and package the outcome.
;;
;; This is the same ~10-line wrapper `run-fragment` (pbt/execute.rkt) uses, but
;; built directly on the core `(require evm-redex)` surface so the language does
;; NOT depend on the pbt layer (and hence not on rackcheck): a fresh top-level
;; frame, the fast Racket executor pinned, the tx-level boxes reset, and any
;; Racket-level failure (fuel exhaustion) folded into an 'error outcome.
;; ======================================================================

(require racket/list
         (only-in evm-redex
                  fresh-machine run machine-field machine-halt reset-tx-state!
                  current-exec-backend)
         (only-in "assemble.rkt" asm-config-gas asm-config-calldata))

(provide (struct-out evm-result) run-source)

;; outcome    : the halt tag — 'stop | 'return | 'revert | 'out-of-gas |
;;              'stack-underflow | 'invalid-opcode | 'error | ...
;; gas-used   : gas consumed (initial minus remaining)
;; gas-left   : gas remaining
;; stack      : the final stack, top element first (list of 256-bit words)
;; returndata : the RETURN/REVERT bytes (list), or '() otherwise
;; machine    : the final machine term, for callers who want everything else
;; err        : an exception message string, or #f
(struct evm-result (outcome gas-used gas-left stack returndata machine err) #:transparent)

;; run-source : (listof byte) asm-config -> evm-result
(define (run-source code config)
  (define gas (asm-config-gas config))
  (define calldata (asm-config-calldata config))
  ;; msg = (msg caller target value data code-addr gas depth static?)
  (define msg (list 'msg 0 0 0 calldata 0 gas 0 #f))
  (reset-tx-state! '())
  (parameterize ([current-exec-backend 'racket])
    (with-handlers ([exn:fail? (lambda (e)
                                 (evm-result 'error gas gas '() '() #f (exn-message e)))])
      (define m0 (fresh-machine code gas #:msg msg))
      (define m (run m0))
      (define halt (machine-halt m))
      (define tag (if (pair? halt) (car halt) halt))
      (define gas-left (car (machine-field m 'gas)))
      (evm-result tag
                  (- gas gas-left)
                  gas-left
                  (car (machine-field m 'stack))
                  (if (and (pair? halt) (memq (car halt) '(return revert))) (cadr halt) '())
                  m
                  #f))))
