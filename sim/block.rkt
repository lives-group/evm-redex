#lang racket/base

;; ======================================================================
;; Block-boundary processing, decoupled from the JSON fixtures it was born in
;; (evm-redex-tests/conformance/blockchain-harness.rkt): the EIP-4788 beacon-root
;; and EIP-2935 history system calls run at the start of a block, and the
;; EIP-4895 withdrawals credited at the end.  The simulator's `sim-mine!` uses
;; these; a batch driver could fold them around a block's transactions.
;; ======================================================================

(require (only-in "../private/interpreter.rkt"
                  run-top-frame resolve-code orig-storage-for reset-tx-state!
                  machine-halt machine-field)
         (only-in "../private/state.rkt"
                  world-ref world-set account-exists? acct-balance acct-with-balance))

(provide SYSTEM-ADDRESS BEACON-ROOTS-ADDRESS HISTORY-STORAGE-ADDRESS
         system-call apply-withdrawal apply-withdrawals)

;; the well-known system addresses (EIP-4788 / EIP-2935)
(define SYSTEM-ADDRESS         #xfffffffffffffffffffffffffffffffffffffffe)
(define BEACON-ROOTS-ADDRESS   #x000F3df6D732807Ef1319fB7B8bB8522d0Beac02)
(define HISTORY-STORAGE-ADDRESS #x0000F90827F1C53a10cb7A02335B175320002935)

;; system-call : world block to data -> world
;; Run `to`'s code as the system address (no gas accounting that matters, no
;; nonce), committing the resulting world only on success.  A no-op if `to` has
;; no account (the system contracts are optional).
(define (system-call world block to data)
  (cond
    [(not (account-exists? world to)) world]
    [else
     (reset-tx-state! world)
     (define done (run-top-frame #:caller SYSTEM-ADDRESS #:target to #:value 0 #:data data
                                 #:code (resolve-code world to) #:code-addr to #:gas 30000000
                                 #:world world #:accessed (list (list SYSTEM-ADDRESS to) '())
                                 #:block block #:tx (list 'tx SYSTEM-ADDRESS 0 '())
                                 #:depth 0 #:static #f #:orig (orig-storage-for to world)))
     (if (memq (car (machine-halt done)) '(stop return))
         (car (machine-field done 'world))
         world)]))

;; apply-withdrawal : world addr amount-wei -> world  (credits the balance)
(define (apply-withdrawal world addr amount-wei)
  (define a (world-ref world addr))
  (world-set world addr (acct-with-balance a (+ (acct-balance a) amount-wei))))

;; apply-withdrawals : world (listof (cons addr amount-wei)) -> world
(define (apply-withdrawals world ws)
  (for/fold ([w world]) ([wd (in-list ws)])
    (apply-withdrawal w (car wd) (cdr wd))))
