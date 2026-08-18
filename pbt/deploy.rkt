#lang racket/base

;; ======================================================================
;; deploy — run a contract's CREATION code through the real semantics and
;; return the resulting world (with the runtime code installed) plus the
;; contract address.  Use it to obtain a realistic post-deployment world —
;; constructor storage, immutables patched into the runtime, etc. — to chain
;; into transaction-mode properties, instead of installing `deployedBytecode`
;; directly (which skips the constructor).
;;
;; Deployment is a contract-creation transaction (to = #f), so it uses CREATE
;; semantics: address = keccak(rlp(deployer, nonce))[12:], with `nonce` the
;; deployer's nonce at the start of the transaction.
;; ======================================================================

(require (only-in "../private/interpreter.rkt" compute-create-address)
         (only-in "../private/transaction.rkt" make-tx)
         (only-in "../private/state.rkt"
                  world-ref world-set acct-nonce acct-balance acct-code
                  acct-with-balance acct-with-nonce)
         "execute.rkt")

(provide (struct-out deploy-result) deploy)

;; world      : the post-deployment world (runtime installed on success)
;; address    : the created contract address (deterministic from deployer+nonce)
;; code       : the installed runtime code (list of bytes) on success, else #f
;; ok?        : whether creation succeeded
(struct deploy-result (world address code ok? gas-used err) #:transparent)

;; an arbitrary default deployer EOA
(define DEFAULT-DEPLOYER #x00000000000000000000000000000000C0FFEE01)
(define DEPLOY-BLOCK '(block 0 0 0 0 0 0 30000000 1 Prague))

;; ensure the deployer exists with the given nonce and enough balance
;; (gas-price is 0, so it only needs to cover the endowment `value`).
(define (ensure-deployer world from nonce value)
  (define a (world-ref world from))
  (define bal (max (acct-balance a) (+ value (expt 2 80))))
  (world-set world from (acct-with-nonce (acct-with-balance a bal) nonce)))

(define (deploy creation
                #:from  [from DEFAULT-DEPLOYER]
                #:value [value 0]
                #:gas   [gas 30000000]
                #:nonce [nonce #f]
                #:world [world '()]
                #:block [block DEPLOY-BLOCK])
  (define n (or nonce (acct-nonce (world-ref world from))))
  (define w0 (ensure-deployer world from n value))
  (define tx (make-tx #:sender from #:nonce n #:to #f #:value value
                      #:data creation #:gas-limit gas #:gas-price 0))
  (define r (run-txn tx w0 block))
  (define w1 (txn-run-world1 r))
  (define addr (compute-create-address from n))
  (define ok? (eq? (txn-run-outcome r) 'success))
  (deploy-result w1 addr
                 (and ok? (acct-code (world-ref w1 addr)))
                 ok? (txn-run-gas-used r) (txn-run-err r)))
