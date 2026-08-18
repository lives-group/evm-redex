#lang racket/base

;; Tutorial example — "Safe Remote Purchase" from Solidity by Example.
;;   https://docs.soliditylang.org/en/v0.8.36/solidity-by-example.html#safe-remote-purchase
;; A four-state escrow: Created -> Locked -> Release -> Inactive.
;; Runnable version of the tutorial's purchase walkthrough:
;;   raco test tutorial/purchase.rkt

(require rackunit racket/path
         evm-redex/pbt
         (only-in evm-redex world-ref world-set acct-nonce acct-balance acct-with-balance))

(define ETH (expt 10 18))
(define CREATED 0) (define LOCKED 1) (define RELEASE 2) (define INACTIVE 3)

;; --- deploy: the seller locks 2x the item value (a payable constructor) ---
(define HERE (path-only (path->complete-path (syntax-source #'here))))
(define ART  (read-artifact (build-path HERE "Purchase.json") #:contract "Purchase"))

(define SELLER #x0000000000000000000000000000000053E11E01)
(define BUYER  #x0000000000000000000000000000000000B0BEA01)   ; the buyer (any EOA)

;; the item value will be 100, so the seller deposits 200 at creation
(define DEP (deploy (artifact-creation ART) #:from SELLER #:value 200))
(check-true (deploy-result-ok? DEP) "Purchase deploys with an even deposit")
(define PURCHASE (deploy-result-address DEP))
(define BLOCK    '(block 0 0 0 0 0 0 30000000 1 Prague))

(define (fund w a wei)
  (world-set w a (acct-with-balance (world-ref w a) (+ (acct-balance (world-ref w a)) wei))))
(define BASE (fund (deploy-result-world DEP) BUYER (* 10 ETH)))

;; --- helpers ----------------------------------------------------------
(define (nonce-of w a) (acct-nonce (world-ref w a)))
(define (send w from value sig . args)
  (run-txn (make-tx #:sender from #:nonce (nonce-of w from) #:to PURCHASE #:value value
                    #:gas-limit 300000 #:gas-price 0 #:data (apply encode-call sig args))
           w BLOCK))
(define (state-of w)
  (car (abi-decode (list "uint256") (call-result-return (call w PURCHASE #:data (encode-call "state()"))))))
(define (value-of w)
  (car (abi-decode (list "uint256") (call-result-return (call w PURCHASE #:data (encode-call "value()"))))))

(check-equal? (state-of BASE) CREATED "a fresh purchase is in the Created state")
(check-equal? (value-of BASE) 100      "the item value is half the deposit")

;; --- the happy path: buyer locks funds, then confirms receipt ---------
(define R-BUY (send BASE BUYER 200 "confirmPurchase()"))
(check-eq? (txn-run-outcome R-BUY) 'success "buyer matches the deposit")
(define LOCKED-W (txn-run-world1 R-BUY))
(check-equal? (state-of LOCKED-W) LOCKED)

(define R-RECV (send LOCKED-W BUYER 0 "confirmReceived()"))
(check-eq? (txn-run-outcome R-RECV) 'success)
(check-equal? (state-of (txn-run-world1 R-RECV)) RELEASE "confirming receipt releases the escrow")

;; --- a property: only the buyer may confirm receipt -------------------
;; From the Locked state, confirmReceived() from anyone other than the buyer
;; reverts (the `onlyBuyer` modifier).
(define OUTSIDER #x00000000000000000000000000000000BADA55001)
(define-evm-property only-buyer-confirms-receipt
  #:given ()
  #:world LOCKED-W
  #:call  (make-tx #:sender OUTSIDER #:nonce 0 #:to PURCHASE #:gas-limit 300000 #:gas-price 0
                   #:data (encode-call "confirmReceived()"))
  #:revert-when #t)

(check-evm-property only-buyer-confirms-receipt #:trials 1)

;; --- a property: confirmPurchase requires exactly twice the value -----
(define ANY-BUYER #x000000000000000000000000000000000B0B0001)
(define BASE2 (fund (deploy-result-world DEP) ANY-BUYER (* 10 ETH)))
(define-evm-property confirm-needs-exact-deposit
  #:given ([v (gen-word-in 0 400)])
  #:world BASE2
  #:call  (make-tx #:sender ANY-BUYER #:nonce (nonce-of BASE2 ANY-BUYER) #:to PURCHASE #:value v
                   #:gas-limit 300000 #:gas-price 0 #:data (encode-call "confirmPurchase()"))
  #:post  (lambda (w0 w1 r)
            (if (= v 200)
                (eq? (txn-run-outcome r) 'success)
                (eq? (txn-run-outcome r) 'revert))))

(check-evm-property confirm-needs-exact-deposit #:trials 60)
