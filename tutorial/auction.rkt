#lang racket/base

;; Tutorial example — the open auction ("Simple Auction") from Solidity by Example.
;;   https://docs.soliditylang.org/en/v0.8.36/solidity-by-example.html#simple-open-auction
;; Runnable version of the tutorial's auction walkthrough:
;;   raco test tutorial/auction.rkt

(require rackunit racket/path
         evm-redex/pbt
         (only-in evm-redex world-ref world-set acct-nonce acct-balance acct-with-balance))

(define ETH (expt 10 18))

;; --- deploy: constructor is (uint biddingTime, address beneficiary) ----
(define HERE (path-only (path->complete-path (syntax-source #'here))))
(define ART  (read-artifact (build-path HERE "SimpleAuction.json") #:contract "SimpleAuction"))

(define SELLER #x0000000000000000000000000000000053E11E01)
(define ALICE  #x000000000000000000000000000000000A11CE001)
(define BOB    #x00000000000000000000000000000000B0B00B001)

(define DEP (deploy (append (artifact-creation ART)
                            (abi-encode (list "uint256" "address") (list 1000 SELLER)))
                    #:from SELLER))
(check-true (deploy-result-ok? DEP) "SimpleAuction deploys")
(define AUCTION (deploy-result-address DEP))
(define BLOCK   '(block 0 0 0 0 0 0 30000000 1 Prague))

;; give the bidders some ether to bid with (this is what `deploy` does for the
;; deployer; here we top up two more accounts by hand)
(define (fund w a wei)
  (world-set w a (acct-with-balance (world-ref w a) (+ (acct-balance (world-ref w a)) wei))))
(define BASE (fund (fund (deploy-result-world DEP) ALICE (* 10 ETH)) BOB (* 10 ETH)))

;; --- helpers: a bid of `value`; read the highest bid/bidder -----------
(define (nonce-of w a) (acct-nonce (world-ref w a)))
(define (bid w from value)
  (run-txn (make-tx #:sender from #:nonce (nonce-of w from) #:to AUCTION #:value value
                    #:gas-limit 300000 #:gas-price 0 #:data (encode-call "bid()"))
           w BLOCK))
(define (highest-bid w)
  (car (abi-decode (list "uint256") (call-result-return (call w AUCTION #:data (encode-call "highestBid()"))))))
(define (highest-bidder w)
  (car (abi-decode (list "address") (call-result-return (call w AUCTION #:data (encode-call "highestBidder()"))))))

;; --- a concrete run: bids must strictly increase ----------------------
(define R1 (bid BASE ALICE 100))
(check-eq? (txn-run-outcome R1) 'success)
(define W1 (txn-run-world1 R1))
(check-equal? (highest-bid W1) 100)
(check-equal? (highest-bidder W1) ALICE)

(check-eq? (txn-run-outcome (bid W1 BOB 50)) 'revert "a bid at or below the highest is rejected")

(define W2 (txn-run-world1 (bid W1 BOB 200)))
(check-equal? (highest-bid W2) 200 "a higher bid wins")
(check-equal? (highest-bidder W2) BOB)

;; --- a property: any accepted bid strictly raises the highest bid -----
;; From W1 (highest bid = 100), a bid of value v succeeds exactly when v > 100,
;; and then the highest bid becomes v.
(define-evm-property bid-raises-the-highest
  #:given ([v (gen-word-in 0 500)])
  #:world W1                                   ; highest bid is 100 here
  #:call  (make-tx #:sender BOB #:nonce (nonce-of W1 BOB) #:to AUCTION #:value v
                   #:gas-limit 300000 #:gas-price 0 #:data (encode-call "bid()"))
  #:post  (lambda (w0 w1 r)
            (if (> v (highest-bid w0))
                (and (eq? (txn-run-outcome r) 'success) (= (highest-bid w1) v))
                (eq? (txn-run-outcome r) 'revert))))

(check-evm-property bid-raises-the-highest #:trials 60)
