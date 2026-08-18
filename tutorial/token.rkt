#lang racket/base

;; Tutorial example — testing Token.sol with evm-redex/pbt.
;; Runnable version of the tutorial's token walkthrough:
;;   raco test tutorial/token.rkt

(require rackunit racket/path
         evm-redex/pbt)

;; --- deploy -----------------------------------------------------------
(define HERE (path-only (path->complete-path (syntax-source #'here))))
(define ART  (read-artifact (build-path HERE "Token.json") #:contract "Token"))

(define DEPLOYER #x00000000000000000000000000000000DEB00001)
(define ALICE    #x000000000000000000000000000000000000A11CE)
(define BOB      #x0000000000000000000000000000000000000B0B00)

(define DEP   (deploy (artifact-creation ART) #:from DEPLOYER))
(check-true (deploy-result-ok? DEP) "Token deploys")
(define TOKEN (deploy-result-address DEP))

;; --- getters, decoded from the ABI ------------------------------------
(define (u256 r) (car (abi-decode (list "uint256") (call-result-return r))))
(define (bal w a) (u256 (call w TOKEN #:data (encode-call "balanceOf(address)" a))))
(define (total w) (u256 (call w TOKEN #:data (encode-call "totalSupply()"))))

;; --- build a world where ALICE holds 1000 tokens ----------------------
(define (send w from sig . args)
  (call-result-world (call w TOKEN #:from from #:data (apply encode-call sig args))))

(define MINTED (send (deploy-result-world DEP) ALICE "mint(address,uint256)" ALICE 1000))
(check-equal? (bal MINTED ALICE) 1000 "mint credits the balance")
(check-equal? (total MINTED) 1000     "mint raises the supply")

;; a direct transfer moves the tokens
(let ([w (send MINTED ALICE "transfer(address,uint256)" BOB 100)])
  (check-equal? (bal w ALICE) 900)
  (check-equal? (bal w BOB) 100))

;; --- property: transfer conserves supply, moving the amount or reverting
(define (tx from sig . args)
  (make-tx #:sender from #:to TOKEN #:gas-limit 200000 #:gas-price 0
           #:data (apply encode-call sig args)))

(define-evm-property transfer-conserves-supply
  #:given ([amount (gen-word-in 0 2000)])       ; straddles ALICE's balance of 1000
  #:world MINTED
  #:call  (tx ALICE "transfer(address,uint256)" BOB amount)
  #:post  (lambda (w0 w1 r)
            (and (= (total w1) (total w0))       ; supply never changes
                 (if (eq? (txn-run-outcome r) 'revert)
                     (= (bal w1 ALICE) (bal w0 ALICE))          ; rolled back
                     (and (= (bal w1 ALICE) (- (bal w0 ALICE) amount))
                          (= (bal w1 BOB)   (+ (bal w0 BOB) amount)))))))

(check-evm-property transfer-conserves-supply #:trials 100)

;; --- property: transfer reverts when the amount exceeds the balance ---
(define-evm-property transfer-reverts-when-insufficient
  #:given ([amount (gen-word-in 1001 100000)])  ; always more than ALICE has
  #:world MINTED
  #:call  (tx ALICE "transfer(address,uint256)" BOB amount)
  #:revert-when #t)

(check-evm-property transfer-reverts-when-insufficient #:trials 50)
