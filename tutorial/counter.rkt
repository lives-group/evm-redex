#lang racket/base

;; Tutorial example — testing Counter.sol with evm-redex/pbt.
;; This module is the runnable version of what the tutorial documents show; run
;; it with `raco test tutorial/counter.rkt`.

(require rackunit racket/path
         evm-redex/pbt)

;; --- load the compiled contract and deploy it -------------------------
(define HERE (path-only (path->complete-path (syntax-source #'here))))
(define ART  (read-artifact (build-path HERE "Counter.json") #:contract "Counter"))

(define DEPLOYER #x00000000000000000000000000000000A11CE001)
(define DEP  (deploy (artifact-creation ART) #:from DEPLOYER))
(check-true (deploy-result-ok? DEP) "Counter deploys")
(define COUNTER (deploy-result-address DEP))
(define BASE    (deploy-result-world DEP))   ; the world just after deployment

;; --- read state: a getter call, decoded from ABI ----------------------
;; `count` is a public variable, so Solidity gives us a `count()` getter.
(define (count-of world)
  (car (abi-decode (list "uint256")
                   (call-result-return
                    (call world COUNTER #:data (encode-call "count()"))))))

(check-equal? (count-of BASE) 0 "a fresh counter starts at zero")

;; --- send transactions and observe the new state ----------------------
(define CALLER #x00000000000000000000000000000000B0B00001)
(define (send world sig . args)
  (call-result-world (call world COUNTER #:from CALLER #:data (apply encode-call sig args))))

(let* ([w (send BASE "increment()")]
       [w (send w "add(uint256)" 5)])
  (check-equal? (count-of w) 6 "increment then add(5) leaves 6"))

;; --- a property: add(n) raises the count by exactly n -----------------
;; The `#:call` form runs a full transaction; `#:post` receives the worlds
;; before (w0) and after (w1), so we compare the decoded counts.
(define (tx sig . args)
  (make-tx #:sender CALLER #:to COUNTER #:gas-limit 200000 #:gas-price 0
           #:data (apply encode-call sig args)))

(define-evm-property add-raises-count-by-n
  #:given ([n (gen-word-in 0 (expt 2 200))])
  #:world BASE
  #:call  (tx "add(uint256)" n)
  #:post  (lambda (w0 w1 r) (= (count-of w1) (+ (count-of w0) n))))

(check-evm-property add-raises-count-by-n #:trials 50)

;; --- a property about reverting: decrement() fails at zero ------------
(define-evm-property decrement-reverts-at-zero
  #:given ()                         ; no generated inputs — a plain assertion
  #:world BASE                       ; count is 0 here
  #:call  (tx "decrement()")
  #:revert-when #t)

(check-evm-property decrement-reverts-at-zero #:trials 1)
