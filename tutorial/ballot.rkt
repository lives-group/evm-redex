#lang racket/base

;; Tutorial example — the "Voting" (Ballot) contract from Solidity by Example.
;;   https://docs.soliditylang.org/en/v0.8.36/solidity-by-example.html#voting
;; Runnable version of the tutorial's Ballot walkthrough:
;;   raco test tutorial/ballot.rkt

(require rackunit racket/path racket/list
         evm-redex/pbt
         (only-in evm-redex world-ref acct-nonce))

;; --- deploy with three named proposals --------------------------------
(define HERE (path-only (path->complete-path (syntax-source #'here))))
(define ART  (read-artifact (build-path HERE "Ballot.json") #:contract "Ballot"))

;; the deployer becomes the chairperson (chairperson = msg.sender)
(define CHAIR #x00000000000000000000000000000000C4A19001)
(define VOTER #x000000000000000000000000000000000E700001)

;; proposal names are bytes32 — pad the ASCII to 32 bytes
(define (name->b32 s)
  (define b (string->bytes/utf-8 s))
  (append (bytes->list b) (make-list (- 32 (bytes-length b)) 0)))
(define NAMES (list (name->b32 "alpha") (name->b32 "beta") (name->b32 "gamma")))

;; constructor args are ABI-encoded and appended to the creation bytecode
(define DEP  (deploy (append (artifact-creation ART)
                             (abi-encode (list "bytes32[]") (list NAMES)))
                     #:from CHAIR))
(check-true (deploy-result-ok? DEP) "Ballot deploys")
(define BALLOT (deploy-result-address DEP))
(define BASE   (deploy-result-world DEP))
(define BLOCK  '(block 0 0 0 0 0 0 30000000 1 Prague))

;; --- helpers: send a tx from any sender; read some getters ------------
(define (nonce-of w a) (let ([acc (world-ref w a)]) (if acc (acct-nonce acc) 0)))
(define (send w from sig . args)
  (txn-run-world1
   (run-txn (make-tx #:sender from #:nonce (nonce-of w from) #:to BALLOT
                     #:gas-limit 300000 #:gas-price 0 #:data (apply encode-call sig args))
            w BLOCK)))

(define (vote-count w i)                     ; proposals(i) returns (name, voteCount)
  (cadr (abi-decode (list "bytes32" "uint256")
                    (call-result-return (call w BALLOT #:data (encode-call "proposals(uint256)" i))))))
(define (winning w)
  (car (abi-decode (list "uint256")
                   (call-result-return (call w BALLOT #:data (encode-call "winningProposal()"))))))

;; --- a concrete run: grant a vote, cast it, read the winner -----------
(let* ([w (send BASE CHAIR "giveRightToVote(address)" VOTER)]  ; chairperson grants
       [w (send w VOTER "vote(uint256)" 1)])                   ; VOTER backs proposal 1
  (check-equal? (vote-count w 1) 1 "the vote is counted for proposal 1")
  (check-equal? (winning w) 1     "proposal 1 is winning"))

;; --- a property: only the chairperson may grant voting rights ---------
(define OUTSIDER #x00000000000000000000000000000000BADA55001)
(define-evm-property only-chair-grants-rights
  #:given ([who gen-address])
  #:world BASE
  #:call  (make-tx #:sender OUTSIDER #:nonce 0 #:to BALLOT #:gas-limit 300000 #:gas-price 0
                   #:data (encode-call "giveRightToVote(address)" who))
  #:revert-when #t)                          ; a non-chairperson caller always reverts

(check-evm-property only-chair-grants-rights #:trials 30)
