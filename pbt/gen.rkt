#lang racket/base

;; ======================================================================
;; rackcheck generators tuned for the EVM, with edge-case bias.
;;
;; Good PBT for the EVM lives or dies on hitting boundary values (0, 1,
;; 2^256-1, 2^255, byte/word boundaries, gas limits), so the default word
;; generator mixes those edges with uniform and small values.  All of these are
;; ordinary rackcheck generators and therefore shrink toward simpler values.
;; ======================================================================

(require racket/list
         rackcheck
         (only-in "../private/words.rkt" MAX-U256 MAX-U64 2^256 2^255))

(provide gen-word gen-word-uniform gen-word-small gen-word-edge gen-word-in
         gen-byte gen-bytes gen-address
         gen-stack gen-stack-of gen-gas
         gen-store gen-account gen-world-with
         gen-selector gen-calldata abi)

;; --- words ------------------------------------------------------------
(define WORD-EDGES
  (list 0 1 2 3
        255 256 65535 65536
        (expt 2 128) (sub1 (expt 2 128))
        2^255 (sub1 (expt 2 255))
        MAX-U64 (add1 MAX-U64)
        MAX-U256 (sub1 MAX-U256)))

(define gen-word-edge    (gen:one-of WORD-EDGES))
(define gen-word-small   (gen:integer-in 0 255))
(define gen-word-uniform (gen:integer-in 0 MAX-U256))

;; balanced default: heavy on edges + small, plus a uniform tail
(define gen-word
  (gen:frequency (list (cons 4 gen-word-edge)
                       (cons 2 gen-word-small)
                       (cons 3 gen-word-uniform))))

(define (gen-word-in lo hi) (gen:integer-in lo hi))

;; --- bytes ------------------------------------------------------------
(define gen-byte (gen:integer-in 0 255))
(define (gen-bytes #:max-length [n 64]) (gen:list gen-byte #:max-length n))

;; --- addresses (160-bit) ----------------------------------------------
(define ADDR-EDGES (list 0 1 2 (sub1 (expt 2 160)) #xdeadbeef #xcafe))
(define gen-address
  (gen:frequency (list (cons 3 (gen:one-of ADDR-EDGES))
                       (cons 2 (gen:integer-in 0 (sub1 (expt 2 160)))))))

;; --- stacks (top-first list of words) ---------------------------------
;; gen-stack: 0..depth words.  gen-stack-of k: exactly k words.
(define (gen-stack #:max-depth [depth 8]) (gen:list gen-word #:max-length depth))
(define (gen-tuple* gens) (if (null? gens) (gen:const '()) (apply gen:tuple gens)))
(define (gen-stack-of k) (gen-tuple* (make-list k gen-word)))

;; --- gas (biased toward small/tight budgets to reach OOG) -------------
(define gen-gas
  (gen:frequency (list (cons 2 (gen:one-of (list 0 1 2 3 100 21000)))
                       (cons 3 (gen:integer-in 0 200000))
                       (cons 2 (gen:integer-in 0 10000000)))))

;; --- storage / accounts / worlds --------------------------------------
;; Store ::= ((key val) ...) with small key space so collisions are likely.
(define gen-storage-key (gen:frequency (list (cons 3 (gen:integer-in 0 8))
                                             (cons 1 gen-word))))
(define (gen-store #:max-entries [n 6])
  (gen:map (gen:list (gen:tuple gen-storage-key gen-word) #:max-length n)
           ;; dedup keys (last wins) so it is a valid assoc Store
           (lambda (kvs)
             (for/fold ([acc '()] #:result (reverse acc))
                       ([kv (in-list kvs)])
               (if (assoc (car kv) acc) acc (cons kv acc))))))

;; account (account nonce balance code storage transient)
(define (gen-account #:code [code '()])
  (gen:map (gen:tuple (gen:integer-in 0 3) gen-word (gen-store))
           (lambda (t) (list 'account (list-ref t 0) (list-ref t 1) code (list-ref t 2) '()))))

;; a World holding the fixed contract at `addr`, plus the account generated for
;; `#:storage`, and a generated caller account.
(define (gen-world-with contract addr
                        #:storage [gstore (gen-store)]
                        #:caller [caller 0]
                        #:caller-balance [gbal gen-word])
  (gen:map (gen:tuple gstore gbal)
           (lambda (t)
             (list (list addr   (list 'account 0 0 contract (list-ref t 0) '()))
                   (list caller (list 'account 0 (list-ref t 1) '() '() '()))))))

;; --- calldata (ABI-ish: 4-byte selector + 32-byte-padded word args) ---
(define gen-selector (gen:list gen-byte #:max-length 4))

(define (word->32bytes w)
  (for/list ([i (in-range 31 -1 -1)]) (bitwise-and (arithmetic-shift w (* -8 i)) #xff)))

;; abi : selector(4 bytes as integer or list) + word args -> byte list
(define (abi selector . args)
  (append (cond [(list? selector) selector]
                [else (for/list ([i (in-range 3 -1 -1)]) (bitwise-and (arithmetic-shift selector (* -8 i)) #xff))])
          (append-map word->32bytes args)))

;; gen-calldata: random selector + n word args (n in 0..max-args)
(define (gen-calldata #:max-args [max-args 3])
  (gen:bind (gen:integer-in 0 max-args)
            (lambda (n)
              (gen:map (gen:tuple gen-selector (gen-tuple* (make-list n gen-word)))
                       (lambda (t) (apply abi (car t) (cadr t)))))))
