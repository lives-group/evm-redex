#lang racket/base

;; ======================================================================
;; Observation vocabulary for property predicates (pre / post / invariant).
;;
;; These are pure readers over a `machine` term, a `World`, or an execution
;; result, layered on the existing accessors in private/interpreter.rkt,
;; private/state.rkt, private/mem.rkt and private/words.rkt.  Property bodies
;; written with the pbt DSL use exactly these names to talk about state.
;; ======================================================================

(require racket/list
         (only-in "../private/interpreter.rkt" machine-field machine-halt)
         (only-in "../private/state.rkt"
                  world-ref account-exists? acct-nonce acct-balance acct-code
                  acct-storage acct-transient storage-ref account-empty?)
         (only-in "../private/mem.rkt" mem-read mem-size)
         (only-in "../private/words.rkt" bytes->word MAX-U256 2^256))

(provide
 ;; machine fields
 pc gas the-stack mem-of code-of world-of return-data logs-of refund-of halt-of
 ;; stack
 stack stack-top stack-depth stack-empty?
 ;; outcome predicates over a machine
 running? halted? stopped? returned? reverted? out-of-gas? exception?
 halt-tag EXN-TAGS
 ;; return data
 return-bytes return-word return-size
 ;; memory
 mem-word mem-byte mem-bytes memory-size
 ;; world / accounts / storage
 account-of exists? balance-of nonce-of code-at storage-at empty-account? sload
 ;; machine-relative world convenience
 m-balance m-nonce m-sload m-storage-at
 ;; constants
 UINT256-MAX WORD-MOD)

;; --- machine field readers --------------------------------------------
(define (mfield m tag) (car (machine-field m tag)))
(define (pc m)          (mfield m 'pc))
(define (gas m)         (mfield m 'gas))
(define (the-stack m)   (mfield m 'stack))
(define (mem-of m)      (mfield m 'memory))
(define (code-of m)     (mfield m 'code))
(define (world-of m)    (mfield m 'world))
(define (return-data m) (mfield m 'return-data))
(define (logs-of m)     (mfield m 'logs))
(define (refund-of m)   (mfield m 'refund))
(define (halt-of m)     (machine-halt m))

;; --- stack (index 0 = top) --------------------------------------------
(define (stack m i)     (list-ref (the-stack m) i))
(define (stack-top m)   (stack m 0))
(define (stack-depth m) (length (the-stack m)))
(define (stack-empty? m) (null? (the-stack m)))

;; --- halt / outcome ---------------------------------------------------
;; Halt ::= running | (stop) | (return Bytes) | (revert Bytes) | Exn
(define EXN-TAGS
  '(out-of-gas stack-underflow stack-overflow invalid-opcode
    invalid-jump out-of-bounds stack-depth-limit write-in-static))

(define (halt-tag m) (let ([h (halt-of m)]) (if (pair? h) (car h) h)))
(define (running? m)   (eq? (halt-of m) 'running))
(define (halted? m)    (not (running? m)))
(define (stopped? m)   (eq? (halt-tag m) 'stop))
(define (returned? m)  (eq? (halt-tag m) 'return))
(define (reverted? m)  (eq? (halt-tag m) 'revert))
(define (out-of-gas? m) (eq? (halt-tag m) 'out-of-gas))
(define (exception? m) (and (memq (halt-tag m) EXN-TAGS) #t))

;; --- return data ------------------------------------------------------
(define (return-bytes m) (return-data m))                    ; list of bytes
(define (return-size m)  (length (return-data m)))
;; interpret the (right-most 32 bytes of the) return data as a 256-bit word
(define (return-word m)
  (define bs (return-data m))
  (bytes->word (list->bytes (if (>= (length bs) 32) (take bs 32) bs))))

;; --- memory -----------------------------------------------------------
(define (memory-size m) (mem-size (mem-of m)))
(define (mem-bytes m off len) (mem-read (mem-of m) off len))
(define (mem-byte m off) (car (mem-read (mem-of m) off 1)))
(define (mem-word m off) (bytes->word (list->bytes (mem-read (mem-of m) off 32))))

;; --- world / accounts / storage (address-keyed) -----------------------
(define (account-of world addr)  (world-ref world addr))
(define (exists? world addr)     (account-exists? world addr))
(define (balance-of world addr)  (acct-balance (world-ref world addr)))
(define (nonce-of world addr)    (acct-nonce   (world-ref world addr)))
(define (code-at world addr)     (acct-code    (world-ref world addr)))
(define (storage-at world addr)  (acct-storage (world-ref world addr)))
(define (empty-account? world addr) (account-empty? (world-ref world addr)))
(define (sload world addr key)   (storage-ref (acct-storage (world-ref world addr)) key))

;; --- machine-relative convenience -------------------------------------
(define (m-balance m addr)     (balance-of (world-of m) addr))
(define (m-nonce m addr)       (nonce-of (world-of m) addr))
(define (m-sload m addr key)   (sload (world-of m) addr key))
(define (m-storage-at m addr)  (storage-at (world-of m) addr))

;; --- constants for property bodies ------------------------------------
(define UINT256-MAX MAX-U256)
(define WORD-MOD 2^256)
