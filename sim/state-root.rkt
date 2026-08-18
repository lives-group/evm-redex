#lang racket/base

;; ======================================================================
;; State root, storage root, and logs hash of a world / log set.
;;
;; These are the standard Ethereum commitments — a Merkle-Patricia trie over the
;; accounts (each RLP-encoded as [nonce, balance, storageRoot, keccak(code)]) and
;; a keccak of the RLP-encoded logs.  They were written in the conformance
;; harness (evm-redex-tests) to check fixtures; lifted here they double as the
;; simulator's "did the state change / here is its fingerprint" primitive, and
;; the harness now requires them from this module.
;; ======================================================================

(require (only-in "../private/mpt.rkt" trie-root)
         (only-in "../private/rlp.rkt" rlp)
         (only-in "../private/keccak.rkt" keccak256)
         (only-in "../private/words.rkt" integer->bytes)
         (only-in "../private/state.rkt"
                  world->alist storage->alist
                  acct-nonce acct-balance acct-code acct-storage))

(provide state-root storage-root logs-hash)

;; storage-root : storage -> 32-byte root
(define (storage-root storage)
  (trie-root (for/list ([kv (in-list (storage->alist storage))])
               (cons (integer->bytes (car kv) 32) (rlp (cadr kv))))))

;; state-root : world -> 32-byte root
(define (state-root world)
  (trie-root
   (for/list ([e (in-list (world->alist world))])
     (define a (cadr e))
     (cons (integer->bytes (car e) 20)
           (rlp (list (acct-nonce a) (acct-balance a)
                      (storage-root (acct-storage a))
                      (keccak256 (list->bytes (acct-code a)))))))))

;; logs-hash : (listof (log addr (topic ...) data)) -> 32-byte keccak
(define (logs-hash logs)
  (keccak256
   (rlp (for/list ([lg (in-list logs)])
          (list (integer->bytes (cadr lg) 20)
                (for/list ([t (in-list (caddr lg))]) (integer->bytes t 32))
                (list->bytes (cadddr lg)))))))
