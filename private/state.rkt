#lang racket/base

;; World-state helpers.
;;
;; Mirrors execution-specs `state.py`: account lookup, storage get/set, account
;; existence/emptiness (EIP-161).  Accounts are
;;   (account nonce balance code storage transient)
;;
;; Conceptually the world is ((addr account) ...) and storage/transient are
;; ((key val) ...) holding only non-zero entries.  Operationally, world and
;; storage are immutable Racket hash tables (`hasheqv`, keyed by address / word)
;; for O(1) get/set instead of O(n) assoc/filter.  The accessors below tolerate
;; the legacy list forms (the empty-list literal `()` used when building fresh
;; accounts, and any pre-built assoc list), converging to hashes on the first
;; write.  `world->alist` / `storage->alist` recover the ((k v) ...) shape for
;; the state-root code that must iterate them.

(provide empty-account
         world-ref world-set world-remove world->alist
         acct-nonce acct-balance acct-code acct-storage acct-transient
         acct-with-balance acct-with-nonce acct-with-code
         acct-with-storage acct-with-transient
         storage-ref storage-set storage->alist
         account-exists? account-empty? account-alive?)

(define empty-account '(account 0 0 () () ()))

;; --- world (hasheqv addr -> account) -----------------------------------
(define (world->hash world)
  (if (hash? world)
      world
      (for/fold ([h (hasheqv)]) ([p (in-list world)])
        (hash-set h (car p) (cadr p)))))

(define (world-ref world addr)
  (cond [(hash? world) (hash-ref world addr empty-account)]
        [else (let ([p (assoc addr world)]) (if p (cadr p) empty-account))]))

(define (account-exists? world addr)
  (cond [(hash? world) (hash-has-key? world addr)]
        [else (and (assoc addr world) #t)]))

(define (world-set world addr acct)
  (hash-set (world->hash world) addr acct))

(define (world-remove world addr)
  (if (hash? world)
      (hash-remove world addr)
      (filter (lambda (e) (not (equal? (car e) addr))) world)))

;; recover ((addr account) ...) for iteration (state root)
(define (world->alist world)
  (cond [(hash? world) (for/list ([(a acct) (in-hash world)]) (list a acct))]
        [else world]))

;; --- account fields ----------------------------------------------------
(define (acct-nonce a)     (list-ref a 1))
(define (acct-balance a)   (list-ref a 2))
(define (acct-code a)      (list-ref a 3))
(define (acct-storage a)   (list-ref a 4))
(define (acct-transient a) (list-ref a 5))

(define (acct-set a i v)
  (let ([l (list->vector a)]) (vector-set! l i v) (vector->list l)))

(define (acct-with-nonce a v)     (acct-set a 1 v))
(define (acct-with-balance a v)   (acct-set a 2 v))
(define (acct-with-code a v)      (acct-set a 3 v))
(define (acct-with-storage a v)   (acct-set a 4 v))
(define (acct-with-transient a v) (acct-set a 5 v))

;; EIP-161 emptiness: nonce 0, balance 0, no code
(define (account-empty? a)
  (and (zero? (acct-nonce a)) (zero? (acct-balance a)) (null? (acct-code a))))

;; "alive" = exists and not empty (EIP-161); governs CALL new-account gas
(define (account-alive? world addr)
  (and (account-exists? world addr)
       (not (account-empty? (world-ref world addr)))))

;; --- storage (hasheqv key -> non-zero val) -----------------------------
(define (store->hash store)
  (if (hash? store)
      store
      (for/fold ([h (hasheqv)]) ([kv (in-list store)])
        (hash-set h (car kv) (cadr kv)))))

(define (storage-ref store key)
  (cond [(hash? store) (hash-ref store key 0)]
        [else (let ([p (assoc key store)]) (if p (cadr p) 0))]))

;; set key->val, dropping the entry when val = 0 (preserves the non-zero-only
;; invariant so the state root matches)
(define (storage-set store key val)
  (let ([h (store->hash store)])
    (if (zero? val) (hash-remove h key) (hash-set h key val))))

;; recover ((key val) ...) for iteration (storage root)
(define (storage->alist store)
  (cond [(hash? store) (for/list ([(k v) (in-hash store)]) (list k v))]
        [else store]))
