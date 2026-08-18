#lang racket/base

;; ======================================================================
;; The transaction simulator: a mutable "chain" that evolves as transactions
;; are submitted.  It threads the world and block through the library's
;; `process-transaction*`, auto-increments nonces, advances blocks, and packages
;; each transaction's outcome as a receipt (with optional call/step trace and
;; ABI-decoded logs and revert reasons).
;;
;; This is the substrate the `#lang evm-redex/sim` scenario language runs on;
;; it is also usable directly via `(require evm-redex/sim)`.
;;
;; Honest limits (inherited from the semantics): the active fork is the
;; `current-fork` parameter, not derived from the block; EIP-7702 authorizations
;; and EIP-4844 blobs are the library's simplified forms (signature recovery
;; assumed done).
;; ======================================================================

(require racket/list
         (only-in evm-redex
                  make-tx process-transaction*
                  world-ref world-set world->alist
                  acct-nonce acct-balance acct-code acct-storage
                  acct-with-balance storage-ref storage->alist
                  compute-create-address
                  current-fork current-exec-backend
                  current-call-observer current-frame-tracer
                  reset-tx-state! keccak256)
         (only-in "../pbt/execute.rkt" call call-result-outcome call-result-return)
         (only-in "abi.rkt" encode-call decode-return decode-revert event-table decode-log)
         (only-in "state-root.rkt" state-root)
         (only-in "block.rkt" system-call apply-withdrawals
                  BEACON-ROOTS-ADDRESS HISTORY-STORAGE-ADDRESS)
         (only-in "trace.rkt" make-call-collector make-step-collector)
         (only-in "receipt.rkt" receipt))

(provide (struct-out simulator)
         make-simulator eth
         sim-account! sim-fund! sim-deploy! sim-send! sim-view sim-mine! sim-withdraw!
         sim-resolve sim-address sim-balance sim-nonce sim-code sim-storage
         sim-state-root sim-diff sim-register-abi! name->address)

;; A 32-byte zero word, for the block-start system calls when we have no real
;; parent beacon root / hash to feed them.
(define ZERO32 (make-list 32 0))
(define (eth n) (* n (expt 10 18)))

(struct simulator (world block fork names abis initial withdrawals next-addr
                   default-gas default-gas-price) #:mutable)

(define (default-block #:number [number 0] #:timestamp [timestamp 0] #:coinbase [coinbase 0]
                       #:basefee [basefee 0] #:gaslimit [gaslimit 30000000] #:chainid [chainid 1]
                       #:fork [fork 'Prague])
  (list 'block number timestamp coinbase basefee 0 0 gaslimit chainid fork))

(define (make-simulator #:fork [fork 'Prague]
                        #:coinbase [coinbase 0] #:number [number 0] #:timestamp [timestamp 0]
                        #:basefee [basefee 0] #:gaslimit [gaslimit 30000000] #:chainid [chainid 1]
                        #:gas [default-gas 10000000] #:gas-price [default-gas-price 0])
  (simulator '() (default-block #:number number #:timestamp timestamp #:coinbase coinbase
                                #:basefee basefee #:gaslimit gaslimit #:chainid chainid #:fork fork)
             fork (make-hasheq) (make-hasheqv) '() '()
             #xACC0000000000000000000000000000000000001
             default-gas default-gas-price))

;; run body with this simulator's fork + the fast executor pinned
(define-syntax-rule (in-sim s body ...)
  (parameterize ([current-fork (simulator-fork s)] [current-exec-backend 'racket]) body ...))

;; --- name / address resolution ----------------------------------------
(define (sim-resolve s who)
  (cond [(exact-nonnegative-integer? who) who]
        [(symbol? who) (or (hash-ref (simulator-names s) who #f)
                           (error 'sim "unknown account name ~a" who))]
        [else (error 'sim "cannot resolve account ~e" who)]))

(define (sim-address s name) (hash-ref (simulator-names s) name #f))

;; The address of an auto-assigned account is derived from its name: the low 20
;; bytes of keccak(name).  This is deterministic — the same name yields the same
;; address across runs, which keeps scenarios reproducible and lets a contract
;; that hashes or hard-codes an address stay stable — and, being a 160-bit hash,
;; never realistically collides with a CREATE address or another name.  Anonymous
;; accounts (name #f) fall back to a per-simulator counter, hashed the same way.
(define (name->address name)
  (as-address (keccak256 (string->bytes/utf-8 (symbol->string name)))))

(define (as-address hash32)
  (for/fold ([acc 0]) ([b (in-bytes (subbytes hash32 12 32))]) (+ (* acc 256) b)))

(define (auto-address! s name)
  (cond
    [name (name->address name)]
    [else (define n (simulator-next-addr s))
          (set-simulator-next-addr! s (add1 n))
          (name->address (string->symbol (format "__anon~a" n)))]))

(define (sim-register-abi! s addr abi) (when abi (hash-set! (simulator-abis s) addr abi)))

;; --- world setup ------------------------------------------------------
;; declare an account, binding NAME.  code = byte list (runtime) or '().
(define (sim-account! s name #:address [addr #f] #:balance [balance 0] #:nonce [nonce 0]
                      #:code [code '()] #:storage [storage '()] #:abi [abi #f])
  (define a (or addr (auto-address! s name)))
  (set-simulator-world! s (world-set (simulator-world s) a
                                     (list 'account nonce balance code storage '())))
  (when name (hash-set! (simulator-names s) name a))
  (sim-register-abi! s a abi)
  ;; keep the initial snapshot in step with setup so the diff starts from here
  (set-simulator-initial! s (simulator-world s))
  a)

(define (sim-fund! s who wei)
  (define addr (sim-resolve s who))
  (define a (world-ref (simulator-world s) addr))
  (set-simulator-world! s (world-set (simulator-world s) addr
                                     (acct-with-balance a (+ (acct-balance a) wei))))
  (set-simulator-initial! s (simulator-world s)))

;; deploy: run a creation tx, bind NAME to the created address, register its ABI.
(define (sim-deploy! s creation #:from from #:value [value 0] #:gas [gas #f] #:name [name #f]
                     #:abi [abi #f])
  (define sender (sim-resolve s from))
  (define nonce (acct-nonce (world-ref (simulator-world s) sender)))
  (define tx (make-tx #:sender sender #:nonce nonce #:to #f #:value value #:data creation
                      #:gas-limit (or gas (simulator-default-gas s)) #:gas-price 0))
  (define addr (compute-create-address sender nonce))
  (in-sim s
    (reset-tx-state! (simulator-world s))
    (define-values (ok? gu post logs out) (process-transaction* tx (simulator-world s) (simulator-block s)))
    (set-simulator-world! s post)
    (unless ok? (error 'sim-deploy! "deployment failed (gas used ~a)" gu))
    (when name (hash-set! (simulator-names s) name addr))
    (sim-register-abi! s addr abi)
    addr))

;; --- transactions -----------------------------------------------------
;; sim-send! : submit a transaction; returns a receipt.  Provide either #:data
;; or #:sig + #:args (encoded for you).  Fee model defaults to gas-price 0
;; (legacy); pass #:max-fee/#:max-priority for EIP-1559.
(define (sim-send! s #:from from #:to to #:value [value 0]
                   #:data [data #f] #:sig [sig #f] #:args [args '()]
                   #:gas [gas #f] #:gas-price [gas-price #f]
                   #:max-fee [max-fee #f] #:max-priority [max-priority #f]
                   #:access-list [access-list '()] #:auth-list [auth-list '()]
                   #:trace [trace #f])
  (define sender (sim-resolve s from))
  (define target (sim-resolve s to))
  (define calldata (if sig (apply encode-call sig (map (lambda (a) (sim-resolve* s a)) args))
                       (or data '())))
  (define nonce (acct-nonce (world-ref (simulator-world s) sender)))
  (define tx (make-tx #:sender sender #:nonce nonce #:to target #:value value #:data calldata
                      #:gas-limit (or gas (simulator-default-gas s))
                      #:gas-price (if (or max-fee max-priority) #f
                                      (or gas-price (simulator-default-gas-price s)))
                      #:max-fee (or max-fee 0) #:max-priority (or max-priority 0)
                      #:access-list access-list #:auth-list auth-list))
  (run-tx s tx trace))

(define (sim-resolve* s a) (if (symbol? a) (sim-resolve s a) a))

;; the shared execution + receipt path
(define (run-tx s tx trace)
  (define-values (call-obs get-calls) (if (eq? trace 'call) (make-call-collector) (values #f #f)))
  (define-values (step-tr get-steps) (if (eq? trace 'step) (make-step-collector) (values #f #f)))
  (in-sim s
    (parameterize ([current-call-observer call-obs] [current-frame-tracer step-tr])
      (with-handlers ([exn:fail? (lambda (e)
                                   (receipt 'error 0 '() #f '() '() #f #f (exn-message e)))])
        (reset-tx-state! (simulator-world s))
        (define-values (ok? gu post logs out)
          (process-transaction* tx (simulator-world s) (simulator-block s)))
        (set-simulator-world! s post)
        (define status (if ok? 'success 'revert))
        (receipt status gu out
                 (and (eq? status 'revert) (decode-revert out))
                 logs (map (lambda (lg) (format-log s lg)) logs)
                 (and get-calls (get-calls)) (and get-steps (get-steps)) #f)))))

;; render one log, decoded against the emitter's ABI when we have it
(define (format-log s lg)
  (define emitter (cadr lg))
  (define abi (hash-ref (simulator-abis s) emitter #f))
  (define decoded (and abi (decode-log lg (event-table abi))))
  (cond
    [decoded (format "~a(~a)" (car decoded)
                     (string-join* (map (lambda (p) (format "~a=~a" (car p) (fmt (cdr p)))) (cadr decoded)) ", "))]
    [else (format "0x~a topics=~a data=0x~a" (number->string emitter 16)
                  (map (lambda (t) (string-append "0x" (number->string t 16))) (caddr lg))
                  (bytes->hex (cadddr lg)))]))

;; sim-view : a read-only frame call (no state change), returning the decoded
;; return value (when #:returns given) or the raw bytes.
(define (sim-view s #:from [from 0] #:to to #:sig [sig #f] #:args [args '()]
                  #:data [data #f] #:returns [returns #f])
  (define target (sim-resolve s to))
  (define calldata (if sig (apply encode-call sig (map (lambda (a) (sim-resolve* s a)) args))
                       (or data '())))
  (in-sim s
    (define r (call (simulator-world s) target #:from (sim-resolve* s from) #:data calldata #:static #t))
    (define ret (call-result-return r))
    (if returns (decode-return returns ret) ret)))

;; --- blocks / withdrawals ---------------------------------------------
(define (sim-withdraw! s who wei)
  (set-simulator-withdrawals! s (cons (cons (sim-resolve s who) wei) (simulator-withdrawals s))))

;; close the current block (block-start system calls + end-of-block withdrawals)
;; and advance number/timestamp.
(define (sim-mine! s #:number [number #f] #:timestamp [timestamp #f] #:coinbase [coinbase #f])
  (define blk (simulator-block s))
  (in-sim s
    ;; EIP-4788 / EIP-2935 system calls (no-ops unless those contracts exist)
    (define w1 (system-call (simulator-world s) blk BEACON-ROOTS-ADDRESS ZERO32))
    (define w2 (system-call w1 blk HISTORY-STORAGE-ADDRESS ZERO32))
    (define w3 (apply-withdrawals w2 (reverse (simulator-withdrawals s))))
    (set-simulator-world! s w3))
  (set-simulator-withdrawals! s '())
  (set-simulator-block! s
    (list 'block (or number (add1 (list-ref blk 1)))
          (or timestamp (+ (list-ref blk 2) 12))
          (or coinbase (list-ref blk 3))
          (list-ref blk 4) (list-ref blk 5) (list-ref blk 6) (list-ref blk 7)
          (list-ref blk 8) (list-ref blk 9))))

;; --- readers ----------------------------------------------------------
(define (sim-balance s who) (acct-balance (world-ref (simulator-world s) (sim-resolve s who))))
(define (sim-nonce s who)   (acct-nonce   (world-ref (simulator-world s) (sim-resolve s who))))
(define (sim-code s who)    (acct-code    (world-ref (simulator-world s) (sim-resolve s who))))
(define (sim-storage s who slot)
  (storage-ref (acct-storage (world-ref (simulator-world s) (sim-resolve s who))) slot))
(define (sim-state-root s) (in-sim s (state-root (simulator-world s))))

;; --- diff -------------------------------------------------------------
;; a structured diff of the current world against the initial snapshot:
;;   (listof (list addr balance-delta nonce-before nonce-after storage-changes))
;; where storage-changes = (listof (list slot before after)).
(define (sim-diff s)
  (define before (simulator-initial s))
  (define after (simulator-world s))
  (define addrs (remove-duplicates (append (map car (world->alist before)) (map car (world->alist after)))))
  (for*/list ([a (in-list (sort addrs <))]
              [ba (in-value (world-ref before a))] [aa (in-value (world-ref after a))]
              #:unless (equal? ba aa))
    (define st-changes
      (let ([sb (make-hash (map (lambda (kv) (cons (car kv) (cadr kv))) (storage->alist (acct-storage ba))))]
            [sa (storage->alist (acct-storage aa))])
        (append
         (for/list ([kv (in-list sa)] #:unless (equal? (hash-ref sb (car kv) 0) (cadr kv)))
           (list (car kv) (hash-ref sb (car kv) 0) (cadr kv)))
         (for/list ([kv (in-list (storage->alist (acct-storage ba)))]
                    #:when (= 0 (storage-ref-alist sa (car kv))) #:unless (= 0 (cadr kv)))
           (list (car kv) (cadr kv) 0)))))
    (list a (- (acct-balance aa) (acct-balance ba)) (acct-nonce ba) (acct-nonce aa) st-changes)))

(define (storage-ref-alist alist k) (cond [(assv k alist) => cadr] [else 0]))

;; --- small helpers ----------------------------------------------------
(define (fmt v) (if (exact-integer? v) (string-append "0x" (number->string v 16)) (format "~a" v)))
(define (string-join* xs sep)
  (cond [(null? xs) ""] [(null? (cdr xs)) (car xs)]
        [else (string-append (car xs) sep (string-join* (cdr xs) sep))]))
(define (bytes->hex bs)
  (apply string-append (map (lambda (b) (define t (number->string b 16))
                              (if (= 1 (string-length t)) (string-append "0" t) t)) bs)))
