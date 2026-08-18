#lang racket/base

;; The transaction layer: process_transaction.
;;
;; Mirrors execution-specs prague `fork.py`: validation, intrinsic gas (with the
;; EIP-7623 calldata floor), EIP-1559 fees, EIP-2930 access-list warming,
;; EIP-3651 warm coinbase, EIP-7702 authorizations, top-level message execution,
;; EIP-3529 refund cap, coinbase priority-fee payment, and EIP-161/EIP-6780
;; account deletion.

(require racket/list
         "words.rkt"
         "gas.rkt"
         "forks.rkt"
         "state.rkt"
         "interpreter.rkt"
         "precompiles.rkt")

(provide (struct-out txn) make-tx process-transaction process-transaction*)

;; A transaction.  For legacy txs pass max-fee = max-priority = gas-price.
;; `to` is #f for contract creation.  `data` is a list of bytes.
;; `access-list` is ((addr (key ...)) ...).
;; `auth-list` is ((authority code-addr auth-nonce) ...)  (EIP-7702, simplified:
;; signature recovery is assumed already done, `authority` is the signer).
(struct txn (sender nonce gas-limit to value data max-fee max-priority access-list auth-list
             blob-hashes max-blob-fee)
  #:transparent)

(define (make-tx #:sender sender #:nonce [nonce 0] #:gas-limit gas-limit #:to [to #f]
                 #:value [value 0] #:data [data '()]
                 #:max-fee [max-fee 0] #:max-priority [max-priority 0]
                 #:gas-price [gas-price #f]
                 #:access-list [access-list '()] #:auth-list [auth-list '()]
                 #:blob-hashes [blob-hashes '()] #:max-blob-fee [max-blob-fee 0])
  (txn sender nonce gas-limit to value data
       (or gas-price max-fee) (or gas-price max-priority) access-list auth-list
       blob-hashes max-blob-fee))

;; EIP-4844 blob gas
(define GAS-PER-BLOB 131072)

;; --- tx-level gas constants --------------------------------------------
(define TX-BASE 21000)
(define TX-DATA-ZERO 4)
(define TX-DATA-NONZERO 16)
(define TX-CREATE 32000)
(define AL-ADDR-COST 2400)
(define AL-KEY-COST 1900)
(define FLOOR-PER-TOKEN 10)
(define AUTH-COST 25000)             ; PER_EMPTY_ACCOUNT_COST (EIP-7702)
(define MAX-REFUND-QUOTIENT 5)       ; EIP-3529

;; --- intrinsic gas (returns (values intrinsic floor)) ------------------
(define (count-bytes data)
  (for/fold ([zero 0] [nonzero 0]) ([b (in-list data)])
    (if (zero? b) (values (add1 zero) nonzero) (values zero (add1 nonzero)))))

(define (intrinsic-gas tx)
  (define-values (zero nonzero) (count-bytes (txn-data tx)))
  (define tokens (+ zero (* 4 nonzero)))
  (define data-cost (* TX-DATA-ZERO tokens))   ; 4*tokens = 4*zero + 16*nonzero
  ;; EIP-3860 (Shanghai): per-word init-code metering.
  (define create-cost
    (if (txn-to tx)
        0
        (+ TX-CREATE (if (init-code-metering?)
                         (* GAS-INIT-CODE-WORD (ceil32-words (length (txn-data tx))))
                         0))))
  (define al-cost
    (for/sum ([entry (in-list (txn-access-list tx))])
      (+ AL-ADDR-COST (* AL-KEY-COST (length (cadr entry))))))
  (define auth-cost (* AUTH-COST (length (txn-auth-list tx))))
  (values (+ TX-BASE data-cost create-cost al-cost auth-cost)
          ;; EIP-7623 (Prague): calldata floor.  Pre-Prague there is no floor.
          (if (calldata-floor?) (+ TX-BASE (* FLOOR-PER-TOKEN tokens)) 0)))

;; --- access-list / coinbase / precompile pre-warming -------------------
(define (initial-accessed tx coinbase)
  (define addrs
    (append (list (txn-sender tx))
            (if (warm-coinbase?) (list coinbase) '())    ; EIP-3651 (Shanghai)
            (if (txn-to tx) (list (txn-to tx)) '())
            (range 1 19)                                 ; precompiles 0x01..0x12
            (map car (txn-access-list tx))))
  (define keys
    (append-map (lambda (e) (map (lambda (k) (list (car e) k)) (cadr e)))
                (txn-access-list tx)))
  (list (remove-duplicates addrs) (remove-duplicates keys)))

;; --- EIP-7702 authorizations -------------------------------------------
(define (delegation-bytes code-addr)
  (append '(#xef #x01 #x00) (bytes->list (integer->bytes code-addr 20))))

(define (apply-authorizations world auth-list)
  (for/fold ([w world]) ([a (in-list auth-list)])
    (define authority (car a)) (define code-addr (cadr a)) (define auth-nonce (caddr a))
    (define acct (world-ref w authority))
    (if (= (acct-nonce acct) auth-nonce)
        (let* ([code (if (zero? code-addr) '() (delegation-bytes code-addr))]
               [a1 (acct-with-code acct code)]
               [a2 (acct-with-nonce a1 (add1 (acct-nonce a1)))])
          (world-set w authority a2))
        w)))

;; --- result helpers ----------------------------------------------------
(define (halt-of-machine m) (machine-halt m))
(define (succeeded? h) (and (memq (car h) '(stop return)) #t))
(define (reverted? h) (eq? (car h) 'revert))

;; ======================================================================
;; process-transaction  : tx world block -> (values success? gas-used post-world logs)
;; process-transaction* : same, plus the top-level frame's return/revert BYTES as
;;   a 5th value (empty on a plain STOP or on creation).  The simulator's receipt
;;   needs the output; the existing 4-value contract is preserved for run-txn and
;;   the conformance harness.
;; ======================================================================
(define (process-transaction tx world block)
  (define-values (ok? gas-used post logs _out) (process-transaction* tx world block))
  (values ok? gas-used post logs))

(define (process-transaction* tx world block)
  (define base-fee (list-ref block 4))
  (define coinbase (list-ref block 3))
  (define sender (txn-sender tx))
  (define-values (intrinsic floor) (intrinsic-gas tx))
  (define priority (min (txn-max-priority tx) (- (txn-max-fee tx) base-fee)))
  (define effective (+ base-fee priority))
  (define blob-base-fee (list-ref block 5))
  (define blob-fee (* (length (txn-blob-hashes tx)) GAS-PER-BLOB blob-base-fee))   ; burned
  (define sender-acct (world-ref world sender))

  ;; --- validation ------------------------------------------------------
  (when (< (txn-gas-limit tx) (max intrinsic floor))
    (error 'process-transaction "gas limit below intrinsic/floor"))
  (when (> (txn-gas-limit tx) (list-ref block 7))
    (error 'process-transaction "gas allowance exceeded (> block gas limit)"))
  ;; EIP-1559 (London): max fee must cover base fee.  Pre-London there is no
  ;; base fee (it is 0 in the block env), so this is a no-op there anyway.
  (when (and (base-fee?) (< (txn-max-fee tx) base-fee))
    (error 'process-transaction "max fee below base fee"))
  (when (> (txn-max-priority tx) (txn-max-fee tx))
    (error 'process-transaction "priority fee greater than max fee"))
  (unless (= (acct-nonce sender-acct) (txn-nonce tx))
    (error 'process-transaction "nonce mismatch"))
  (when (>= (txn-nonce tx) (sub1 (expt 2 64)))
    (error 'process-transaction "nonce at maximum"))   ; cannot be incremented
  (when (and (reject-sender-code?) (not (null? (acct-code sender-acct))))
    ;; EIP-3607 (London): sender must not have code (unless EIP-7702 delegation)
    (let ([c (acct-code sender-acct)])
      (unless (and (>= (length c) 3) (= (car c) #xef) (= (cadr c) #x01) (= (caddr c) #x00))
        (error 'process-transaction "sender has code (EIP-3607)"))))
  (when (and (pair? (txn-blob-hashes tx)) (< (txn-max-blob-fee tx) blob-base-fee))
    (error 'process-transaction "max fee per blob gas below block blob base fee"))
  (when (< (acct-balance sender-acct)
           (+ (* (txn-gas-limit tx) (txn-max-fee tx)) (txn-value tx) blob-fee))
    (error 'process-transaction "insufficient balance for gas+value+blob"))

  (reset-tx-state! world)

  ;; --- charge gas upfront, bump nonce, apply 7702 auths ----------------
  (define w0
    (let* ([a (acct-with-balance sender-acct (- (acct-balance sender-acct)
                                                 (* (txn-gas-limit tx) effective) blob-fee))]
           [a (acct-with-nonce a (add1 (acct-nonce a)))])
      (world-set world sender a)))
  (define w1 (apply-authorizations w0 (txn-auth-list tx)))
  (define accessed (initial-accessed tx coinbase))
  (define avail (- (txn-gas-limit tx) intrinsic))
  (define snapshot w1)          ; execution rolls back to here on failure
  (define txenv (list 'tx sender effective '()))

  ;; --- execute the message --------------------------------------------
  (define-values (halt frame-gas exec-world exec-logs exec-refund)
    (if (txn-to tx)
        (run-call tx w1 block txenv accessed avail effective)
        (run-create tx w1 block txenv accessed avail)))

  (define ok? (succeeded? halt))
  (define rev? (reverted? halt))
  (define eff-gas-left (if (or ok? rev?) frame-gas 0))
  (define refund-counter (if ok? exec-refund 0))
  (define exec-world* (if ok? exec-world snapshot))
  (define logs (if ok? exec-logs '()))

  ;; --- gas accounting (refund cap + EIP-7623 floor) -------------------
  (define gas-used-pre (- (txn-gas-limit tx) eff-gas-left))
  (define refund (min refund-counter (quotient gas-used-pre (refund-quotient))))
  (define gas-used (max (- gas-used-pre refund) floor))

  ;; --- refund unused gas to sender, pay coinbase priority fee ----------
  (define w-refunded
    (let* ([a (world-ref exec-world* sender)]
           [back (* (- (txn-gas-limit tx) gas-used) effective)])
      (world-set exec-world* sender (acct-with-balance a (+ (acct-balance a) back)))))
  (define w-coinbase
    (let* ([a (world-ref w-refunded coinbase)]
           [fee (* gas-used priority)])
      (world-set w-refunded coinbase (acct-with-balance a (+ (acct-balance a) fee)))))

  ;; --- delete self-destructed (this-tx) and empty touched accounts -----
  (define w-deleted
    (if ok?
        (for/fold ([w w-coinbase]) ([addr (in-list (tx-deleted))])
          (world-remove w addr))
        w-coinbase))
  (define w-final
    (for/fold ([w w-deleted]) ([addr (in-list (cons coinbase (if ok? (tx-touched) '())))])
      (if (and (account-exists? w addr) (account-empty? (world-ref w addr)))
          (world-remove w addr)
          w)))

  ;; the top-level frame's return/revert bytes (for the simulator receipt)
  (define output (if (memq (car halt) '(return revert)) (cadr halt) '()))
  (values ok? gas-used w-final logs output))

;; --- top-level CALL ----------------------------------------------------
(define (run-call tx world block txenv accessed avail effective)
  (define to (txn-to tx)) (define value (txn-value tx)) (define sender (txn-sender tx))
  (mark-touched! sender) (mark-touched! to)
  (define w1 (if (> value 0) (transfer world sender to value) world))
  (cond
    [(precompile-addr? to)
     (define-values (ok? gl out) (run-precompile to (txn-data tx) avail))
     (values (if ok? (list 'return out) '(out-of-gas)) gl w1 '() 0)]
    [else
     (define done (run-top-frame #:caller sender #:target to #:value value #:data (txn-data tx)
                                 #:code (resolve-code w1 to) #:code-addr to #:gas avail
                                 #:world w1 #:accessed accessed #:block block #:tx txenv
                                 #:orig (orig-storage-for to w1)))
     (values (machine-halt done) (car (machine-field done 'gas))
             (car (machine-field done 'world)) (car (machine-field done 'logs))
             (car (machine-field done 'refund)))]))

;; --- top-level CREATE --------------------------------------------------
(define (run-create tx world block txenv accessed avail)
  (define sender (txn-sender tx)) (define value (txn-value tx))
  (define nonce (sub1 (acct-nonce (world-ref world sender))))   ; pre-increment nonce
  (define addr (compute-create-address sender nonce))
  (define existing (world-ref world addr))
  (cond
    ;; address collision: creation aborts, consuming all gas (AddressCollision)
    [(or (> (acct-nonce existing) 0) (not (null? (acct-code existing))))
     (values '(out-of-gas) 0 world '() 0)]
    [else (run-create/init tx world block txenv accessed avail sender value addr existing)]))

(define (run-create/init tx world block txenv accessed avail sender value addr existing)
  (mark-touched! addr)
  (mark-created! addr)         ; register so a SELFDESTRUCT during init can delete it
  ;; new account: nonce 1, KEEP any pre-existing balance, empty code/storage
  (define w1 (world-set world addr (list 'account 1 (acct-balance existing) '() '() '())))
  (define w2 (if (> value 0) (transfer w1 sender addr value) w1))
  (define done (run-top-frame #:caller sender #:target addr #:value value #:data '()
                              #:code (txn-data tx) #:code-addr addr #:gas avail
                              #:world w2 #:accessed accessed #:block block #:tx txenv
                              #:orig '()))
  (define halt (machine-halt done))
  (cond
    [(and (memq (car halt) '(stop return)))
     (define output (if (eq? (car halt) 'return) (cadr halt) '()))
     (define gas-left (car (machine-field done 'gas)))
     (define deposit (* GAS-CODE-DEPOSIT-PER-BYTE (length output)))
     (cond
       [(or (> (length output) MAX-CODE-SIZE)
            (and (reject-ef-code?) (pair? output) (= (car output) #xef))  ; EIP-3541
            (< gas-left deposit))
        ;; deploy failed
        (values '(out-of-gas) 0 world '() 0)]
       [else
        (define w (car (machine-field done 'world)))
        (define deployed (world-set w addr (acct-with-code (world-ref w addr) output)))
        (values '(stop) (- gas-left deposit) deployed
                (car (machine-field done 'logs)) (car (machine-field done 'refund)))])]
    [else
     (values halt (car (machine-field done 'gas)) (car (machine-field done 'world))
             (car (machine-field done 'logs)) (car (machine-field done 'refund)))]))

;; helper: set nonce on a bare account list
(define (acct-set-nonce a v) (acct-with-nonce a v))
