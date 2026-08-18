#lang racket/base

;; Gas constants and memory-expansion cost.
;;
;; Mirrors the constants in execution-specs `vm/gas.py`.  Only the subset needed
;; by the current milestones is filled in; the rest are placeholders to be
;; completed in M2.

(provide (all-defined-out))

(require "forks.rkt")

;; --- base step costs ----------------------------------------------------
(define GAS-ZERO 0)
(define GAS-JUMPDEST 1)
(define GAS-BASE 2)
(define GAS-VERY-LOW 3)
(define GAS-LOW 5)
(define GAS-MID 8)
(define GAS-HIGH 10)
(define GAS-EXPONENTIATION 10)
(define GAS-EXPONENTIATION-PER-BYTE 50)
(define GAS-MEMORY 3)
(define GAS-KECCAK256 30)
(define GAS-KECCAK256-WORD 6)
(define GAS-COPY 3)
(define GAS-BLOCK-HASH 20)

;; --- logs (LOG0..LOG4) --------------------------------------------------
(define GAS-LOG 375)
(define GAS-LOG-TOPIC 375)
(define GAS-LOG-DATA 8)

;; --- EIP-2929 warm/cold access -----------------------------------------
(define GAS-WARM-ACCESS 100)
(define GAS-COLD-SLOAD 2100)
(define GAS-COLD-ACCOUNT-ACCESS 2600)

;; --- calls / creates ---------------------------------------------------
(define GAS-CALL-VALUE 9000)
(define GAS-NEW-ACCOUNT 25000)
(define GAS-CREATE 32000)
(define GAS-CODE-DEPOSIT-PER-BYTE 200)
(define GAS-INIT-CODE-WORD 2)
(define GAS-SELF-DESTRUCT 5000)
(define GAS-SELF-DESTRUCT-NEW-ACCOUNT 25000)
(define STACK-DEPTH-LIMIT 1024)
(define MAX-CODE-SIZE 24576)        ; EIP-170
(define MAX-INIT-CODE-SIZE 49152)   ; EIP-3860 (2 * MAX-CODE-SIZE)

;; all-but-one-64th (EIP-150)
(define (max-message-call-gas g) (- g (quotient g 64)))

;; --- storage (EIP-2200 / EIP-3529) -------------------------------------
(define GAS-STORAGE-SET 20000)
(define GAS-STORAGE-UPDATE 5000)
(define GAS-STORAGE-CLEAR-REFUND 4800)
(define GAS-CALL-STIPEND 2300)

;; SSTORE dynamic gas (EIP-2200 + EIP-2929) for a single store.
;; Returns the gas to charge given the cold-slot flag and the
;; original/current/new values.  Does NOT include the cold-access surcharge's
;; refund side; use sstore-refund for that.
(define (sstore-gas cold? original current new)
  (+ (if cold? GAS-COLD-SLOAD 0)
     (cond [(and (= original current) (not (= current new)))
            (if (zero? original) GAS-STORAGE-SET (- GAS-STORAGE-UPDATE GAS-COLD-SLOAD))]
           [else GAS-WARM-ACCESS])))

;; SSTORE refund delta (EIP-3529), added to evm.refund_counter.
(define (sstore-refund original current new)
  (let ([r 0] [clear (sstore-clear-refund-amount)])
    (when (not (= current new))
      (when (and (not (zero? original)) (not (zero? current)) (zero? new))
        (set! r (+ r clear)))
      (when (and (not (zero? original)) (zero? current))
        (set! r (- r clear)))
      (when (= original new)
        (if (zero? original)
            (set! r (+ r (- GAS-STORAGE-SET GAS-WARM-ACCESS)))
            (set! r (+ r (- GAS-STORAGE-UPDATE GAS-COLD-SLOAD GAS-WARM-ACCESS))))))
    r))

;; --- helpers ------------------------------------------------------------
;; number of 32-byte words needed to hold `len` bytes
(define (ceil32-words len) (quotient (+ len 31) 32))
(define (ceil32 len) (* 32 (ceil32-words len)))

;; total memory cost for a memory of `size` bytes (gas.py: calculate_memory_gas_cost)
(define (memory-cost size)
  (let ([words (ceil32-words size)])
    (+ (* GAS-MEMORY words)
       (quotient (* words words) 512))))

;; expansion cost when growing from `old-size` bytes to cover `new-size` bytes
(define (memory-expansion-cost old-size new-size)
  (if (<= new-size old-size)
      0
      (- (memory-cost new-size) (memory-cost old-size))))

;; cost of a memory access of `len` bytes at `off` (no expansion for len = 0)
(define (memory-access-cost old-size off len)
  (if (zero? len)
      0
      (memory-expansion-cost old-size (+ off len))))
