#lang racket/base

;; Helpers for EVM memory and code.
;;
;; EVM memory is conceptually an infinite zero-extended byte array, materialized
;; in 32-byte-word-aligned chunks (matching execution-specs, where `evm.memory`
;; is always a multiple of 32 bytes long).
;;
;; Operationally, memory is an immutable Racket `bytes` (O(1) indexed access);
;; the grammar non-terminal `Mem` is `any`.  The accessors below tolerate the
;; legacy empty-list literal `()` used to initialise a fresh frame (and any
;; byte-list value), always producing `bytes`.  `mem-read` returns a byte LIST,
;; since its callers (RETURN/LOG data, Keccak input) expect the grammar's
;; `Bytes` shape.  Code stays a byte list (grammar `Code`).

(provide mem-size
         mem-read
         mem-write
         mem-touch
         code-slice)

(define (ceil32 len) (* 32 (quotient (+ len 31) 32)))

;; coerce a memory value (immutable bytes, the () init literal, or a byte list)
;; to bytes without copying when it is already bytes
(define (as-bytes m)
  (cond [(bytes? m) m]
        [(null? m) #""]
        [else (list->bytes m)]))

(define (mem-size m) (bytes-length (as-bytes m)))

;; read `len` bytes starting at `off`, zero-extending past the end; -> byte list
(define (mem-read m off len)
  (define b (as-bytes m))
  (define n (bytes-length b))
  (for/list ([i (in-range off (+ off len))])
    (if (and (>= i 0) (< i n)) (bytes-ref b i) 0)))

;; write `bs` (a byte list or bytes) at offset `off`, growing the memory
;; (zero-filled, rounded up to a 32-byte boundary) as needed
;;
;; A zero-length write neither writes nor grows.  In execution-specs the two
;; halves are separate: `calculate_gas_extend_memory` skips extensions whose
;; size is 0, so `expand_by` is 0 and `evm.memory` is unchanged, and
;; `memory_write` of an empty value is a no-op.  Here expansion is a side effect
;; of this function, so the size-0 case has to be excluded here or the copy
;; opcodes (CALLDATACOPY, CODECOPY, EXTCODECOPY, RETURNDATACOPY, MCOPY) would
;; round `off + 0` up to a 32-byte boundary and grow memory for free -- charging
;; the correct zero gas, since `memory-access-cost` already special-cases len 0,
;; while leaving MSIZE observably wrong.  MSTORE and MSTORE8 always pass a
;; non-empty payload, so they are unaffected.
(define (mem-write m off bs)
  (define b (as-bytes m))
  (define src (if (bytes? bs) bs (list->bytes bs)))
  (define len (bytes-length src))
  (cond
    [(zero? len) b]
    [else
     (define old (bytes-length b))
     (define size (ceil32 (max old (+ off len))))
     (define out (make-bytes size 0))
     (bytes-copy! out 0 b)
     (bytes-copy! out off src)
     (bytes->immutable-bytes out)]))

;; extend memory (zero-filled, 32-byte aligned) to cover [off, off+len) without
;; disturbing existing bytes; a no-op when len = 0 or already large enough
(define (mem-touch m off len)
  (define b (as-bytes m))
  (cond
    [(zero? len) b]
    [else
     (define need (ceil32 (+ off len)))
     (cond
       [(>= (bytes-length b) need) b]
       [else
        (define out (make-bytes need 0))
        (bytes-copy! out 0 b)
        (bytes->immutable-bytes out)])]))

;; read `len` code bytes starting at `start`, zero-padding past the end
(define (code-slice c start len)
  (let ([vec (list->vector c)]
        [n (length c)])
    (for/list ([i (in-range start (+ start len))])
      (if (and (>= i 0) (< i n)) (vector-ref vec i) 0))))
