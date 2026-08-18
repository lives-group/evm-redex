#lang racket/base

;; Keccak-256 (the original Keccak with 0x01 padding, as used by Ethereum —
;; NOT the FIPS-202 SHA3-256 which uses 0x06 domain separation).
;;
;; Pure Racket implementation of the Keccak-f[1600] sponge.  Mirrors the role of
;; execution-specs `ethereum.crypto.hash.keccak256`.

(require racket/list)

(provide keccak256        ; bytes -> bytes (32)
         keccak256-word)  ; bytes -> natural (the 256-bit digest as an integer)

(define MASK64 #xFFFFFFFFFFFFFFFF)

;; rotation offsets in lane order (lane index = x + 5y)
(define ROT
  #(0  1  62 28 27
    36 44 6  55 20
    3  10 43 25 39
    41 45 15 21 8
    18 2  61 56 14))

(define RC
  (vector
   #x0000000000000001 #x0000000000008082 #x800000000000808A #x8000000080008000
   #x000000000000808B #x0000000080000001 #x8000000080008081 #x8000000000008009
   #x000000000000008A #x0000000000000088 #x0000000080008009 #x000000008000000A
   #x000000008000808B #x800000000000008B #x8000000000008089 #x8000000000008003
   #x8000000000008002 #x8000000000000080 #x000000000000800A #x800000008000000A
   #x8000000080008081 #x8000000000008080 #x0000000080000001 #x8000000080008008))

(define (rot64 x n)
  (if (zero? n)
      x
      (bitwise-and MASK64
                   (bitwise-ior (bitwise-and MASK64 (arithmetic-shift x n))
                                (arithmetic-shift x (- n 64))))))

;; lane index helper
(define (idx x y) (+ x (* 5 y)))

(define (keccak-f! A)
  (for ([round (in-range 24)])
    ;; --- theta ---
    (define C (make-vector 5 0))
    (for ([x (in-range 5)])
      (vector-set! C x
        (bitwise-xor (vector-ref A (idx x 0)) (vector-ref A (idx x 1))
                     (vector-ref A (idx x 2)) (vector-ref A (idx x 3))
                     (vector-ref A (idx x 4)))))
    (define D (make-vector 5 0))
    (for ([x (in-range 5)])
      (vector-set! D x
        (bitwise-xor (vector-ref C (modulo (- x 1) 5))
                     (rot64 (vector-ref C (modulo (+ x 1) 5)) 1))))
    (for* ([x (in-range 5)] [y (in-range 5)])
      (vector-set! A (idx x y) (bitwise-xor (vector-ref A (idx x y)) (vector-ref D x))))
    ;; --- rho + pi ---
    (define B (make-vector 25 0))
    (for* ([x (in-range 5)] [y (in-range 5)])
      (vector-set! B (idx y (modulo (+ (* 2 x) (* 3 y)) 5))
                   (rot64 (vector-ref A (idx x y)) (vector-ref ROT (idx x y)))))
    ;; --- chi ---
    (for* ([x (in-range 5)] [y (in-range 5)])
      (vector-set! A (idx x y)
        (bitwise-xor (vector-ref B (idx x y))
                     (bitwise-and (bitwise-and MASK64
                                               (bitwise-not (vector-ref B (idx (modulo (+ x 1) 5) y))))
                                  (vector-ref B (idx (modulo (+ x 2) 5) y))))))
    ;; --- iota ---
    (vector-set! A 0 (bitwise-xor (vector-ref A 0) (vector-ref RC round))))
  A)

(define (load64-le bs off)
  (for/fold ([acc 0]) ([i (in-range 8)])
    (bitwise-ior acc (arithmetic-shift (bytes-ref bs (+ off i)) (* 8 i)))))

(define (store64-le x)
  (for/list ([i (in-range 8)]) (bitwise-and (arithmetic-shift x (* -8 i)) #xff)))

(define RATE 136)  ; bytes (1088 bits) for 256-bit output

;; pad10*1 with the Keccak (0x01) domain byte
(define (pad msg)
  (let* ([len (bytes-length msg)]
         [padlen (- RATE (modulo len RATE))]   ; 1..RATE
         [pad (make-bytes padlen 0)])
    (bytes-set! pad 0 (bitwise-ior (bytes-ref pad 0) #x01))
    (bytes-set! pad (sub1 padlen) (bitwise-ior (bytes-ref pad (sub1 padlen)) #x80))
    (bytes-append msg pad)))

(define (keccak256-pure msg)
  (define data (pad msg))
  (define A (make-vector 25 0))
  (for ([blk (in-range 0 (bytes-length data) RATE)])
    (for ([i (in-range (quotient RATE 8))])    ; 17 rate lanes
      (vector-set! A i (bitwise-xor (vector-ref A i) (load64-le data (+ blk (* 8 i))))))
    (keccak-f! A))
  ;; squeeze first 32 bytes = lanes 0..3
  (apply bytes-append
         (for/list ([i (in-range 4)]) (apply bytes (store64-le (vector-ref A i))))))

;; Use the native accelerator when it loaded and passed its known-answer
;; self-test (see native.rkt); otherwise the pure sponge above.  Byte-identical
;; either way — the accelerator only activates after matching the oracle.
(require (only-in "native.rkt" native-keccak256))
(define keccak256 (or native-keccak256 keccak256-pure))

(define (keccak256-word msg)
  (for/fold ([acc 0]) ([b (in-bytes (keccak256 msg))])
    (+ (arithmetic-shift acc 8) b)))
