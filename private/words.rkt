#lang racket/base

;; 256-bit (and 64-bit) modular word arithmetic for the EVM.
;;
;; The EVM operates on 256-bit unsigned words.  Redex has no native U256, so we
;; represent words as Racket exact non-negative integers and force every result
;; back into range with `wrap`.  Signed opcodes interpret the same bit pattern
;; as two's complement via `unsigned->signed` / `signed->unsigned`.
;;
;; Mirrors `ethereum.utils.numeric` and the arithmetic/comparison/bitwise
;; instruction modules of execution-specs.

(require racket/math)

(provide (all-defined-out))

(define 2^256 (expt 2 256))
(define MAX-U256 (sub1 2^256))
(define 2^255 (expt 2 255))
(define 2^64 (expt 2 64))
(define MAX-U64 (sub1 2^64))

;; wrap : integer -> u256   (reduce mod 2^256 into [0, 2^256))
(define (wrap n) (modulo n 2^256))
(define (wrap64 n) (modulo n 2^64))

(define (u256? n) (and (exact-nonnegative-integer? n) (< n 2^256)))

;; --- signed <-> unsigned (two's complement over 256 bits) ---------------
(define (unsigned->signed n)
  (let ([n (wrap n)])
    (if (>= n 2^255) (- n 2^256) n)))

(define (signed->unsigned n) (wrap n))

;; --- arithmetic ---------------------------------------------------------
(define (u-add a b) (wrap (+ a b)))
(define (u-sub a b) (wrap (- a b)))
(define (u-mul a b) (wrap (* a b)))

;; EVM division/mod return 0 on a zero divisor.
(define (u-div a b) (if (zero? b) 0 (quotient a b)))
(define (u-mod a b) (if (zero? b) 0 (remainder a b)))

(define (u-sdiv a b)
  (if (zero? b)
      0
      (let ([sa (unsigned->signed a)] [sb (unsigned->signed b)])
        ;; truncated (toward zero) division, then re-wrap
        (wrap (quotient sa sb)))))

(define (u-smod a b)
  (if (zero? b)
      0
      (let ([sa (unsigned->signed a)] [sb (unsigned->signed b)])
        ;; result takes the sign of the dividend (C-style remainder)
        (wrap (- sa (* sb (quotient sa sb)))))))

(define (u-addmod a b n) (if (zero? n) 0 (modulo (+ a b) n)))
(define (u-mulmod a b n) (if (zero? n) 0 (modulo (* a b) n)))

;; EXP : modular exponentiation, 0^0 = 1 per EVM.
(define (u-exp a b)
  (cond [(zero? b) 1]
        [else (modular-expt a b 2^256)]))

(define (modular-expt-pure base e m)
  (let loop ([base (modulo base m)] [e e] [acc 1])
    (cond [(zero? e) (modulo acc m)]
          [(odd? e) (loop (modulo (* base base) m) (quotient e 2) (modulo (* acc base) m))]
          [else     (loop (modulo (* base base) m) (quotient e 2) acc)])))

;; Use GMP's mpz_powm (see native.rkt) only for large operands, where it is
;; ~10x faster (e.g. the multi-thousand-bit modexp stress fixtures).  Small
;; moduli (256-bit words, 381-bit BLS field, etc.) stay on the pure loop, which
;; is already fast and avoids per-call FFI marshalling overhead.  When libgmp is
;; absent, native-modular-expt is #f and everything uses the pure path.
(require (only-in "native.rkt" native-modular-expt))
;; Route to GMP only for genuine big exponentiations: the pure loop runs
;; (integer-length e) iterations, so a LARGE EXPONENT is what makes it expensive
;; (RSA-scale modexp).  A small exponent is cheap in pure code no matter how big
;; the modulus is, and marshalling a huge modulus into GMP would only add cost —
;; so gate on the exponent's bit length, not the modulus'.
(define GMP-EXP-BIT-THRESHOLD 512)
(define modular-expt
  (if native-modular-expt
      (lambda (base e m)
        (if (> (integer-length e) GMP-EXP-BIT-THRESHOLD)
            (native-modular-expt base e m)
            (modular-expt-pure base e m)))
      modular-expt-pure))

;; SIGNEXTEND : extend the sign bit of the byte at position `b` (0 = least
;; significant byte) up through the high bytes.
(define (u-signextend b x)
  (if (>= b 31)
      (wrap x)
      (let* ([bit-pos (+ (* 8 b) 7)]
             [mask (sub1 (arithmetic-shift 1 (add1 bit-pos)))]
             [sign-bit (bitwise-and x (arithmetic-shift 1 bit-pos))])
        (if (zero? sign-bit)
            (bitwise-and x mask)
            (wrap (bitwise-ior x (bitwise-not mask)))))))

;; --- comparison (return 1 / 0) ------------------------------------------
(define (bool->word b) (if b 1 0))
(define (u-lt a b) (bool->word (< a b)))
(define (u-gt a b) (bool->word (> a b)))
(define (u-slt a b) (bool->word (< (unsigned->signed a) (unsigned->signed b))))
(define (u-sgt a b) (bool->word (> (unsigned->signed a) (unsigned->signed b))))
(define (u-eq a b) (bool->word (= a b)))
(define (u-iszero a) (bool->word (zero? a)))

;; --- bitwise ------------------------------------------------------------
(define (u-and a b) (bitwise-and a b))
(define (u-or a b) (bitwise-ior a b))
(define (u-xor a b) (bitwise-xor a b))
(define (u-not a) (wrap (bitwise-not a)))

;; BYTE : the i-th byte of x, counting from the most-significant (0..31).
(define (u-byte i x)
  (if (>= i 32)
      0
      (bitwise-and (arithmetic-shift x (- (* 8 (- 31 i)))) #xff)))

(define (u-shl shift value) (if (>= shift 256) 0 (wrap (arithmetic-shift value shift))))
(define (u-shr shift value) (if (>= shift 256) 0 (arithmetic-shift value (- shift))))

;; SAR : arithmetic (sign-propagating) right shift.
(define (u-sar shift value)
  (let ([sv (unsigned->signed value)])
    (cond [(>= shift 256) (if (negative? sv) MAX-U256 0)]
          [else (wrap (arithmetic-shift sv (- shift)))])))

;; --- byte (de)serialization --------------------------------------------
;; word->bytes32 : u256 -> bytes (big-endian, fixed 32 bytes)
(define (word->bytes32 w) (integer->bytes w 32))

(define (integer->bytes n len)
  (let ([bs (make-bytes len 0)])
    (let loop ([i (sub1 len)] [n (wrap n)])
      (if (< i 0)
          bs
          (begin (bytes-set! bs i (bitwise-and n #xff))
                 (loop (sub1 i) (arithmetic-shift n -8)))))))

;; bytes->word : bytes (big-endian, arbitrary length) -> integer
(define (bytes->word bs)
  (for/fold ([acc 0]) ([b (in-bytes bs)])
    (+ (arithmetic-shift acc 8) b)))
