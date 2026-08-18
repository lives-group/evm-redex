#lang racket/base

;; SHA-256 and RIPEMD-160, pure Racket, for the 0x02 and 0x03 precompiles.

(provide sha256 ripemd160)

(define M32 #xFFFFFFFF)
(define (m32 x) (bitwise-and x M32))
(define (rotr32 x n) (m32 (bitwise-ior (arithmetic-shift x (- n)) (arithmetic-shift x (- 32 n)))))
(define (rotl32 x n) (m32 (bitwise-ior (arithmetic-shift x n) (arithmetic-shift x (- n 32)))))
(define (shr x n) (arithmetic-shift x (- n)))

;; ===================== SHA-256 =====================
(define SHA256-K
  (vector
   #x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
   #xd807aa98 #x12835b01 #x243185be #x550c7dc3 #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
   #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
   #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
   #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13 #x650a7354 #x766a0abb #x81c2c92e #x92722c85
   #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
   #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
   #x748f82ee #x78a5636f #x84c87814 #x8cc70208 #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))

(define (pad-msg msg block-bytes len-bytes big-endian-len?)
  ;; append 0x80, zeros, then 64-bit length (in bits)
  (define bitlen (* 8 (bytes-length msg)))
  (define base (+ (bytes-length msg) 1))
  (define padlen (modulo (- block-bytes (modulo (+ base len-bytes) block-bytes)) block-bytes))
  (define total (+ base padlen len-bytes))
  (define out (make-bytes total 0))
  (bytes-copy! out 0 msg)
  (bytes-set! out (bytes-length msg) #x80)
  (for ([i (in-range len-bytes)])
    (define shift (* 8 (if big-endian-len? (- len-bytes 1 i) i)))
    (bytes-set! out (+ base padlen i) (bitwise-and (arithmetic-shift bitlen (- shift)) #xff)))
  out)

(define (sha256 msg)
  (define data (pad-msg msg 64 8 #t))
  (define H (vector #x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))
  (for ([blk (in-range 0 (bytes-length data) 64)])
    (define w (make-vector 64 0))
    (for ([i (in-range 16)])
      (vector-set! w i (+ (arithmetic-shift (bytes-ref data (+ blk (* 4 i))) 24)
                          (arithmetic-shift (bytes-ref data (+ blk (* 4 i) 1)) 16)
                          (arithmetic-shift (bytes-ref data (+ blk (* 4 i) 2)) 8)
                          (bytes-ref data (+ blk (* 4 i) 3)))))
    (for ([i (in-range 16 64)])
      (define s0 (bitwise-xor (rotr32 (vector-ref w (- i 15)) 7) (rotr32 (vector-ref w (- i 15)) 18) (shr (vector-ref w (- i 15)) 3)))
      (define s1 (bitwise-xor (rotr32 (vector-ref w (- i 2)) 17) (rotr32 (vector-ref w (- i 2)) 19) (shr (vector-ref w (- i 2)) 10)))
      (vector-set! w i (m32 (+ (vector-ref w (- i 16)) s0 (vector-ref w (- i 7)) s1))))
    (let loop ([i 0] [a (vector-ref H 0)] [b (vector-ref H 1)] [c (vector-ref H 2)] [d (vector-ref H 3)]
                     [e (vector-ref H 4)] [f (vector-ref H 5)] [g (vector-ref H 6)] [h (vector-ref H 7)])
      (cond
        [(= i 64)
         (vector-set! H 0 (m32 (+ (vector-ref H 0) a))) (vector-set! H 1 (m32 (+ (vector-ref H 1) b)))
         (vector-set! H 2 (m32 (+ (vector-ref H 2) c))) (vector-set! H 3 (m32 (+ (vector-ref H 3) d)))
         (vector-set! H 4 (m32 (+ (vector-ref H 4) e))) (vector-set! H 5 (m32 (+ (vector-ref H 5) f)))
         (vector-set! H 6 (m32 (+ (vector-ref H 6) g))) (vector-set! H 7 (m32 (+ (vector-ref H 7) h)))]
        [else
         (define S1 (bitwise-xor (rotr32 e 6) (rotr32 e 11) (rotr32 e 25)))
         (define ch (bitwise-xor (bitwise-and e f) (bitwise-and (bitwise-not e) g)))
         (define t1 (m32 (+ h S1 ch (vector-ref SHA256-K i) (vector-ref w i))))
         (define S0 (bitwise-xor (rotr32 a 2) (rotr32 a 13) (rotr32 a 22)))
         (define maj (bitwise-xor (bitwise-and a b) (bitwise-and a c) (bitwise-and b c)))
         (define t2 (m32 (+ S0 maj)))
         (loop (add1 i) (m32 (+ t1 t2)) a b c (m32 (+ d t1)) e f g)])))
  (apply bytes (for*/list ([i (in-range 8)] [s (in-list '(24 16 8 0))])
                 (bitwise-and (arithmetic-shift (vector-ref H i) (- s)) #xff))))

;; ===================== RIPEMD-160 =====================
(define RL #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
             7 4 13 1 10 6 15 3 12 0 9 5 2 14 11 8
             3 10 14 4 9 15 8 1 2 7 0 6 13 11 5 12
             1 9 11 10 0 8 12 4 13 3 7 15 14 5 6 2
             4 0 5 9 7 12 2 10 14 1 3 8 11 6 15 13))
(define RR #(5 14 7 0 9 2 11 4 13 6 15 8 1 10 3 12
             6 11 3 7 0 13 5 10 14 15 8 12 4 9 1 2
             15 5 1 3 7 14 6 9 11 8 12 2 10 0 4 13
             8 6 4 1 3 11 15 0 5 12 2 13 9 7 10 14
             12 15 10 4 1 5 8 7 6 2 13 14 0 3 9 11))
(define SL #(11 14 15 12 5 8 7 9 11 13 14 15 6 7 9 8
             7 6 8 13 11 9 7 15 7 12 15 9 11 7 13 12
             11 13 6 7 14 9 13 15 14 8 13 6 5 12 7 5
             11 12 14 15 14 15 9 8 9 14 5 6 8 6 5 12
             9 15 5 11 6 8 13 12 5 12 13 14 11 8 5 6))
(define SR #(8 9 9 11 13 15 15 5 7 7 8 11 14 14 12 6
             9 13 15 7 12 8 9 11 7 7 12 7 6 15 13 11
             9 7 15 11 8 6 6 14 12 13 5 14 13 13 7 5
             15 5 8 11 14 14 6 14 6 9 12 9 12 5 15 8
             8 5 12 9 12 5 14 6 8 13 6 5 15 13 11 11))
(define KL #(#x00000000 #x5a827999 #x6ed9eba1 #x8f1bbcdc #xa953fd4e))
(define KR #(#x50a28be6 #x5c4dd124 #x6d703ef3 #x7a6d76e9 #x00000000))

(define (rmd-f j x y z)
  (cond [(< j 16) (bitwise-xor x y z)]
        [(< j 32) (bitwise-ior (bitwise-and x y) (bitwise-and (bitwise-not x) z))]
        [(< j 48) (bitwise-xor (bitwise-ior x (m32 (bitwise-not y))) z)]
        [(< j 64) (bitwise-ior (bitwise-and x z) (bitwise-and y (bitwise-not z)))]
        [else (bitwise-xor x (bitwise-ior y (m32 (bitwise-not z))))]))

(define (ripemd160 msg)
  (define data (pad-msg msg 64 8 #f))           ; little-endian length
  (define h (vector #x67452301 #xefcdab89 #x98badcfe #x10325476 #xc3d2e1f0))
  (for ([blk (in-range 0 (bytes-length data) 64)])
    (define X (make-vector 16 0))
    (for ([i (in-range 16)])
      (vector-set! X i (+ (bytes-ref data (+ blk (* 4 i)))
                          (arithmetic-shift (bytes-ref data (+ blk (* 4 i) 1)) 8)
                          (arithmetic-shift (bytes-ref data (+ blk (* 4 i) 2)) 16)
                          (arithmetic-shift (bytes-ref data (+ blk (* 4 i) 3)) 24))))
    (define-values (al bl cl dl el) (values (vector-ref h 0) (vector-ref h 1) (vector-ref h 2) (vector-ref h 3) (vector-ref h 4)))
    (define-values (ar br cr dr er) (values (vector-ref h 0) (vector-ref h 1) (vector-ref h 2) (vector-ref h 3) (vector-ref h 4)))
    (for ([j (in-range 80)])
      (define round (quotient j 16))
      ;; left line
      (define tl (m32 (+ (rotl32 (m32 (+ al (rmd-f j bl cl dl) (vector-ref X (vector-ref RL j)) (vector-ref KL round))) (vector-ref SL j)) el)))
      (set! al el) (set! el dl) (set! dl (rotl32 cl 10)) (set! cl bl) (set! bl tl)
      ;; right line
      (define tr (m32 (+ (rotl32 (m32 (+ ar (rmd-f (- 79 j) br cr dr) (vector-ref X (vector-ref RR j)) (vector-ref KR round))) (vector-ref SR j)) er)))
      (set! ar er) (set! er dr) (set! dr (rotl32 cr 10)) (set! cr br) (set! br tr))
    (define t (m32 (+ (vector-ref h 1) cl dr)))
    (vector-set! h 1 (m32 (+ (vector-ref h 2) dl er)))
    (vector-set! h 2 (m32 (+ (vector-ref h 3) el ar)))
    (vector-set! h 3 (m32 (+ (vector-ref h 4) al br)))
    (vector-set! h 4 (m32 (+ (vector-ref h 0) bl cr)))
    (vector-set! h 0 t))
  (apply bytes (for*/list ([i (in-range 5)] [s (in-list '(0 8 16 24))])
                 (bitwise-and (arithmetic-shift (vector-ref h i) (- s)) #xff))))
