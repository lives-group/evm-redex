#lang racket/base

;; Precompiled contracts (Prague).
;;
;; Implemented: 0x01 ecrecover, 0x02 sha256, 0x03 ripemd160, 0x04 identity,
;; 0x05 modexp (EIP-2565 gas), 0x06 ecadd, 0x07 ecmul, 0x08 ecpairing (bn254),
;; 0x09 blake2f.
;; Deferred: 0x0a point-evaluation (KZG), 0x0b..0x12 BLS12-381.
;;
;; `run-precompile addr data gas` -> (values ok? gas-left output-bytes-list).
;; A precompile that runs out of gas or hits invalid input returns ok?=#f with
;; gas-left 0 (the call fails, consuming all forwarded gas).

(require racket/list
         "words.rkt"
         "keccak.rkt"
         "hashes.rkt"
         "ec.rkt"
         "pairing.rkt"
         "bls.rkt"
         (only-in "native.rkt" native-secp256k1-recover))

(provide precompile-addr? run-precompile)

;; 0x0b G1ADD, 0x0c G1MSM, 0x0d G2ADD, 0x0e G2MSM, 0x0f PAIRING implemented;
;; 0x10/0x11 map-to-curve and 0x0a KZG still deferred.
(define IMPLEMENTED '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17))
(define (precompile-addr? addr) (and (member addr IMPLEMENTED) #t))

;; --- input reader (zero-extended) --------------------------------------
(define (rd vec off) (if (< off (vector-length vec)) (vector-ref vec off) 0))
;; big-endian bytes -> integer, O(n log n) via a balanced shift+add tree.
;; (The naive `(+ (* a 256) byte)` fold is O(n^2) because `a` grows to an n-byte
;; bignum every step — pathological for the modexp precompile, whose modulus size
;; is attacker-chosen and reaches ~1MB in the EEST stress fixtures.)
(define (rd-int vec off len)
  (let rec ([lo off] [n len])
    (cond [(= n 0) 0]
          [(= n 1) (rd vec lo)]
          [else (define half (quotient n 2))
                (define hi-len (- n half))
                (+ (arithmetic-shift (rec lo hi-len) (* 8 half))
                   (rec (+ lo hi-len) half))])))
(define (rd-bytes vec off len) (for/list ([i (in-range len)]) (rd vec (+ off i))))
(define (ceil-div a b) (quotient (+ a b -1) b))
(define (words32 len) (ceil-div len 32))

(define (run-precompile addr data gas)
  (define vec (list->vector data))
  (define len (length data))
  (case addr
    [(1) (pc-ecrecover vec gas)]
    [(2) (pc-hash vec len gas (lambda (bs) (sha256 bs)) 60 12)]
    [(3) (pc-hash vec len gas (lambda (bs) (bytes-append (make-bytes 12 0) (ripemd160 bs))) 600 120)]
    [(4) (pc-identity data len gas)]
    [(5) (pc-modexp vec len gas)]
    [(6) (pc-ecadd vec gas)]
    [(7) (pc-ecmul vec gas)]
    [(8) (pc-ecpairing vec len gas)]
    [(9) (pc-blake2f vec len gas)]
    [(10) (pc-point-evaluation vec len gas)]
    [(11) (pc-bls-g1add vec len gas)]
    [(12) (pc-bls-msm vec len gas 160 12000 G1-DISCOUNT decode-g1 g1-in-subgroup? g1-add g1-mul enc-g1)]
    [(13) (pc-bls-g2add vec len gas)]
    [(14) (pc-bls-msm vec len gas 288 22500 G2-DISCOUNT decode-g2 g2-in-subgroup? g2-add g2-mul enc-g2)]
    [(15) (pc-bls-pairing vec len gas)]
    [(16) (pc-bls-map-g1 vec len gas)]
    [(17) (pc-bls-map-g2 vec len gas)]
    [else (values #f 0 '())]))

(define (pc-bls-map-g1 vec len gas)
  (cond
    [(not (= len 64)) (values #f 0 '())]
    [(< gas 5500) (values #f 0 '())]
    [else
     (define u (decode-fp vec 0))
     (if (eq? u 'bad) (values #f 0 '())
         (values #t (- gas 5500) (enc-g1 (map-fp-to-g1 u))))]))

(define (pc-bls-map-g2 vec len gas)
  (cond
    [(not (= len 128)) (values #f 0 '())]
    [(< gas 23800) (values #f 0 '())]
    [else
     (define c0 (decode-fp vec 0)) (define c1 (decode-fp vec 64))
     (if (or (eq? c0 'bad) (eq? c1 'bad)) (values #f 0 '())
         (values #t (- gas 23800) (enc-g2 (map-fp2-to-g2 (list c0 c1)))))]))

;; EIP-2537 MSM discount tables (k = 1..128 -> index k-1; k>128 uses last)
(define G1-DISCOUNT
  #(1000 949 848 797 764 750 738 728 719 712 705 698 692 687 682 677 673 669 665 661 658 654
    651 648 645 642 640 637 635 632 630 627 625 623 621 619 617 615 613 611 609 608 606 604 603
    601 599 598 596 595 593 592 591 589 588 586 585 584 582 581 580 579 577 576 575 574 573 572
    570 569 568 567 566 565 564 563 562 561 560 559 558 557 556 555 554 553 552 551 550 549 548
    547 547 546 545 544 543 542 541 540 540 539 538 537 536 536 535 534 533 532 532 531 530 529
    528 528 527 526 525 525 524 523 522 522 521 520 520 519))
(define G2-DISCOUNT
  #(1000 1000 923 884 855 832 812 796 782 770 759 749 740 732 724 717 711 704 699 693 688 683
    679 674 670 666 663 659 655 652 649 646 643 640 637 634 632 629 627 624 622 620 618 615 613
    611 609 607 606 604 602 600 598 597 595 593 592 590 589 587 586 584 583 582 580 579 578 576
    575 574 573 571 570 569 568 567 566 565 563 562 561 560 559 558 557 556 555 554 553 552 552
    551 550 549 548 547 546 545 545 544 543 542 541 541 540 539 538 537 537 536 535 535 534 533
    532 532 531 530 530 529 528 528 527 526 526 525 524 524))

(define (pc-bls-msm vec len gas item-len base disc decode in-sub? add mul enc)
  (cond
    [(or (zero? len) (not (zero? (modulo len item-len)))) (values #f 0 '())]
    [else
     (define k (quotient len item-len))
     (define req (quotient (* k base (vector-ref disc (min (sub1 k) 127))) 1000))
     (cond
       [(< gas req) (values #f 0 '())]
       [else
        (let/ec return
          (define acc 'inf)
          (for ([i (in-range k)])
            (define o (* i item-len))
            (define pt (decode vec o))
            (when (eq? pt 'bad) (return #f 0 '()))
            (unless (in-sub? pt) (return #f 0 '()))
            (set! acc (add acc (mul (rd-int vec (+ o (- item-len 32)) 32) pt))))
          (values #t (- gas req) (enc acc)))])]))

;; ===================== 0x0a point evaluation (KZG, EIP-4844) ===========
(define FIELD-ELEMENTS-PER-BLOB 4096)
(define BLS-MODULUS-OUT 52435875175126190479447740508185965837690552500527637822603658699938581184513)

(define (pc-point-evaluation vec len gas)
  (cond
    [(not (= len 192)) (values #f 0 '())]
    [(< gas 50000) (values #f 0 '())]
    [else
     (define vh (rd-bytes vec 0 32))
     (define z (rd-int vec 32 32)) (define y (rd-int vec 64 32))
     (define commitment (rd-bytes vec 96 48)) (define proof (rd-bytes vec 144 48))
     (define computed-vh (cons 1 (cdr (bytes->list (sha256 (apply bytes commitment))))))
     (cond
       [(or (>= z BLS-MODULUS-OUT) (>= y BLS-MODULUS-OUT)) (values #f 0 '())]
       [(not (equal? vh computed-vh)) (values #f 0 '())]
       [(not (kzg-verify commitment z y proof)) (values #f 0 '())]
       [else
        (values #t (- gas 50000)
                (append (bytes->list (integer->bytes FIELD-ELEMENTS-PER-BLOB 32))
                        (bytes->list (integer->bytes BLS-MODULUS-OUT 32))))])]))

;; ===================== BLS12-381 (EIP-2537) ============================
;; field element: 64 bytes = 16 zero bytes || 48-byte big-endian value < p
(define (decode-fp vec off)        ; -> int or 'bad
  (if (for/or ([i (in-range 16)]) (not (zero? (rd vec (+ off i)))))
      'bad
      (let ([v (rd-int vec (+ off 16) 48)]) (if (>= v BLS-P) 'bad v))))

(define (decode-g1 vec off)        ; -> point (cons (list x)(list y)) / 'inf / 'bad
  (define x (decode-fp vec off)) (define y (decode-fp vec (+ off 64)))
  (cond [(or (eq? x 'bad) (eq? y 'bad)) 'bad]
        [(and (zero? x) (zero? y)) 'inf]
        [else (let ([pt (cons (list x) (list y))]) (if (g1-on-curve? pt) pt 'bad))]))

(define (decode-g2 vec off)        ; -> point (cons (c0 c1)(c0 c1)) / 'inf / 'bad
  (define x0 (decode-fp vec off))        (define x1 (decode-fp vec (+ off 64)))
  (define y0 (decode-fp vec (+ off 128)))(define y1 (decode-fp vec (+ off 192)))
  (cond [(ormap (lambda (e) (eq? e 'bad)) (list x0 x1 y0 y1)) 'bad]
        [(and (zero? x0) (zero? x1) (zero? y0) (zero? y1)) 'inf]
        [else (let ([pt (cons (list x0 x1) (list y0 y1))]) (if (g2-on-curve? pt) pt 'bad))]))

(define (enc-fp v) (append (make-list 16 0) (bytes->list (integer->bytes v 48))))
(define (enc-g1 pt)
  (if (eq? pt 'inf) (make-list 128 0)
      (append (enc-fp (car (car pt))) (enc-fp (car (cdr pt))))))
(define (enc-g2 pt)
  (if (eq? pt 'inf) (make-list 256 0)
      (append (enc-fp (car (car pt))) (enc-fp (cadr (car pt)))
              (enc-fp (car (cdr pt))) (enc-fp (cadr (cdr pt))))))

(define (pc-bls-g1add vec len gas)
  (cond
    [(not (= len 256)) (values #f 0 '())]
    [(< gas 375) (values #f 0 '())]
    [else
     (define a (decode-g1 vec 0)) (define b (decode-g1 vec 128))
     (if (or (eq? a 'bad) (eq? b 'bad)) (values #f 0 '())
         (values #t (- gas 375) (enc-g1 (g1-add a b))))]))

(define (pc-bls-g2add vec len gas)
  (cond
    [(not (= len 512)) (values #f 0 '())]
    [(< gas 600) (values #f 0 '())]
    [else
     (define a (decode-g2 vec 0)) (define b (decode-g2 vec 256))
     (if (or (eq? a 'bad) (eq? b 'bad)) (values #f 0 '())
         (values #t (- gas 600) (enc-g2 (g2-add a b))))]))

(define (pc-bls-pairing vec len gas)
  (define k (quotient len 384))
  (define req (+ 37700 (* 32600 k)))
  (cond
    [(or (zero? len) (not (zero? (modulo len 384)))) (values #f 0 '())]
    [(< gas req) (values #f 0 '())]
    [else
     (let/ec return
       (define pairs
         (for/list ([i (in-range k)])
           (define o (* i 384))
           (define g1 (decode-g1 vec o)) (define g2 (decode-g2 vec (+ o 128)))
           (when (or (eq? g1 'bad) (eq? g2 'bad)) (return #f 0 '()))
           ;; pairing requires subgroup membership
           (unless (and (g1-in-subgroup? g1) (bn-g2-in-subgroup? g2)) (return #f 0 '()))
           (cons g1 g2)))
       (values #t (- gas req) (append (make-list 31 0) (list (if (bls-pairing-check pairs) 1 0)))))]))

;; --- 0x04 identity -----------------------------------------------------
(define (pc-identity data len gas)
  (define req (+ 15 (* 3 (words32 len))))
  (if (< gas req) (values #f 0 '()) (values #t (- gas req) data)))

;; --- 0x02 sha256 / 0x03 ripemd160 --------------------------------------
(define (pc-hash vec len gas hashfn base per-word)
  (define req (+ base (* per-word (words32 len))))
  (if (< gas req)
      (values #f 0 '())
      (values #t (- gas req) (bytes->list (hashfn (apply bytes (rd-bytes vec 0 len)))))))

;; --- 0x01 ecrecover ----------------------------------------------------
(define (pc-ecrecover vec gas)
  (define req 3000)
  (cond
    [(< gas req) (values #f 0 '())]
    [else
     (define h (rd-int vec 0 32))
     (define v (rd-int vec 32 32))
     (define r (rd-int vec 64 32))
     (define s (rd-int vec 96 32))
     (define out (ecrecover-addr h v r s))
     (values #t (- gas req) (if out (append (make-list 12 0) (bytes->list out)) '()))]))

;; pure secp256k1 recovery: (h v r s) -> recovered point (cons x y) or #f
(define (ec-recover-point-pure h v r s)
  (define recid (- v 27))
  (define x r)
  (define alpha (modulo (+ (* x x x) 7) SECP-P))
  (define beta (mod-sqrt alpha SECP-P))
  (cond
    [(not (= (modulo (* beta beta) SECP-P) alpha)) #f]   ; x not on curve
    [else
     (define y (if (= (modulo beta 2) recid) beta (- SECP-P beta)))
     (define R (cons x y))
     (define e (modulo h SECP-N))
     (define sR (pt-mul s R SECP-P 0))
     (define neg-eG (pt-mul (modulo (- SECP-N e) SECP-N) SECP-G SECP-P 0))
     (pt-mul (modinv r SECP-N) (pt-add sR neg-eG SECP-P 0) SECP-P 0)]))

(define (ecrecover-addr h v r s)
  (cond
    [(or (not (memv v '(27 28))) (< r 1) (>= r SECP-N) (< s 1) (>= s SECP-N)) #f]
    [else
     ;; native libsecp256k1 when present (oracle-gated in native.rkt), else pure
     (define Q (if native-secp256k1-recover
                   (native-secp256k1-recover h v r s)
                   (ec-recover-point-pure h v r s)))
     (cond
       [(not Q) #f]
       [else
        (subbytes (keccak256 (bytes-append (integer->bytes (car Q) 32) (integer->bytes (cdr Q) 32)))
                  12 32)])]))

;; --- 0x05 modexp (EIP-2565) --------------------------------------------
(define (pc-modexp vec len gas)
  (define bsize (rd-int vec 0 32))
  (define esize (rd-int vec 32 32))
  (define msize (rd-int vec 64 32))
  (define b (rd-int vec 96 bsize))
  (define e (rd-int vec (+ 96 bsize) esize))
  (define m (rd-int vec (+ 96 bsize esize) msize))
  ;; adjusted exponent length
  (define exp-head (rd-int vec (+ 96 bsize) (min 32 esize)))
  (define adj
    (cond [(<= esize 32) (if (zero? e) 0 (max 0 (- (integer-length e) 1)))]
          [else (+ (* 8 (- esize 32)) (if (zero? exp-head) 0 (max 0 (- (integer-length exp-head) 1))))]))
  (define mc (let ([w (ceil-div (max bsize msize) 8)]) (* w w)))
  (define req (max 200 (quotient (* mc (max adj 1)) 3)))
  (cond
    [(< gas req) (values #f 0 '())]
    [else
     (define result (cond [(zero? msize) 0]
                          [(= m 0) 0]
                          [else (modular-expt b e m)]))
     (values #t (- gas req) (rd-bytes (list->vector (bytes->list (integer->bytes result msize))) 0 msize))]))

;; --- 0x06 ecadd / 0x07 ecmul (bn254) -----------------------------------
;; (0,0) encodes the point at infinity
(define (parse-bn-point vec off)
  (define x (rd-int vec off 32)) (define y (rd-int vec (+ off 32) 32))
  (cond [(and (zero? x) (zero? y)) (cons 'inf #f)]
        [(or (>= x BN-P) (>= y BN-P)) (cons 'bad #f)]
        [(on-curve? (cons x y) BN-P 0 BN-B) (cons 'ok (cons x y))]
        [else (cons 'bad #f)]))

(define (bn-out P)
  (if (not P)
      (append (make-list 32 0) (make-list 32 0))
      (append (bytes->list (integer->bytes (car P) 32)) (bytes->list (integer->bytes (cdr P) 32)))))

(define (pc-ecadd vec gas)
  (define req 150)
  (cond
    [(< gas req) (values #f 0 '())]
    [else
     (define p1 (parse-bn-point vec 0))
     (define p2 (parse-bn-point vec 64))
     (if (or (eq? (car p1) 'bad) (eq? (car p2) 'bad))
         (values #f 0 '())
         (values #t (- gas req)
                 (bn-out (bn-g1-add (if (eq? (car p1) 'inf) #f (cdr p1))
                                    (if (eq? (car p2) 'inf) #f (cdr p2))))))]))

(define (pc-ecmul vec gas)
  (define req 6000)
  (cond
    [(< gas req) (values #f 0 '())]
    [else
     (define p (parse-bn-point vec 0))
     (define k (rd-int vec 64 32))
     (if (eq? (car p) 'bad)
         (values #f 0 '())
         (values #t (- gas req)
                 (bn-out (bn-g1-mul k (if (eq? (car p) 'inf) #f (cdr p))))))]))

;; --- 0x08 ecpairing (bn254, EIP-197/1108) ------------------------------
(define (pc-ecpairing vec len gas)
  (define k (quotient len 192))
  (define req (+ 45000 (* 34000 k)))
  (cond
    [(not (zero? (modulo len 192))) (values #f 0 '())]   ; malformed input
    [(< gas req) (values #f 0 '())]
    [else
     ;; parse + validate all pairs; any invalid point -> failure
     (let/ec return
       (define pairs
         (for/list ([i (in-range k)])
           (define o (* i 192))
           (define g1x (rd-int vec o 32))       (define g1y (rd-int vec (+ o 32) 32))
           (define x-im (rd-int vec (+ o 64) 32))  (define x-re (rd-int vec (+ o 96) 32))
           (define y-im (rd-int vec (+ o 128) 32)) (define y-re (rd-int vec (+ o 160) 32))
           (when (or (>= g1x P) (>= g1y P) (>= x-im P) (>= x-re P) (>= y-im P) (>= y-re P))
             (return #f 0 '()))
           (define g1 (if (and (zero? g1x) (zero? g1y)) 'inf (cons g1x g1y)))
           (define g2 (if (and (zero? x-im) (zero? x-re) (zero? y-im) (zero? y-re))
                          'inf (cons (list x-re x-im) (list y-re y-im))))
           (unless (and (on-g1? g1) (on-g2? g2) (bn-g2-in-subgroup? g2))
             (return #f 0 '()))
           (cons g1 g2)))
       (values #t (- gas req) (append (make-list 31 0) (list (if (bn-pairing-check pairs) 1 0)))))]))

;; --- 0x09 blake2f ------------------------------------------------------
(define M64 #xFFFFFFFFFFFFFFFF)
(define BLAKE-IV
  (vector #x6a09e667f3bcc908 #xbb67ae8584caa73b #x3c6ef372fe94f82b #xa54ff53a5f1d36f1
          #x510e527fade682d1 #x9b05688c2b3e6c1f #x1f83d9abfb41bd6b #x5be0cd19137e2179))
(define BLAKE-SIGMA
  #(#(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
    #(14 10 4 8 9 15 13 6 1 12 0 2 11 7 5 3)
    #(11 8 12 0 5 2 15 13 10 14 3 6 7 1 9 4)
    #(7 9 3 1 13 12 11 14 2 6 5 10 4 0 15 8)
    #(9 0 5 7 2 4 10 15 14 1 11 12 6 8 3 13)
    #(2 12 6 10 0 11 8 3 4 13 7 5 15 14 1 9)
    #(12 5 1 15 14 13 4 10 0 7 6 3 9 2 8 11)
    #(13 11 7 14 12 1 3 9 5 0 15 4 8 6 2 10)
    #(6 15 14 9 11 3 0 8 12 2 13 7 1 4 10 5)
    #(10 2 8 4 7 6 1 5 15 11 9 14 3 12 13 0)))
(define (rotr64 x n) (bitwise-and M64 (bitwise-ior (arithmetic-shift x (- n)) (arithmetic-shift x (- 64 n)))))
(define (rd-int-le vec off) (for/fold ([a 0]) ([i (in-range 8)]) (+ a (arithmetic-shift (rd vec (+ off i)) (* 8 i)))))
(define (int64->le x) (for/list ([i (in-range 8)]) (bitwise-and (arithmetic-shift x (* -8 i)) #xff)))

(define (pc-blake2f vec len gas)
  (define f (rd vec 212))
  (cond
    [(not (= len 213)) (values #f 0 '())]
    [(not (memv f '(0 1))) (values #f 0 '())]
    [else
     (define rounds (rd-int vec 0 4))
     (cond
       [(< gas rounds) (values #f 0 '())]
       [else
        (define h (build-vector 8 (lambda (i) (rd-int-le vec (+ 4 (* 8 i))))))
        (define m (build-vector 16 (lambda (i) (rd-int-le vec (+ 68 (* 8 i))))))
        (define t0 (rd-int-le vec 196)) (define t1 (rd-int-le vec 204))
        (define v (make-vector 16 0))
        (for ([i (in-range 8)]) (vector-set! v i (vector-ref h i)) (vector-set! v (+ i 8) (vector-ref BLAKE-IV i)))
        (vector-set! v 12 (bitwise-xor (vector-ref v 12) t0))
        (vector-set! v 13 (bitwise-xor (vector-ref v 13) t1))
        (when (= f 1) (vector-set! v 14 (bitwise-xor (vector-ref v 14) M64)))
        (define (G a b c d x y)
          (vector-set! v a (bitwise-and M64 (+ (vector-ref v a) (vector-ref v b) x)))
          (vector-set! v d (rotr64 (bitwise-xor (vector-ref v d) (vector-ref v a)) 32))
          (vector-set! v c (bitwise-and M64 (+ (vector-ref v c) (vector-ref v d))))
          (vector-set! v b (rotr64 (bitwise-xor (vector-ref v b) (vector-ref v c)) 24))
          (vector-set! v a (bitwise-and M64 (+ (vector-ref v a) (vector-ref v b) y)))
          (vector-set! v d (rotr64 (bitwise-xor (vector-ref v d) (vector-ref v a)) 16))
          (vector-set! v c (bitwise-and M64 (+ (vector-ref v c) (vector-ref v d))))
          (vector-set! v b (rotr64 (bitwise-xor (vector-ref v b) (vector-ref v c)) 63)))
        (for ([r (in-range rounds)])
          (define s (vector-ref BLAKE-SIGMA (modulo r 10)))
          (define (mm i) (vector-ref m (vector-ref s i)))
          (G 0 4 8 12 (mm 0) (mm 1)) (G 1 5 9 13 (mm 2) (mm 3))
          (G 2 6 10 14 (mm 4) (mm 5)) (G 3 7 11 15 (mm 6) (mm 7))
          (G 0 5 10 15 (mm 8) (mm 9)) (G 1 6 11 12 (mm 10) (mm 11))
          (G 2 7 8 13 (mm 12) (mm 13)) (G 3 4 9 14 (mm 14) (mm 15)))
        (define out
          (append-map (lambda (i) (int64->le (bitwise-xor (vector-ref h i) (vector-ref v i) (vector-ref v (+ i 8)))))
                      (range 8)))
        (values #t (- gas rounds) out)])]))
