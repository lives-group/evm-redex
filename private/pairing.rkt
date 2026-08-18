#lang racket/base

;; bn254 (alt_bn128) optimal-ate pairing for the ecpairing precompile (0x08).
;;
;; Follows the structure of py_ecc.bn128 (which execution-specs uses): field
;; extensions are represented as coefficient lists over Fp, with Fp12 a direct
;; degree-12 extension Fp[w]/(w^12 - 18 w^6 + 82).  G1 is over Fp, G2 over Fp2;
;; the Miller loop runs over Fp12 after twisting G2 and casting G1.

(require racket/list
         "words.rkt"   ; modular-expt
         "ec.rkt")     ; BN-P

(provide bn-pairing-check bn-g1-add bn-g1-mul)

(define P BN-P)

;; G1 group ops for the ecadd/ecmul precompiles (points as (cons x y) or #f).
;; Thin wrappers over ec.rkt's generic curve arithmetic so the native (mcl)
;; accelerator can be swapped in below without touching precompiles.rkt.
(define (bn-g1-add a b) (pt-add a b BN-P 0))
(define (bn-g1-mul k a) (pt-mul k a BN-P 0))
(define R-ORDER 21888242871839275222246405745257275088548364400416034343698204186575808495617)

;; ===================== generic field extension Fp[x]/(modulus) ==========
;; element = list of `deg` integers (low degree first).  modulus-coeffs are the
;; `deg` non-leading coefficients m_0..m_{deg-1} of x^deg = -(m_0 + m_1 x + ...).
(define (mk-fqp deg modulus-coeffs)
  (define mcv (list->vector modulus-coeffs))
  (define (zero) (make-list deg 0))
  (define (one) (cons 1 (make-list (sub1 deg) 0)))
  (define (from-int n) (cons (modulo n P) (make-list (sub1 deg) 0)))
  (define (add a b) (map (lambda (x y) (modulo (+ x y) P)) a b))
  (define (sub a b) (map (lambda (x y) (modulo (- x y) P)) a b))
  (define (neg a) (map (lambda (x) (modulo (- x) P)) a))
  (define (eq? a b) (equal? a b))
  (define (is-zero? a) (andmap zero? a))
  ;; multiply with reduction (py_ecc algorithm)
  (define (mul a b)
    (define av (list->vector a)) (define bv (list->vector b))
    (define blen (sub1 (* 2 deg)))
    (define c (make-vector blen 0))
    (for* ([i (in-range deg)] [j (in-range deg)])
      (vector-set! c (+ i j) (+ (vector-ref c (+ i j)) (* (vector-ref av i) (vector-ref bv j)))))
    ;; reduce: while length > deg, fold top coefficient down
    (let loop ([len blen])
      (cond
        [(= len deg) (for/list ([i (in-range deg)]) (modulo (vector-ref c i) P))]
        [else
         (define top (vector-ref c (sub1 len)))
         (define exp (- len deg 1))
         (for ([i (in-range deg)])
           (vector-set! c (+ exp i) (- (vector-ref c (+ exp i)) (* top (vector-ref mcv i)))))
         (loop (sub1 len))])))
  (define (pow a e)
    (let loop ([e e] [base a] [acc (one)])
      (cond [(= e 0) acc]
            [(odd? e) (loop (quotient e 2) (mul base base) (mul acc base))]
            [else (loop (quotient e 2) (mul base base) acc)])))
  ;; inverse via extended euclidean over Fp[x]
  (define (deg-of poly)
    (let loop ([i (sub1 (length poly))])
      (cond [(< i 0) 0] [(not (zero? (list-ref poly i))) i] [else (loop (sub1 i))])))
  (define (poly-rounded-div a b)
    (define dega (deg-of a)) (define degb (deg-of b))
    (define temp (list->vector a))
    (define o (make-vector (length a) 0))
    (for ([i (in-range (- dega degb) -1 -1)])
      (define coef (modulo (* (vector-ref temp (+ degb i)) (modular-expt (list-ref b degb) (- P 2) P)) P))
      (vector-set! o i (modulo (+ (vector-ref o i) coef) P))
      (for ([c (in-range (add1 degb))])
        (vector-set! temp (+ c i) (modulo (- (vector-ref temp (+ c i)) (* coef (list-ref b c))) P))))
    (vector->list o))
  (define (inv a)
    (define lm (cons 1 (make-list deg 0)))
    (define hm (make-list (add1 deg) 0))
    (define low (append a '(0)))
    (define high (append modulus-coeffs '(1)))
    (let loop ([lm lm] [hm hm] [low low] [high high])
      (cond
        [(> (deg-of low) 0)
         (define r0 (poly-rounded-div high low))
         (define r (append r0 (make-list (- (add1 deg) (length r0)) 0)))
         (define nm (list->vector hm))
         (define new (list->vector high))
         (for* ([i (in-range (add1 deg))] [j (in-range (- (add1 deg) i))])
           (vector-set! nm (+ i j) (modulo (- (vector-ref nm (+ i j)) (* (list-ref lm i) (list-ref r j))) P))
           (vector-set! new (+ i j) (modulo (- (vector-ref new (+ i j)) (* (list-ref low i) (list-ref r j))) P)))
         (loop (vector->list nm) lm (vector->list new) low)]
        [else
         (define inv0 (modular-expt (car low) (- P 2) P))
         (for/list ([i (in-range deg)]) (modulo (* (list-ref lm i) inv0) P))])))
  (define (div a b) (mul a (inv b)))
  (list deg zero one from-int add sub neg eq? is-zero? mul pow inv div))

;; field accessors
(define (f-zero f) ((list-ref f 1)))
(define (f-one f) ((list-ref f 2)))
(define (f-from f n) ((list-ref f 3) n))
(define (f+ f a b) ((list-ref f 4) a b))
(define (f- f a b) ((list-ref f 5) a b))
(define (f-neg f a) ((list-ref f 6) a))
(define (f= f a b) ((list-ref f 7) a b))
(define (f-zero? f a) ((list-ref f 8) a))
(define (f* f a b) ((list-ref f 9) a b))
(define (f-pow f a e) ((list-ref f 10) a e))
(define (f-div f a b) ((list-ref f 12) a b))

(define FQ2 (mk-fqp 2 '(1 0)))                    ; u^2 + 1
(define FQ12 (mk-fqp 12 (list 82 0 0 0 0 0 (modulo -18 P) 0 0 0 0 0)))  ; w^12 - 18 w^6 + 82

;; ===================== curve ops over a field ==========================
;; point = (cons x y) or 'inf
(define (c-double f pt)
  (define x (car pt)) (define y (cdr pt))
  (define m (f-div f (f* f (f-from f 3) (f* f x x)) (f* f (f-from f 2) y)))
  (define nx (f- f (f* f m m) (f* f (f-from f 2) x)))
  (cons nx (f- f (f* f m (f- f x nx)) y)))

(define (c-add f p1 p2)
  (cond
    [(eq? p1 'inf) p2]
    [(eq? p2 'inf) p1]
    [else
     (define x1 (car p1)) (define y1 (cdr p1)) (define x2 (car p2)) (define y2 (cdr p2))
     (cond
       [(and (f= f x1 x2) (f= f y1 y2)) (c-double f p1)]
       [(f= f x1 x2) 'inf]
       [else
        (define m (f-div f (f- f y2 y1) (f- f x2 x1)))
        (define nx (f- f (f- f (f* f m m) x1) x2))
        (cons nx (f- f (f* f m (f- f x1 nx)) y1))])]))

(define (c-mul f k pt)
  (let loop ([k k] [base pt] [acc 'inf])
    (cond [(= k 0) acc]
          [(odd? k) (loop (quotient k 2) (c-double f base) (c-add f acc base))]
          [else (loop (quotient k 2) (c-double f base) acc)])))

(define (c-neg f pt) (if (eq? pt 'inf) 'inf (cons (car pt) (f-neg f (cdr pt)))))

;; ===================== twist G2(FQ2) -> FQ12, cast G1 -> FQ12 ===========
(define W (list 0 1 0 0 0 0 0 0 0 0 0 0))          ; FQ12 element w

(define (fq12-from-fq2-pair c0 c1)
  ;; build FQ12 [c0, 0,0,0,0,0, c1, 0,0,0,0,0]
  (list c0 0 0 0 0 0 c1 0 0 0 0 0))

(define (twist pt)
  ;; pt over FQ2: x=(x0 x1), y=(y0 y1)
  (define x (car pt)) (define y (cdr pt))
  (define x0 (modulo (- (car x) (* (cadr x) 9)) P)) (define x1 (cadr x))
  (define y0 (modulo (- (car y) (* (cadr y) 9)) P)) (define y1 (cadr y))
  (define nx (fq12-from-fq2-pair x0 x1))
  (define ny (fq12-from-fq2-pair y0 y1))
  (cons (f* FQ12 nx (f* FQ12 W W)) (f* FQ12 ny (f* FQ12 W (f* FQ12 W W)))))

(define (cast-g1 pt)
  (cons (cons (car pt) (make-list 11 0)) (cons (cdr pt) (make-list 11 0))))

;; ===================== Miller loop + final exponentiation ==============
(define ATE-LOOP-COUNT 29793968203157093288)
(define LOG-ATE 63)

(define (linefunc P1 P2 T)
  (define x1 (car P1)) (define y1 (cdr P1))
  (define x2 (car P2)) (define y2 (cdr P2))
  (define xt (car T)) (define yt (cdr T))
  (cond
    [(not (f= FQ12 x1 x2))
     (define m (f-div FQ12 (f- FQ12 y2 y1) (f- FQ12 x2 x1)))
     (f- FQ12 (f* FQ12 m (f- FQ12 xt x1)) (f- FQ12 yt y1))]
    [(f= FQ12 y1 y2)
     (define m (f-div FQ12 (f* FQ12 (f-from FQ12 3) (f* FQ12 x1 x1)) (f* FQ12 (f-from FQ12 2) y1)))
     (f- FQ12 (f* FQ12 m (f- FQ12 xt x1)) (f- FQ12 yt y1))]
    [else (f- FQ12 xt x1)]))

(define (miller-loop Q PP)        ; Q,PP are FQ12 points
  (cond
    [(or (eq? Q 'inf) (eq? PP 'inf)) (f-one FQ12)]
    [else
     (let loop ([i LOG-ATE] [R Q] [f (f-one FQ12)])
       (cond
         [(< i 0)
          ;; final adjustment for bn128
          (define Q1 (cons (f-pow FQ12 (car Q) P) (f-pow FQ12 (cdr Q) P)))
          (define nQ2 (cons (f-pow FQ12 (car Q1) P) (f-neg FQ12 (f-pow FQ12 (cdr Q1) P))))
          (define f1 (f* FQ12 f (linefunc R Q1 PP)))
          (define R1 (c-add FQ12 R Q1))
          (f* FQ12 f1 (linefunc R1 nQ2 PP))]
         [else
          (define f2 (f* FQ12 (f* FQ12 f f) (linefunc R R PP)))
          (define R2 (c-double FQ12 R))
          (if (bitwise-bit-set? ATE-LOOP-COUNT i)
              (loop (sub1 i) (c-add FQ12 R2 Q) (f* FQ12 f2 (linefunc R2 Q PP)))
              (loop (sub1 i) R2 f2))]))]))

(define FINAL-EXP (quotient (- (expt P 12) 1) R-ORDER))

;; ===================== public: pairing check ===========================
;; pairs : list of (G1-point . G2-point), points as (cons x y) over Fp / Fp2, or 'inf
;; returns #t iff product of pairings == 1
(define (bn-pairing-check pairs)
  (define acc
    (for/fold ([acc (f-one FQ12)]) ([pr (in-list pairs)])
      (define g1 (car pr)) (define g2 (cdr pr))
      (if (or (eq? g1 'inf) (eq? g2 'inf))
          acc
          (f* FQ12 acc (miller-loop (twist g2) (cast-g1 g1))))))
  (f= FQ12 (f-pow FQ12 acc FINAL-EXP) (f-one FQ12)))

;; expose curve/field helpers for the precompile (validation + subgroup check)
(provide on-g1? on-g2? bn-g2-in-subgroup? P R-ORDER)

(define B2 (f-div FQ2 (list 3 0) (list 9 1)))     ; twist b' = 3/(9+u)

(define (on-g1? pt)            ; pt = (cons x y) ints, or 'inf
  (or (eq? pt 'inf)
      (let ([x (car pt)] [y (cdr pt)])
        (= (modulo (- (* y y) (+ (* x x x) 3)) P) 0))))

(define (on-g2? pt)            ; pt = (cons x y), x,y FQ2 = (c0 c1)
  (or (eq? pt 'inf)
      (let* ([x (car pt)] [y (cdr pt)]
             [lhs (f* FQ2 y y)]
             [rhs (f+ FQ2 (f* FQ2 x (f* FQ2 x x)) B2)])
        (f= FQ2 lhs rhs))))

(define (bn-g2-in-subgroup? pt)
  (or (eq? pt 'inf) (eq? (c-mul FQ2 R-ORDER pt) 'inf)))

;; ======================================================================
;; Native (mcl) acceleration for bn254 — adopt the mcl G1 ops and pairing
;; only after they reproduce the pure results on a known point/pair (the pure
;; implementation is the oracle).  Absent libmclbn384_256.so, this is a no-op
;; and everything stays pure.  See private/native.rkt and mcl.nix.
;; ======================================================================
(require (only-in "native.rkt"
                  native-bn-g1-add native-bn-g1-mul native-bn-pairing-check))

(define BN-G2-GEN
  (cons (list 10857046999023057135944570762232829481370756359578518086990519993285655852781
              11559732032986387107991004021392285783925812861821192530917403151452391805634)
        (list 8495653923123431417604973247489272438418190587263600148770280649306958101930
              4082367875863433681332203403145435568316851327593401208105741076214120093531)))

(define (bn-native-ok?)
  (and native-bn-g1-add native-bn-g1-mul native-bn-pairing-check
       (with-handlers ([(lambda (_) #t) (lambda (_) #f)])
         (define G1 (cons 1 2))
         (define nG1 (cons 1 (- BN-P 2)))            ; -G1
         (and (equal? (native-bn-g1-add G1 G1)       (bn-g1-add G1 G1))
              (equal? (native-bn-g1-mul 7 G1)        (bn-g1-mul 7 G1))
              (equal? (native-bn-g1-add #f G1)       G1)
              (eq? #t (native-bn-pairing-check (list (cons G1 BN-G2-GEN) (cons nG1 BN-G2-GEN))))
              (eq? #f (native-bn-pairing-check (list (cons G1 BN-G2-GEN))))))))

(when (bn-native-ok?)
  (set! bn-g1-add native-bn-g1-add)
  (set! bn-g1-mul native-bn-g1-mul)
  (set! bn-pairing-check native-bn-pairing-check))
