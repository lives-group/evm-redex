#lang racket/base

;; BLS12-381 (EIP-2537) — field tower, G1/G2 group ops, and the optimal-ate
;; pairing, following py_ecc.optimized_bls12_381.  Fp2 = Fp[u]/(u^2+1),
;; Fp12 = Fp[w]/(w^12 - 2 w^6 + 2) (direct degree-12 extension).
;;
;; This module provides the math (curve ops, subgroup checks, pairing check);
;; the precompile encoding/gas wiring lives in private/precompiles.rkt.

(require racket/list
         "fields.rkt"
         "bls-map-constants.rkt")

(require "words.rkt")   ; modular-expt

(provide BLS-P BLS-R BLS-MODULUS
         g1-add g1-mul g1-on-curve? g1-in-subgroup? g1-inf?
         g2-add g2-mul g2-on-curve? g2-in-subgroup? g2-inf?
         bls-pairing-check kzg-verify map-fp-to-g1 map-fp2-to-g2
         G1-GEN G2-GEN)

(define BLS-P 4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272559787)
(define BLS-R 52435875175126190479447740508185965837690552500527637822603658699938581184513)

(define FQ2 (mk-fqp BLS-P 2 '(1 0)))                                  ; u^2 + 1
(define FQ12 (mk-fqp BLS-P 12 (list 2 0 0 0 0 0 (modulo -2 BLS-P) 0 0 0 0 0))) ; w^12 - 2w^6 + 2

;; ===================== generic curve ops over a field f ================
;; point = (cons x y) or 'inf
(define (c-double f pt)
  (cond
    [(eq? pt 'inf) 'inf]
    [else
     (define x (car pt)) (define y (cdr pt))
     (define m (f-div f (f* f (f-from f 3) (f* f x x)) (f* f (f-from f 2) y)))
     (define nx (f- f (f* f m m) (f* f (f-from f 2) x)))
     (cons nx (f- f (f* f m (f- f x nx)) y))]))

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

;; ===================== G1 (over Fp, as a degree-1 field) ================
(define FQ1 (mk-fqp BLS-P 1 '()))         ; Fp itself as a field record
(define (g1pt x y) (cons (list x) (list y)))       ; wrap ints into FQ1 elements
(define (g1-inf? pt) (eq? pt 'inf))
(define (g1-add a b) (c-add FQ1 a b))
(define (g1-mul k a) (c-mul FQ1 k a))
(define B1 (list 4))
(define (g1-on-curve? pt)
  (or (eq? pt 'inf)
      (f= FQ1 (f* FQ1 (cdr pt) (cdr pt)) (f+ FQ1 (f* FQ1 (car pt) (f* FQ1 (car pt) (car pt))) B1))))
(define (g1-in-subgroup? pt) (or (eq? pt 'inf) (eq? (c-mul FQ1 BLS-R pt) 'inf)))

;; ===================== G2 (over Fp2) ===================================
(define B2 (f+ FQ2 (list 4 0) (list 0 4)))         ; 4 + 4u
(define (g2-inf? pt) (eq? pt 'inf))
(define (g2-add a b) (c-add FQ2 a b))
(define (g2-mul k a) (c-mul FQ2 k a))
(define (g2-on-curve? pt)
  (or (eq? pt 'inf)
      (f= FQ2 (f* FQ2 (cdr pt) (cdr pt))
          (f+ FQ2 (f* FQ2 (car pt) (f* FQ2 (car pt) (car pt))) B2))))
(define (g2-in-subgroup? pt) (or (eq? pt 'inf) (eq? (c-mul FQ2 BLS-R pt) 'inf)))

;; ===================== twist G2 -> Fp12, cast G1 -> Fp12 ================
(define W (list 0 1 0 0 0 0 0 0 0 0 0 0))

(define (fq12-pair c0 c1) (list c0 0 0 0 0 0 c1 0 0 0 0 0))

(define (twist pt)        ; pt over FQ2: x=(x0 x1) y=(y0 y1); xi = 1 + u for BLS
  (define x (car pt)) (define y (cdr pt))
  (define x0 (modulo (- (car x) (cadr x)) BLS-P)) (define x1 (cadr x))
  (define y0 (modulo (- (car y) (cadr y)) BLS-P)) (define y1 (cadr y))
  (cons (f* FQ12 (fq12-pair x0 x1) (f* FQ12 W W))
        (f* FQ12 (fq12-pair y0 y1) (f* FQ12 W (f* FQ12 W W)))))

(define (cast-g1 pt)      ; pt = (cons (list x) (list y)) -> Fp12 point
  (cons (cons (car (car pt)) (make-list 11 0)) (cons (car (cdr pt)) (make-list 11 0))))

;; ===================== Miller loop + final exponentiation ==============
(define ATE-LOOP-COUNT 15132376222941642752)       ; |z|, z = -0xd201000000010000
(define LOG-ATE 62)

(define (linefunc P1 P2 T)
  (define x1 (car P1)) (define y1 (cdr P1)) (define x2 (car P2)) (define y2 (cdr P2))
  (define xt (car T)) (define yt (cdr T))
  (cond
    [(not (f= FQ12 x1 x2))
     (define m (f-div FQ12 (f- FQ12 y2 y1) (f- FQ12 x2 x1)))
     (f- FQ12 (f* FQ12 m (f- FQ12 xt x1)) (f- FQ12 yt y1))]
    [(f= FQ12 y1 y2)
     (define m (f-div FQ12 (f* FQ12 (f-from FQ12 3) (f* FQ12 x1 x1)) (f* FQ12 (f-from FQ12 2) y1)))
     (f- FQ12 (f* FQ12 m (f- FQ12 xt x1)) (f- FQ12 yt y1))]
    [else (f- FQ12 xt x1)]))

(define FINAL-EXP (quotient (- (expt BLS-P 12) 1) BLS-R))

(define (miller-loop Q PP)
  (let loop ([i LOG-ATE] [R Q] [f (f-one FQ12)])
    (cond
      [(< i 0) f]
      [else
       (define f2 (f* FQ12 (f* FQ12 f f) (linefunc R R PP)))
       (define R2 (c-double FQ12 R))
       (if (bitwise-bit-set? ATE-LOOP-COUNT i)
           (loop (sub1 i) (c-add FQ12 R2 Q) (f* FQ12 f2 (linefunc R2 Q PP)))
           (loop (sub1 i) R2 f2))])))

;; pairs: list of (G1 . G2) points (G1 as (cons (list x)(list y)), G2 as
;; (cons (c0 c1)(c0 c1))), or with 'inf.  Returns #t iff product of pairings == 1.
(define (bls-pairing-check pairs)
  (define acc
    (for/fold ([acc (f-one FQ12)]) ([pr (in-list pairs)])
      (define g1 (car pr)) (define g2 (cdr pr))
      (if (or (eq? g1 'inf) (eq? g2 'inf))
          acc
          (f* FQ12 acc (miller-loop (twist g2) (cast-g1 g1))))))
  (f= FQ12 (f-pow FQ12 acc FINAL-EXP) (f-one FQ12)))

;; ===================== KZG point evaluation (EIP-4844) =================
(define BLS-MODULUS BLS-R)        ; scalar field order

(define G1-GEN
  (cons (list 3685416753713387016781088315183077757961620795782546409894578378688607592378376318836054947676345821548104185464507)
        (list 1339506544944476473020471379941921221584933875938349620426543736416511423956333506472724655353366534992391756441569)))
(define G2-GEN
  (cons (list 352701069587466618187139116011060144890029952792775240219908644239793785735715026873347600343865175952761926303160
              3059144344244213709971259814753781636986470325476647558659373206291635324768958432433509563104347017837885763365758)
        (list 1985150602287291935568054521177171638300868978215655730859378665066344726373823718423869104263333984641494340347905
              927553665492332455747201965776037880757740193453592970025027978793976877002675564980949289727957565575433344219582)))
(define (g1-neg pt) (if (eq? pt 'inf) 'inf (cons (car pt) (list (modulo (- (car (cdr pt))) BLS-P)))))
(define (g2-neg pt) (if (eq? pt 'inf) 'inf (cons (car pt) (f-neg FQ2 (cdr pt)))))

(define (bytes->int bs) (for/fold ([a 0]) ([b (in-list bs)]) (+ (* a 256) b)))
(define MASK381 (sub1 (expt 2 381)))

;; Fp sqrt (p = 3 mod 4); -> int or #f
(define (fp-sqrt a)
  (define c (modular-expt a (quotient (+ BLS-P 1) 4) BLS-P))
  (and (= (modulo (* c c) BLS-P) (modulo a BLS-P)) c))

;; Fp2 sqrt (complex method, p = 3 mod 4); -> FQ2 elem or #f
(define (fp2-sqrt a)
  (define neg1 (f-neg FQ2 (f-one FQ2)))
  (define a1 (f-pow FQ2 a (quotient (- BLS-P 3) 4)))
  (define alpha (f* FQ2 a1 (f* FQ2 a1 a)))
  (define a0 (f* FQ2 (f-pow FQ2 alpha BLS-P) alpha))
  (cond
    [(f= FQ2 a0 neg1) #f]
    [else
     (define x0 (f* FQ2 a1 a))
     (define res
       (if (f= FQ2 alpha neg1)
           (f* FQ2 (list 0 1) x0)
           (f* FQ2 (f-pow FQ2 (f+ FQ2 (f-one FQ2) alpha) (quotient (- BLS-P 1) 2)) x0)))
     (and (f= FQ2 (f* FQ2 res res) a) res)]))

;; lexicographic "is larger" for sign selection
(define (fp-larger? y) (> (* 2 y) BLS-P))
(define (fp2-larger? y) (let ([c1 (cadr y)] [c0 (car y)])
                          (cond [(not (= c1 (modulo (- c1) BLS-P))) (> (* 2 c1) BLS-P)]
                                [else (> (* 2 c0) BLS-P)])))

;; decompress a 48-byte compressed G1 point -> point / 'inf / 'bad
(define (decompress-g1 bs)
  (define b0 (car bs))
  (cond
    [(zero? (bitwise-and b0 #x80)) 'bad]
    [(not (zero? (bitwise-and b0 #x40)))
     (if (and (= (bitwise-and b0 #x3f) 0) (andmap zero? (cdr bs))) 'inf 'bad)]
    [else
     (define sflag (not (zero? (bitwise-and b0 #x20))))
     (define x (bitwise-and (bytes->int bs) MASK381))
     (cond
       [(>= x BLS-P) 'bad]
       [else
        (define y0 (fp-sqrt (modulo (+ (* x x x) 4) BLS-P)))
        (cond
          [(not y0) 'bad]
          [else
           (define yL (max y0 (modulo (- y0) BLS-P)))
           (define yS (min y0 (modulo (- y0) BLS-P)))
           (cons (list x) (list (if sflag yL yS)))])])]))

;; decompress a 96-byte compressed G2 point -> point / 'inf / 'bad
(define (decompress-g2 bs)
  (define b0 (car bs))
  (cond
    [(zero? (bitwise-and b0 #x80)) 'bad]
    [(not (zero? (bitwise-and b0 #x40)))
     (if (and (= (bitwise-and b0 #x3f) 0) (andmap zero? (cdr bs))) 'inf 'bad)]
    [else
     (define sflag (not (zero? (bitwise-and b0 #x20))))
     (define x1 (bitwise-and (bytes->int (take bs 48)) MASK381))     ; imaginary part (flagged half)
     (define x0 (bytes->int (drop bs 48)))
     (cond
       [(or (>= x0 BLS-P) (>= x1 BLS-P)) 'bad]
       [else
        (define x (list x0 x1))
        (define rhs (f+ FQ2 (f* FQ2 x (f* FQ2 x x)) B2))
        (define y (fp2-sqrt rhs))
        (cond
          [(not y) 'bad]
          [else
           (define ny (f-neg FQ2 y))
           (define yy (if sflag (if (fp2-larger? y) y ny) (if (fp2-larger? y) ny y)))
           (cons x yy)])])]))

(define KZG-SETUP-G2
  (decompress-g2
   (let ([hex "b5bfd7dd8cdeb128843bc287230af38926187075cbfbefa81009a2ce615ac53d2914e5870cb452d2afaaab24f3499f72185cbfee53492714734429b7b38608e23926c911cceceac9a36851477ba4c60b087041de621000edc98edada20c1def2"])
     (for/list ([i (in-range 0 (string-length hex) 2)])
       (string->number (substring hex i (+ i 2)) 16)))))

;; kzg-verify: commitment/proof are 48-byte lists; z,y ints < BLS_MODULUS
(define (kzg-verify commitment z y proof)
  (define C (decompress-g1 commitment))
  (define PR (decompress-g1 proof))
  (cond
    [(or (eq? C 'bad) (eq? PR 'bad)) #f]
    [else
     (define P-y (g1-add C (g1-mul (modulo (- BLS-MODULUS y) BLS-MODULUS) G1-GEN)))
     (define X-z (g2-add KZG-SETUP-G2 (g2-mul (modulo (- BLS-MODULUS z) BLS-MODULUS) G2-GEN)))
     (bls-pairing-check (list (cons P-y (g2-neg G2-GEN)) (cons PR X-z)))]))

;; ===================== map-to-curve (RFC 9380 SSWU + isogeny) ==========
(define (sgn0-fp x) (modulo x 2))
(define (sgn0-fp2 x) (if (not (zero? (car x))) (modulo (car x) 2) (modulo (cadr x) 2)))
(define (fp-inv x) (modular-expt x (- BLS-P 2) BLS-P))

;; --- G1: arithmetic over Fp (plain integers) ---
(define (swu-g1 t)
  (define p BLS-P)
  (define t2 (modulo (* t t) p))
  (define zt2 (modulo (* ISO11-Z t2) p))
  (define temp (modulo (+ zt2 (modulo (* zt2 zt2) p)) p))
  (define den0 (modulo (- (* ISO11-A temp)) p))
  (define den (if (zero? den0) (modulo (* ISO11-Z ISO11-A) p) den0))
  (define num (modulo (* ISO11-B (modulo (+ temp 1) p)) p))
  (define v (modulo (* den den den) p))
  (define u (modulo (+ (modulo (* num num num) p)
                       (modulo (* ISO11-A num (modulo (* den den) p)) p)
                       (modulo (* ISO11-B v) p)) p))
  (define uv (modulo (* u v) p))
  (define result (modulo (* uv (modular-expt (modulo (* uv (modulo (* v v) p)) p) P-MINUS-3-DIV-4 p)) p))
  (define is-root (zero? (modulo (- (modulo (* result result v) p) u) p)))
  (define y0 (if is-root result
                 (modulo (* result (modulo (* (modulo (* t t t) p) SQRT-MINUS-11-CUBED) p)) p)))
  (define num2 (if is-root num (modulo (* num zt2) p)))
  (define y (if (= (sgn0-fp t) (sgn0-fp y0)) y0 (modulo (- y0) p)))
  (values num2 (modulo (* y den) p) den))

(define (iso-map-g1 x y z)
  (define p BLS-P)
  (define zpows (for/list ([i (in-range 1 16)]) (modular-expt z i p)))
  (define mapped (make-vector 4 0))
  (for ([i (in-range 4)])
    (define ki (list-ref ISO11-MAP i))
    (vector-set! mapped i
      (for/fold ([m (last ki)]) ([kij (in-list (reverse (drop-right ki 1)))] [j (in-naturals)])
        (modulo (+ (* m x) (modulo (* (list-ref zpows j) kij) p)) p))))
  (vector-set! mapped 1 (modulo (* (vector-ref mapped 1) z) p))
  (vector-set! mapped 2 (modulo (* (vector-ref mapped 2) y) p))
  (vector-set! mapped 3 (modulo (* (vector-ref mapped 3) z) p))
  (values (modulo (* (vector-ref mapped 0) (vector-ref mapped 3)) p)    ; x_num*y_den
          (modulo (* (vector-ref mapped 1) (vector-ref mapped 2)) p)    ; x_den*y_num
          (modulo (* (vector-ref mapped 1) (vector-ref mapped 3)) p)))  ; x_den*y_den

(define (map-fp-to-g1 u)        ; u in [0,p); -> G1 point (cons (list x)(list y))
  (define-values (sx sy sz) (swu-g1 u))
  (define-values (xg yg zg) (iso-map-g1 sx sy sz))
  (define zi (fp-inv zg))
  (g1-mul H-EFF-G1 (cons (list (modulo (* xg zi) BLS-P)) (list (modulo (* yg zi) BLS-P)))))

;; --- G2: arithmetic over Fp2 ---
(define (sqrt-div-fq2 u v)
  (define v7 (f* FQ2 v (f-pow FQ2 v 6)))
  (define temp1 (f* FQ2 u v7))
  (define temp2 (f* FQ2 temp1 (f-pow FQ2 v 8)))
  (define gamma (f* FQ2 (f-pow FQ2 temp2 P-MINUS-9-DIV-16) temp1))
  (let loop ([roots EIGHTH-ROOTS] [found #f] [res gamma])
    (cond
      [(null? roots) (values found res)]
      [else
       (define cand (f* FQ2 (car roots) gamma))
       (if (and (not found) (f-zero? FQ2 (f- FQ2 (f* FQ2 (f* FQ2 cand cand) v) u)))
           (loop (cdr roots) #t cand)
           (loop (cdr roots) found res))])))

(define (swu-g2 t)
  (define t2 (f* FQ2 t t))
  (define zt2 (f* FQ2 ISO3-Z t2))
  (define temp (f+ FQ2 zt2 (f* FQ2 zt2 zt2)))
  (define den0 (f-neg FQ2 (f* FQ2 ISO3-A temp)))
  (define den (if (f-zero? FQ2 den0) (f* FQ2 ISO3-Z ISO3-A) den0))
  (define num (f* FQ2 ISO3-B (f+ FQ2 temp (f-one FQ2))))
  (define v (f* FQ2 den (f* FQ2 den den)))
  (define u (f+ FQ2 (f* FQ2 num (f* FQ2 num num))
                (f+ FQ2 (f* FQ2 ISO3-A (f* FQ2 num (f* FQ2 den den))) (f* FQ2 ISO3-B v))))
  (define-values (success cand0) (sqrt-div-fq2 u v))
  (define cand1 (f* FQ2 cand0 (f* FQ2 t t2)))             ; cand * t^3
  (define u2 (f* FQ2 (f-pow FQ2 zt2 3) u))                ; (Z t^2)^3 * u
  (define-values (y2 success2)
    (let loop ([etas ETAS] [y cand0] [s2 #f])
      (cond [(null? etas) (values y s2)]
            [else
             (define ec (f* FQ2 (car etas) cand1))
             (if (and (not success) (not s2) (f-zero? FQ2 (f- FQ2 (f* FQ2 (f* FQ2 ec ec) v) u2)))
                 (loop (cdr etas) ec #t)
                 (loop (cdr etas) y s2))])))
  (define num2 (if success num (f* FQ2 num zt2)))
  (define y3 (if (= (sgn0-fp2 t) (sgn0-fp2 y2)) y2 (f-neg FQ2 y2)))
  (values num2 (f* FQ2 y3 den) den))

(define (iso-map-g2 x y z)
  (define zpows (list z (f* FQ2 z z) (f* FQ2 z (f* FQ2 z z))))
  (define mapped (make-vector 4 (f-zero FQ2)))
  (for ([i (in-range 4)])
    (define ki (list-ref ISO3-MAP i))
    (vector-set! mapped i
      (for/fold ([m (last ki)]) ([kij (in-list (reverse (drop-right ki 1)))] [j (in-naturals)])
        (f+ FQ2 (f* FQ2 m x) (f* FQ2 (list-ref zpows j) kij)))))
  (vector-set! mapped 2 (f* FQ2 (vector-ref mapped 2) y))
  (vector-set! mapped 3 (f* FQ2 (vector-ref mapped 3) z))
  (values (f* FQ2 (vector-ref mapped 0) (vector-ref mapped 3))
          (f* FQ2 (vector-ref mapped 1) (vector-ref mapped 2))
          (f* FQ2 (vector-ref mapped 1) (vector-ref mapped 3))))

(define (map-fp2-to-g2 u)        ; u = FQ2 (list c0 c1); -> G2 point
  (define-values (sx sy sz) (swu-g2 u))
  (define-values (xg yg zg) (iso-map-g2 sx sy sz))
  (define zi (f-inv FQ2 zg))
  (g2-mul H-EFF-G2 (cons (f* FQ2 xg zi) (f* FQ2 yg zi))))

;; ======================================================================
;; Native (blst) acceleration — swap the pure G1/G2 group ops and pairing
;; for blst when available (see native.rkt).  The pure implementation is the
;; ORACLE: we adopt each native op only after it reproduces the pure result on
;; the generator, so a wrong binding / missing library silently stays pure.
;; ======================================================================
(require (only-in "native.rkt"
                  native-bls-g1-add native-bls-g1-mul
                  native-bls-g2-add native-bls-g2-mul native-bls-pairing-check
                  native-bls-map-g1 native-bls-map-g2))

(define (bls-native-ok?)
  (and native-bls-g1-add native-bls-g1-mul native-bls-g2-add
       native-bls-g2-mul native-bls-pairing-check
       native-bls-map-g1 native-bls-map-g2
       (with-handlers ([(lambda (_) #t) (lambda (_) #f)])
         (define ng1 (g1-mul (- BLS-R 1) G1-GEN))    ; -G1
         (and (equal? (native-bls-g1-add G1-GEN G1-GEN) (g1-add G1-GEN G1-GEN))
              (equal? (native-bls-g1-mul 5 G1-GEN)      (g1-mul 5 G1-GEN))
              (equal? (native-bls-g1-add G1-GEN ng1)    'inf)
              (equal? (native-bls-g2-add G2-GEN G2-GEN) (g2-add G2-GEN G2-GEN))
              (equal? (native-bls-g2-mul 5 G2-GEN)      (g2-mul 5 G2-GEN))
              (equal? (native-bls-map-g1 5)             (map-fp-to-g1 5))
              (equal? (native-bls-map-g2 (list 3 7))   (map-fp2-to-g2 (list 3 7)))
              (eq? #t (native-bls-pairing-check (list (cons G1-GEN G2-GEN) (cons ng1 G2-GEN))))
              (eq? #f (native-bls-pairing-check (list (cons G1-GEN G2-GEN))))))))

(when (bls-native-ok?)
  (set! g1-add native-bls-g1-add)
  (set! g1-mul native-bls-g1-mul)
  (set! g2-add native-bls-g2-add)
  (set! g2-mul native-bls-g2-mul)
  (set! bls-pairing-check native-bls-pairing-check)
  (set! map-fp-to-g1 native-bls-map-g1)
  (set! map-fp2-to-g2 native-bls-map-g2))
