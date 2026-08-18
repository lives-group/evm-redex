#lang racket/base

;; ======================================================================
;; M13 — optional native crypto accelerators (FFI) with a pure-Racket
;; fallback and an ORACLE self-test gate.
;;
;; Pure-Racket Keccak / secp256k1 / BLS are correct but slow; on large
;; corpora the crypto dominates.  This module tries to bind fast native
;; libraries (libkeccak, libsecp256k1) at load time.  Crucially, an
;; accelerator is installed ONLY if it reproduces a hard-coded known-answer
;; vector — so a missing library, a wrong symbol, or an ABI mismatch can
;; never corrupt results; it just leaves the accelerator at #f and the
;; caller keeps using the pure implementation (the "oracle" of record).
;;
;; This module deliberately has NO evm dependencies (so keccak.rkt et al.
;; may require it without a cycle), and every FFI attempt is wrapped so any
;; failure degrades to #f.  When no native library is present (the common
;; case), `native-status` reports 'pure for everything and behaviour is
;; byte-identical to the pure build.
;; ======================================================================

(require ffi/unsafe
         ffi/unsafe/define)

(provide native-keccak256          ; (or/c #f (bytes -> bytes))   ; bytes -> 32 bytes
         native-secp256k1-recover  ; (or/c #f (h v r s -> (cons x y) | #f))
         native-modular-expt       ; (or/c #f (base e m -> natural))  ; GMP mpz_powm
         native-bls-g1-add native-bls-g1-mul     ; BLS12-381 via blst (or #f)
         native-bls-g2-add native-bls-g2-mul
         native-bls-pairing-check
         native-bls-map-g1 native-bls-map-g2     ; map-to-curve (EIP-2537 0x10/0x11)
         native-bn-g1-add native-bn-g1-mul       ; bn254/alt_bn128 via mcl (or #f)
         native-bn-pairing-check
         native-status)            ; assoc list: (accelerator . 'native|'pure)

;; --- helpers ------------------------------------------------------------
(define (try thunk) (with-handlers ([(lambda (_) #t) (lambda (_) #f)]) (thunk)))

;; load a shared lib by any of several names; #f if none resolve
(define (load-lib . names)
  (for/or ([n (in-list names)]) (try (lambda () (ffi-lib n)))))

(define (hex->bytes s)
  (list->bytes
   (for/list ([i (in-range 0 (string-length s) 2)])
     (string->number (substring s i (+ i 2)) 16))))

;; install `fn` only if it maps `oracle-in` to `oracle-out` (known answer);
;; returns fn or #f.
(define (oracle-gate fn oracle-in oracle-out)
  (and fn
       (let ([got (try (lambda () (fn oracle-in)))])
         (and got (equal? got oracle-out) fn))))

;; ======================================================================
;; Keccak-256 via a native library (best effort).
;; Candidate: libkeccak's simple one-shot hashing is non-trivial to bind
;; blindly, so we probe a couple of common entry points and gate on the
;; empty-string vector.  Absent a match this stays #f (pure fallback).
;; ======================================================================
(define KECCAK-EMPTY
  (hex->bytes "c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"))

(define native-keccak256
  (let ()
    (define lib (load-lib "libkeccak" "libkeccak.so" "libethkeccak"))
    (define raw
      (and lib
           (try
            (lambda ()
              ;; expected C ABI: void eth_keccak256(const uint8_t* in, size_t len, uint8_t out[32])
              (define f (get-ffi-obj "eth_keccak256" lib
                                     (_fun _pointer _size _pointer -> _void)))
              (lambda (msg)
                (define out (make-bytes 32))
                (f msg (bytes-length msg) out)
                out)))))
    (oracle-gate raw #"" KECCAK-EMPTY)))

;; ======================================================================
;; secp256k1 public-key recovery via libsecp256k1 (stable C ABI).
;; Returns the recovered point (cons X Y) as integers, or #f, mirroring the
;; pure `ecrecover` up to (but not including) the final keccak/address step.
;; Gated on a known recovery vector so a wrong binding never activates.
;; ======================================================================
(define SECP256K1_CONTEXT_VERIFY #x0101)

(define native-secp256k1-recover
  (let ()
    (define lib (load-lib "libsecp256k1" "libsecp256k1.so" "libsecp256k1.so.0"
                          "libsecp256k1.so.1" "libsecp256k1.so.2"))
    (define raw
     (and lib
         (try
          (lambda ()
            (define ctx-create
              (get-ffi-obj "secp256k1_context_create" lib (_fun _uint -> _pointer)))
            (define sig-parse
              (get-ffi-obj "secp256k1_ecdsa_recoverable_signature_parse_compact" lib
                           (_fun _pointer _pointer _pointer _int -> _int)))
            (define recover
              (get-ffi-obj "secp256k1_ecdsa_recover" lib
                           (_fun _pointer _pointer _pointer _pointer -> _int)))
            (define pubkey-serialize
              (get-ffi-obj "secp256k1_ec_pubkey_serialize" lib
                           (_fun _pointer _pointer _pointer _pointer _uint -> _int)))
            (define ctx (ctx-create SECP256K1_CONTEXT_VERIFY))
            (define (int->b32 n)
              (define b (make-bytes 32 0))
              (let loop ([n n] [i 31])
                (when (>= i 0) (bytes-set! b i (bitwise-and n #xff)) (loop (arithmetic-shift n -8) (sub1 i))))
              b)
            (define fn
              (lambda (h v r s)
                (define recid (- v 27))
                (cond
                  [(not (memv recid '(0 1))) #f]
                  [else
                   (define sig (make-bytes 65 0))
                   (define compact (bytes-append (int->b32 r) (int->b32 s)))
                   (define msg (int->b32 h))
                   (cond
                     [(not (= 1 (sig-parse ctx sig compact recid))) #f]
                     [else
                      (define pub (make-bytes 64 0))
                      (cond
                        [(not (= 1 (recover ctx pub sig msg))) #f]
                        [else
                         (define out (make-bytes 65 0))
                         (define len (make-bytes 8 0))
                         (integer->integer-bytes 65 8 #f #f len 0)
                         (pubkey-serialize ctx out len pub 2)  ; 2 = UNCOMPRESSED
                         (define (b->int bs from to)
                           (for/fold ([acc 0]) ([i (in-range from to)])
                             (+ (arithmetic-shift acc 8) (bytes-ref bs i))))
                         (cons (b->int out 1 33) (b->int out 33 65))])])])))
            fn))))
    ;; oracle gate: a known recovery vector (sig of key d=1, nonce k=2 → point G)
    (and raw
         (equal?
          (try (lambda () (raw #x0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef
                               27
                               #xc6047f9441ed7d6d3045406e95c07cd85c778e4b8cef3ca7abac09b95c709ee5
                               #x6393e27de5cca5ae18b442eb0fb62563aecd69d98b4d854b5667a790730e366a)))
          (cons #x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798
                #x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8))
         raw)))

;; ======================================================================
;; GMP mpz_powm for big modular exponentiation (modexp precompile 0x05).
;; libgmp is a Racket dependency, so it is normally present.  This is ~10x
;; faster than the pure square-and-multiply for multi-thousand-bit operands
;; (Montgomery + sliding window + sub-quadratic mulmod).  We marshal via
;; base-16 strings (robust and simple).  Gated against a local pure reference
;; on several vectors — including a large one — so a marshalling bug disables it.
;; ======================================================================

;; local reference (native.rkt has no evm deps, so it carries its own oracle)
(define (ref-modexp base e m)
  (let loop ([b (modulo base m)] [e e] [acc 1])
    (cond [(zero? e) (modulo acc m)]
          [(odd? e) (loop (modulo (* b b) m) (quotient e 2) (modulo (* acc b) m))]
          [else     (loop (modulo (* b b) m) (quotient e 2) acc)])))

(define native-modular-expt
  (let ()
    (define gmp (load-lib "libgmp" "libgmp.so" "libgmp.so.10" "libgmp.so.10.5.0"))
    (define raw
      (and gmp
           (try
            (lambda ()
              (define (obj name sig) (get-ffi-obj name gmp sig))
              (define z-init  (obj "__gmpz_init"       (_fun _pointer -> _void)))
              (define z-clear (obj "__gmpz_clear"      (_fun _pointer -> _void)))
              (define z-sets  (obj "__gmpz_set_str"    (_fun _pointer _string _int -> _int)))
              (define z-gets  (obj "__gmpz_get_str"    (_fun _pointer _int _pointer -> _pointer)))
              (define z-powm  (obj "__gmpz_powm"       (_fun _pointer _pointer _pointer _pointer -> _void)))
              (define z-size  (obj "__gmpz_sizeinbase" (_fun _pointer _int -> _size)))
              (define (mpz) (define p (malloc 16 'atomic-interior)) (z-init p) p)
              (lambda (base e m)
                (cond
                  [(zero? m) 0]
                  [else
                   (define zb (mpz)) (define ze (mpz)) (define zm (mpz)) (define zr (mpz))
                   (z-sets zb (number->string base 16) 16)
                   (z-sets ze (number->string e 16) 16)
                   (z-sets zm (number->string m 16) 16)
                   (z-powm zr zb ze zm)
                   (define buf (malloc (+ 2 (z-size zr 16)) 'atomic))
                   (z-gets buf 16 zr)
                   (define r (string->number (cast buf _pointer _string) 16))
                   (z-clear zb) (z-clear ze) (z-clear zm) (z-clear zr)
                   (or r 0)]))))))
    ;; oracle gate: small + large known-answer vectors vs the local reference
    (and raw
         (let ([b1 (- (expt 2 2048) 17)] [e1 (- (expt 2 2048) 5)] [m1 (+ (expt 2 2048) 3)])
           (and (equal? (try (lambda () (raw 3 7 5)))            (ref-modexp 3 7 5))
                (equal? (try (lambda () (raw 2 10 1000)))        (ref-modexp 2 10 1000))
                (equal? (try (lambda () (raw 0 0 7)))            (ref-modexp 0 0 7))
                (equal? (try (lambda () (raw b1 e1 m1)))         (ref-modexp b1 e1 m1))
                raw)))))

;; ======================================================================
;; BLS12-381 (EIP-2537 precompiles 0x0b–0x11 + KZG 0x0a) via blst.
;; Points use the SAME representation as private/bls.rkt: 'inf, or G1 as
;; (cons (list x) (list y)) and G2 as (cons (list x0 x1) (list y0 y1)), with
;; coordinates as integers mod p.  These are RAW accelerators (no internal
;; oracle) — bls.rkt self-tests them against its pure implementation on the
;; generator before adopting them, so correctness is gated there.
;;
;; NOTE: nixpkgs ships blst as a static archive only; build a shared object
;;   cc -shared -o libblst.so -Wl,--whole-archive $blst/lib/libblst.a \
;;      -Wl,--no-whole-archive
;; and put it on LD_LIBRARY_PATH.  blst is BLS12-381 ONLY (not bn254/alt_bn128,
;; which is the 0x08 ecpairing precompile — that stays pure).
;; ======================================================================
(define (bls-int->be48 n)
  (define b (make-bytes 48 0))
  (let loop ([n n] [i 47])
    (when (>= i 0) (bytes-set! b i (bitwise-and n #xff)) (loop (arithmetic-shift n -8) (sub1 i))))
  b)
(define (bls-be48->int b off)
  (for/fold ([a 0]) ([i (in-range 48)]) (+ (* a 256) (bytes-ref b (+ off i)))))

(define-values (native-bls-g1-add native-bls-g1-mul
                native-bls-g2-add native-bls-g2-mul native-bls-pairing-check
                native-bls-map-g1 native-bls-map-g2)
  (let ([blst (load-lib "libblst" "libblst.so" "libblst.so.0")])
    (if (not blst)
        (values #f #f #f #f #f #f #f)
        (with-handlers ([(lambda (_) #t) (lambda (_) (values #f #f #f #f #f #f #f))])
          (define (o n s) (get-ffi-obj n blst s))
          (define fp<-be   (o "blst_fp_from_bendian"          (_fun _pointer _pointer -> _void)))
          (define be<-fp   (o "blst_bendian_from_fp"          (_fun _pointer _pointer -> _void)))
          (define p1<-aff  (o "blst_p1_from_affine"           (_fun _pointer _pointer -> _void)))
          (define p1->aff  (o "blst_p1_to_affine"             (_fun _pointer _pointer -> _void)))
          (define p1-add   (o "blst_p1_add_or_double_affine"  (_fun _pointer _pointer _pointer -> _void)))
          (define p1-mult  (o "blst_p1_mult"                  (_fun _pointer _pointer _pointer _size -> _void)))
          (define p1-inf?  (o "blst_p1_affine_is_inf"         (_fun _pointer -> _bool)))
          (define p2<-aff  (o "blst_p2_from_affine"           (_fun _pointer _pointer -> _void)))
          (define p2->aff  (o "blst_p2_to_affine"             (_fun _pointer _pointer -> _void)))
          (define p2-add   (o "blst_p2_add_or_double_affine"  (_fun _pointer _pointer _pointer -> _void)))
          (define p2-mult  (o "blst_p2_mult"                  (_fun _pointer _pointer _pointer _size -> _void)))
          (define p2-inf?  (o "blst_p2_affine_is_inf"         (_fun _pointer -> _bool)))
          (define miller   (o "blst_miller_loop"              (_fun _pointer _pointer _pointer -> _void)))
          (define finalexp (o "blst_final_exp"                (_fun _pointer _pointer -> _void)))
          (define fp12-mul (o "blst_fp12_mul"                 (_fun _pointer _pointer _pointer -> _void)))
          (define fp12-1?  (o "blst_fp12_is_one"              (_fun _pointer -> _bool)))
          (define fp12-one (o "blst_fp12_one"                 (_fun -> _pointer)))
          (define map->g1  (o "blst_map_to_g1"                (_fun _pointer _pointer _pointer -> _void)))
          (define map->g2  (o "blst_map_to_g2"                (_fun _pointer _pointer _pointer -> _void)))
          ;; scalar -> little-endian bytes + bit count for pN_mult
          (define (k->le k)
            (define nb (max 1 (quotient (+ (integer-length k) 7) 8)))
            (define sc (make-bytes nb 0))
            (let loop ([n k] [i 0]) (when (< i nb) (bytes-set! sc i (bitwise-and n #xff)) (loop (arithmetic-shift n -8) (add1 i))))
            (values sc (integer-length k)))
          ;; G1 affine buffer (96) <-> point
          (define (g1->aff pt)
            (define a (malloc 96 'atomic))
            (fp<-be a (bls-int->be48 (car (car pt))))
            (fp<-be (ptr-add a 48) (bls-int->be48 (car (cdr pt))))
            a)
          (define (aff->g1 a)
            (if (p1-inf? a) 'inf
                (let ([xb (make-bytes 48)] [yb (make-bytes 48)])
                  (be<-fp xb a) (be<-fp yb (ptr-add a 48))
                  (cons (list (bls-be48->int xb 0)) (list (bls-be48->int yb 0))))))
          ;; G2 affine buffer (192): x.c0 x.c1 y.c0 y.c1
          (define (g2->aff pt)
            (define a (malloc 192 'atomic))
            (define x (car pt)) (define y (cdr pt))
            (fp<-be a (bls-int->be48 (car x)))          (fp<-be (ptr-add a 48)  (bls-int->be48 (cadr x)))
            (fp<-be (ptr-add a 96) (bls-int->be48 (car y))) (fp<-be (ptr-add a 144) (bls-int->be48 (cadr y)))
            a)
          (define (aff->g2 a)
            (if (p2-inf? a) 'inf
                (let ([x0 (make-bytes 48)] [x1 (make-bytes 48)] [y0 (make-bytes 48)] [y1 (make-bytes 48)])
                  (be<-fp x0 a) (be<-fp x1 (ptr-add a 48)) (be<-fp y0 (ptr-add a 96)) (be<-fp y1 (ptr-add a 144))
                  (cons (list (bls-be48->int x0 0) (bls-be48->int x1 0))
                        (list (bls-be48->int y0 0) (bls-be48->int y1 0))))))
          (define (g1add a b)
            (cond [(eq? a 'inf) b] [(eq? b 'inf) a]
                  [else (define A (g1->aff a)) (define B (g1->aff b))
                        (define Aj (malloc 144 'atomic)) (define oj (malloc 144 'atomic)) (define oa (malloc 96 'atomic))
                        (p1<-aff Aj A) (p1-add oj Aj B) (p1->aff oa oj) (aff->g1 oa)]))
          (define (g1mul k a)
            (cond [(or (eq? a 'inf) (zero? k)) 'inf]
                  [else (define A (g1->aff a)) (define Aj (malloc 144 'atomic)) (p1<-aff Aj A)
                        (define-values (sc nbits) (k->le k))
                        (define oj (malloc 144 'atomic)) (define oa (malloc 96 'atomic))
                        (p1-mult oj Aj sc nbits) (p1->aff oa oj) (aff->g1 oa)]))
          (define (g2add a b)
            (cond [(eq? a 'inf) b] [(eq? b 'inf) a]
                  [else (define A (g2->aff a)) (define B (g2->aff b))
                        (define Aj (malloc 288 'atomic)) (define oj (malloc 288 'atomic)) (define oa (malloc 192 'atomic))
                        (p2<-aff Aj A) (p2-add oj Aj B) (p2->aff oa oj) (aff->g2 oa)]))
          (define (g2mul k a)
            (cond [(or (eq? a 'inf) (zero? k)) 'inf]
                  [else (define A (g2->aff a)) (define Aj (malloc 288 'atomic)) (p2<-aff Aj A)
                        (define-values (sc nbits) (k->le k))
                        (define oj (malloc 288 'atomic)) (define oa (malloc 192 'atomic))
                        (p2-mult oj Aj sc nbits) (p2->aff oa oj) (aff->g2 oa)]))
          (define (pairing-check pairs)
            (define acc (malloc 576 'atomic)) (memcpy acc (fp12-one) 576)
            (for ([pr (in-list pairs)])
              (unless (or (eq? (car pr) 'inf) (eq? (cdr pr) 'inf))
                (define tmp (malloc 576 'atomic))
                (miller tmp (g2->aff (cdr pr)) (g1->aff (car pr)))
                (fp12-mul acc acc tmp)))
            (finalexp acc acc)
            (and (fp12-1? acc) #t))
          ;; map-to-curve (EIP-2537 0x10/0x11): single field element -> point,
          ;; incl. isogeny + cofactor clear (v=NULL selects the 1-element variant)
          (define (map-g1 u)   ; u = integer in [0,p)
            (define fu (malloc 48 'atomic)) (fp<-be fu (bls-int->be48 u))
            (define oj (malloc 144 'atomic)) (define oa (malloc 96 'atomic))
            (map->g1 oj fu #f) (p1->aff oa oj) (aff->g1 oa))
          (define (map-g2 u)   ; u = (list c0 c1)
            (define fu (malloc 96 'atomic))
            (fp<-be fu (bls-int->be48 (car u))) (fp<-be (ptr-add fu 48) (bls-int->be48 (cadr u)))
            (define oj (malloc 288 'atomic)) (define oa (malloc 192 'atomic))
            (map->g2 oj fu #f) (p2->aff oa oj) (aff->g2 oa))
          (values g1add g1mul g2add g2mul pairing-check map-g1 map-g2)))))

;; ======================================================================
;; bn254 / alt_bn128 (ecadd 0x06, ecmul 0x07, ecpairing 0x08) via herumi/mcl.
;; blst does NOT cover this curve; mcl's MCL_BN_SNARK1 is exactly the Ethereum
;; bn254.  Build libmclbn384_256.so with mcl.nix (not in nixpkgs).  Points match
;; ec.rkt / pairing.rkt: G1 as (cons x y) or #f (add/mul) / 'inf (pairing); G2 as
;; (cons (list x-re x-im) (list y-re y-im)) or 'inf.  RAW (no internal oracle) —
;; pairing.rkt self-tests against its pure ops before adopting them.
;; ======================================================================
(define BN254-R 21888242871839275222246405745257275088548364400416034343698204186575808495617)

(define-values (native-bn-g1-add native-bn-g1-mul native-bn-pairing-check)
  (let ([mcl (load-lib "libmclbn384_256" "libmclbn384_256.so" "libmclbn384_256.so.1")])
    (if (not mcl)
        (values #f #f #f)
        (with-handlers ([(lambda (_) #t) (lambda (_) (values #f #f #f))])
          (define (o n s) (get-ffi-obj n mcl s))
          (define bn-init (o "mclBn_init" (_fun _int _int -> _int)))
          ;; MCL_BN_SNARK1 = 4; compiled-time var for the 384_256 lib = 46
          (unless (= 0 (bn-init 4 46)) (error "mclBn_init failed"))
          (define g1-set  (o "mclBnG1_setStr"  (_fun _pointer _string _size _int -> _int)))
          (define g1-get  (o "mclBnG1_getStr"  (_fun _pointer _size _pointer _int -> _size)))
          (define g1-addf (o "mclBnG1_add"     (_fun _pointer _pointer _pointer -> _void)))
          (define g1-mulf (o "mclBnG1_mul"     (_fun _pointer _pointer _pointer -> _void)))
          (define fr-set  (o "mclBnFr_setStr"  (_fun _pointer _string _size _int -> _int)))
          (define g2-set  (o "mclBnG2_setStr"  (_fun _pointer _string _size _int -> _int)))
          (define miller  (o "mclBn_millerLoop"(_fun _pointer _pointer _pointer -> _void)))
          (define gt-mul  (o "mclBnGT_mul"     (_fun _pointer _pointer _pointer -> _void)))
          (define gt-set1 (o "mclBnGT_setInt"  (_fun _pointer _int64 -> _void)))
          (define final   (o "mclBn_finalExp"  (_fun _pointer _pointer -> _void)))
          (define gt-1?   (o "mclBnGT_isOne"   (_fun _pointer -> _int)))
          (define (hx n) (number->string n 16))
          (define (mk-g1 pt)            ; pt = (cons x y) or #f/'inf
            (define g (malloc 144 'atomic))
            (if (or (not pt) (eq? pt 'inf)) (g1-set g "0" 1 16)
                (let ([s (format "1 ~a ~a" (hx (car pt)) (hx (cdr pt)))]) (g1-set g s (string-length s) 16)))
            g)
          (define (g1->pt g)
            (define buf (make-bytes 400)) (define n (g1-get buf 400 g 16))
            (define s (bytes->string/utf-8 (subbytes buf 0 n)))
            (if (string=? s "0") #f
                (let ([ps (cdr (regexp-split #px" +" s))])
                  (cons (string->number (car ps) 16) (string->number (cadr ps) 16)))))
          (define (mk-fr k)
            (define f (malloc 32 'atomic)) (define s (hx (modulo k BN254-R)))
            (fr-set f s (string-length s) 16) f)
          (define (mk-g2 pt)           ; pt = (cons (list xr xi)(list yr yi)) or 'inf
            (define g (malloc 288 'atomic))
            (if (eq? pt 'inf) (g2-set g "0" 1 16)
                (let ([s (format "1 ~a ~a ~a ~a" (hx (caar pt)) (hx (cadar pt)) (hx (cadr pt)) (hx (caddr pt)))])
                  (g2-set g s (string-length s) 16)))
            g)
          (define (bn-g1-add a b)      ; a,b = (cons x y) or #f
            (define r (malloc 144 'atomic)) (g1-addf r (mk-g1 a) (mk-g1 b)) (g1->pt r))
          (define (bn-g1-mul k a)
            (if (or (not a) (zero? (modulo k BN254-R))) #f
                (let ([r (malloc 144 'atomic)]) (g1-mulf r (mk-g1 a) (mk-fr k)) (g1->pt r))))
          (define (bn-pairing-check pairs)
            (define acc (malloc 576 'atomic)) (gt-set1 acc 1)
            (for ([pr (in-list pairs)])
              (unless (or (eq? (car pr) 'inf) (eq? (cdr pr) 'inf))
                (define t (malloc 576 'atomic))
                (miller t (mk-g1 (car pr)) (mk-g2 (cdr pr)))
                (gt-mul acc acc t)))
            (final acc acc)
            (= 1 (gt-1? acc)))
          (values bn-g1-add bn-g1-mul bn-pairing-check)))))

;; ======================================================================
;; Status (for the runner banner / a degradation unit test).
;; ======================================================================
(define native-status
  (list (cons 'keccak256 (if native-keccak256 'native 'pure))
        (cons 'secp256k1-recover (if native-secp256k1-recover 'native 'pure))
        (cons 'modular-expt (if native-modular-expt 'native 'pure))
        (cons 'blst (if native-bls-g1-add 'native 'pure))
        (cons 'mcl-bn254 (if native-bn-g1-add 'native 'pure))))
