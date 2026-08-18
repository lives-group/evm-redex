#lang racket/base

;; Elliptic-curve arithmetic for the ecrecover (secp256k1) and ecadd/ecmul
;; (alt_bn128 / bn254) precompiles.  Affine coordinates; the point at infinity
;; is #f.  Field/curve are passed explicitly as (p a b).

(require "words.rkt")   ; modular-expt

(provide (all-defined-out))

;; modular inverse via Fermat's little theorem (p prime)
(define (modinv a p) (modular-expt (modulo a p) (- p 2) p))

;; square root for p ≡ 3 (mod 4): a^((p+1)/4)
(define (mod-sqrt a p) (modular-expt a (quotient (+ p 1) 4) p))

;; point = (cons x y) or #f (infinity)
(define (pt-double P p a)
  (cond
    [(not P) #f]
    [(zero? (cdr P)) #f]
    [else
     (define x1 (car P)) (define y1 (cdr P))
     (define lam (modulo (* (+ (* 3 x1 x1) a) (modinv (* 2 y1) p)) p))
     (define x3 (modulo (- (* lam lam) (* 2 x1)) p))
     (cons x3 (modulo (- (* lam (- x1 x3)) y1) p))]))

(define (pt-add P Q p a)
  (cond
    [(not P) Q]
    [(not Q) P]
    [else
     (define x1 (car P)) (define y1 (cdr P))
     (define x2 (car Q)) (define y2 (cdr Q))
     (cond
       [(and (= x1 x2) (zero? (modulo (+ y1 y2) p))) #f]      ; P = -Q
       [(and (= x1 x2) (= y1 y2)) (pt-double P p a)]
       [else
        (define lam (modulo (* (- y2 y1) (modinv (- x2 x1) p)) p))
        (define x3 (modulo (- (* lam lam) x1 x2) p))
        (cons x3 (modulo (- (* lam (- x1 x3)) y1) p))])]))

(define (pt-mul k P p a)
  (let loop ([k k] [Q P] [R #f])
    (cond [(zero? k) R]
          [(odd? k) (loop (quotient k 2) (pt-double Q p a) (pt-add R Q p a))]
          [else      (loop (quotient k 2) (pt-double Q p a) R)])))

(define (on-curve? P p a b)
  (or (not P)
      (zero? (modulo (- (* (cdr P) (cdr P))
                        (+ (* (car P) (car P) (car P)) (* a (car P)) b))
                     p))))

;; ===================== secp256k1 =====================
(define SECP-P #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F)
(define SECP-N #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141)
(define SECP-G (cons #x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
                     #x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8))

;; ===================== bn254 / alt_bn128 =====================
(define BN-P 21888242871839275222246405745257275088696311157297823662689037894645226208583)
(define BN-B 3)
