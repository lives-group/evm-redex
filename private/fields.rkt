#lang racket/base

;; Generic field-extension arithmetic Fp[x]/(modulus), parameterized by the
;; prime p.  Elements are coefficient lists (low degree first, length = deg).
;; Used by both bn254 (private/pairing.rkt) and BLS12-381 (private/bls.rkt).
;;
;; Style follows py_ecc's FQP: mul = convolve + reduce; inv = extended Euclid
;; over Fp[x].  `mk-fqp` returns a field record (a list of closures); access via
;; the f-* helpers below.

(require racket/list
         "words.rkt")   ; modular-expt

(provide mk-fqp
         f-zero f-one f-from f+ f- f-neg f= f-zero? f* f-pow f-inv f-div)

;; modulus-coeffs: the `deg` non-leading coefficients m_0..m_{deg-1} of
;; x^deg = -(m_0 + m_1 x + ... + m_{deg-1} x^{deg-1}).
(define (mk-fqp p deg modulus-coeffs)
  (define mcv (list->vector modulus-coeffs))
  (define (zero) (make-list deg 0))
  (define (one) (cons 1 (make-list (sub1 deg) 0)))
  (define (from-int n) (cons (modulo n p) (make-list (sub1 deg) 0)))
  (define (add a b) (map (lambda (x y) (modulo (+ x y) p)) a b))
  (define (sub a b) (map (lambda (x y) (modulo (- x y) p)) a b))
  (define (neg a) (map (lambda (x) (modulo (- x) p)) a))
  (define (eq? a b) (equal? a b))
  (define (is-zero? a) (andmap zero? a))
  (define (mul a b)
    (define av (list->vector a)) (define bv (list->vector b))
    (define blen (sub1 (* 2 deg)))
    (define c (make-vector blen 0))
    (for* ([i (in-range deg)] [j (in-range deg)])
      (vector-set! c (+ i j) (+ (vector-ref c (+ i j)) (* (vector-ref av i) (vector-ref bv j)))))
    (let loop ([len blen])
      (cond
        [(= len deg) (for/list ([i (in-range deg)]) (modulo (vector-ref c i) p))]
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
  (define (deg-of poly)
    (let loop ([i (sub1 (length poly))])
      (cond [(< i 0) 0] [(not (zero? (list-ref poly i))) i] [else (loop (sub1 i))])))
  (define (poly-rounded-div a b)
    (define dega (deg-of a)) (define degb (deg-of b))
    (define temp (list->vector a))
    (define o (make-vector (length a) 0))
    (for ([i (in-range (- dega degb) -1 -1)])
      (define coef (modulo (* (vector-ref temp (+ degb i)) (modular-expt (list-ref b degb) (- p 2) p)) p))
      (vector-set! o i (modulo (+ (vector-ref o i) coef) p))
      (for ([c (in-range (add1 degb))])
        (vector-set! temp (+ c i) (modulo (- (vector-ref temp (+ c i)) (* coef (list-ref b c))) p))))
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
           (vector-set! nm (+ i j) (modulo (- (vector-ref nm (+ i j)) (* (list-ref lm i) (list-ref r j))) p))
           (vector-set! new (+ i j) (modulo (- (vector-ref new (+ i j)) (* (list-ref low i) (list-ref r j))) p)))
         (loop (vector->list nm) lm (vector->list new) low)]
        [else
         (define inv0 (modular-expt (car low) (- p 2) p))
         (for/list ([i (in-range deg)]) (modulo (* (list-ref lm i) inv0) p))])))
  (define (div a b) (mul a (inv b)))
  (list deg zero one from-int add sub neg eq? is-zero? mul pow inv div))

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
(define (f-inv f a) ((list-ref f 11) a))
(define (f-div f a b) ((list-ref f 12) a b))
