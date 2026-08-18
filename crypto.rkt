#lang racket/base

;; evm-redex/crypto — the low-level elliptic-curve and finite-field primitives
;; underlying the EVM precompiles (bn254/alt_bn128 and BLS12-381).
;;
;;   (require evm-redex/crypto)
;;
;; These are internals of the precompile layer, exposed as a stable public
;; surface so that conformance and unit tests (e.g. the crypto milestone suite)
;; can exercise the curve arithmetic directly without reaching into private/.

(require "private/ec.rkt"
         "private/bls.rkt")

(provide (all-from-out "private/ec.rkt")
         (all-from-out "private/bls.rkt"))
