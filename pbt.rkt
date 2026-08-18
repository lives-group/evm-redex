#lang racket/base

;; evm-redex/pbt — public entry point for the property-based testing DSL.
;;
;;   (require evm-redex/pbt)
;;
;; A module path of the form `evm-redex/pbt` resolves to this file (pbt.rkt), so
;; it re-exports the DSL assembled in pbt/main.rkt (the property forms, the
;; observation vocabulary, the EVM-tuned generators, deploy/run/call, the solc
;; artifact reader, and the rackcheck combinators).

(require "pbt/main.rkt")
(provide (all-from-out "pbt/main.rkt"))
