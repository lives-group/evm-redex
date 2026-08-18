#lang racket/base

;; evm-redex/sim — the transaction simulator, programmatic surface.
;;
;;   (require evm-redex/sim)   ; the simulator engine as functions
;;   #lang evm-redex/sim       ; the scenario language (see sim/lang/reader.rkt)
;;
;; A module path `evm-redex/sim` resolves to this file; `#lang evm-redex/sim`
;; resolves the reader at `sim/lang/reader.rkt`.  Both drive the one engine
;; (sim/engine.rkt).

(require "sim/engine.rkt" "sim/receipt.rkt" "sim/abi.rkt" "sim/state-root.rkt")

(provide (all-from-out "sim/engine.rkt")
         (all-from-out "sim/receipt.rkt")
         ;; ABI conveniences (decode a return, a revert reason, an event log)
         decode-return decode-revert
         ;; state commitments
         state-root storage-root logs-hash)
