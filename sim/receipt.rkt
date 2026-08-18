#lang racket/base

;; ======================================================================
;; The receipt a `sim-send!` returns, and how it prints.
;;
;; status       : 'success | 'revert | 'error
;; gas-used     : gas consumed by the transaction
;; output       : the top-level return/revert bytes
;; revert-reason: a decoded string when status is 'revert, else #f
;; logs         : the raw (log addr (topic ...) data) terms
;; logs-text    : one human line per log (decoded against known ABIs when possible)
;; calls        : the call-tree forest (when trace=call was requested), else #f
;; steps        : the step list (when trace=step was requested), else #f
;; err          : an exception message when status is 'error, else #f
;; ======================================================================

(require racket/format
         (only-in "trace.rkt" print-call-tree print-steps))

(provide (struct-out receipt) print-receipt)

(struct receipt (status gas-used output revert-reason logs logs-text calls steps err) #:transparent)

(define (print-receipt r #:label [label #f])
  (when label (printf "~a\n" label))
  (when (receipt-steps r)
    (printf "  step trace:\n") (print-steps (receipt-steps r)))
  (when (receipt-calls r)
    (printf "  call trace:\n") (print-call-tree (receipt-calls r)))
  (printf "  status:    ~a~a\n" (receipt-status r)
          (if (receipt-revert-reason r) (format "  (~a)" (receipt-revert-reason r)) ""))
  (printf "  gas used:  ~a\n" (receipt-gas-used r))
  (when (receipt-err r) (printf "  error:     ~a\n" (receipt-err r)))
  (for ([lt (in-list (receipt-logs-text r))]) (printf "  log:       ~a\n" lt))
  (printf "  return:    0x~a\n" (bytes->hex (receipt-output r))))

(define (bytes->hex bs)
  (apply string-append (map (lambda (b) (define s (number->string b 16))
                              (if (= 1 (string-length s)) (string-append "0" s) s)) bs)))
