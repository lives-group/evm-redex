#lang racket/base

;; ======================================================================
;; Trace collectors built on the interpreter's two hooks:
;;   - a CALL TREE from `current-call-observer` (enter/exit at each nested
;;     CALL/CREATE), with caller/callee/value/gas and success per node;
;;   - a STEP TRACE from `current-frame-tracer` (pc, opcode, resulting stack),
;;     tagged by depth so nested frames indent.
;; Each `make-*-collector` returns the hook procedure to install and a thunk to
;; read the collected result afterwards.
;; ======================================================================

(require racket/list
         (only-in evm-redex machine-field)
         (only-in "abi.rkt" decode-revert))

(provide (struct-out call-node)
         make-call-collector print-call-tree
         (struct-out step) make-step-collector print-steps
         opcode-mnemonic)

;; --- call tree ---------------------------------------------------------
(struct call-node (kind caller callee value gas success? gas-left output children) #:transparent #:mutable)

;; -> (values observer get-forest)
;; The top-level tx frame is not an observed call; the forest is the list of
;; calls that frame (and its descendants) made, nested.
(define (make-call-collector)
  (define roots '())                 ; completed top-level nodes, in reverse
  (define stack '())                 ; open nodes (deepest first)
  (define (observer ev . args)
    (case ev
      [(enter)
       (define-values (kind caller callee value gas depth) (apply values args))
       (set! stack (cons (call-node kind caller callee value gas #f #f '() '()) stack))]
      [(exit)
       (when (pair? stack)
         (define-values (ok? gas-left output) (apply values args))
         (define node (car stack))
         (set-call-node-success?! node ok?)
         (set-call-node-gas-left! node gas-left)
         (set-call-node-output! node output)
         (set-call-node-children! node (reverse (call-node-children node)))
         (set! stack (cdr stack))
         (if (pair? stack)
             (set-call-node-children! (car stack) (cons node (call-node-children (car stack))))
             (set! roots (cons node roots))))]))
  (values observer (lambda () (reverse roots))))

(define (print-call-tree forest #:indent [indent 0])
  (for ([n (in-list forest)])
    (printf "~a~a ~a -> ~a  value=~a gas=~a  ~a~a\n"
            (make-string (* 2 indent) #\space)
            (call-node-kind n)
            (addr->hex (call-node-caller n)) (addr->hex (call-node-callee n))
            (call-node-value n) (call-node-gas n)
            (if (call-node-success? n) "✓" "✗")
            (if (and (not (call-node-success? n)) (pair? (call-node-output n)))
                (format "  ~a" (decode-revert (call-node-output n))) ""))
    (print-call-tree (call-node-children n) #:indent (add1 indent))))

;; --- step trace --------------------------------------------------------
(struct step (depth addr pc op stack) #:transparent)

;; -> (values frame-tracer get-steps)
(define (make-step-collector)
  (define steps '())
  (define (tracer addr code kind)
    (lambda (pc op m2)
      (define depth (list-ref (car (machine-field m2 'msg)) 7))
      (set! steps (cons (step depth addr pc op (car (machine-field m2 'stack))) steps))))
  (values tracer (lambda () (reverse steps))))

(define (print-steps steps)
  (for ([s (in-list steps)])
    (printf "~a~a  ~a  stack ~a\n"
            (make-string (* 2 (step-depth s)) #\space)
            (~pc (step-pc s))
            (opcode-mnemonic (step-op s))
            (map word->hex (take-safe (step-stack s) 6)))))

;; --- helpers -----------------------------------------------------------
(define (addr->hex a) (string-append "0x" (number->string a 16)))
(define (word->hex n) (string-append "0x" (number->string n 16)))
(define (~pc n) (let ([s (number->string n)]) (string-append (make-string (max 0 (- 4 (string-length s))) #\space) s)))
(define (take-safe xs n) (if (> (length xs) n) (take xs n) xs))

;; a compact opcode name for the step trace (PUSH/DUP/SWAP by range, else table)
(define (opcode-mnemonic op)
  (cond
    [(and (>= op #x60) (<= op #x7f)) (format "PUSH~a" (- op #x5f))]
    [(and (>= op #x80) (<= op #x8f)) (format "DUP~a" (- op #x7f))]
    [(and (>= op #x90) (<= op #x9f)) (format "SWAP~a" (- op #x8f))]
    [(hash-ref OPNAMES op #f) => values]
    [else (format "0x~a" (number->string op 16))]))

(define OPNAMES
  (hasheqv #x00 "STOP" #x01 "ADD" #x02 "MUL" #x03 "SUB" #x04 "DIV" #x0a "EXP"
           #x10 "LT" #x11 "GT" #x14 "EQ" #x15 "ISZERO" #x16 "AND" #x17 "OR"
           #x20 "KECCAK256" #x33 "CALLER" #x34 "CALLVALUE" #x35 "CALLDATALOAD"
           #x36 "CALLDATASIZE" #x50 "POP" #x51 "MLOAD" #x52 "MSTORE" #x54 "SLOAD"
           #x55 "SSTORE" #x56 "JUMP" #x57 "JUMPI" #x5b "JUMPDEST" #x5f "PUSH0"
           #xf1 "CALL" #xf3 "RETURN" #xf4 "DELEGATECALL" #xfa "STATICCALL"
           #xfd "REVERT" #xff "SELFDESTRUCT"))
