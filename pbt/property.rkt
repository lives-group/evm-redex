#lang racket/base

;; ======================================================================
;; The property DSL.
;;
;; `define-evm-property` describes a Hoare-style contract over a FIXED program
;; (the code is the subject; the environment/inputs are generated) and expands
;; to a rackcheck `property`.  Supported clauses:
;;
;;   #:code CODE                fragment mode (routine of opcodes)
;;   #:call CALL                transaction mode (a txn built with make-tx)
;;   #:contract CODE #:address A   convenience: install CODE at A (txn mode)
;;   #:given ([x gen] ...)      generated inputs (concrete leaves)
;;   #:stack / #:gas / #:memory / #:world / #:msg / #:orig-storage   (fragment)
;;   #:world / #:block          (transaction)
;;   #:pre  EXPR                precondition on the inputs (Hoare antecedent)
;;   #:post (lambda (m0 m1) ...)          / (lambda (w0 w1 result) ...)
;;   #:invariant (lambda (m) ...)         per-step (fragment) / boundary (txn)
;;   #:revert-when EXPR         obligation: under EXPR the run must revert
;;   #:succeed-when EXPR        obligation: under EXPR the run must succeed
;;   #:fuel / #:trials / #:seed
;;
;; Semantics is implication: when #:pre holds, the run must satisfy every given
;; obligation; when #:pre is false the trial passes vacuously.
;; ======================================================================

(require (for-syntax racket/base syntax/parse)
         rackcheck
         ;; functional runner (rackcheck's public API is rackunit-only; this
         ;; internal submodule exposes `check` returning a result we can inspect)
         (submod rackcheck/prop private)
         "execute.rkt"
         (only-in "vocab.rkt" reverted? stopped? returned?))

(provide define-evm-property
         check-evm-property        ; rackunit-integrated (use inside test suites)
         run-evm-property          ; functional: -> evm-result
         evm-property-holds?       ; functional: -> boolean
         (struct-out evm-result)
         (struct-out evm-property)
         install-contract DEFAULT-BLOCK)

(struct evm-property (prop trials seed) #:transparent)

(define (succeeded-frag? m) (or (stopped? m) (returned? m)))

(define DEFAULT-BLOCK '(block 0 0 0 0 0 0 30000000 1 Prague))

(define (install-contract code addr [balance 0])
  (list (list addr (list 'account 0 balance code '() '()))))

;; --- obligation evaluators (runtime) ----------------------------------
;; pre : boolean (#t when clause absent)
;; post: procedure or #f ; inv: procedure or #f
;; rev/suc: the condition value or #f (obligation only when truthy)
(define (eval-frag r pre post inv rev suc)
  (define m0 (frag-run-pre r))
  (define m1 (frag-run-post r))
  (or (not pre)
      (and (or (not post) (and (post m0 m1) #t))
           (or (not inv)
               (andmap (lambda (m) (and (inv m) #t))
                       (or (frag-run-trace r) (list m1))))
           (or (not rev) (reverted? m1))
           (or (not suc) (succeeded-frag? m1)))))

(define (eval-txn r pre post inv rev suc)
  (define w0 (txn-run-world0 r))
  (define w1 (txn-run-world1 r))
  (or (not pre)
      (and (or (not post) (and (post w0 w1 r) #t))
           (or (not inv) (and (inv w0) (inv w1) #t))
           (or (not rev) (eq? (txn-run-outcome r) 'revert))
           (or (not suc) (eq? (txn-run-outcome r) 'success)))))

;; --- runners ----------------------------------------------------------
;; #:deadline is a wall-clock budget in SECONDS (default 60).  Contract-level
;; checks run the full semantics per trial (~tens of ms each), so with many
;; trials you may need to raise it; a 'timed-out result means the budget was hit
;; before all trials ran (inconclusive, not a failure).

;; rackunit-integrated: registers a check pass/failure (use inside test suites).
(define (check-evm-property p #:trials [trials #f] #:seed [seed #f] #:deadline [deadline #f])
  (check-property (config-for p trials seed deadline) (evm-property-prop p)))

;; functional: returns an evm-result you can inspect programmatically.
;;   status         : 'passed | 'falsified | 'timed-out
;;   counterexample : the shrunk list of generated argument values, or #f
(struct evm-result (status counterexample tests) #:transparent)

(define (run-evm-property p #:trials [trials #f] #:seed [seed #f] #:deadline [deadline #f])
  (define res (check (config-for p trials seed deadline) (evm-property-prop p)))
  (define st (result-status res))
  (evm-result st (and (eq? st 'falsified) (result-args/smallest res))
              (or trials (evm-property-trials p))))

(define (evm-property-holds? p #:trials [trials #f] #:seed [seed #f] #:deadline [deadline #f])
  (eq? (evm-result-status (run-evm-property p #:trials trials #:seed seed #:deadline deadline))
       'passed))

(define (config-for p trials seed deadline)
  (define t (or trials (evm-property-trials p)))
  (define s (or seed (evm-property-seed p)))
  (define dl (+ (current-inexact-milliseconds) (* (or deadline 60) 1000)))
  (if s (make-config #:tests t #:seed s #:deadline dl)
        (make-config #:tests t #:deadline dl)))

;; --- the macro --------------------------------------------------------
(define-syntax (define-evm-property stx)
  (syntax-parse stx
    [(_ name:id
        (~alt
         (~optional (~seq #:code code:expr))
         (~optional (~seq #:contract contract:expr))
         (~optional (~seq #:address address:expr))
         (~once (~seq #:given ([gid:id gg:expr] ...)))
         (~optional (~seq #:stack stack:expr))
         (~optional (~seq #:gas gas:expr))
         (~optional (~seq #:memory memory:expr))
         (~optional (~seq #:world world:expr))
         (~optional (~seq #:msg msg:expr))
         (~optional (~seq #:orig-storage orig:expr))
         (~optional (~seq #:call call:expr))
         (~optional (~seq #:block block:expr))
         (~optional (~seq #:pre pre:expr))
         (~optional (~seq #:post post:expr))
         (~optional (~seq #:invariant inv:expr))
         (~optional (~seq #:revert-when revw:expr))
         (~optional (~seq #:succeed-when sucw:expr))
         (~optional (~seq #:fuel fuel:expr))
         (~optional (~seq #:trials trials:expr))
         (~optional (~seq #:seed seed:expr)))
        ...)
     (define trace? (and (attribute inv) #t))
     (cond
       [(attribute code)
        #`(define name
            (evm-property
             (property name ([gid gg] ...)
               (eval-frag
                (run-fragment code
                              #:stack (~? stack '())
                              #:gas   (~? gas 1000000)
                              #:memory (~? memory '())
                              #:world (~? world '())
                              #:msg (~? msg #f)
                              #:orig-storage (~? orig '())
                              #:fuel (~? fuel 1000000)
                              #:trace? #,trace?)
                (~? pre #t) (~? post #f) (~? inv #f) (~? revw #f) (~? sucw #f)))
             (~? trials 1000) (~? seed #f)))]
       [(attribute call)
        #`(define name
            (evm-property
             (property name ([gid gg] ...)
               (eval-txn
                (run-txn call
                         (~? world (install-contract (~? contract '()) (~? address 0)))
                         (~? block DEFAULT-BLOCK))
                (~? pre #t) (~? post #f) (~? inv #f) (~? revw #f) (~? sucw #f)))
             (~? trials 1000) (~? seed #f)))]
       [else
        (raise-syntax-error 'define-evm-property
                            "need #:code (fragment mode) or #:call (transaction mode)" stx)])]))
