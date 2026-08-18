#lang racket/base

;; ======================================================================
;; Execution glue for property-based testing.
;;
;; Two entry points build an initial state from (fixed code + generated
;; environment), run the existing concrete interpreter, and classify the
;; outcome uniformly — folding the machine's `halt` field together with any
;; Racket-level exception (fuel exhaustion, invalid transaction) into a single
;; outcome symbol so property predicates can talk about revert / success /
;; exceptions without caring how they arose.
;;
;; Every run resets the interpreter's transaction-level mutable boxes so trials
;; are independent.
;; ======================================================================

(require racket/list
         (only-in "../private/interpreter.rkt"
                  fresh-machine run run-trace machine-field machine-halt reset-tx-state!
                  run-top-frame resolve-code orig-storage-for current-exec-backend)
         (only-in "../private/transaction.rkt" process-transaction))

(provide (struct-out frag-run) (struct-out txn-run) (struct-out call-result)
         run-fragment run-txn call
         machine-with-field CALL-BLOCK)

;; Property-based testing runs thousands of trials, so it always uses the fast
;; Racket executor (exec-racket) rather than the Redex `exec` reference: every
;; entry point below pins `current-exec-backend` to 'racket, independent of the
;; EVM_EXEC_BACKEND environment variable.  (Set EVM_ORACLE=1 to still run the
;; per-step differential oracle underneath, e.g. to differentially validate a
;; property against the spec.)
(define-syntax-rule (with-fast-executor body ...)
  (parameterize ([current-exec-backend 'racket]) body ...))

;; A fragment run: pre/post are machine terms; `trace` is the list of machines
;; (when #:trace? was set) else #f; `outcome` is a symbol; `err` is an
;; exception message string or #f.
(struct frag-run (pre post trace outcome err) #:transparent)

;; A transaction run: world0/world1 are Worlds; `ok?` is the success flag;
;; `outcome` is 'success | 'revert | 'error; `err` is a message or #f.
(struct txn-run (world0 world1 ok? gas-used logs outcome err) #:transparent)

;; --- functional field override on a machine term ----------------------
(define (machine-with-field m tag . args)
  (cons 'machine
        (for/list ([f (in-list (cdr m))])
          (if (eq? (car f) tag) (cons tag args) f))))

(define (mfield m tag) (car (machine-field m tag)))

;; halt-tag : the leading symbol of the halt field (running | stop | return |
;; revert | out-of-gas | stack-underflow | ...).
(define (halt-tag m)
  (define h (machine-halt m))
  (if (pair? h) (car h) h))

;; ======================================================================
;; run-fragment : run a fixed code fragment from a generated initial state.
;; Returns a frag-run.  Uses run-trace when #:trace? so per-step invariants
;; can be checked.
;; ======================================================================
(define (run-fragment code
                      #:stack [stack '()]
                      #:gas [gas 1000000]
                      #:memory [memory '()]
                      #:world [world '()]
                      #:msg [msg #f]
                      #:block [block #f]
                      #:tx [tx #f]
                      #:orig-storage [orig '()]
                      #:fuel [fuel 1000000]
                      #:trace? [trace? #f])
  (define base (fresh-machine code gas #:world world #:orig-storage orig))
  (define pre
    (for/fold ([m base])
              ([kv (in-list (append (list (list 'stack stack) (list 'memory memory))
                                    (if msg   (list (list 'msg msg)) '())
                                    (if block (list (list 'block block)) '())
                                    (if tx    (list (list 'tx tx)) '())))])
      (apply machine-with-field m kv)))
  (reset-tx-state! world)
  (with-fast-executor
   (with-handlers ([exn:fail? (lambda (e) (frag-run pre pre #f 'error (exn-message e)))])
     (cond
       [trace?
        (define tr (run-trace pre #:fuel fuel))
        (define post (last tr))
        (frag-run pre post tr (halt-tag post) #f)]
       [else
        (define post (run pre #:fuel fuel))
        (frag-run pre post #f (halt-tag post) #f)]))))

;; ======================================================================
;; run-txn : run a whole transaction against a world holding the fixed
;; contract.  Wraps process-transaction, which raises on an invalid tx.
;; ======================================================================
(define (run-txn tx world block)
  (reset-tx-state! world)
  (with-fast-executor
   (with-handlers ([exn:fail? (lambda (e) (txn-run world world #f 0 '() 'error (exn-message e)))])
     (define-values (ok? gas-used post logs) (process-transaction tx world block))
     (txn-run world post ok? gas-used logs (if ok? 'success 'revert) #f))))

;; ======================================================================
;; call : run a message call to `addr` at the FRAME level (no tx validation /
;; gas charging), exposing the RETURN data so getters / view functions can be
;; read.  Returns a call-result.
;;   outcome   : 'return | 'stop | 'revert | 'out-of-gas | ... | 'error
;;   return    : the returned/reverted bytes (list), or '()
;;   world     : the post-call world (unchanged on staticcall / revert paths)
;;   gas-left  : remaining gas
;;   logs      : the logs emitted by the call ('() unless it succeeded — a
;;               reverted call emits none)
;; ======================================================================
(struct call-result (outcome return world gas-left err logs) #:transparent)

(define CALL-BLOCK '(block 0 0 0 0 0 0 30000000 1 Prague))
(define DEFAULT-CALLER #x00000000000000000000000000000000CA11E401)

(define (call world addr
              #:from   [from DEFAULT-CALLER]
              #:value  [value 0]
              #:data   [data '()]
              #:gas    [gas 30000000]
              #:block  [block CALL-BLOCK]
              #:static [static? #f])
  (reset-tx-state! world)
  (with-fast-executor
   (with-handlers ([exn:fail? (lambda (e) (call-result 'error '() world 0 (exn-message e) '()))])
     (define done
       (run-top-frame #:caller from #:target addr #:value value #:data data
                      #:code (resolve-code world addr) #:code-addr addr #:gas gas
                      #:world world #:accessed (list (list from addr) '())
                      #:block block #:tx (list 'tx from 0 '()) #:depth 0
                      #:static static? #:orig (orig-storage-for addr world)))
     (define h (machine-halt done))
     (define tag (if (pair? h) (car h) h))
     (define out (if (and (pair? h) (memq (car h) '(return revert))) (cadr h) '()))
     (call-result tag out (car (machine-field done 'world)) (car (machine-field done 'gas)) #f
                  ;; a call that did not succeed emits no logs
                  (if (memq tag '(return stop)) (car (machine-field done 'logs)) '())))))
