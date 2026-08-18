#lang racket/base

;; ======================================================================
;; The module language for `#lang evm-redex/sim`.
;;
;; The reader hands the whole scenario source to this module as a string; our
;; `#%module-begin` parses it (sim/parse.rkt) and drives the simulator engine
;; (sim/engine.rkt) command by command against an evolving chain, printing a
;; receipt per transaction and, at the end, the state root and a diff from the
;; initial state.  `@file` code references resolve relative to the scenario file.
;; ======================================================================

(require racket/list racket/path
         (for-syntax racket/base)
         (only-in "parse.rkt" parse-scenario
                  command-head command-positionals command-fields command-src loc-line
                  code-ref? code-ref-file code-ref-contract hexval? hexval-bytes hexval-int)
         (only-in "engine.rkt"
                  make-simulator sim-account! sim-fund! sim-deploy! sim-send! sim-view
                  sim-mine! sim-withdraw! simulator-block set-simulator-block!
                  sim-address sim-state-root sim-diff)
         (only-in "receipt.rkt" print-receipt)
         (only-in "../pbt/solc.rkt" read-artifact artifact-creation artifact-abi hex->bytes))

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [sim-module-begin #%module-begin]))

(define-syntax-rule (sim-module-begin src)
  (#%module-begin
   ;; the running scenario module's own path, for @file resolution
   (define here (variable-reference->module-source (#%variable-reference)))
   (define root (if (path? here) (or (path-only here) (current-directory)) (current-directory)))
   (run-scenario src root)))

;; --- value coercion ----------------------------------------------------
(define (->int v)
  (cond [(hexval? v) (hexval-int v)] [(exact-integer? v) v]
        [else (error 'sim "expected a number, got ~e" v)]))
(define (->arg v) (if (hexval? v) (hexval-int v) v))    ; symbols pass through for name resolution
(define (field f fields [default #f]) (hash-ref fields f default))
(define (field-int f fields [default #f]) (let ([v (field f fields)]) (if v (->int v) default)))

;; resolve a code value (a code-ref @file, or inline 0x bytes) -> (values creation abi)
(define (resolve-code v root)
  (cond
    [(code-ref? v)
     (define path (build-path root (code-ref-file v)))
     (cond
       [(regexp-match? #px"\\.json$" (code-ref-file v))
        (define art (read-artifact path #:contract (code-ref-contract v)))
        (values (artifact-creation art) (artifact-abi art))]
       [else (values (hex->bytes (call-with-input-file path port->string-trim)) #f)])]
    [(hexval? v) (values (hexval-bytes v) #f)]
    [else (error 'sim "code must be @file or 0x bytecode, got ~e" v)]))

(define (port->string-trim p)
  (let loop ([acc '()]) (define l (read-line p 'any))
    (if (eof-object? l) (apply string-append (reverse acc)) (loop (cons (string-trim-nl l) acc)))))
(define (string-trim-nl s) (regexp-replace* #px"\\s+" s ""))

;; --- the driver --------------------------------------------------------
(define (run-scenario src root)
  (define cmds (parse-scenario src))
  ;; the fork is fixed at simulator creation, so read it first
  (define fork (let ([c (findf (lambda (c) (eq? (command-head c) 'fork)) cmds)])
                 (if c (car (command-positionals c)) 'Prague)))
  (define s (make-simulator #:fork fork))
  (define default-trace (box #f))
  (for ([c (in-list cmds)]) (run-command s c root default-trace))
  (print-final s))

(define (run-command s c root default-trace)
  (define f (command-fields c))
  (define pos (command-positionals c))
  (case (command-head c)
    [(fork) (void)]                                   ; already applied
    [(account contract)
     (define-values (creation abi code)
       (let ([cv (field 'code f)]) (if cv (let-values ([(cr ab) (resolve-code cv root)]) (values cr ab cr)) (values #f #f '()))))
     (sim-account! s (car pos)
                   #:address (and (pair? (cdr pos)) (->int (cadr pos)))
                   #:balance (or (field-int 'balance f) 0)
                   #:nonce (or (field-int 'nonce f) 0)
                   #:code code
                   #:storage (parse-storage-field (field 'storage f))
                   #:abi abi)]
    [(deploy)
     (define-values (creation abi) (resolve-code (field 'code f) root))
     (sim-deploy! s creation #:from (field 'from f) #:value (or (field-int 'value f) 0)
                  #:name (car pos) #:abi abi)
     (printf "deployed ~a at 0x~a\n" (car pos) (number->string (sim-address s (car pos)) 16))]
    [(tx)
     (define tr (or (trace-mode (field 'trace f)) (unbox default-trace)))
     (define r (sim-send! s #:from (field 'from f) #:to (field 'to f)
                          #:value (or (field-int 'value f) 0)
                          #:gas (field-int 'gas f)
                          #:data (data-of f) #:sig (field 'sig f) #:args (args-of f)
                          #:gas-price (field-int 'gasprice f)
                          #:max-fee (field-int 'maxfee f) #:max-priority (field-int 'maxpriority f)
                          #:trace tr))
     (print-receipt r #:label (format "tx ~a -> ~a" (field 'from f) (field 'to f)))]
    [(call)
     (define v (sim-view s #:from (or (field 'from f) 0) #:to (field 'to f)
                         #:sig (field 'sig f) #:args (args-of f) #:data (data-of f)
                         #:returns (returns-of f)))
     (printf "call ~a.~a = ~a\n" (field 'to f) (or (field 'sig f) "") v)]
    [(mine)
     (sim-mine! s #:number (block-val (field 'number f) s 1)
                #:timestamp (block-val (field 'timestamp f) s 2)
                #:coinbase (block-val (field 'coinbase f) s 3))]
    [(block) (apply-block-fields s f)]
    [(withdrawal) (sim-withdraw! s (field 'to f) (field-int 'amount f 0))]
    [(trace) (set-box! default-trace (trace-mode (car pos)))]))

;; --- helpers -----------------------------------------------------------
(define (data-of f) (let ([d (field 'data f)]) (and d (if (hexval? d) (hexval-bytes d) d))))
(define (args-of f) (let ([a (field 'args f)]) (if a (map ->arg a) '())))
(define (returns-of f) (let ([r (field 'returns f)]) (and r (if (symbol? r) (symbol->string r) r))))
(define (trace-mode v) (case v [(call) 'call] [(step) 'step] [else #f]))

(define (parse-storage-field v)
  (if v (for/list ([kv (in-list v)]) (list (->int (car kv)) (->int (cadr kv)))) '()))

;; a block field value: absolute number, or (rel . n) meaning current + n
(define (block-val v s idx)
  (cond [(not v) #f]
        [(and (pair? v) (eq? (car v) 'rel)) (+ (list-ref (simulator-block s) idx) (cdr v))]
        [else (->int v)]))

(define (apply-block-fields s f)
  (define blk (simulator-block s))
  (define (upd idx key) (let ([nv (block-val (field key f) s idx)]) (if nv nv (list-ref blk idx))))
  (set-simulator-block! s
    (list 'block (upd 1 'number) (upd 2 'timestamp) (upd 3 'coinbase) (upd 4 'basefee)
          (list-ref blk 5) (list-ref blk 6) (upd 7 'gaslimit) (list-ref blk 8) (list-ref blk 9))))

;; --- final state -------------------------------------------------------
(define (print-final s)
  (printf "\n--- final state ---\n")
  (printf "state root: 0x~a\n" (bytes->hex (sim-state-root s)))
  (define diff (sim-diff s))
  (cond
    [(null? diff) (printf "(no state changes)\n")]
    [else
     (for ([d (in-list diff)])
       (printf "0x~a" (number->string (car d) 16))
       (unless (zero? (cadr d)) (printf "  balance ~a~a" (if (positive? (cadr d)) "+" "") (cadr d)))
       (unless (= (caddr d) (cadddr d)) (printf "  nonce ~a->~a" (caddr d) (cadddr d)))
       (newline)
       (for ([sc (in-list (list-ref d 4))])
         (printf "    slot 0x~a: 0x~a -> 0x~a\n" (number->string (car sc) 16)
                 (number->string (cadr sc) 16) (number->string (caddr sc) 16))))]))

(define (bytes->hex bs)
  (apply string-append (for/list ([b (in-bytes (if (bytes? bs) bs (list->bytes bs)))])
                         (define t (number->string b 16)) (if (= 1 (string-length t)) (string-append "0" t) t))))
