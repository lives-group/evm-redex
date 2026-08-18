#lang racket/base

;; ======================================================================
;; The scenario parser for `#lang evm-redex/sim`.
;;
;; A scenario is a sequence of lines; each is a comment, a directive (`.fork`,
;; `.account`, `.contract`, `.deploy`, `.block`, `.trace`) or an action (`tx`,
;; `call`, `mine`, `withdrawal`).  Every command is `HEAD positional* field*`
;; where a field is `key=value`.  Values: numbers (`0x..`/decimal, optional
;; `eth`/`gwei`/`wei` unit), account names (identifiers), quoted strings
;; (signatures), tuples `(a, b, ...)` (call args), storage maps `{slot:val,..}`,
;; code refs `@file[:Contract]`, and relative `+N` (block fields).
;;
;; Produces a list of `command` structs; the runtime (sim/runtime.rkt) drives the
;; engine from them.
;; ======================================================================

(require racket/string racket/list)

(provide (struct-out command) (struct-out loc) (struct-out code-ref) (struct-out hexval)
         parse-scenario asm-scenario-error exn:fail:evm-sim?)

(struct loc (line) #:transparent)
(struct command (head positionals fields src) #:transparent)  ; fields: hash sym->value
(struct code-ref (file contract) #:transparent)               ; @file[:Contract]
;; a 0x literal keeps both readings: the byte list (for code/data) and the
;; integer (for values/args/slots).  The runtime coerces per context.
(struct hexval (bytes int) #:transparent)

(struct exn:fail:evm-sim exn:fail (src) #:transparent)
(define (asm-scenario-error src fmt . args)
  (raise (exn:fail:evm-sim
          (string-append (if src (format "line ~a: " (loc-line src)) "") (apply format fmt args))
          (current-continuation-marks) src)))

(define HEADS '(tx call mine withdrawal))
(define DIRECTIVES '(fork account contract deploy block trace))

(define (parse-scenario input)
  (define text (if (string? input) input (port->string* input)))
  (filter values
    (for/list ([line (in-list (string-split text "\n" #:trim? #f))] [n (in-naturals 1)])
      (parse-line (strip-comment line) (loc n)))))

(define (port->string* p)
  (apply string-append (let loop () (define l (read-line p 'any))
                          (if (eof-object? l) '() (cons (string-append l "\n") (loop))))))

(define (strip-comment line)
  (define i (comment-index line))
  (if i (substring line 0 i) line))

;; a ';' that is not inside quotes starts a comment
(define (comment-index line)
  (let loop ([i 0] [q #f])
    (cond [(>= i (string-length line)) #f]
          [(char=? (string-ref line i) #\") (loop (add1 i) (not q))]
          [(and (not q) (char=? (string-ref line i) #\;)) i]
          [else (loop (add1 i) q)])))

(define (parse-line line src)
  (define toks (tokenize line src))
  (cond
    [(null? toks) #f]
    [else
     (define head-str (car toks))
     (define-values (head kind)
       (cond
         [(string-prefix? head-str ".")
          (define d (string->symbol (substring head-str 1)))
          (unless (memq d DIRECTIVES) (asm-scenario-error src "unknown directive ~a" head-str))
          (values d 'directive)]
         [(memq (string->symbol head-str) HEADS) (values (string->symbol head-str) 'action)]
         [else (asm-scenario-error src "unknown command ~a" head-str)]))
     (define-values (positionals fields) (parse-args (cdr toks) src))
     (command head positionals fields src)]))

;; --- tokenizing: whitespace splits at top level; quotes/()/{} group ----
(define (tokenize line src)
  (define n (string-length line))
  (let loop ([i 0] [toks '()])
    (cond
      [(>= i n) (reverse toks)]
      [(char-whitespace? (string-ref line i)) (loop (add1 i) toks)]
      [else
       (define j (token-end line i src))
       (loop j (cons (substring line i j) toks))])))

;; scan to the end of one token, keeping "..", (..), {..} balanced
(define (token-end line i src)
  (define n (string-length line))
  (let loop ([k i] [depth 0] [q #f])
    (cond
      [(>= k n) (when (or (> depth 0) q) (asm-scenario-error src "unbalanced quotes/brackets")) k]
      [q (loop (add1 k) depth (not (char=? (string-ref line k) #\")))]
      [else
       (define c (string-ref line k))
       (cond
         [(char=? c #\") (loop (add1 k) depth #t)]
         [(or (char=? c #\() (char=? c #\{)) (loop (add1 k) (add1 depth) q)]
         [(or (char=? c #\)) (char=? c #\})) (loop (add1 k) (max 0 (sub1 depth)) q)]
         [(and (= depth 0) (char-whitespace? c)) k]
         [else (loop (add1 k) depth q)])])))

(define (parse-args toks src)
  (let loop ([ts toks] [pos '()] [fields (hasheq)])
    (cond
      [(null? ts) (values (reverse pos) fields)]
      [else
       (define t (car ts))
       (define eq (kv-split t))
       (cond
         [eq (loop (cdr ts) pos (hash-set fields (string->symbol (car eq)) (parse-value (cdr eq) src)))]
         [else (loop (cdr ts) (cons (parse-value t src) pos) fields)])])))

;; split at the first top-level '=' (not inside quotes/brackets)
(define (kv-split t)
  (let loop ([i 0] [depth 0] [q #f])
    (cond
      [(>= i (string-length t)) #f]
      [q (loop (add1 i) depth (not (char=? (string-ref t i) #\")))]
      [else
       (define c (string-ref t i))
       (cond
         [(char=? c #\") (loop (add1 i) depth #t)]
         [(or (char=? c #\() (char=? c #\{)) (loop (add1 i) (add1 depth) q)]
         [(or (char=? c #\)) (char=? c #\})) (loop (add1 i) (sub1 depth) q)]
         [(and (= depth 0) (char=? c #\=)) (cons (substring t 0 i) (substring t (add1 i)))]
         [else (loop (add1 i) depth q)])])))

;; --- value parsing -----------------------------------------------------
(define (parse-value s src)
  (define t (string-trim s))
  (cond
    [(= 0 (string-length t)) (asm-scenario-error src "empty value")]
    [(string-prefix? t "\"") (substring t 1 (sub1 (string-length t)))]                 ; string
    [(string-prefix? t "(") (map (lambda (x) (parse-value x src)) (split-top (inner t))) ] ; tuple
    [(string-prefix? t "{") (parse-storage (inner t) src)]                             ; storage map
    [(string-prefix? t "@") (parse-code-ref (substring t 1))]                          ; code ref
    [(string-prefix? t "+") (cons 'rel (parse-number (substring t 1) src))]            ; relative
    [(number-like? t) (parse-number t src)]
    [(identifier? t) (string->symbol t)]                                              ; account name
    [else (asm-scenario-error src "cannot parse value ~s" t)]))

(define (inner t) (substring t 1 (sub1 (string-length t))))

;; split a comma-separated group at depth 0
(define (split-top s)
  (let loop ([cs (string->list s)] [depth 0] [q #f] [cur '()] [acc '()])
    (define (flush) (let ([x (string-trim (list->string (reverse cur)))]) (if (= 0 (string-length x)) acc (cons x acc))))
    (cond
      [(null? cs) (reverse (flush))]
      [q (loop (cdr cs) depth (not (char=? (car cs) #\")) (cons (car cs) cur) acc)]
      [else
       (define c (car cs))
       (cond
         [(char=? c #\") (loop (cdr cs) depth #t (cons c cur) acc)]
         [(or (char=? c #\() (char=? c #\{)) (loop (cdr cs) (add1 depth) q (cons c cur) acc)]
         [(or (char=? c #\)) (char=? c #\})) (loop (cdr cs) (sub1 depth) q (cons c cur) acc)]
         [(and (= depth 0) (char=? c #\,)) (loop (cdr cs) depth q '() (flush))]
         [else (loop (cdr cs) depth q (cons c cur) acc)])])))

(define (parse-storage s src)
  (for/list ([pair (in-list (split-top s))])
    (define kv (string-split pair ":"))
    (unless (= 2 (length kv)) (asm-scenario-error src "storage entry must be slot:value, got ~s" pair))
    (list (parse-number (string-trim (car kv)) src) (parse-number (string-trim (cadr kv)) src))))

(define (parse-code-ref s)
  (define parts (string-split s ":"))
  (if (= 2 (length parts)) (code-ref (car parts) (cadr parts)) (code-ref s #f)))

;; numbers: 0x-hex or decimal, optional eth/gwei/wei unit
(define (number-like? t) (regexp-match? #px"^(0x[0-9a-fA-F]+|[0-9]+)(eth|gwei|wei)?$" t))
(define (parse-number t src)
  (define m (regexp-match #px"^(0x[0-9a-fA-F]+|[0-9]+)(eth|gwei|wei)?$" t))
  (unless m (asm-scenario-error src "expected a number, got ~s" t))
  (define hex? (string-prefix? (cadr m) "0x"))
  (define base (if hex? (string->number (substring (cadr m) 2) 16) (string->number (cadr m))))
  (cond
    [(and hex? (not (caddr m)))                      ; a bare 0x literal: keep both readings
     (hexval (hex-string->bytes (substring (cadr m) 2)) base)]
    [else
     (case (caddr m)
       [("eth") (* base (expt 10 18))]
       [("gwei") (* base (expt 10 9))]
       [else base])]))

(define (hex-string->bytes h0)
  (define h (if (odd? (string-length h0)) (string-append "0" h0) h0))
  (for/list ([i (in-range 0 (string-length h) 2)]) (string->number (substring h i (+ i 2)) 16)))

(define (identifier? t) (regexp-match? #px"^[A-Za-z_][A-Za-z0-9_]*$" t))
