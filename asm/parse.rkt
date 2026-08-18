#lang racket/base

;; ======================================================================
;; The parser: EVM-assembly text -> a parsed program (directives, then a stream
;; of labels and instructions), carrying source positions for error messages.
;;
;; Used by BOTH the `#lang evm-redex/asm` reader (asm/lang/reader.rkt) and the
;; programmatic `assemble` entry point (asm/assemble.rkt), so the two never drift.
;;
;; Grammar (one instruction per token; case-insensitive mnemonics):
;;   line        ::= (label | directive | instruction)* comment?
;;   comment     ::= ';' .... (to end of line)
;;   label       ::= IDENT ':'
;;   directive   ::= '.gas' NUMBER | '.calldata' HEX
;;   instruction ::= MNEMONIC operand?
;;   operand     ::= HEX | NUMBER | IDENT        (IDENT = a label reference)
;;                   (only the PUSH family takes an operand)
;;
;; Raw-bytecode fallback: if the first meaningful token starts with `0x`, the
;; whole source is taken as one hex blob (paste solc output directly).  A
;; mnemonic program never starts with `0x`, so this is unambiguous.
;; ======================================================================

(require racket/string
         racket/list
         (only-in "opcodes.rkt" mnemonic->opinfo known-mnemonic?))

(provide (struct-out parsed) (struct-out loc)
         (struct-out dir) (struct-out label) (struct-out instr) (struct-out raw-bytes)
         parse-evm asm-error (struct-out exn:fail:evm-asm))

;; --- AST ---------------------------------------------------------------
(struct loc (line col) #:transparent)                 ; 1-based line, 0-based col
(struct parsed (directives items) #:transparent)      ; items: (or label instr raw-bytes), in order
(struct dir (name value src) #:transparent)           ; name: 'gas | 'calldata ; value: integer | (listof byte)
(struct label (name src) #:transparent)               ; name: symbol
(struct instr (mnemonic operand src) #:transparent)   ; operand: #f | integer | (cons 'label symbol)
(struct raw-bytes (bytes) #:transparent)              ; the hex-fallback whole program

(struct exn:fail:evm-asm exn:fail (src) #:transparent)

(define (asm-error src fmt . args)
  (raise (exn:fail:evm-asm
          (string-append
           (if src (format "~a:~a: " (loc-line src) (add1 (loc-col src))) "")
           (apply format fmt args))
          (current-continuation-marks)
          src)))

;; --- tokenizing --------------------------------------------------------
;; -> (listof (cons string loc)), comments and blank space stripped.
(define (tokenize text)
  (for*/list ([(line n) (in-indexed (in-list (string-split text "\n" #:trim? #f)))]
              [tok (in-list (line-tokens (strip-comment line) (add1 n)))])
    tok))

(define (strip-comment line)
  (define i (for/first ([c (in-string line)] [k (in-naturals)] #:when (char=? c #\;)) k))
  (if i (substring line 0 i) line))

(define (line-tokens line n)
  (let loop ([i 0] [acc '()])
    (cond
      [(>= i (string-length line)) (reverse acc)]
      [(char-whitespace? (string-ref line i)) (loop (add1 i) acc)]
      [else
       (define j (let scan ([k i])
                   (if (or (>= k (string-length line)) (char-whitespace? (string-ref line k)))
                       k (scan (add1 k)))))
       (loop j (cons (cons (substring line i j) (loc n i)) acc))])))

;; --- literal recognisers ----------------------------------------------
(define (hex-token? s) (and (> (string-length s) 2) (string-prefix? (string-downcase s) "0x")))
(define (all-hex? s) (regexp-match? #px"^[0-9a-fA-F]+$" s))
(define (decimal? s) (regexp-match? #px"^[0-9]+$" s))
(define (ident? s) (regexp-match? #px"^[A-Za-z_][A-Za-z0-9_.]*$" s))

;; parse a HEX or NUMBER operand into an exact integer (labels handled by caller)
(define (literal->integer s src)
  (cond
    [(hex-token? s)
     (define h (substring s 2))
     (unless (and (> (string-length h) 0) (all-hex? h)) (asm-error src "malformed hex literal ~s" s))
     (string->number h 16)]
    [(decimal? s) (string->number s 10)]
    [else (asm-error src "expected a number or hex literal, got ~s" s)]))

;; hex string -> (listof byte), padding an odd nibble count (like solc hex).
(define (hex->byte-list s src)
  (define h0 (if (string-prefix? (string-downcase s) "0x") (substring s 2) s))
  (unless (all-hex? h0) (asm-error src "malformed hex in ~s" s))
  (define h (if (odd? (string-length h0)) (string-append "0" h0) h0))
  (for/list ([i (in-range 0 (string-length h) 2)]) (string->number (substring h i (+ i 2)) 16)))

;; --- the parse ---------------------------------------------------------
(define (parse-evm input #:source [_src 'evm])
  (define text (if (string? input) input (port->string* input)))
  (define toks (tokenize text))
  (cond
    [(null? toks) (parsed '() '())]
    ;; raw-bytecode fallback: first token starts with 0x
    [(hex-token? (caar toks))
     (parsed '() (list (raw-bytes (parse-raw toks))))]
    [else (parse-tokens toks)]))

(define (port->string* p)
  (apply string-append
         (let loop () (define l (read-line p 'any)) (if (eof-object? l) '() (cons (string-append l "\n") (loop))))))

;; every token must be hex; concatenate into one byte list
(define (parse-raw toks)
  (append*
   (for/list ([t (in-list toks)])
     (define s (car t)) (define src (cdr t))
     (hex->byte-list s src))))

(define (parse-tokens toks)
  (let loop ([ts toks] [dirs '()] [items '()])
    (cond
      [(null? ts) (parsed (reverse dirs) (reverse items))]
      [else
       (define tok (car ts)) (define s (car tok)) (define src (cdr tok))
       (cond
         ;; label definition: IDENT ':'
         [(string-suffix? s ":")
          (define name (substring s 0 (sub1 (string-length s))))
          (unless (ident? name) (asm-error src "invalid label name ~s" name))
          (loop (cdr ts) dirs (cons (label (string->symbol name) src) items))]
         ;; directive
         [(string-prefix? s ".")
          (unless (null? items) (asm-error src "directive ~a must appear before any instruction" s))
          (define-values (d rest) (parse-directive s src (cdr ts)))
          (loop rest (cons d dirs) items)]
         ;; instruction
         [else
          (define mn (string->symbol s))
          ;; bare PUSH is an assembler pseudo-op (auto-sized), not a real opcode
          (unless (or (bare-push? mn) (known-mnemonic? mn)) (asm-error src "unknown opcode ~s" s))
          (define-values (operand rest) (parse-operand mn (cdr ts) src))
          (loop rest dirs (cons (instr (canonical mn) operand src) items))])])))

(define (bare-push? mn) (string=? (string-upcase (symbol->string mn)) "PUSH"))

(define (canonical mn) (string->symbol (string-upcase (symbol->string mn))))

;; .gas NUMBER | .calldata HEX
(define (parse-directive s src ts)
  (define name (string-downcase (substring s 1)))
  (when (null? ts) (asm-error src "directive .~a needs an argument" name))
  (define arg (car ts)) (define asrc (cdr arg))
  (cond
    [(string=? name "gas")      (values (dir 'gas (literal->integer (car arg) asrc) src) (cdr ts))]
    [(string=? name "calldata") (values (dir 'calldata (hex->byte-list (car arg) asrc) src) (cdr ts))]
    [else (asm-error src "unknown directive .~a (expected .gas or .calldata)" name)]))

;; only the PUSH family takes an operand.  Bare PUSH and PUSH1..PUSH32 read one
;; operand token (a number, a hex literal, or a label reference); PUSH0 and every
;; other opcode take none.
(define (parse-operand mn ts src)
  (define name (string-upcase (symbol->string mn)))
  (define takes? (and (string-prefix? name "PUSH") (not (string=? name "PUSH0"))))
  (cond
    [(not takes?) (values #f ts)]
    [(null? ts) (asm-error src "~a needs an operand (a number, 0x-hex, or a label)" name)]
    [else
     (define t (car ts)) (define s (car t)) (define tsrc (cdr t))
     (define operand
       (cond
         [(or (hex-token? s) (decimal? s)) (literal->integer s tsrc)]
         [(ident? s) (cons 'label (string->symbol s))]
         [else (asm-error tsrc "invalid PUSH operand ~s" s)]))
     (values operand (cdr ts))]))
