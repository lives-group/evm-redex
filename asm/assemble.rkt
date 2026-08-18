#lang racket/base

;; ======================================================================
;; The assembler: a parsed program -> a byte list, resolving labels.
;;
;; Two passes, and NO fixpoint: every instruction's size is known before any
;; label is resolved, so label offsets are computed in one sweep.
;;
;;   * an explicit PUSHk, or a bare `PUSH <literal>`, has a size fixed by the
;;     literal itself (min 1 byte; `PUSH 0` -> PUSH1 0x00, write `PUSH0` for the
;;     zero-byte opcode);
;;   * `PUSH <label>` is ALWAYS emitted as PUSH2 (two bytes) — a pc fits in two
;;     bytes and the size is constant, so no label's position depends on another
;;     label's resolution.
;;
;; Also provides `assemble` (text -> bytes) for programmatic use and tests, and
;; `disassemble` (bytes -> text) for the round-trip tests.
;; ======================================================================

(require racket/list
         racket/string
         racket/format
         "parse.rkt"
         (only-in "opcodes.rkt"
                  mnemonic->opinfo opinfo-byte opinfo-imm opcode->mnemonic push-immediate-size))

;; (asm-error, the AST structs, and loc come from parse.rkt)

(provide assemble-parsed assemble
         directives->config
         disassemble
         (struct-out asm-config))

;; the environment a program asks for via directives (defaults applied here)
(struct asm-config (gas calldata) #:transparent)

(define DEFAULT-GAS 30000000)

(define (directives->config dirs)
  (for/fold ([c (asm-config DEFAULT-GAS '())]) ([d (in-list dirs)])
    (case (dir-name d)
      [(gas)      (struct-copy asm-config c [gas (dir-value d)])]
      [(calldata) (struct-copy asm-config c [calldata (dir-value d)])]
      [else c])))

;; --- sizing ------------------------------------------------------------
;; minimum bytes to hold a non-negative integer (0 -> 1, i.e. one 0x00 byte)
(define (byte-length n) (max 1 (quotient (+ (integer-length n) 7) 8)))

;; big-endian encode n into exactly `width` bytes
(define (int->bytes n width)
  (for/list ([i (in-range (sub1 width) -1 -1)]) (bitwise-and (arithmetic-shift n (* -8 i)) #xff)))

;; the byte width an instruction's PUSH immediate will occupy (0 if not a PUSH
;; that carries data)
(define (instr-imm-width in)
  (define mn (instr-mnemonic in))
  (define op (instr-operand in))
  (cond
    [(eq? mn 'PUSH)                                  ; bare PUSH: label -> 2, literal -> minimal
     (if (label-ref? op) 2 (byte-length op))]
    [else (opinfo-imm (require-op mn (instr-src in)))]))  ; explicit PUSHk -> k, others -> 0

(define (label-ref? op) (and (pair? op) (eq? (car op) 'label)))

(define (require-op mn src)
  (or (mnemonic->opinfo mn) (asm-error src "unknown opcode ~a" mn)))

;; total byte size of one item
(define (item-size it)
  (cond
    [(label? it) 0]
    [(raw-bytes? it) (length (raw-bytes-bytes it))]
    [(instr? it) (+ 1 (instr-imm-width it))]))

;; --- the two passes ----------------------------------------------------
(define (assemble-parsed p)
  (define items (parsed-items p))
  ;; pass 1: label -> pc
  (define labels (make-hasheq))
  (for/fold ([pc 0]) ([it (in-list items)])
    (when (label? it)
      (when (hash-has-key? labels (label-name it))
        (asm-error (label-src it) "duplicate label ~a" (label-name it)))
      (hash-set! labels (label-name it) pc))
    (+ pc (item-size it)))
  ;; pass 2: emit
  (define bytes
    (append*
     (for/list ([it (in-list items)])
       (cond
         [(label? it) '()]
         [(raw-bytes? it) (raw-bytes-bytes it)]
         [(instr? it) (emit-instr it labels)]))))
  (values bytes (directives->config (parsed-directives p))))

(define (emit-instr in labels)
  (define mn (instr-mnemonic in))
  (define op (instr-operand in))
  (define src (instr-src in))
  (cond
    ;; bare PUSH: pick the opcode from the chosen width
    [(eq? mn 'PUSH)
     (define-values (val width)
       (if (label-ref? op)
           (values (resolve-label (cdr op) labels src) 2)
           (values op (byte-length op))))
     (cons (+ #x5f width) (int->bytes val width))]      ; 0x5f+width = PUSHwidth
    ;; a real opcode: emit its byte, plus k immediate bytes if it is PUSHk
    [else
     (define oi (require-op mn src))
     (define k (opinfo-imm oi))                         ; PUSHk -> k, everything else -> 0
     (cond
       [(zero? k) (list (opinfo-byte oi))]              ; incl. PUSH0 and all non-PUSH opcodes
       [else
        (define v (cond [(label-ref? op) (resolve-label (cdr op) labels src)]
                        [op op]
                        [else (asm-error src "~a needs an operand" mn)]))
        (unless (<= (byte-length v) k)
          (asm-error src "operand ~a does not fit in ~a (needs ~a bytes)" v mn (byte-length v)))
        (cons (opinfo-byte oi) (int->bytes v k))])]))

(define (resolve-label name labels src)
  (or (hash-ref labels name #f) (asm-error src "undefined label ~a" name)))

;; --- programmatic entry ------------------------------------------------
;; assemble : (or string input-port) -> (values (listof byte) asm-config)
(define (assemble input) (assemble-parsed (parse-evm input)))

;; --- disassembler ------------------------------------------------------
;; bytes -> a string of one instruction per line (PUSH immediates inline as hex),
;; skipping PUSH data the same way the interpreter does.
(define (disassemble code)
  (define v (list->vector code))
  (define n (vector-length v))
  (string-join
   (let loop ([i 0] [acc '()])
     (cond
       [(>= i n) (reverse acc)]
       [else
        (define op (vector-ref v i))
        (define name (opcode->mnemonic op))
        (define k (push-immediate-size op))
        (cond
          [(> k 0)
           (define imm (for/list ([j (in-range (add1 i) (min n (+ i 1 k)))]) (vector-ref v j)))
           (loop (+ i 1 k) (cons (format "~a 0x~a" name (bytes->hex imm)) acc))]
          [name  (loop (add1 i) (cons (symbol->string name) acc))]
          [else  (loop (add1 i) (cons (format "INVALID ; 0x~a" (byte->hex op)) acc))])]))
   "\n"))

(define (byte->hex b) (~r b #:base 16 #:min-width 2 #:pad-string "0"))
(define (bytes->hex bs) (apply string-append (map byte->hex bs)))
