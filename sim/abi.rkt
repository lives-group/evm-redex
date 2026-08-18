#lang racket/base

;; ======================================================================
;; ABI conveniences for the simulator: encode a call from a signature + args,
;; decode a return value, turn a REVERT's bytes into a readable reason, and
;; decode an event log against a contract's ABI.
;;
;; Built on the ABI codec already in the library (pbt/solc.rkt) — this module
;; adds only the simulator-facing conveniences (revert selectors, event topics).
;; ======================================================================

(require racket/list
         racket/string
         (only-in "../private/keccak.rkt" keccak256)
         (only-in "../pbt/solc.rkt"
                  encode-call abi-decode function-selector parse-arg-types))

(provide encode-call
         decode-return
         decode-revert
         event-table decode-log)

;; --- return data -------------------------------------------------------
;; decode-return : type-string-or-list bytes -> list of values
;;   (decode-return "(uint256)" bs)  or  (decode-return '("uint256" "bool") bs)
(define (decode-return types bs)
  (define ts (if (string? types) (parse-arg-types (wrap-tuple types)) types))
  (abi-decode ts bs))

;; a bare "uint256,bool" or "(uint256,bool)" -> a synthetic sig for parse-arg-types
(define (wrap-tuple s)
  (define t (string-trim s))
  (define inner (if (and (> (string-length t) 1) (char=? (string-ref t 0) #\())
                    (substring t 1 (sub1 (string-length t)))
                    t))
  (string-append "_(" inner ")"))

;; --- revert reason -----------------------------------------------------
(define ERROR-SELECTOR (function-selector "Error(string)"))     ; 0x08c379a0
(define PANIC-SELECTOR (function-selector "Panic(uint256)"))    ; 0x4e487b71

(define PANIC-CODES
  (hash #x01 "assertion failed"
        #x11 "arithmetic overflow/underflow"
        #x12 "division or modulo by zero"
        #x21 "invalid enum value"
        #x22 "invalid storage byte array"
        #x31 "pop on empty array"
        #x32 "array index out of bounds"
        #x41 "out-of-memory / too much allocation"
        #x51 "call to a zero-initialised function"))

;; decode-revert : (listof byte) -> string
(define (decode-revert bs)
  (cond
    [(null? bs) "reverted (no data)"]
    [(< (length bs) 4) (format "reverted 0x~a" (bytes->hex bs))]
    [else
     (define sel (selector-of bs))
     (define body (drop bs 4))
     (cond
       [(= sel ERROR-SELECTOR)
        (define msg (with-handlers ([exn:fail? (lambda (_) #f)])
                      (car (abi-decode '("string") body))))
        (if msg (format "Error(~s)" msg) (format "Error(?) 0x~a" (bytes->hex bs)))]
       [(= sel PANIC-SELECTOR)
        (define code (with-handlers ([exn:fail? (lambda (_) #f)])
                       (car (abi-decode '("uint256") body))))
        (format "Panic(0x~a~a)" (number->string (or code 0) 16)
                (let ([d (hash-ref PANIC-CODES code #f)]) (if d (format ": ~a" d) "")))]
       [else (format "custom error 0x~a (data 0x~a)"
                     (number->string sel 16) (bytes->hex body))])]))

(define (selector-of bs)
  (for/fold ([acc 0]) ([b (in-list (take bs 4))]) (+ (* acc 256) b)))

;; --- event logs --------------------------------------------------------
;; event-table : abi(jsexpr list) -> hash topic0(integer) -> (vector name inputs)
;; where inputs is the list of (type name indexed?) for the event.
(define (event-table abi)
  (cond
    [(not (list? abi)) (hash)]
    [else
     (for/fold ([h (hash)]) ([e (in-list abi)] #:when (equal? (hash-ref* e 'type) "event"))
       (define name (hash-ref* e 'name))
       (define inputs (for/list ([i (in-list (hash-ref* e 'inputs '()))])
                        (list (hash-ref* i 'type) (hash-ref* i 'name "")
                              (and (hash-ref* i 'indexed #f) #t))))
       (define sig (string-append name "(" (string-join (map car inputs) ",") ")"))
       (define topic0 (for/fold ([acc 0]) ([b (in-bytes (keccak256 (string->bytes/utf-8 sig)))])
                        (+ (* acc 256) b)))
       (hash-set h topic0 (vector name inputs)))]))

;; hash-ref that tolerates symbol/string keys and jsexpr
(define (hash-ref* h k [default #f])
  (cond [(not (hash? h)) default]
        [(hash-has-key? h k) (hash-ref h k)]
        [else default]))

;; decode-log : (log addr (topic ...) data) event-table
;;   -> (list name (list (cons arg-name value) ...))  or #f if the event is unknown
;; Indexed args are read from the topics (after topic0); non-indexed from `data`
;; via abi-decode.  Dynamic indexed args are only the hash, so shown as the raw
;; topic word.
(define (decode-log lg table)
  (define topics (caddr lg))
  (cond
    [(null? topics) #f]
    [else
     (define entry (hash-ref table (car topics) #f))
     (cond
       [(not entry) #f]
       [else
        (define name (vector-ref entry 0))
        (define inputs (vector-ref entry 1))
        (define non-indexed-types (for/list ([i (in-list inputs)] #:unless (caddr i)) (car i)))
        (define non-indexed-vals
          (with-handlers ([exn:fail? (lambda (_) #f)])
            (abi-decode non-indexed-types (cadddr lg))))
        (let loop ([ins inputs] [tps (cdr topics)] [nis (or non-indexed-vals '())] [acc '()])
          (cond
            [(null? ins) (list name (reverse acc))]
            [(caddr (car ins))          ; indexed -> next topic
             (loop (cdr ins) (if (null? tps) tps (cdr tps)) nis
                   (cons (cons (cadr (car ins)) (if (null? tps) 'indexed (car tps))) acc))]
            [else                        ; non-indexed -> next decoded value
             (loop (cdr ins) tps (if (null? nis) nis (cdr nis))
                   (cons (cons (cadr (car ins)) (if (null? nis) '? (car nis))) acc))]))])]))

(define (bytes->hex bs)
  (apply string-append (map (lambda (b) (define s (number->string b 16))
                              (if (= 1 (string-length s)) (string-append "0" s) s)) bs)))
