#lang racket/base

;; ======================================================================
;; Read solc / Foundry / Hardhat build artifacts and pull out the two
;; bytecodes as byte lists usable by `deploy` (creation) and `#:contract`
;; (runtime), plus helpers to compute a function selector and encode a call.
;;
;; Recognised JSON shapes:
;;   * Foundry (`forge build`, out/C.sol/C.json) — top-level
;;       { "bytecode": {"object": "0x.."}, "deployedBytecode": {"object": "0x.."}, "abi": [...] }
;;   * Hardhat — top-level, bytecodes as hex strings
;;       { "bytecode": "0x..", "deployedBytecode": "0x..", "abi": [...] }
;;   * solc --combined-json bin,bin-runtime,abi
;;       { "contracts": { "file.sol:C": { "bin": "..", "bin-runtime": "..", "abi": "[...]" } } }
;;   * solc --standard-json output
;;       { "contracts": { "file.sol": { "C": { "evm": {"bytecode":{"object":..},
;;                                                     "deployedBytecode":{"object":..}}, "abi":[...] } } } }
;; ======================================================================

(require racket/string
         racket/list
         json
         (only-in "../private/keccak.rkt" keccak256))

(provide (struct-out artifact)
         read-artifact read-artifact/string
         hex->bytes function-selector encode-call
         abi-encode abi-decode parse-arg-types)

;; creation / runtime are byte lists (or #f if the shape lacked them).
;;
;; srcmap  : the RUNTIME source map, verbatim (solc's `;`-separated string), or
;;           #f when the artifact was built without it.  Nothing in the library
;;           reads it; it is what lets a tool turn bytecode coverage into
;;           Solidity-line coverage (the coverage harness in the companion
;;           evm-redex-tests project does).  Ask solc for it with
;;             solc --combined-json bin,bin-runtime,abi,srcmap-runtime …
;;           which leaves the bytecode byte-for-byte identical.
;; sources : the file list the source map's file indices refer to, in index
;;           order (solc's `sourceList`), or #f.
(struct artifact (name creation runtime abi raw srcmap sources) #:transparent)

;; --- hex ---------------------------------------------------------------
(define (hex->bytes s)
  (cond
    [(not (string? s)) #f]
    [else
     (define h0 (if (string-prefix? s "0x") (substring s 2) s))
     (define h (if (odd? (string-length h0)) (string-append "0" h0) h0))
     (for/list ([i (in-range 0 (string-length h) 2)])
       (string->number (substring h i (+ i 2)) 16))]))

;; a bytecode field may be a hex string or {"object": "0x.."}
(define (bc->hex x)
  (cond [(string? x) x]
        [(and (hash? x) (hash-has-key? x 'object)) (hash-ref x 'object)]
        [else #f]))

(define (jref j . path)
  (let loop ([j j] [p path])
    (cond [(null? p) j]
          [(and (hash? j) (hash-has-key? j (car p))) (loop (hash-ref j (car p)) (cdr p))]
          [else #f])))

;; abi field is sometimes a JSON-encoded string (combined-json) — normalise
(define (parse-abi a)
  (cond [(string? a) (with-handlers ([exn:fail? (lambda (_) a)]) (string->jsexpr a))]
        [else a]))

;; --- selection among multiple contracts -------------------------------
;; entries : list of (name . contract-jsexpr).  Pick by `name` (matching the
;; part after the last ':' or the whole key), or the unique one.
(define (pick entries name where)
  (cond
    [name
     (or (for/first ([e (in-list entries)]
                     #:when (let ([k (car e)])
                              (or (equal? k name)
                                  (equal? (last (string-split k ":")) name)
                                  (equal? (last (string-split k "/")) name))))
           e)
         (error 'read-artifact "contract ~s not found in ~a; have: ~a"
                name where (map car entries)))]
    [(= 1 (length entries)) (car entries)]
    [else (error 'read-artifact
                 "~a has multiple contracts; pass #:contract; have: ~a"
                 where (map car entries))]))

;; --- main --------------------------------------------------------------
(define (read-artifact path #:contract [name #f])
  (extract (call-with-input-file path read-json) name (format "~a" path)))

(define (read-artifact/string str #:contract [name #f])
  (extract (string->jsexpr str) name "<string>"))

(define (extract j name where)
  (cond
    ;; Foundry / Hardhat: top-level bytecode / deployedBytecode
    [(or (hash-has-key? j 'bytecode) (hash-has-key? j 'deployedBytecode))
     (artifact (or name "")
               (hex->bytes (bc->hex (hash-ref j 'bytecode #f)))
               (hex->bytes (bc->hex (hash-ref j 'deployedBytecode #f)))
               (parse-abi (hash-ref j 'abi #f)) j
               (and (hash? (hash-ref j 'deployedBytecode #f))
                    (jref (hash-ref j 'deployedBytecode) 'sourceMap))
               #f)]
    ;; solc combined-json / standard-json under "contracts"
    [(hash-has-key? j 'contracts)
     (define cs (hash-ref j 'contracts))
     (cond
       ;; combined-json: contracts is a flat map "file:Name" -> {bin, bin-runtime}
       [(for/or ([(_ v) (in-hash cs)]) (and (hash? v) (hash-has-key? v 'bin)))
        (define e (pick (for/list ([(k v) (in-hash cs)]) (cons (symbol->string* k) v)) name where))
        (artifact (car e)
                  (hex->bytes (hash-ref (cdr e) 'bin #f))
                  (hex->bytes (hash-ref (cdr e) 'bin-runtime #f))
                  (parse-abi (hash-ref (cdr e) 'abi #f)) j
                  (hash-ref (cdr e) 'srcmap-runtime #f)
                  (hash-ref j 'sourceList #f))]
       ;; standard-json: contracts is "file" -> {"Name" -> {evm{...}, abi}}
       [else
        (define entries
          (append*
           (for/list ([(_file inner) (in-hash cs)])
             (for/list ([(nm cj) (in-hash inner)]) (cons (symbol->string* nm) cj)))))
        (define e (pick entries name where))
        (artifact (car e)
                  (hex->bytes (jref (cdr e) 'evm 'bytecode 'object))
                  (hex->bytes (jref (cdr e) 'evm 'deployedBytecode 'object))
                  (hash-ref (cdr e) 'abi #f) j
                  (jref (cdr e) 'evm 'deployedBytecode 'sourceMap)
                  (source-list/standard j))])]
    [else (error 'read-artifact "unrecognised artifact JSON (~a)" where)]))

;; standard-json states the file indices as {"sources": {"a.sol": {"id": 0}, …}};
;; the source map refers to them by index, so flatten to a list in index order.
(define (source-list/standard j)
  (define srcs (hash-ref j 'sources #f))
  (and (hash? srcs)
       (let ([ided (for/list ([(f v) (in-hash srcs)] #:when (and (hash? v) (hash-ref v 'id #f)))
                     (cons (hash-ref v 'id) (symbol->string* f)))])
         (and (pair? ided) (map cdr (sort ided < #:key car))))))

(define (symbol->string* k) (if (symbol? k) (symbol->string k) k))

;; ======================================================================
;; ABI encoding (Contract ABI spec: head/tail scheme).
;;
;; Supported types: uint<M>/int<M> (and bare uint/int = 256), address, bool,
;; bytes<M> (fixed 1..32), bytes, string, T[] (dynamic array), T[k] (fixed
;; array), and tuples (T1,T2,...).  Values:
;;   uint/int/address        -> integer          bool -> #t/#f or 1/0
;;   bytes<M> / bytes         -> byte list or Racket bytes
;;   string                   -> Racket string (or byte list)
;;   T[] / T[k]               -> a list of element values
;;   (T1,...)                 -> a list of tuple element values
;; ======================================================================

;; function-selector : "transfer(address,uint256)" -> integer (4 bytes)
(define (function-selector sig)
  (for/fold ([acc 0]) ([b (in-bytes (subbytes (keccak256 (string->bytes/utf-8 sig)) 0 4))])
    (+ (* acc 256) b)))

;; encode-call : signature + argument values -> full calldata byte list
;; (4-byte selector followed by the ABI encoding of the argument tuple).
(define (encode-call sig . args)
  (append (int->bytes (function-selector sig) 4)
          (abi-encode (parse-arg-types sig) args)))

;; --- helpers ----------------------------------------------------------
(define (int->bytes n len)
  (for/list ([i (in-range (sub1 len) -1 -1)]) (bitwise-and (arithmetic-shift n (* -8 i)) #xff)))

;; a 256-bit big-endian word (two's complement for negatives)
(define (word->bytes n)
  (int->bytes (modulo (cond [(eq? n #t) 1] [(eq? n #f) 0] [else n]) (expt 2 256)) 32))

(define (->bytelist x)
  (cond [(bytes? x) (bytes->list x)]
        [(string? x) (bytes->list (string->bytes/utf-8 x))]
        [(list? x) x]
        [else (error 'abi-encode "expected bytes/string/list, got ~e" x)]))

(define (pad-right-32 bs)     ; pad a byte list up to the next multiple of 32
  (define r (modulo (length bs) 32))
  (if (zero? r) bs (append bs (make-list (- 32 r) 0))))

;; --- type inspection --------------------------------------------------
;; split a comma-separated type list at depth 0 (respecting () and [])
(define (split-types s)
  (let loop ([cs (string->list s)] [depth 0] [cur '()] [acc '()])
    (define (flush) (cons (string-trim (list->string (reverse cur))) acc))
    (cond
      [(null? cs) (reverse (if (null? cur) acc (flush)))]
      [else
       (define c (car cs))
       (cond
         [(and (char=? c #\,) (= depth 0)) (loop (cdr cs) depth '() (flush))]
         [(or (char=? c #\() (char=? c #\[)) (loop (cdr cs) (add1 depth) (cons c cur) acc)]
         [(or (char=? c #\)) (char=? c #\])) (loop (cdr cs) (sub1 depth) (cons c cur) acc)]
         [else (loop (cdr cs) depth (cons c cur) acc)])])))

;; parse "name(t1,t2,...)" -> (list "t1" "t2" ...)
(define (parse-arg-types sig)
  (define op (for/first ([c (in-string sig)] [i (in-naturals)] #:when (char=? c #\()) i))
  (if (not op) '()
      (split-types (substring sig (add1 op) (sub1 (string-length sig))))))

;; split a trailing array dimension: "T[k]" -> (values "T" k), "T[]" -> (values "T" #f),
;; non-array -> (values t 'none)
(define (split-array t)
  (cond
    [(and (> (string-length t) 0) (char=? (string-ref t (sub1 (string-length t))) #\]))
     (define lb (for/last ([c (in-string t)] [i (in-naturals)] #:when (char=? c #\[)) i))
     (values (substring t 0 lb)
             (let ([inside (substring t (add1 lb) (sub1 (string-length t)))])
               (if (zero? (string-length inside)) #f (string->number inside))))]
    [else (values t 'none)]))

(define (tuple-type? t)
  (and (> (string-length t) 1)
       (char=? (string-ref t 0) #\() (char=? (string-ref t (sub1 (string-length t))) #\))))
(define (tuple-inner t) (substring t 1 (sub1 (string-length t))))

(define (type-dynamic? t)
  (define-values (base dim) (split-array t))
  (cond
    [(not (eq? dim 'none)) (if dim (type-dynamic? base) #t)]  ; T[]: dynamic; T[k]: iff base dynamic
    [(or (string=? t "bytes") (string=? t "string")) #t]
    [(tuple-type? t) (ormap type-dynamic? (split-types (tuple-inner t)))]
    [else #f]))

;; --- the encoder ------------------------------------------------------
;; abi-encode : (listof type-string) (listof value) -> byte list (tuple head/tail)
(define (abi-encode types values)
  (unless (= (length types) (length values))
    (error 'abi-encode "arity: ~a types vs ~a values" (length types) (length values)))
  (define encs (map abi-encode-type types values))
  (define dyns (map type-dynamic? types))
  (define headlen (for/sum ([d (in-list dyns)] [e (in-list encs)]) (if d 32 (length e))))
  (let loop ([ds dyns] [es encs] [offset headlen] [head '()] [tail '()])
    (cond
      [(null? es) (append (append* (reverse head)) (append* (reverse tail)))]
      [(car ds) (loop (cdr ds) (cdr es) (+ offset (length (car es)))
                      (cons (word->bytes offset) head) (cons (car es) tail))]
      [else (loop (cdr ds) (cdr es) offset (cons (car es) head) tail)])))

;; encode one value at a type: for static types this is the head content; for
;; dynamic types this is the tail content (the offset is placed by abi-encode).
(define (abi-encode-type t v)
  (define-values (base dim) (split-array t))
  (cond
    [(not (eq? dim 'none))
     (cond
       [dim (abi-encode (make-list dim base) v)]                         ; T[k]
       [else (append (word->bytes (length v))                           ; T[]
                     (abi-encode (make-list (length v) base) v))])]
    [(or (string=? t "bytes") (string=? t "string"))
     (define bs (->bytelist v))
     (append (word->bytes (length bs)) (pad-right-32 bs))]
    [(regexp-match? #px"^bytes([1-9]|[12][0-9]|3[0-2])$" t)              ; bytesN (left-aligned)
     (define bs (->bytelist v))
     (append bs (make-list (- 32 (length bs)) 0))]
    [(or (regexp-match? #px"^u?int([0-9]*)$" t) (string=? t "address") (string=? t "bool"))
     (word->bytes v)]
    [(tuple-type? t) (abi-encode (split-types (tuple-inner t)) v)]
    [else (error 'abi-encode "unsupported type ~s" t)]))

;; --- decoder (symmetric to abi-encode) --------------------------------
;; abi-decode : (listof type-string) (byte list) -> (listof value)
;; uint/address -> integer ; int -> signed integer ; bool -> #t/#f ;
;; bytesN/bytes -> byte list ; string -> Racket string ; arrays/tuples -> lists.
(define (abi-decode types bs)
  (decode-tuple types (list->vector bs) 0))

(define (vref vec i) (if (< i (vector-length vec)) (vector-ref vec i) 0))
(define (u256-at vec off) (for/fold ([a 0]) ([i (in-range 32)]) (+ (* a 256) (vref vec (+ off i)))))

(define (static-size t)
  (define-values (base dim) (split-array t))
  (cond
    [(and (not (eq? dim 'none)) dim) (* dim (static-size base))]        ; T[k]
    [(tuple-type? t) (for/sum ([m (in-list (split-types (tuple-inner t)))]) (static-size m))]
    [else 32]))

(define (decode-tuple types vec base)
  (let loop ([ts types] [cursor base] [acc '()])
    (cond
      [(null? ts) (reverse acc)]
      [(type-dynamic? (car ts))
       (loop (cdr ts) (+ cursor 32)
             (cons (decode-dynamic (car ts) vec (+ base (u256-at vec cursor))) acc))]
      [else
       (loop (cdr ts) (+ cursor (static-size (car ts)))
             (cons (decode-static (car ts) vec cursor) acc))])))

(define (decode-static t vec pos)
  (define-values (base dim) (split-array t))
  (cond
    [(and (not (eq? dim 'none)) dim) (decode-tuple (make-list dim base) vec pos)]  ; T[k] static
    [(tuple-type? t) (decode-tuple (split-types (tuple-inner t)) vec pos)]
    [(regexp-match #px"^bytes([1-9]|[12][0-9]|3[0-2])$" t)
     => (lambda (m) (for/list ([i (in-range (string->number (cadr m)))]) (vref vec (+ pos i))))]
    [(regexp-match? #px"^int([0-9]*)$" t)
     (define u (u256-at vec pos)) (if (>= u (expt 2 255)) (- u (expt 2 256)) u)]
    [(string=? t "bool") (not (zero? (u256-at vec pos)))]
    [else (u256-at vec pos)]))   ; uint / address

(define (decode-dynamic t vec pos)
  (define-values (base dim) (split-array t))
  (cond
    [(not (eq? dim 'none))
     (if dim (decode-tuple (make-list dim base) vec pos)                ; T[k], dynamic base
         (decode-tuple (make-list (u256-at vec pos) base) vec (+ pos 32)))]  ; T[]
    [(or (string=? t "bytes") (string=? t "string"))
     (define n (u256-at vec pos))
     (define raw (for/list ([i (in-range n)]) (vref vec (+ pos 32 i))))
     (if (string=? t "string") (bytes->string/utf-8 (list->bytes raw) #\?) raw)]
    [(tuple-type? t) (decode-tuple (split-types (tuple-inner t)) vec pos)]
    [else (error 'abi-decode "unexpected dynamic type ~s" t)]))
