#lang racket/base

;; Merkle-Patricia Trie root computation.
;;
;; Mirrors execution-specs `ethereum.trie` (patricialize / encode_internal_node /
;; root).  We only need the *root hash*, so nodes are never persisted — they are
;; built recursively and collapsed to a 32-byte keccak (or inlined when their RLP
;; is < 32 bytes).
;;
;; `trie-root` takes an assoc list of (key-bytes . value-bytes).  With secured?=#t
;; (the default, used for the state and storage tries) keys are keccak256-hashed
;; first.  Values are already-RLP-encoded byte strings.

(require racket/list
         "keccak.rkt"
         "rlp.rkt")

(provide trie-root EMPTY-TRIE-ROOT bytes->nibbles)

;; keccak256(rlp(b"")) — the canonical empty-trie root
(define EMPTY-TRIE-ROOT (keccak256 (rlp (bytes))))

;; a byte string -> list of nibbles (high nibble first)
(define (bytes->nibbles bs)
  (append-map (lambda (b) (list (arithmetic-shift b -4) (bitwise-and b #xf)))
              (bytes->list bs)))

;; hex-prefix (compact) encoding of a nibble list
(define (nibbles->compact nibbles leaf?)
  (define flag (if leaf? 2 0))
  (define is-odd? (odd? (length nibbles)))
  (define-values (first-byte rest)
    (if is-odd?
        (values (+ (* 16 (+ flag 1)) (car nibbles)) (cdr nibbles))
        (values (* 16 flag) nibbles)))
  (apply bytes first-byte
         (let loop ([ns rest])
           (if (null? ns) '()
               (cons (+ (* 16 (car ns)) (cadr ns)) (loop (cddr ns)))))))

(define (common-prefix-length a b)
  (let loop ([a a] [b b] [n 0])
    (if (and (pair? a) (pair? b) (= (car a) (car b)))
        (loop (cdr a) (cdr b) (add1 n))
        n)))

;; node ::= (leaf nibbles value) | (ext nibbles ref) | (branch (ref ...16) value)
;; encode-internal-node : node-or-#f -> rlp-extended (bytes32 hash, or inline structure)
(define (encode-internal node)
  (define unencoded
    (cond
      [(not node) (bytes)]
      [(eq? (car node) 'leaf)   (list (nibbles->compact (cadr node) #t) (caddr node))]
      [(eq? (car node) 'ext)    (list (nibbles->compact (cadr node) #f) (caddr node))]
      [(eq? (car node) 'branch) (append (cadr node) (list (caddr node)))]))
  (define encoded (rlp unencoded))
  (if (< (bytes-length encoded) 32) unencoded (keccak256 encoded)))

;; obj : list of (nibble-list . value-bytes)
(define (patricialize obj level)
  (cond
    [(null? obj) #f]
    [(null? (cdr obj))
     (list 'leaf (drop (caar obj) level) (cdar obj))]
    [else
     (define substring (drop (caar obj) level))
     (define prefix-len
       (for/fold ([pl (length substring)]) ([p (in-list obj)] #:break (zero? pl))
         (min pl (common-prefix-length substring (drop (car p) level)))))
     (cond
       [(> prefix-len 0)
        (list 'ext (take substring prefix-len)
              (encode-internal (patricialize obj (+ level prefix-len))))]
       [else
        (define branches (make-vector 16 '()))
        (define value (bytes))
        (for ([p (in-list obj)])
          (if (= (length (car p)) level)
              (set! value (cdr p))
              (let ([nib (list-ref (car p) level)])
                (vector-set! branches nib (cons p (vector-ref branches nib))))))
        (list 'branch
              (for/list ([k (in-range 16)])
                (encode-internal (patricialize (reverse (vector-ref branches k)) (add1 level))))
              value)])]))

(define (trie-root pairs #:secured? [secured? #t])
  (define obj
    (map (lambda (p)
           (cons (bytes->nibbles (if secured? (keccak256 (car p)) (car p))) (cdr p)))
         pairs))
  (define node (patricialize obj 0))
  (if (not node)
      EMPTY-TRIE-ROOT
      (let ([enc (encode-internal node)])
        (if (bytes? enc) enc (keccak256 (rlp enc))))))
