#lang racket/base

;; Minimal RLP encoding (Recursive Length Prefix).
;;
;; Mirrors execution-specs `ethereum.rlp`.  Used by CREATE for contract-address
;; computation now; will back the Merkle-Patricia trie in M5.
;;
;; `rlp` accepts:
;;   - a `bytes` value            -> encoded as a byte string
;;   - a non-negative integer     -> encoded as its minimal big-endian bytes
;;   - a list of the above        -> encoded as an RLP list

(require racket/list)

(provide rlp rlp-encode-bytes uint->be)

;; minimal big-endian encoding of a non-negative integer (0 -> empty)
(define (uint->be n)
  (if (zero? n)
      (bytes)
      (let loop ([n n] [acc '()])
        (if (zero? n)
            (apply bytes acc)
            (loop (arithmetic-shift n -8) (cons (bitwise-and n #xff) acc))))))

(define (length-prefix len short-base long-base)
  (if (< len 56)
      (bytes (+ short-base len))
      (let ([lb (uint->be len)])
        (bytes-append (bytes (+ long-base (bytes-length lb))) lb))))

;; encode a raw byte string
(define (rlp-encode-bytes bs)
  (cond
    [(and (= (bytes-length bs) 1) (< (bytes-ref bs 0) #x80)) bs]
    [else (bytes-append (length-prefix (bytes-length bs) #x80 #xb7) bs)]))

;; encode a list of already-encoded items (payload = their concatenation)
(define (rlp-encode-list items)
  (define payload (apply bytes-append items))
  (bytes-append (length-prefix (bytes-length payload) #xc0 #xf7) payload))

(define (rlp x)
  (cond
    [(bytes? x) (rlp-encode-bytes x)]
    [(exact-nonnegative-integer? x) (rlp-encode-bytes (uint->be x))]
    [(list? x) (rlp-encode-list (map rlp x))]
    [else (error 'rlp "cannot encode ~a" x)]))
