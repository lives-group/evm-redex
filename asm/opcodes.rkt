#lang racket/base

;; ======================================================================
;; The canonical opcode table: mnemonic <-> byte, in both directions, plus the
;; immediate size of each PUSH.  The library did not have one — `exec-racket`
;; dispatches on the raw byte and the names live only in comments
;; (interpreter.rkt, forks.rkt:96-116) — so an assembler needs to bring its own.
;; This is the single source of truth; `mnemonic->opcode` and `opcode->mnemonic`
;; are both derived from OPCODES below.
;;
;; The set mirrors exactly what `exec-racket` implements
;; (private/interpreter.rkt:1305-1399): anything not here is not a real opcode
;; and the assembler rejects it (the interpreter would halt `invalid-opcode`).
;; ======================================================================

(require racket/list)

(provide (struct-out opinfo)
         mnemonic->opinfo opcode->mnemonic known-mnemonic?
         push-immediate-size all-mnemonics)

;; name    : the canonical uppercase mnemonic (symbol)
;; byte    : the opcode byte
;; imm     : bytes of inline immediate data the opcode consumes (PUSH1..32 -> 1..32,
;;           everything else 0)
(struct opinfo (name byte imm) #:transparent)

;; --- the base table (non-PUSH/DUP/SWAP; those ranges are generated) ----
(define BASE
  '((STOP           #x00) (ADD            #x01) (MUL            #x02) (SUB          #x03)
    (DIV            #x04) (SDIV           #x05) (MOD            #x06) (SMOD         #x07)
    (ADDMOD         #x08) (MULMOD         #x09) (EXP            #x0a) (SIGNEXTEND   #x0b)
    (LT             #x10) (GT             #x11) (SLT            #x12) (SGT          #x13)
    (EQ             #x14) (ISZERO         #x15) (AND            #x16) (OR           #x17)
    (XOR            #x18) (NOT            #x19) (BYTE           #x1a) (SHL          #x1b)
    (SHR            #x1c) (SAR            #x1d)
    (KECCAK256      #x20)
    (ADDRESS        #x30) (BALANCE        #x31) (ORIGIN         #x32) (CALLER       #x33)
    (CALLVALUE      #x34) (CALLDATALOAD   #x35) (CALLDATASIZE   #x36) (CALLDATACOPY #x37)
    (CODESIZE       #x38) (CODECOPY       #x39) (GASPRICE       #x3a) (EXTCODESIZE  #x3b)
    (EXTCODECOPY    #x3c) (RETURNDATASIZE #x3d) (RETURNDATACOPY #x3e) (EXTCODEHASH  #x3f)
    (BLOCKHASH      #x40) (COINBASE       #x41) (TIMESTAMP      #x42) (NUMBER       #x43)
    (PREVRANDAO     #x44) (GASLIMIT       #x45) (CHAINID        #x46) (SELFBALANCE  #x47)
    (BASEFEE        #x48) (BLOBHASH       #x49) (BLOBBASEFEE    #x4a)
    (POP            #x50) (MLOAD          #x51) (MSTORE         #x52) (MSTORE8      #x53)
    (SLOAD          #x54) (SSTORE         #x55) (JUMP           #x56) (JUMPI        #x57)
    (PC             #x58) (MSIZE          #x59) (GAS            #x5a) (JUMPDEST     #x5b)
    (TLOAD          #x5c) (TSTORE         #x5d) (MCOPY          #x5e) (PUSH0        #x5f)
    (LOG0           #xa0) (LOG1           #xa1) (LOG2           #xa2) (LOG3         #xa3)
    (LOG4           #xa4)
    (CREATE         #xf0) (CALL           #xf1) (CALLCODE       #xf2) (RETURN       #xf3)
    (DELEGATECALL   #xf4) (CREATE2        #xf5) (STATICCALL     #xfa) (REVERT       #xfd)
    (INVALID        #xfe) (SELFDESTRUCT   #xff)))

;; --- the full table, with the three generated ranges ------------------
(define OPCODES
  (append
   (for/list ([e (in-list BASE)]) (opinfo (car e) (cadr e) 0))
   ;; PUSH1..PUSH32 : 0x60..0x7f, immediate size 1..32
   (for/list ([k (in-range 1 33)]) (opinfo (string->symbol (format "PUSH~a" k)) (+ #x5f k) k))
   ;; DUP1..DUP16 : 0x80..0x8f
   (for/list ([k (in-range 1 17)]) (opinfo (string->symbol (format "DUP~a" k))  (+ #x7f k) 0))
   ;; SWAP1..SWAP16 : 0x90..0x9f
   (for/list ([k (in-range 1 17)]) (opinfo (string->symbol (format "SWAP~a" k)) (+ #x8f k) 0))))

(define BY-NAME (for/hasheq ([o (in-list OPCODES)]) (values (opinfo-name o) o)))
(define BY-BYTE (for/hasheqv ([o (in-list OPCODES)]) (values (opinfo-byte o) o)))

;; mnemonic->opinfo : symbol -> opinfo or #f.  Case-insensitive: the caller may
;; pass `add`, `Add`, or `ADD`.
(define (mnemonic->opinfo sym)
  (hash-ref BY-NAME (string->symbol (string-upcase (symbol->string sym))) #f))

(define (known-mnemonic? sym) (and (mnemonic->opinfo sym) #t))

;; opcode->mnemonic : byte -> symbol or #f (for the disassembler)
(define (opcode->mnemonic byte)
  (define o (hash-ref BY-BYTE byte #f))
  (and o (opinfo-name o)))

;; push-immediate-size : byte -> 1..32 for a PUSH opcode, else 0
(define (push-immediate-size byte)
  (if (and (>= byte #x60) (<= byte #x7f)) (- byte #x5f) 0))

(define (all-mnemonics) (sort (hash-keys BY-NAME) symbol<?))
