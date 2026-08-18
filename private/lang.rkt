#lang racket/base

;; The Redex grammar of EVM configurations.
;;
;; This is the syntactic backbone shared by every later layer (interpreter,
;; message, transaction).  The `Machine` non-terminal mirrors the fields of the
;; `Evm` dataclass in execution-specs `vm/__init__.py`; `World` mirrors
;; `state.py`.  Words are plain naturals interpreted mod 2^256 by
;; `private/words.rkt` — the grammar does NOT enforce the 256-bit bound, the
;; metafunctions do.
;;
;; Convention: structured non-terminals are Capitalized so that the lowercase
;; constructor tags (`machine`, `msg`, `account`, `stack`, `halt`, ...) are
;; treated by Redex as literals rather than as same-named non-terminals.

(require redex/reduction-semantics)

(provide evm)

(define-language evm
  ;; --- scalars ---------------------------------------------------------
  [w        ::= natural]            ; a 256-bit word (wrapped by words.rkt)
  [byte     ::= natural]            ; 0..255
  [addr     ::= natural]            ; 160-bit address as a natural
  [bool     ::= #t #f]

  ;; --- byte sequences (code, memory, calldata, return data) ------------
  ;; Byte lists at runtime, but `any` in the grammar: they are only ever read
  ;; through Racket helpers (never destructured by a rule's pattern), and
  ;; validating them as (byte ...) on every metafunction call costs O(bytes) PER
  ;; STEP -- ruinous for KB-sized contract code / return data.  (Tier 2)
  [Bytes    ::= any]               ; conceptually (byte ...)
  [Code     ::= any]               ; conceptually (byte ...)
  ;; Conceptually a flat, zero-extended byte vector.  Operationally represented
  ;; as an immutable Racket `bytes` (O(1) indexed access) rather than a byte
  ;; list, so the non-terminal is `any`; all access goes through private/mem.rkt.
  [Mem      ::= any]                ; immutable bytes; see private/mem.rkt

  ;; --- stack: top element first ----------------------------------------
  [Stack    ::= (w ...)]

  ;; --- logs ------------------------------------------------------------
  [Log      ::= (log addr (w ...) Bytes)]      ; (log address topics data)
  [Logs     ::= (Log ...)]

  ;; --- world state -----------------------------------------------------
  ;; Store and World are conceptually the association lists shown below, but are
  ;; operationally represented as immutable Racket hash tables (O(1) get/set)
  ;; keyed by word / address; hence `any`.  All access goes through
  ;; private/state.rkt, which also exposes storage->alist / world->alist for the
  ;; state-root code that must iterate them.
  [Store    ::= any]                ; conceptually ((w w) ...), non-zero entries; a hasheqv
  [Acct     ::= (account w w Code Store Store)] ; nonce balance code storage transient
  [World    ::= any]                ; conceptually ((addr Acct) ...); a hasheqv

  ;; --- access lists (EIP-2929/2930) ------------------------------------
  [AAddrs   ::= (addr ...)]
  [AKeys    ::= ((addr w) ...)]

  ;; --- message (a single call frame's input) ---------------------------
  ;; (msg caller target value data code-addr gas depth static?)
  [Msg      ::= (msg addr addr w Bytes addr w natural bool)]

  ;; --- transaction environment -----------------------------------------
  ;; (tx origin gasprice blob-versioned-hashes)
  [TxEnv    ::= (tx addr w (w ...))]

  ;; --- halt / running status -------------------------------------------
  [Halt     ::= running
                (stop)
                (return Bytes)
                (revert Bytes)
                Exn]
  [Exn      ::= (out-of-gas)
                (stack-underflow)
                (stack-overflow)
                (invalid-opcode byte)
                (invalid-jump)
                (out-of-bounds)
                (stack-depth-limit)
                (write-in-static)]

  ;; --- machine configuration (one call frame) --------------------------
  ;; Mirrors the Evm dataclass; tagged fields for readability.
  [Machine  ::= (machine
                 (pc w)
                 (gas w)
                 (stack Stack)
                 (memory Mem)
                 (code Code)
                 (return-data Bytes)
                 (halt Halt)
                 (logs Logs)
                 (refund integer)
                 (accessed AAddrs AKeys)
                 (msg Msg)
                 (world World)
                 (block Block)
                 (tx TxEnv)
                 (orig-storage Store))]

  ;; --- block / transaction environment ---------------------------------
  ;; (block number timestamp coinbase basefee blobbasefee prevrandao gaslimit chainid fork)
  ;; The trailing `fork` tag records the active hardfork.  It makes machine terms
  ;; distinct per fork so Redex's term-keyed metafunction cache never returns a
  ;; cross-fork result — critical because `exec` reduction has tx-level side
  ;; effects (created/deleted/touched sets) that a cache hit would skip.  (M12)
  [Block    ::= (block w w addr w w w w w any)])
