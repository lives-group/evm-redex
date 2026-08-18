#lang racket/base

;; The intra-call interpreter: a Redex reduction relation `evm-step` over
;; `Machine` configurations, plus a deterministic `run` driver.
;;
;; Mirrors the dispatch loop of execution-specs `vm/interpreter.py`
;; (`execute_code`): while running and pc < len(code), fetch the opcode, charge
;; gas, apply its effect, advance the pc.  Milestone M1 implements a subset of
;; opcodes (arithmetic, comparison, bitwise, stack, PUSH/DUP/SWAP, JUMP family,
;; MLOAD/MSTORE, STOP/RETURN); unimplemented opcodes halt with `invalid-opcode`.

(require redex/reduction-semantics
         racket/list
         "lang.rkt"
         "words.rkt"
         "gas.rkt"
         "forks.rkt"
         "mem.rkt"
         "state.rkt"
         "keccak.rkt"
         "rlp.rkt"
         "precompiles.rkt")

(provide evm-step run run-trace fresh-machine
         machine-field machine-halt machine-result
         ;; message / transaction support (used by transaction.rkt)
         build-child run-top-frame transfer resolve-code
         compute-create-address compute-create2-address
         reset-tx-state! tx-deleted tx-touched mark-touched! mark-created! tx-created
         orig-storage-for
         ;; fast Racket executor + Redex reference, for the equivalence oracle
         exec-racket exec-spec exec-oracle-step pure-styleA-op?
         current-exec-backend
         ;; tracing hooks (what coverage / trace tools are built on)
         current-frame-tracer current-call-observer)

;; ---- transaction-level mutable accumulators ---------------------------
;; Reset once per transaction.  `the-tx-world` is the storage snapshot taken at
;; transaction start, used to read SSTORE "original" values (EIP-2200).  The
;; created/deleted/touched sets back EIP-6780 (selfdestruct) and EIP-161 (empty
;; account deletion).  do-call / do-create snapshot created+deleted around a
;; child run and restore them on revert/exception.
(define the-tx-world (box '()))
(define the-created   (box '()))
(define the-deleted   (box '()))
(define the-touched   (box '()))

(define (reset-tx-state! world)
  (set-box! the-tx-world world)
  (set-box! the-created '()) (set-box! the-deleted '()) (set-box! the-touched '()))
(define (tx-created)  (unbox the-created))
(define (tx-deleted)  (unbox the-deleted))
(define (tx-touched)  (unbox the-touched))
(define (mark-touched! addr)
  (unless (member addr (unbox the-touched)) (set-box! the-touched (cons addr (unbox the-touched)))))
(define (mark-created! addr)
  (unless (member addr (unbox the-created)) (set-box! the-created (cons addr (unbox the-created)))))

;; SSTORE "original" storage of `target`: from the tx-start snapshot if set,
;; else the account's current storage (standalone interpreter use).
(define (orig-storage-for target world)
  (if (null? (unbox the-tx-world))
      (acct-storage (world-ref world target))
      (acct-storage (world-ref (unbox the-tx-world) target))))

;; fresh-machine : code(list of bytes) gas [world] [msg] [block] [tx] -> Machine
;; A minimal top-level call frame for testing the interpreter in isolation.
(define (fresh-machine code gas
                       #:world [world '()]
                       #:msg [msg #f]
                       #:block [block '(block 0 0 0 0 0 0 30000000 1 Prague)]
                       #:tx [tx '(tx 0 0 ())]
                       #:orig-storage [orig '()])
  (term (machine (pc 0) (gas ,gas) (stack ()) (memory ()) (code ,code)
                 (return-data ()) (halt running) (logs ()) (refund 0)
                 (accessed () ()) (msg ,(or msg `(msg 0 0 0 () 0 ,gas 0 #f)))
                 (world ,world) (block ,block) (tx ,tx) (orig-storage ,orig))))

;; ======================================================================
;; Field accessors (Redex metafunctions, for use in rule right-hand sides)
;; ======================================================================
(define-metafunction evm
  pc-of : Machine -> w
  [(pc-of (machine (pc w) any ...)) w])

(define-metafunction evm
  gas-of : Machine -> w
  [(gas-of (machine any_1 (gas w) any ...)) w])

(define-metafunction evm
  stack-of : Machine -> Stack
  [(stack-of (machine any_1 any_2 (stack Stack) any ...)) Stack])

(define-metafunction evm
  mem-of : Machine -> Mem
  [(mem-of (machine any_1 any_2 any_3 (memory Mem) any ...)) Mem])

(define-metafunction evm
  code-of : Machine -> Code
  [(code-of (machine any_1 any_2 any_3 any_4 (code Code) any ...)) Code])

(define-metafunction evm
  halt-of : Machine -> Halt
  [(halt-of (machine any_1 any_2 any_3 any_4 any_5 any_6 (halt Halt) any ...)) Halt])

;; ======================================================================
;; Field updaters (functional record update)
;; ======================================================================
(define-metafunction evm
  with-pc : Machine w -> Machine
  [(with-pc (machine (pc _) any ...) w) (machine (pc w) any ...)])

(define-metafunction evm
  with-gas : Machine w -> Machine
  [(with-gas (machine any_1 (gas _) any ...) w) (machine any_1 (gas w) any ...)])

(define-metafunction evm
  with-stack : Machine Stack -> Machine
  [(with-stack (machine any_1 any_2 (stack _) any ...) Stack)
   (machine any_1 any_2 (stack Stack) any ...)])

(define-metafunction evm
  with-mem : Machine Mem -> Machine
  [(with-mem (machine any_1 any_2 any_3 (memory _) any ...) Mem)
   (machine any_1 any_2 any_3 (memory Mem) any ...)])

(define-metafunction evm
  with-halt : Machine Halt -> Machine
  [(with-halt (machine any_1 any_2 any_3 any_4 any_5 any_6 (halt _) any ...) Halt)
   (machine any_1 any_2 any_3 any_4 any_5 any_6 (halt Halt) any ...)])

;; advance pc by 1 (or by n)
(define-metafunction evm
  advance : Machine -> Machine
  [(advance Machine) (with-pc Machine ,(add1 (term (pc-of Machine))))])

(define-metafunction evm
  advance-by : Machine natural -> Machine
  [(advance-by Machine natural) (with-pc Machine ,(+ (term (pc-of Machine)) (term natural)))])

;; ======================================================================
;; Pure operator tables (escape to words.rkt)
;; ======================================================================
(define-metafunction evm
  apply-op : any w w -> w
  [(apply-op add  w_a w_b) ,(u-add  (term w_a) (term w_b))]
  [(apply-op sub  w_a w_b) ,(u-sub  (term w_a) (term w_b))]
  [(apply-op mul  w_a w_b) ,(u-mul  (term w_a) (term w_b))]
  [(apply-op div  w_a w_b) ,(u-div  (term w_a) (term w_b))]
  [(apply-op sdiv w_a w_b) ,(u-sdiv (term w_a) (term w_b))]
  [(apply-op mod  w_a w_b) ,(u-mod  (term w_a) (term w_b))]
  [(apply-op smod w_a w_b) ,(u-smod (term w_a) (term w_b))]
  [(apply-op lt   w_a w_b) ,(u-lt   (term w_a) (term w_b))]
  [(apply-op gt   w_a w_b) ,(u-gt   (term w_a) (term w_b))]
  [(apply-op slt  w_a w_b) ,(u-slt  (term w_a) (term w_b))]
  [(apply-op sgt  w_a w_b) ,(u-sgt  (term w_a) (term w_b))]
  [(apply-op eq   w_a w_b) ,(u-eq   (term w_a) (term w_b))]
  [(apply-op and  w_a w_b) ,(u-and  (term w_a) (term w_b))]
  [(apply-op or   w_a w_b) ,(u-or   (term w_a) (term w_b))]
  [(apply-op xor  w_a w_b) ,(u-xor  (term w_a) (term w_b))])

(define-metafunction evm
  apply-uop : any w -> w
  [(apply-uop iszero w) ,(u-iszero (term w))]
  [(apply-uop not    w) ,(u-not    (term w))])

;; ======================================================================
;; Generic stack operations with gas + underflow handling
;; ======================================================================
;; binary op: pop 2, push 1
(define-metafunction evm
  binop : Machine natural any -> Machine
  [(binop Machine natural_c any_op)                ; success
   (advance (with-gas (with-stack Machine (w_res w ...))
                      ,(- (term w_g) (term natural_c))))
   (where (w_a w_b w ...) (stack-of Machine))
   (where w_g (gas-of Machine))
   (side-condition (>= (term w_g) (term natural_c)))
   (where w_res (apply-op any_op w_a w_b))]
  [(binop Machine natural_c any_op)                ; operands present, gas short
   (with-halt Machine (out-of-gas))
   (where (w_a w_b w ...) (stack-of Machine))]
  [(binop Machine natural_c any_op)                ; underflow
   (with-halt Machine (stack-underflow))])

;; unary op: pop 1, push 1
(define-metafunction evm
  unop : Machine natural any -> Machine
  [(unop Machine natural_c any_op)
   (advance (with-gas (with-stack Machine (w_res w ...))
                      ,(- (term w_g) (term natural_c))))
   (where (w_a w ...) (stack-of Machine))
   (where w_g (gas-of Machine))
   (side-condition (>= (term w_g) (term natural_c)))
   (where w_res (apply-uop any_op w_a))]
  [(unop Machine natural_c any_op)
   (with-halt Machine (out-of-gas))
   (where (w_a w ...) (stack-of Machine))]
  [(unop Machine natural_c any_op)
   (with-halt Machine (stack-underflow))])

;; ======================================================================
;; Opcode dispatch
;; ======================================================================
(define-metafunction evm
  exec : byte Machine -> Machine

  ;; --- 0x00 STOP -------------------------------------------------------
  [(exec 0 Machine) (with-halt Machine (stop))]

  ;; --- arithmetic ------------------------------------------------------
  [(exec 1  Machine) (binop Machine 3 add)]
  [(exec 2  Machine) (binop Machine 5 mul)]
  [(exec 3  Machine) (binop Machine 3 sub)]
  [(exec 4  Machine) (binop Machine 5 div)]
  [(exec 5  Machine) (binop Machine 5 sdiv)]
  [(exec 6  Machine) (binop Machine 5 mod)]
  [(exec 7  Machine) (binop Machine 5 smod)]

  ;; --- comparison / bitwise -------------------------------------------
  [(exec 16 Machine) (binop Machine 3 lt)]
  [(exec 17 Machine) (binop Machine 3 gt)]
  [(exec 18 Machine) (binop Machine 3 slt)]
  [(exec 19 Machine) (binop Machine 3 sgt)]
  [(exec 20 Machine) (binop Machine 3 eq)]
  [(exec 21 Machine) (unop  Machine 3 iszero)]
  [(exec 22 Machine) (binop Machine 3 and)]
  [(exec 23 Machine) (binop Machine 3 or)]
  [(exec 24 Machine) (binop Machine 3 xor)]
  [(exec 25 Machine) (unop  Machine 3 not)]

  ;; --- 0x50 POP --------------------------------------------------------
  [(exec 80 Machine)
   (advance (with-gas (with-stack Machine (w ...)) ,(- (term w_g) GAS-BASE)))
   (where (w_top w ...) (stack-of Machine))
   (where w_g (gas-of Machine))
   (side-condition (>= (term w_g) GAS-BASE))]
  [(exec 80 Machine) (with-halt Machine (stack-underflow))]

  ;; --- 0x51 MLOAD ------------------------------------------------------
  [(exec 81 Machine)
   (advance (with-gas (with-mem (with-stack Machine (w_val w ...)) Mem_new)
                      ,(- (term w_g) (term natural_cost))))
   (where (w_off w ...) (stack-of Machine))
   (where Mem (mem-of Machine))
   (where natural_cost
          ,(+ GAS-VERY-LOW (memory-access-cost (mem-size (term Mem)) (term w_off) 32)))
   (where w_g (gas-of Machine))
   (side-condition (>= (term w_g) (term natural_cost)))
   (where w_val ,(bytes->word (list->bytes (mem-read (term Mem) (term w_off) 32))))
   (where Mem_new ,(mem-touch (term Mem) (term w_off) 32))]
  [(exec 81 Machine) (mload-fail Machine)]

  ;; --- 0x52 MSTORE -----------------------------------------------------
  [(exec 82 Machine)
   (advance (with-gas (with-mem (with-stack Machine (w ...)) Mem_new)
                      ,(- (term w_g) (term natural_cost))))
   (where (w_off w_val w ...) (stack-of Machine))
   (where Mem (mem-of Machine))
   (where natural_cost
          ,(+ GAS-VERY-LOW (memory-access-cost (mem-size (term Mem)) (term w_off) 32)))
   (where w_g (gas-of Machine))
   (side-condition (>= (term w_g) (term natural_cost)))
   (where Mem_new ,(mem-write (term Mem) (term w_off) (bytes->list (word->bytes32 (term w_val)))))]
  [(exec 82 Machine) (mstore-fail Machine)]

  ;; --- 0x56 JUMP -------------------------------------------------------
  [(exec 86 Machine)
   (with-gas (with-pc (with-stack Machine (w ...)) w_dest) ,(- (term w_g) GAS-MID))
   (where (w_dest w ...) (stack-of Machine))
   (where w_g (gas-of Machine))
   (side-condition (>= (term w_g) GAS-MID))
   (side-condition (valid-jump? (term (code-of Machine)) (term w_dest)))]
  [(exec 86 Machine)
   (with-halt Machine (invalid-jump))
   (where (w_dest w ...) (stack-of Machine))]
  [(exec 86 Machine) (with-halt Machine (stack-underflow))]

  ;; --- 0x57 JUMPI ------------------------------------------------------
  [(exec 87 Machine)                              ; taken (cond != 0, valid dest)
   (with-gas (with-pc (with-stack Machine (w ...)) w_dest) ,(- (term w_g) GAS-HIGH))
   (where (w_dest w_cond w ...) (stack-of Machine))
   (where w_g (gas-of Machine))
   (side-condition (>= (term w_g) GAS-HIGH))
   (side-condition (not (zero? (term w_cond))))
   (side-condition (valid-jump? (term (code-of Machine)) (term w_dest)))]
  [(exec 87 Machine)                              ; not taken (cond == 0)
   (advance (with-gas (with-stack Machine (w ...)) ,(- (term w_g) GAS-HIGH)))
   (where (w_dest w_cond w ...) (stack-of Machine))
   (where w_g (gas-of Machine))
   (side-condition (>= (term w_g) GAS-HIGH))
   (side-condition (zero? (term w_cond)))]
  [(exec 87 Machine)                              ; taken but invalid dest
   (with-halt Machine (invalid-jump))
   (where (w_dest w_cond w ...) (stack-of Machine))
   (where w_g (gas-of Machine))
   (side-condition (>= (term w_g) GAS-HIGH))]
  [(exec 87 Machine) (with-halt Machine (stack-underflow))]

  ;; --- 0x58 PC ---------------------------------------------------------
  [(exec 88 Machine)
   (advance (with-gas (with-stack Machine (w_pc w ...)) ,(- (term w_g) GAS-BASE)))
   (where (w ...) (stack-of Machine))
   (where w_pc (pc-of Machine))
   (where w_g (gas-of Machine))
   (side-condition (>= (term w_g) GAS-BASE))
   (side-condition (<= (add1 (length (term (w ...)))) 1024))]

  ;; --- 0x5b JUMPDEST ---------------------------------------------------
  [(exec 91 Machine)
   (advance (with-gas Machine ,(- (term w_g) GAS-JUMPDEST)))
   (where w_g (gas-of Machine))
   (side-condition (>= (term w_g) GAS-JUMPDEST))]

  ;; --- 0x60..0x7f PUSH1..PUSH32 ---------------------------------------
  [(exec byte Machine) (push-op Machine ,(- (term byte) 95))
   (side-condition (and (>= (term byte) 96) (<= (term byte) 127)))]

  ;; --- 0x80..0x8f DUP1..DUP16 -----------------------------------------
  [(exec byte Machine) (dup-op Machine ,(- (term byte) 127))
   (side-condition (and (>= (term byte) 128) (<= (term byte) 143)))]

  ;; --- 0x90..0x9f SWAP1..SWAP16 ---------------------------------------
  [(exec byte Machine) (swap-op Machine ,(- (term byte) 143))
   (side-condition (and (>= (term byte) 144) (<= (term byte) 159)))]

  ;; --- 0xf3 RETURN -----------------------------------------------------
  [(exec 243 Machine)
   (with-halt (with-gas (with-mem (with-stack Machine (w ...)) Mem_new)
                        ,(- (term w_g) (term natural_cost)))
              (return Bytes_out))
   (where (w_off w_len w ...) (stack-of Machine))
   (where Mem (mem-of Machine))
   (where natural_cost
          ,(memory-access-cost (mem-size (term Mem)) (term w_off) (term w_len)))
   (where w_g (gas-of Machine))
   (side-condition (>= (term w_g) (term natural_cost)))
   (where Mem_new ,(mem-touch (term Mem) (term w_off) (term w_len)))
   (where Bytes_out ,(mem-read (term Mem) (term w_off) (term w_len)))]
  [(exec 243 Machine) (with-halt Machine (stack-underflow))]

  ;; --- extra arithmetic / bitwise -------------------------------------
  [(exec 8  Machine) ,(stack-op (term Machine) 3 8 (lambda (a b n) (list (u-addmod a b n))))]
  [(exec 9  Machine) ,(stack-op (term Machine) 3 8 (lambda (a b n) (list (u-mulmod a b n))))]
  [(exec 10 Machine) ,(exp-op (term Machine))]
  [(exec 11 Machine) ,(stack-op (term Machine) 2 5 (lambda (b x) (list (u-signextend b x))))]
  [(exec 26 Machine) ,(stack-op (term Machine) 2 3 (lambda (i x) (list (u-byte i x))))]
  [(exec 27 Machine) ,(stack-op (term Machine) 2 3 (lambda (sh v) (list (u-shl sh v))))]
  [(exec 28 Machine) ,(stack-op (term Machine) 2 3 (lambda (sh v) (list (u-shr sh v))))]
  [(exec 29 Machine) ,(stack-op (term Machine) 2 3 (lambda (sh v) (list (u-sar sh v))))]

  ;; --- 0x20 KECCAK256 --------------------------------------------------
  [(exec 32 Machine) ,(keccak-op (term Machine))]

  ;; --- environment ----------------------------------------------------
  [(exec 48 Machine) ,(let ([m (term Machine)]) (stack-op m 0 GAS-BASE (lambda () (list (current-target m)))))]
  [(exec 49 Machine) ,(extaccount-op (term Machine) (lambda (w a) (acct-balance (world-ref w a))))]
  [(exec 50 Machine) ,(let ([m (term Machine)]) (stack-op m 0 GAS-BASE (lambda () (list (tx-origin m)))))]
  [(exec 51 Machine) ,(let ([m (term Machine)]) (stack-op m 0 GAS-BASE (lambda () (list (msg-caller m)))))]
  [(exec 52 Machine) ,(let ([m (term Machine)]) (stack-op m 0 GAS-BASE (lambda () (list (msg-value m)))))]
  [(exec 53 Machine) ,(let ([m (term Machine)]) (stack-op m 1 GAS-VERY-LOW (lambda (off) (list (list-load32 (msg-data m) off)))))]
  [(exec 54 Machine) ,(let ([m (term Machine)]) (stack-op m 0 GAS-BASE (lambda () (list (length (msg-data m))))))]
  [(exec 55 Machine) ,(let ([m (term Machine)]) (copy3-op m (lambda (off len) (code-slice (msg-data m) off len))))]
  [(exec 56 Machine) ,(let ([m (term Machine)]) (stack-op m 0 GAS-BASE (lambda () (list (length (m-code m))))))]
  [(exec 57 Machine) ,(let ([m (term Machine)]) (copy3-op m (lambda (off len) (code-slice (m-code m) off len))))]
  [(exec 58 Machine) ,(let ([m (term Machine)]) (stack-op m 0 GAS-BASE (lambda () (list (tx-gasprice m)))))]
  [(exec 59 Machine) ,(extaccount-op (term Machine) (lambda (w a) (length (acct-code (world-ref w a)))))]
  [(exec 60 Machine) ,(extcodecopy-op (term Machine))]
  [(exec 61 Machine) ,(let ([m (term Machine)]) (stack-op m 0 GAS-BASE (lambda () (list (length (m-retdata m))))))]
  [(exec 62 Machine) ,(returndatacopy-op (term Machine))]
  [(exec 63 Machine) ,(extaccount-op (term Machine) extcodehash-project)]

  ;; --- block ----------------------------------------------------------
  [(exec 64 Machine) ,(stack-op (term Machine) 1 GAS-BLOCK-HASH (lambda (num) (list 0)))]
  [(exec 65 Machine) ,(let ([m (term Machine)]) (stack-op m 0 GAS-BASE (lambda () (list (blk-ref m 3)))))]
  [(exec 66 Machine) ,(let ([m (term Machine)]) (stack-op m 0 GAS-BASE (lambda () (list (blk-ref m 2)))))]
  [(exec 67 Machine) ,(let ([m (term Machine)]) (stack-op m 0 GAS-BASE (lambda () (list (blk-ref m 1)))))]
  [(exec 68 Machine) ,(let ([m (term Machine)]) (stack-op m 0 GAS-BASE (lambda () (list (blk-ref m 6)))))]
  [(exec 69 Machine) ,(let ([m (term Machine)]) (stack-op m 0 GAS-BASE (lambda () (list (blk-ref m 7)))))]
  [(exec 70 Machine) ,(let ([m (term Machine)]) (stack-op m 0 GAS-BASE (lambda () (list (blk-ref m 8)))))]
  [(exec 71 Machine) ,(let ([m (term Machine)]) (stack-op m 0 GAS-LOW (lambda () (list (acct-balance (world-ref (m-world m) (current-target m)))))))]
  [(exec 72 Machine) ,(let ([m (term Machine)]) (stack-op m 0 GAS-BASE (lambda () (list (blk-ref m 4)))))]
  [(exec 73 Machine) ,(let ([m (term Machine)]) (stack-op m 1 GAS-VERY-LOW (lambda (i) (list (let ([bs (tx-blobs m)]) (if (< i (length bs)) (list-ref bs i) 0))))))]
  [(exec 74 Machine) ,(let ([m (term Machine)]) (stack-op m 0 GAS-BASE (lambda () (list (blk-ref m 5)))))]

  ;; --- storage / memory / misc ----------------------------------------
  [(exec 83 Machine) ,(mstore8-op (term Machine))]
  [(exec 84 Machine) ,(sload-op (term Machine))]
  [(exec 85 Machine) ,(sstore-op (term Machine))]
  [(exec 89 Machine) ,(let ([m (term Machine)]) (stack-op m 0 GAS-BASE (lambda () (list (mem-size (m-mem m))))))]
  [(exec 90 Machine) ,(gas-op (term Machine))]
  [(exec 92 Machine) ,(tload-op (term Machine))]
  [(exec 93 Machine) ,(tstore-op (term Machine))]
  [(exec 94 Machine) ,(mcopy-op (term Machine))]
  [(exec 95 Machine) ,(stack-op (term Machine) 0 GAS-BASE (lambda () (list 0)))]

  ;; --- LOG0..LOG4 ------------------------------------------------------
  [(exec 160 Machine) ,(log-op (term Machine) 0)]
  [(exec 161 Machine) ,(log-op (term Machine) 1)]
  [(exec 162 Machine) ,(log-op (term Machine) 2)]
  [(exec 163 Machine) ,(log-op (term Machine) 3)]
  [(exec 164 Machine) ,(log-op (term Machine) 4)]

  ;; --- system: CALL family / CREATE / SELFDESTRUCT --------------------
  [(exec 240 Machine) ,(do-create (term Machine) 'create)]
  [(exec 241 Machine) ,(do-call   (term Machine) 'call)]
  [(exec 242 Machine) ,(do-call   (term Machine) 'callcode)]
  [(exec 244 Machine) ,(do-call   (term Machine) 'delegatecall)]
  [(exec 245 Machine) ,(do-create (term Machine) 'create2)]
  [(exec 250 Machine) ,(do-call   (term Machine) 'staticcall)]
  [(exec 255 Machine) ,(selfdestruct-op (term Machine))]

  ;; --- 0xfd REVERT -----------------------------------------------------
  [(exec 253 Machine) ,(revert-op (term Machine))]

  ;; --- anything else: invalid -----------------------------------------
  [(exec byte Machine) (with-halt Machine (invalid-opcode byte))])

;; memory-op failure helper: decide oog vs underflow
(define-metafunction evm
  mload-fail : Machine -> Machine
  [(mload-fail Machine) (with-halt Machine (out-of-gas))
   (where (w_off w ...) (stack-of Machine))]
  [(mload-fail Machine) (with-halt Machine (stack-underflow))])

(define-metafunction evm
  mstore-fail : Machine -> Machine
  [(mstore-fail Machine) (with-halt Machine (out-of-gas))
   (where (w_off w_val w ...) (stack-of Machine))]
  [(mstore-fail Machine) (with-halt Machine (stack-underflow))])

;; PUSHn: read n code bytes after pc, push as a word, advance pc by n+1
(define-metafunction evm
  push-op : Machine natural -> Machine
  [(push-op Machine natural_n)
   (advance-by (with-gas (with-stack Machine (w_val w ...))
                         ,(- (term w_g) GAS-VERY-LOW))
               ,(add1 (term natural_n)))
   (where (w ...) (stack-of Machine))
   (where w_g (gas-of Machine))
   (side-condition (>= (term w_g) GAS-VERY-LOW))
   (side-condition (<= (add1 (length (term (w ...)))) 1024))
   (where w_val ,(bytes->word
                  (list->bytes
                   (code-slice (term (code-of Machine))
                               (add1 (term (pc-of Machine)))
                               (term natural_n)))))]
  [(push-op Machine natural_n) (with-halt Machine (out-of-gas))])

;; DUPn: duplicate the n-th stack item (1-based) onto the top
(define-metafunction evm
  dup-op : Machine natural -> Machine
  [(dup-op Machine natural_n)
   (advance (with-gas (with-stack Machine ,(cons (list-ref (term Stack) (sub1 (term natural_n)))
                                                 (term Stack)))
                      ,(- (term w_g) GAS-VERY-LOW)))
   (where Stack (stack-of Machine))
   (side-condition (>= (length (term Stack)) (term natural_n)))
   (side-condition (<= (add1 (length (term Stack))) 1024))
   (where w_g (gas-of Machine))
   (side-condition (>= (term w_g) GAS-VERY-LOW))]
  [(dup-op Machine natural_n) (with-halt Machine (stack-underflow))
   (side-condition (< (length (term (stack-of Machine))) (term natural_n)))]
  [(dup-op Machine natural_n) (with-halt Machine (out-of-gas))])

;; SWAPn: swap the top with the (n+1)-th stack item
(define-metafunction evm
  swap-op : Machine natural -> Machine
  [(swap-op Machine natural_n)
   (advance (with-gas (with-stack Machine Stack_new) ,(- (term w_g) GAS-VERY-LOW)))
   (where Stack (stack-of Machine))
   (side-condition (>= (length (term Stack)) (add1 (term natural_n))))
   (where w_g (gas-of Machine))
   (side-condition (>= (term w_g) GAS-VERY-LOW))
   (where Stack_new ,(swap-list (term Stack) (term natural_n)))]
  [(swap-op Machine natural_n) (with-halt Machine (stack-underflow))
   (side-condition (< (length (term (stack-of Machine))) (add1 (term natural_n))))]
  [(swap-op Machine natural_n) (with-halt Machine (out-of-gas))])

;; ======================================================================
;; step / run
;; ======================================================================
(define-metafunction evm
  op@ : Machine -> byte
  [(op@ Machine)
   ,(let ([c (term (code-of Machine))] [p (term (pc-of Machine))])
      (if (< p (length c)) (list-ref c p) 0))])

(define-metafunction evm
  step : Machine -> Machine
  [(step Machine) (exec (op@ Machine) Machine)])

;; The reduction relation: fire one step while running and pc in range;
;; an implicit STOP fires when running runs off the end of the code.
(define evm-step
  (reduction-relation
   evm
   #:domain Machine
   (--> Machine (step Machine)
        (where running (halt-of Machine))
        (side-condition (< (term (pc-of Machine)) (length (term (code-of Machine)))))
        step)
   (--> Machine (with-halt Machine (stop))
        (where running (halt-of Machine))
        (side-condition (>= (term (pc-of Machine)) (length (term (code-of Machine)))))
        implicit-stop)))

;; --- Racket-side helpers ------------------------------------------------
;; field : machine-term symbol -> field arguments (list)
(define (machine-field m tag) (cdr (assq tag (cdr m))))
(define (machine-halt m) (car (machine-field m 'halt)))

;; set-machine-field : replace the field whose head is `tag` with (tag . args)
(define (set-machine-field m tag . args)
  (cons 'machine
        (for/list ([f (in-list (cdr m))])
          (if (eq? (car f) tag) (cons tag args) f))))

;; machine-result : -> (values halt-symbol-or-pair gas-left)
(define (machine-result m) (values (machine-halt m) (car (machine-field m 'gas))))

;; ======================================================================
;; Racket field accessors / setters used by the M2 opcode helpers below
;; ======================================================================
(define (m-pc m)      (car (machine-field m 'pc)))
(define (m-gas m)     (car (machine-field m 'gas)))
(define (m-stack m)   (car (machine-field m 'stack)))
(define (m-mem m)     (car (machine-field m 'memory)))
(define (m-code m)    (car (machine-field m 'code)))
(define (m-world m)   (car (machine-field m 'world)))
(define (m-msg m)     (car (machine-field m 'msg)))
(define (m-block m)   (car (machine-field m 'block)))
(define (m-tx m)      (car (machine-field m 'tx)))
(define (m-logs m)    (car (machine-field m 'logs)))
(define (m-refund m)  (car (machine-field m 'refund)))
(define (m-orig m)    (car (machine-field m 'orig-storage)))
(define (m-retdata m) (car (machine-field m 'return-data)))
(define (m-accessed m) (machine-field m 'accessed))      ; (AAddrs AKeys)

(define (set-pc m v)      (set-machine-field m 'pc v))
(define (set-gas m v)     (set-machine-field m 'gas v))
(define (set-stack m v)   (set-machine-field m 'stack v))
(define (set-mem m v)     (set-machine-field m 'memory v))
(define (set-world m v)   (set-machine-field m 'world v))
(define (set-logs m v)    (set-machine-field m 'logs v))
(define (set-refund m v)  (set-machine-field m 'refund v))
(define (set-halt m v)    (set-machine-field m 'halt v))
(define (set-accessed m pair) (set-machine-field m 'accessed (car pair) (cadr pair)))
(define (advance1 m) (set-pc m (add1 (m-pc m))))

;; message / env field readers
(define (msg-static? m)    (eq? (list-ref (m-msg m) 8) #t))
(define (current-target m) (list-ref (m-msg m) 2))
(define (msg-caller m)     (list-ref (m-msg m) 1))
(define (msg-value m)      (list-ref (m-msg m) 3))
(define (msg-data m)       (list-ref (m-msg m) 4))
(define (blk-ref m i)      (list-ref (m-block m) i))
(define (tx-origin m)      (list-ref (m-tx m) 1))
(define (tx-gasprice m)    (list-ref (m-tx m) 2))
(define (tx-blobs m)       (list-ref (m-tx m) 3))

(define ADDR-MASK (sub1 (expt 2 160)))
(define (as-addr w) (bitwise-and w ADDR-MASK))
(define (byte-len n) (if (zero? n) 0 (quotient (+ (integer-length n) 7) 8)))

;; EIP-2929 access tracking
(define (touch-address acc addr)
  (if (member addr (car acc))
      (values GAS-WARM-ACCESS acc)
      (values GAS-COLD-ACCOUNT-ACCESS (list (cons addr (car acc)) (cadr acc)))))
(define (member-skey acc addr key) (and (member (list addr key) (cadr acc)) #t))
(define (add-skey acc addr key)    (list (car acc) (cons (list addr key) (cadr acc))))

;; ======================================================================
;; Generic stack op: pop `npop` (top-first), charge `cost`, push the words
;; returned by `producer` (top-first), advance pc by 1.  Handles underflow,
;; out-of-gas and overflow uniformly.
;; ======================================================================
(define (stack-op m npop cost producer)
  (define s (m-stack m))
  (cond
    [(< (length s) npop) (set-halt m '(stack-underflow))]
    [(< (m-gas m) cost)  (set-halt m '(out-of-gas))]
    [else
     (define pushed (apply producer (take s npop)))
     (define ns (append pushed (drop s npop)))
     (if (> (length ns) 1024)
         (set-halt m '(stack-overflow))
         (advance1 (set-gas (set-stack m ns) (- (m-gas m) cost))))]))

;; load 32 bytes from a byte list at `off`, zero-extended, as a word
(define (list-load32 lst off) (bytes->word (list->bytes (code-slice lst off 32))))

;; ---- KECCAK256 --------------------------------------------------------
(define (keccak-op m)
  (define s (m-stack m))
  (cond
    [(< (length s) 2) (set-halt m '(stack-underflow))]
    [else
     (define off (list-ref s 0)) (define len (list-ref s 1)) (define rest (drop s 2))
     (define cost (+ GAS-KECCAK256 (* GAS-KECCAK256-WORD (ceil32-words len))
                     (memory-access-cost (mem-size (m-mem m)) off len)))
     (if (< (m-gas m) cost) (set-halt m '(out-of-gas))
         (let ([h (keccak256-word (list->bytes (mem-read (m-mem m) off len)))]
               [nm (mem-touch (m-mem m) off len)])
           (advance1 (set-stack (set-mem (set-gas m (- (m-gas m) cost)) nm) (cons h rest)))))]))

;; ---- EXP --------------------------------------------------------------
(define (exp-op m)
  (define s (m-stack m))
  (cond
    [(< (length s) 2) (set-halt m '(stack-underflow))]
    [else
     (define base (list-ref s 0)) (define e (list-ref s 1)) (define rest (drop s 2))
     (define cost (+ GAS-EXPONENTIATION (* GAS-EXPONENTIATION-PER-BYTE (byte-len e))))
     (if (< (m-gas m) cost) (set-halt m '(out-of-gas))
         (advance1 (set-stack (set-gas m (- (m-gas m) cost)) (cons (u-exp base e) rest))))]))

;; ---- copy ops ---------------------------------------------------------
;; CALLDATACOPY / CODECOPY: 3 pops (destOff srcOff len); get-src off len -> bytes
(define (copy3-op m get-src)
  (define s (m-stack m))
  (cond
    [(< (length s) 3) (set-halt m '(stack-underflow))]
    [else
     (define d (list-ref s 0)) (define off (list-ref s 1)) (define len (list-ref s 2))
     (define rest (drop s 3))
     (define cost (+ GAS-VERY-LOW (* GAS-COPY (ceil32-words len))
                     (memory-access-cost (mem-size (m-mem m)) d len)))
     (if (< (m-gas m) cost) (set-halt m '(out-of-gas))
         (let ([nm (mem-write (m-mem m) d (get-src off len))])
           (advance1 (set-stack (set-mem (set-gas m (- (m-gas m) cost)) nm) rest))))]))

(define (returndatacopy-op m)
  (define s (m-stack m))
  (cond
    [(< (length s) 3) (set-halt m '(stack-underflow))]
    [else
     (define d (list-ref s 0)) (define off (list-ref s 1)) (define len (list-ref s 2))
     (define rest (drop s 3))
     (define cost (+ GAS-VERY-LOW (* GAS-COPY (ceil32-words len))
                     (memory-access-cost (mem-size (m-mem m)) d len)))
     (cond
       [(< (m-gas m) cost) (set-halt m '(out-of-gas))]
       [(> (+ off len) (length (m-retdata m))) (set-halt m '(out-of-bounds))]
       [else
        (define nm (mem-write (m-mem m) d (code-slice (m-retdata m) off len)))
        (advance1 (set-stack (set-mem (set-gas m (- (m-gas m) cost)) nm) rest))])]))

(define (mcopy-op m)
  (define s (m-stack m))
  (cond
    [(< (length s) 3) (set-halt m '(stack-underflow))]
    [else
     (define d (list-ref s 0)) (define src (list-ref s 1)) (define len (list-ref s 2))
     (define rest (drop s 3))
     (define memcost (if (zero? len) 0
                         (memory-expansion-cost (mem-size (m-mem m)) (max (+ d len) (+ src len)))))
     (define cost (+ GAS-VERY-LOW (* GAS-COPY (ceil32-words len)) memcost))
     (if (< (m-gas m) cost) (set-halt m '(out-of-gas))
         (let* ([data (mem-read (m-mem m) src len)]
                [nm (mem-touch (mem-write (m-mem m) d data) src len)])
           (advance1 (set-stack (set-mem (set-gas m (- (m-gas m) cost)) nm) rest))))]))

(define (extcodecopy-op m)
  (define s (m-stack m))
  (cond
    [(< (length s) 4) (set-halt m '(stack-underflow))]
    [else
     (define addr (as-addr (list-ref s 0)))
     (define d (list-ref s 1)) (define off (list-ref s 2)) (define len (list-ref s 3))
     (define rest (drop s 4))
     (define-values (acost acc2) (touch-address (m-accessed m) addr))
     (define cost (+ acost (* GAS-COPY (ceil32-words len))
                     (memory-access-cost (mem-size (m-mem m)) d len)))
     (if (< (m-gas m) cost) (set-halt m '(out-of-gas))
         (let ([nm (mem-write (m-mem m) d (code-slice (acct-code (world-ref (m-world m) addr)) off len))])
           (advance1 (set-stack (set-accessed (set-mem (set-gas m (- (m-gas m) cost)) nm) acc2) rest))))]))

;; ---- MSTORE8 ----------------------------------------------------------
(define (mstore8-op m)
  (define s (m-stack m))
  (cond
    [(< (length s) 2) (set-halt m '(stack-underflow))]
    [else
     (define off (list-ref s 0)) (define val (list-ref s 1)) (define rest (drop s 2))
     (define cost (+ GAS-VERY-LOW (memory-access-cost (mem-size (m-mem m)) off 1)))
     (if (< (m-gas m) cost) (set-halt m '(out-of-gas))
         (let ([nm (mem-write (m-mem m) off (list (bitwise-and val #xff)))])
           (advance1 (set-stack (set-mem (set-gas m (- (m-gas m) cost)) nm) rest))))]))

;; ---- account-access ops (BALANCE / EXTCODESIZE / EXTCODEHASH) ---------
(define (extaccount-op m project)
  (define s (m-stack m))
  (cond
    [(< (length s) 1) (set-halt m '(stack-underflow))]
    [else
     (define addr (as-addr (list-ref s 0))) (define rest (drop s 1))
     (define-values (cost acc2) (touch-address (m-accessed m) addr))
     (if (< (m-gas m) cost) (set-halt m '(out-of-gas))
         (advance1 (set-stack (set-accessed (set-gas m (- (m-gas m) cost)) acc2)
                              (cons (project (m-world m) addr) rest))))]))

(define (extcodehash-project world addr)
  (if (account-exists? world addr)
      (let ([a (world-ref world addr)])
        (if (account-empty? a) 0 (keccak256-word (list->bytes (acct-code a)))))
      0))

;; ---- storage ops ------------------------------------------------------
(define (sload-op m)
  (define s (m-stack m))
  (cond
    [(< (length s) 1) (set-halt m '(stack-underflow))]
    [else
     (define key (list-ref s 0)) (define rest (drop s 1)) (define tgt (current-target m))
     (define cold? (not (member-skey (m-accessed m) tgt key)))
     (define cost (if cold? GAS-COLD-SLOAD GAS-WARM-ACCESS))
     (if (< (m-gas m) cost) (set-halt m '(out-of-gas))
         (let ([acc2 (if cold? (add-skey (m-accessed m) tgt key) (m-accessed m))]
               [val (storage-ref (acct-storage (world-ref (m-world m) tgt)) key)])
           (advance1 (set-stack (set-accessed (set-gas m (- (m-gas m) cost)) acc2) (cons val rest)))))]))

(define (sstore-op m)
  (cond
    [(msg-static? m) (set-halt m '(write-in-static))]
    [else
     (define s (m-stack m))
     (cond
       [(< (length s) 2) (set-halt m '(stack-underflow))]
       [(<= (m-gas m) GAS-CALL-STIPEND) (set-halt m '(out-of-gas))]
       [else
        (define key (list-ref s 0)) (define val (list-ref s 1)) (define rest (drop s 2))
        (define tgt (current-target m))
        (define cold? (not (member-skey (m-accessed m) tgt key)))
        (define orig (storage-ref (m-orig m) key))
        (define current (storage-ref (acct-storage (world-ref (m-world m) tgt)) key))
        (define cost (sstore-gas cold? orig current val))
        (if (< (m-gas m) cost) (set-halt m '(out-of-gas))
            (let* ([acc2 (if cold? (add-skey (m-accessed m) tgt key) (m-accessed m))]
                   [a (world-ref (m-world m) tgt)]
                   [w2 (world-set (m-world m) tgt
                                  (acct-with-storage a (storage-set (acct-storage a) key val)))])
              (advance1
               (set-stack
                (set-refund
                 (set-accessed (set-world (set-gas m (- (m-gas m) cost)) w2) acc2)
                 (+ (m-refund m) (sstore-refund orig current val)))
                rest))))])]))

(define (tload-op m)
  (define s (m-stack m))
  (cond
    [(< (length s) 1) (set-halt m '(stack-underflow))]
    [(< (m-gas m) GAS-WARM-ACCESS) (set-halt m '(out-of-gas))]
    [else
     (define key (list-ref s 0)) (define rest (drop s 1)) (define tgt (current-target m))
     (define val (storage-ref (acct-transient (world-ref (m-world m) tgt)) key))
     (advance1 (set-stack (set-gas m (- (m-gas m) GAS-WARM-ACCESS)) (cons val rest)))]))

(define (tstore-op m)
  (cond
    [(msg-static? m) (set-halt m '(write-in-static))]
    [else
     (define s (m-stack m))
     (cond
       [(< (length s) 2) (set-halt m '(stack-underflow))]
       [(< (m-gas m) GAS-WARM-ACCESS) (set-halt m '(out-of-gas))]
       [else
        (define key (list-ref s 0)) (define val (list-ref s 1)) (define rest (drop s 2))
        (define tgt (current-target m)) (define a (world-ref (m-world m) tgt))
        (define w2 (world-set (m-world m) tgt
                              (acct-with-transient a (storage-set (acct-transient a) key val))))
        (advance1 (set-stack (set-world (set-gas m (- (m-gas m) GAS-WARM-ACCESS)) w2) rest))])]))

;; ---- GAS opcode (needs post-charge value) -----------------------------
(define (gas-op m)
  (if (< (m-gas m) GAS-BASE)
      (set-halt m '(out-of-gas))
      (let ([g (- (m-gas m) GAS-BASE)])
        (if (> (add1 (length (m-stack m))) 1024)
            (set-halt m '(stack-overflow))
            (advance1 (set-stack (set-gas m g) (cons g (m-stack m))))))))

;; ---- LOGn -------------------------------------------------------------
(define (log-op m n)
  (cond
    [(msg-static? m) (set-halt m '(write-in-static))]
    [else
     (define s (m-stack m))
     (cond
       [(< (length s) (+ 2 n)) (set-halt m '(stack-underflow))]
       [else
        (define off (list-ref s 0)) (define len (list-ref s 1))
        (define topics (take (drop s 2) n)) (define rest (drop s (+ 2 n)))
        (define cost (+ GAS-LOG (* GAS-LOG-TOPIC n) (* GAS-LOG-DATA len)
                        (memory-access-cost (mem-size (m-mem m)) off len)))
        (if (< (m-gas m) cost) (set-halt m '(out-of-gas))
            (let ([entry (list 'log (current-target m) topics (mem-read (m-mem m) off len))]
                  [nm (mem-touch (m-mem m) off len)])
              (advance1 (set-logs (set-mem (set-stack (set-gas m (- (m-gas m) cost)) rest) nm)
                                  (append (m-logs m) (list entry))))))])]))

;; ---- REVERT -----------------------------------------------------------
(define (revert-op m)
  (define s (m-stack m))
  (cond
    [(< (length s) 2) (set-halt m '(stack-underflow))]
    [else
     (define off (list-ref s 0)) (define len (list-ref s 1))
     (define cost (memory-access-cost (mem-size (m-mem m)) off len))
     (if (< (m-gas m) cost) (set-halt m '(out-of-gas))
         (set-halt (set-gas m (- (m-gas m) cost)) (list 'revert (mem-read (m-mem m) off len))))]))

;; ======================================================================
;; Message layer: CALL / CALLCODE / DELEGATECALL / STATICCALL / CREATE /
;; CREATE2 / SELFDESTRUCT.  These run nested frames via `run` (mutual
;; recursion across the metafunction/Racket boundary) with snapshot/rollback.
;; ======================================================================

;; value transfer (creates the recipient if absent); from = to nets to zero
(define (transfer world from to amount)
  (define a (world-ref world from))
  (define w1 (world-set world from (acct-with-balance a (- (acct-balance a) amount))))
  (define b (world-ref w1 to))
  (world-set w1 to (acct-with-balance b (+ (acct-balance b) amount))))

;; EIP-7702 delegation: code 0xef0100 ++ <20-byte address> resolves to the
;; delegate account's code (one level).
(define (resolve-code world addr)
  (define code (acct-code (world-ref world addr)))
  (if (and (= (length code) 23)
           (= (list-ref code 0) #xef) (= (list-ref code 1) #x01) (= (list-ref code 2) #x00))
      (acct-code (world-ref world (as-addr (bytes->word (list->bytes (take (drop code 3) 20))))))
      code))

(define (compute-create-address sender nonce)
  (as-addr (bytes->word (subbytes (keccak256 (rlp (list (integer->bytes sender 20) nonce))) 12 32))))

(define (compute-create2-address sender salt init-code)
  (define data (bytes-append (bytes #xff) (integer->bytes sender 20)
                             (integer->bytes salt 32) (keccak256 init-code)))
  (as-addr (bytes->word (subbytes (keccak256 data) 12 32))))

(define (build-child #:caller caller #:target target #:value value #:data data
                     #:code-addr code-addr #:code code #:gas gas #:depth depth
                     #:static static? #:world world #:accessed acc #:logs logs
                     #:refund refund #:block block #:tx tx #:orig orig)
  (term (machine (pc 0) (gas ,gas) (stack ()) (memory ()) (code ,code)
                 (return-data ()) (halt running) (logs ,logs) (refund ,refund)
                 (accessed ,(car acc) ,(cadr acc))
                 (msg (msg ,caller ,target ,value ,data ,code-addr ,gas ,depth ,static?))
                 (world ,world) (block ,block) (tx ,tx) (orig-storage ,orig))))

;; run-top-frame : build and run a top-level call frame (used by transaction.rkt)
(define (run-top-frame #:caller caller #:target target #:value value #:data data
                       #:code code #:code-addr code-addr #:gas gas #:world world
                       #:accessed accessed #:block block #:tx txenv
                       #:depth [depth 0] #:static [static? #f] #:orig orig)
  (run (build-child #:caller caller #:target target #:value value #:data data
                    #:code-addr code-addr #:code code #:gas gas #:depth depth
                    #:static static? #:world world #:accessed accessed #:logs '()
                    #:refund 0 #:block block #:tx txenv #:orig orig)))

;; halt classification
(define (halt-success? h) (and (memq (car h) '(stop return)) #t))
(define (halt-revert? h)  (eq? (car h) 'revert))
(define (halt-output h)   (if (memq (car h) '(return revert)) (cadr h) '()))

;; ---- CALL family ------------------------------------------------------
(define (do-call m kind)
  (define s (m-stack m))
  (define has-value? (memq kind '(call callcode)))
  (define nargs (if has-value? 7 6))
  (cond
    [(< (length s) nargs) (set-halt m '(stack-underflow))]
    [else
     (define gas-req (list-ref s 0))
     (define to (as-addr (list-ref s 1)))
     (define value (if has-value? (list-ref s 2) 0))
     (define b (if has-value? 3 2))
     (define args-off (list-ref s b))      (define args-len (list-ref s (+ b 1)))
     (define ret-off (list-ref s (+ b 2))) (define ret-len (list-ref s (+ b 3)))
     (define rest (drop s nargs))
     (define static? (msg-static? m))
     (cond
       [(and (eq? kind 'call) (> value 0) static?) (set-halt m '(write-in-static))]
       [else
        (define mem-hi (max (if (zero? args-len) 0 (+ args-off args-len))
                            (if (zero? ret-len) 0 (+ ret-off ret-len))))
        (define memory-cost (if (zero? mem-hi) 0 (memory-expansion-cost (mem-size (m-mem m)) mem-hi)))
        (define-values (access-cost acc1) (touch-address (m-accessed m) to))
        (define create-cost (if (and (eq? kind 'call) (> value 0) (not (account-alive? (m-world m) to)))
                                GAS-NEW-ACCOUNT 0))
        (define transfer-cost (if (and has-value? (> value 0)) GAS-CALL-VALUE 0))
        (define extra (+ access-cost create-cost transfer-cost))
        (define avail (- (m-gas m) memory-cost extra))
        (cond
          [(< avail 0) (set-halt m '(out-of-gas))]
          [else
           (define gas-capped (min gas-req (max-message-call-gas avail)))
           (define stipend (if (and has-value? (> value 0)) GAS-CALL-STIPEND 0))
           (define forwarded (+ gas-capped stipend))
           (define gas-after (- (m-gas m) (+ memory-cost extra gas-capped)))
           (define mem-touched (mem-touch (m-mem m) 0 mem-hi))
           (define snap-world (m-world m)) (define snap-logs (m-logs m)) (define snap-refund (m-refund m))
           (define depth (list-ref (m-msg m) 7))
           (define self (current-target m))
           (define-values (c-caller c-target c-value c-transfer? c-static)
             (cond [(eq? kind 'call)         (values self to value #t static?)]
                   [(eq? kind 'callcode)     (values self self value #t static?)]
                   [(eq? kind 'delegatecall) (values (msg-caller m) self (msg-value m) #f static?)]
                   [(eq? kind 'staticcall)   (values self to 0 #f #t)]))
           (define args (mem-read (m-mem m) args-off args-len))
           (define sender-balance (acct-balance (world-ref (m-world m) self)))
           (cond
             [(or (> (+ depth 1) STACK-DEPTH-LIMIT)
                  (and c-transfer? (> c-value 0) (< sender-balance c-value)))
              ;; immediate failure: refund forwarded, push 0
              (advance1 (set-machine-field
                         (set-stack (set-mem (set-accessed (set-gas m (+ gas-after forwarded)) acc1) mem-touched)
                                    (cons 0 rest))
                         'return-data '()))]
             [else
              (mark-touched! c-target)
              (define snap-created (unbox the-created)) (define snap-deleted (unbox the-deleted))
              (define world1 (if (and c-transfer? (> c-value 0)) (transfer (m-world m) self c-target c-value)
                                 (m-world m)))
              (define child (build-child #:caller c-caller #:target c-target #:value c-value
                                         #:data args #:code-addr to #:code (resolve-code world1 to)
                                         #:gas forwarded #:depth (+ depth 1) #:static c-static
                                         #:world world1 #:accessed acc1 #:logs snap-logs
                                         #:refund snap-refund #:block (m-block m) #:tx (m-tx m)
                                         #:orig (orig-storage-for c-target world1)))
              (define obs (current-call-observer))
              (when obs (obs 'enter kind c-caller to c-value forwarded (+ depth 1)))
              (define done
                (if (precompile-addr? to)
                    (let-values ([(pok? pgl pout) (run-precompile to args forwarded)])
                      (set-gas (set-machine-field child 'halt (if pok? (list 'return pout) '(out-of-gas))) pgl))
                    (run child)))
              (define ch (machine-halt done))
              (define ok? (halt-success? ch)) (define rev? (halt-revert? ch))
              (when obs (obs 'exit ok? (car (machine-field done 'gas)) (halt-output ch)))
              (unless ok? (set-box! the-created snap-created) (set-box! the-deleted snap-deleted))
              (define output (halt-output ch))
              (define gas-final (+ gas-after (if (or ok? rev?) (m-gas done) 0)))
              (define write-len (min ret-len (length output)))
              (define mem-final (if (zero? write-len) mem-touched (mem-write mem-touched ret-off (take output write-len))))
              (define base-m (set-accessed (set-machine-field (set-mem (set-gas m gas-final) mem-final)
                                                              'return-data output)
                                           (m-accessed done)))
              (define committed
                (if ok?
                    (set-refund (set-logs (set-world base-m (m-world done)) (m-logs done)) (m-refund done))
                    (set-refund (set-logs (set-world base-m snap-world) snap-logs) snap-refund)))
              (advance1 (set-stack committed (cons (if ok? 1 0) rest)))])])])]))

;; ---- CREATE / CREATE2 -------------------------------------------------
(define (do-create m kind)
  (cond
    [(msg-static? m) (set-halt m '(write-in-static))]
    [else
     (define s (m-stack m))
     (define nargs (if (eq? kind 'create2) 4 3))
     (cond
       [(< (length s) nargs) (set-halt m '(stack-underflow))]
       [else
        (define value (list-ref s 0)) (define off (list-ref s 1)) (define len (list-ref s 2))
        (define salt (if (eq? kind 'create2) (list-ref s 3) 0))
        (define rest (drop s nargs))
        (cond
          [(> len MAX-INIT-CODE-SIZE) (set-halt m '(out-of-gas))]
          [else
           (define words (ceil32-words len))
           (define cost (+ GAS-CREATE (memory-access-cost (mem-size (m-mem m)) off len)
                           (* GAS-INIT-CODE-WORD words)
                           (if (eq? kind 'create2) (* GAS-KECCAK256-WORD words) 0)))
           (cond
             [(< (m-gas m) cost) (set-halt m '(out-of-gas))]
             [else
              (define gas-after (- (m-gas m) cost))
              (define forwarded (max-message-call-gas gas-after))
              (define gas-kept (- gas-after forwarded))
              (define self (current-target m))
              (define self-acct (world-ref (m-world m) self))
              (define nonce (acct-nonce self-acct))
              (define init-code (mem-read (m-mem m) off len))
              (define new-addr (if (eq? kind 'create2)
                                   (compute-create2-address self salt (list->bytes init-code))
                                   (compute-create-address self nonce)))
              (define mem-touched (mem-touch (m-mem m) off len))
              (define depth (list-ref (m-msg m) 7))
              (define acc1 (let ([a (m-accessed m)])
                             (if (member new-addr (car a)) a (list (cons new-addr (car a)) (cadr a)))))
              (cond
                [(or (> (+ depth 1) STACK-DEPTH-LIMIT) (< (acct-balance self-acct) value)
                     (= nonce (sub1 (expt 2 64))))
                 ;; failure: bump sender nonce, refund forwarded, push 0
                 (define w1 (world-set (m-world m) self (acct-with-nonce self-acct (add1 nonce))))
                 (advance1 (set-machine-field
                            (set-stack (set-world (set-mem (set-accessed (set-gas m gas-after) acc1) mem-touched) w1)
                                       (cons 0 rest))
                            'return-data '()))]
                [else
                 (define existing (world-ref (m-world m) new-addr))
                 (cond
                   [(or (> (acct-nonce existing) 0) (not (null? (acct-code existing))))
                    ;; address collision: bump nonce, consume forwarded, push 0
                    (define w1 (world-set (m-world m) self (acct-with-nonce self-acct (add1 nonce))))
                    (advance1 (set-machine-field
                               (set-stack (set-world (set-mem (set-accessed (set-gas m gas-kept) acc1) mem-touched) w1)
                                          (cons 0 rest))
                               'return-data '()))]
                   [else
                    (define snap-world (m-world m)) (define snap-logs (m-logs m)) (define snap-refund (m-refund m))
                    (define snap-created (unbox the-created)) (define snap-deleted (unbox the-deleted))
                    (mark-touched! new-addr)
                    ;; register as created-this-tx BEFORE running init, so a SELFDESTRUCT
                    ;; during init schedules this account for deletion (EIP-6780)
                    (set-box! the-created (cons new-addr (unbox the-created)))
                    (define w1 (world-set (m-world m) self (acct-with-nonce self-acct (add1 nonce))))
                    (define w2 (world-set w1 new-addr (acct-with-nonce empty-account 1)))
                    (define w3 (if (> value 0) (transfer w2 self new-addr value) w2))
                    (define child (build-child #:caller self #:target new-addr #:value value #:data '()
                                               #:code-addr new-addr #:code init-code #:gas forwarded
                                               #:depth (+ depth 1) #:static #f #:world w3 #:accessed acc1
                                               #:logs snap-logs #:refund snap-refund
                                               #:block (m-block m) #:tx (m-tx m)
                                               #:orig (orig-storage-for new-addr w3)))
                    (define obs (current-call-observer))
                    (when obs (obs 'enter kind self new-addr value forwarded (+ depth 1)))
                    (define done (run child))
                    (define ch (machine-halt done))
                    (define ok? (halt-success? ch)) (define rev? (halt-revert? ch))
                    (when obs (obs 'exit ok? (car (machine-field done 'gas)) (halt-output ch)))
                    (unless ok? (set-box! the-created snap-created) (set-box! the-deleted snap-deleted))
                    (define output (halt-output ch))
                    (cond
                      [ok?
                       (define code-len (length output))
                       (define deposit (* GAS-CODE-DEPOSIT-PER-BYTE code-len))
                       (cond
                         [(or (> code-len MAX-CODE-SIZE)
                              (and (> code-len 0) (= (car output) #xef))
                              (< (m-gas done) deposit))
                          ;; deploy fails: rollback, forwarded consumed, push 0
                          (advance1 (set-machine-field
                                     (set-stack (set-accessed (set-world (set-mem (set-gas m gas-kept) mem-touched) snap-world)
                                                              (m-accessed done))
                                                (cons 0 rest))
                                     'return-data '()))]
                         [else
                          (define deployed (world-set (m-world done) new-addr
                                                      (acct-with-code (world-ref (m-world done) new-addr) output)))
                          (define gas-final (+ gas-kept (- (m-gas done) deposit)))
                          (define committed
                            (set-refund (set-logs (set-world (set-accessed (set-mem (set-gas m gas-final) mem-touched)
                                                                           (m-accessed done))
                                                             deployed)
                                                  (m-logs done))
                                        (m-refund done)))
                          (advance1 (set-machine-field (set-stack committed (cons new-addr rest)) 'return-data '()))])]
                      [rev?
                       (define gas-final (+ gas-kept (m-gas done)))
                       (advance1 (set-machine-field
                                  (set-stack (set-refund (set-logs (set-world (set-accessed (set-mem (set-gas m gas-final) mem-touched)
                                                                                            (m-accessed done))
                                                                              snap-world)
                                                                   snap-logs)
                                                         snap-refund)
                                             (cons 0 rest))
                                  'return-data output))]
                      [else
                       (advance1 (set-machine-field
                                  (set-stack (set-accessed (set-world (set-mem (set-gas m gas-kept) mem-touched) snap-world)
                                                           (m-accessed done))
                                             (cons 0 rest))
                                  'return-data '()))])])])])])])]))

;; ---- SELFDESTRUCT (balance transfer + halt; deletion deferred to M4) --
(define (selfdestruct-op m)
  (cond
    [(msg-static? m) (set-halt m '(write-in-static))]
    [else
     (define s (m-stack m))
     (cond
       [(< (length s) 1) (set-halt m '(stack-underflow))]
       [else
        (define benef (as-addr (list-ref s 0)))
        (define self (current-target m))
        (define bal (acct-balance (world-ref (m-world m) self)))
        ;; EIP-2929: cold access adds 2600; warm adds 0 (NOT 100, unlike call ops)
        (define cold? (not (member benef (car (m-accessed m)))))
        (define acost (if cold? GAS-COLD-ACCOUNT-ACCESS 0))
        (define acc1 (if cold? (list (cons benef (car (m-accessed m))) (cadr (m-accessed m))) (m-accessed m)))
        (define new-cost (if (and (> bal 0) (not (account-alive? (m-world m) benef))) GAS-SELF-DESTRUCT-NEW-ACCOUNT 0))
        (define cost (+ GAS-SELF-DESTRUCT acost new-cost))
        (if (< (m-gas m) cost) (set-halt m '(out-of-gas))
            ;; move_ether: always transfer (self->self nets to zero, keeping balance).
            ;; Burning only happens when the account is deleted (created this tx).
            (let ([w (transfer (m-world m) self benef bal)])
              (mark-touched! benef)
              (define created-this-tx? (and (member self (unbox the-created)) #t))
              (define already? (and (member self (unbox the-deleted)) #t))
              ;; EIP-6780 (Cancun): only delete if the account was created in this
              ;; tx.  Pre-Cancun, SELFDESTRUCT always schedules deletion.
              (define delete? (or (not (fork-at-least? 'Cancun)) created-this-tx?))
              (when (and delete? (not already?))
                (set-box! the-deleted (cons self (unbox the-deleted))))
              ;; Pre-London (EIP-3529 removed it) grants a 24000 refund on the
              ;; first-time destruction of an account this tx.
              (define refund (if (not already?) (selfdestruct-refund-amount) 0))
              (set-halt (set-accessed
                         (set-refund
                          (set-world (set-gas m (- (m-gas m) cost)) w)
                          (+ (m-refund m) refund))
                         acc1)
                        '(stop))))])]))

;; valid-jump? : code(list) dest -> boolean  (JUMPDEST not inside PUSH data)
;; The JUMPDEST set is computed once per code object and cached — the machine's
;; `code` field is eq?-stable across steps — so a JUMP/JUMPI is an O(1) hash
;; lookup instead of re-scanning the whole program each time (which was O(n^2)
;; per call and made real contracts, e.g. ERC721, slow to test).
(define jumpdest-cache (make-weak-hasheq))
(define (jumpdests-of code)
  (hash-ref! jumpdest-cache code
    (lambda ()
      (define vec (list->vector code))
      (define n (vector-length vec))
      (let loop ([i 0] [acc (hasheqv)])
        (cond [(>= i n) acc]
              [else
               (define op (vector-ref vec i))
               (cond [(= op 91) (loop (add1 i) (hash-set acc i #t))]   ; JUMPDEST
                     [(and (>= op 96) (<= op 127))                     ; PUSH1..32: skip data
                      (loop (+ i 1 (- op 95)) acc)]
                     [else (loop (add1 i) acc)])])))))
(define (valid-jump? code dest)
  (and (hash-ref (jumpdests-of code) dest #f) #t))

;; swap-list : swap element 0 with element n
(define (swap-list lst n)
  (let ([vec (list->vector lst)])
    (let ([a (vector-ref vec 0)] [b (vector-ref vec n)])
      (vector-set! vec 0 b)
      (vector-set! vec n a)
      (vector->list vec))))

;; ======================================================================
;; exec-racket : a plain-Racket dispatcher equivalent to the Redex `exec`
;; metafunction (200-412), but with O(1) opcode dispatch and no per-step
;; Redex metafunction machinery (no whole-Machine term hashing / grammar
;; match).  It is the hot-path executor; the Redex `exec` metafunction is
;; kept intact as the executable *reference specification* and as the
;; differential oracle (see exec-oracle-step / fork-step below).
;;
;; Two kinds of opcode:
;;  - "Style-B" (stateful): the Redex rule body is already `,(helper ...)`,
;;    so we call the SAME helper here -- behaviourally identical by
;;    construction, including its mutation of the tx-level boxes (38-52).
;;  - "Style-A" (side-effect-free): reimplemented below, mirroring EXACTLY
;;    the gas cost and the failure-mode *ordering* of the Redex rule.  Each
;;    `r-*` was derived from the cited rule; the subtle cases are annotated.
;; ======================================================================

;; STOP 0x00 (204)
;; (arithmetic/comparison/bitwise 0x01-0x07, 0x10-0x19 use `stack-op`
;;  directly in the table: it reproduces binop/unop's underflow->gas->success
;;  ordering exactly, and those ops never overflow.)

;; POP 0x50 (228-233): success needs >=1 elem AND gas>=BASE; the second clause
;; is an UNCONDITIONAL catch-all -> anything else (empty OR gas-short) reports
;; stack-underflow (NOT out-of-gas).
(define (r-pop m)
  (define s (m-stack m))
  (if (and (pair? s) (>= (m-gas m) GAS-BASE))
      (advance1 (set-gas (set-stack m (cdr s)) (- (m-gas m) GAS-BASE)))
      (set-halt m '(stack-underflow))))

;; MLOAD 0x51 (236-247 + mload-fail 415-419)
(define (r-mload m)
  (define s (m-stack m))
  (cond
    [(< (length s) 1) (set-halt m '(stack-underflow))]
    [else
     (define off (car s))
     (define cost (+ GAS-VERY-LOW (memory-access-cost (mem-size (m-mem m)) off 32)))
     (if (< (m-gas m) cost)
         (set-halt m '(out-of-gas))
         (let ([val (bytes->word (list->bytes (mem-read (m-mem m) off 32)))]
               [nm  (mem-touch (m-mem m) off 32)])
           (advance1 (set-stack (set-mem (set-gas m (- (m-gas m) cost)) nm)
                                (cons val (cdr s))))))]))

;; MSTORE 0x52 (250-260 + mstore-fail 421-425)
(define (r-mstore m)
  (define s (m-stack m))
  (cond
    [(< (length s) 2) (set-halt m '(stack-underflow))]
    [else
     (define off (list-ref s 0)) (define val (list-ref s 1))
     (define cost (+ GAS-VERY-LOW (memory-access-cost (mem-size (m-mem m)) off 32)))
     (if (< (m-gas m) cost)
         (set-halt m '(out-of-gas))
         (let ([nm (mem-write (m-mem m) off (bytes->list (word->bytes32 val)))])
           (advance1 (set-stack (set-mem (set-gas m (- (m-gas m) cost)) nm)
                                (drop s 2)))))]))

;; JUMP 0x56 (263-272): underflow if empty; else JUMP iff gas>=MID AND valid
;; dest; else invalid-jump.  QUIRK: gas-short with a dest present reports
;; invalid-jump, not out-of-gas.  Sets pc to dest (no advance).
(define (r-jump m)
  (define s (m-stack m))
  (cond
    [(< (length s) 1) (set-halt m '(stack-underflow))]
    [(and (>= (m-gas m) GAS-MID) (valid-jump? (m-code m) (car s)))
     (set-gas (set-pc (set-stack m (cdr s)) (car s)) (- (m-gas m) GAS-MID))]
    [else (set-halt m '(invalid-jump))]))

;; JUMPI 0x57 (275-293): only when >=2 elems AND gas>=HIGH do we branch on the
;; condition; otherwise (too few operands OR gas-short) -> stack-underflow.
(define (r-jumpi m)
  (define s (m-stack m))
  (cond
    [(and (>= (length s) 2) (>= (m-gas m) GAS-HIGH))
     (define dest (list-ref s 0)) (define c (list-ref s 1)) (define rest (drop s 2))
     (define g2 (- (m-gas m) GAS-HIGH))
     (cond
       [(and (not (zero? c)) (valid-jump? (m-code m) dest))
        (set-gas (set-pc (set-stack m rest) dest) g2)]
       [(zero? c) (advance1 (set-gas (set-stack m rest) g2))]
       [else (set-halt m '(invalid-jump))])]
    [else (set-halt m '(stack-underflow))]))

;; PC 0x58 (296-302): SINGLE opcode clause (gas>=BASE AND depth+1<=1024).  Its
;; gas-short / full-stack edge has no dedicated rule and falls through to exec's
;; catch-all (412) -> (invalid-opcode 88).  Delegate that residual to the spec
;; so behaviour is identical by construction (this edge is otherwise unreachable
;; in practice since PC costs only GAS-BASE).
(define (r-pc m)
  (define s (m-stack m))
  (if (and (>= (m-gas m) GAS-BASE) (<= (add1 (length s)) 1024))
      (advance1 (set-stack (set-gas m (- (m-gas m) GAS-BASE)) (cons (m-pc m) s)))
      (term (exec 88 ,m))))

;; JUMPDEST 0x5b (305-308): SINGLE clause (gas>=JUMPDEST); its gas-short edge
;; falls through to exec's catch-all (412) -> (invalid-opcode 91).  Delegate
;; that residual to the spec.
(define (r-jumpdest m)
  (if (>= (m-gas m) GAS-JUMPDEST)
      (advance1 (set-gas m (- (m-gas m) GAS-JUMPDEST)))
      (term (exec 91 ,m))))

;; PUSH1..32 0x60-0x7f (push-op 428-443): success needs gas>=VERY-LOW AND
;; depth+1<=1024; reads n code bytes after pc; advance by n+1.  QUIRK: the
;; single failure clause labels BOTH gas-short and stack-overflow as
;; out-of-gas.
(define (r-push m n)
  (define s (m-stack m))
  (if (and (>= (m-gas m) GAS-VERY-LOW) (<= (add1 (length s)) 1024))
      (let ([val (bytes->word (list->bytes (code-slice (m-code m) (add1 (m-pc m)) n)))])
        (set-pc (set-gas (set-stack m (cons val s)) (- (m-gas m) GAS-VERY-LOW))
                (+ (m-pc m) 1 n)))
      (set-halt m '(out-of-gas))))

;; DUP1..16 0x80-0x8f (dup-op 446-459): underflow if depth<n; else dup nth
;; (1-based) when depth+1<=1024 AND gas>=VERY-LOW; else out-of-gas (this catch
;; all also covers stack-overflow).
(define (r-dup m n)
  (define s (m-stack m))
  (cond
    [(< (length s) n) (set-halt m '(stack-underflow))]
    [(and (<= (add1 (length s)) 1024) (>= (m-gas m) GAS-VERY-LOW))
     (advance1 (set-stack (set-gas m (- (m-gas m) GAS-VERY-LOW))
                          (cons (list-ref s (sub1 n)) s)))]
    [else (set-halt m '(out-of-gas))]))

;; SWAP1..16 0x90-0x9f (swap-op 462-473): underflow if depth<n+1; else swap top
;; with (n+1)-th when gas>=VERY-LOW; else out-of-gas.
(define (r-swap m n)
  (define s (m-stack m))
  (cond
    [(< (length s) (add1 n)) (set-halt m '(stack-underflow))]
    [(>= (m-gas m) GAS-VERY-LOW)
     (advance1 (set-stack (set-gas m (- (m-gas m) GAS-VERY-LOW)) (swap-list s n)))]
    [else (set-halt m '(out-of-gas))]))

;; RETURN 0xf3 (323-335): success needs >=2 elems AND gas>=cost; the second
;; clause is an UNCONDITIONAL catch-all -> too few operands OR gas-short both
;; report stack-underflow.  Return data is a byte LIST (mem-read).
(define (r-return m)
  (define s (m-stack m))
  (cond
    [(< (length s) 2) (set-halt m '(stack-underflow))]
    [else
     (define off (list-ref s 0)) (define len (list-ref s 1))
     (define cost (memory-access-cost (mem-size (m-mem m)) off len))
     (if (< (m-gas m) cost)
         (set-halt m '(stack-underflow))
         (let ([nm  (mem-touch (m-mem m) off len)]
               [out (mem-read (m-mem m) off len)])
           (set-halt (set-mem (set-gas (set-stack m (drop s 2)) (- (m-gas m) cost)) nm)
                     (list 'return out))))]))

;; The O(1) dispatcher.  PUSH/DUP/SWAP handled by range first; everything else
;; by a `case` over the opcode byte.  Style-B entries call the exact same
;; helper the corresponding Redex `exec` clause escapes to (338-409); the
;; default matches the catch-all invalid-opcode clause (412).
(define (exec-racket op m)
  (cond
    [(and (>= op 96)  (<= op 127)) (r-push m (- op 95))]
    [(and (>= op 128) (<= op 143)) (r-dup  m (- op 127))]
    [(and (>= op 144) (<= op 159)) (r-swap m (- op 143))]
    [else
     (case op
       ;; --- Style-A ---------------------------------------------------
       [(0)  (set-halt m '(stop))]
       [(1)  (stack-op m 2 3 (lambda (a b) (list (u-add  a b))))]
       [(2)  (stack-op m 2 5 (lambda (a b) (list (u-mul  a b))))]
       [(3)  (stack-op m 2 3 (lambda (a b) (list (u-sub  a b))))]
       [(4)  (stack-op m 2 5 (lambda (a b) (list (u-div  a b))))]
       [(5)  (stack-op m 2 5 (lambda (a b) (list (u-sdiv a b))))]
       [(6)  (stack-op m 2 5 (lambda (a b) (list (u-mod  a b))))]
       [(7)  (stack-op m 2 5 (lambda (a b) (list (u-smod a b))))]
       [(16) (stack-op m 2 3 (lambda (a b) (list (u-lt  a b))))]
       [(17) (stack-op m 2 3 (lambda (a b) (list (u-gt  a b))))]
       [(18) (stack-op m 2 3 (lambda (a b) (list (u-slt a b))))]
       [(19) (stack-op m 2 3 (lambda (a b) (list (u-sgt a b))))]
       [(20) (stack-op m 2 3 (lambda (a b) (list (u-eq  a b))))]
       [(21) (stack-op m 1 3 (lambda (a)   (list (u-iszero a))))]
       [(22) (stack-op m 2 3 (lambda (a b) (list (u-and a b))))]
       [(23) (stack-op m 2 3 (lambda (a b) (list (u-or  a b))))]
       [(24) (stack-op m 2 3 (lambda (a b) (list (u-xor a b))))]
       [(25) (stack-op m 1 3 (lambda (a)   (list (u-not a))))]
       [(80) (r-pop m)]
       [(81) (r-mload m)]
       [(82) (r-mstore m)]
       [(86) (r-jump m)]
       [(87) (r-jumpi m)]
       [(88) (r-pc m)]
       [(91) (r-jumpdest m)]
       [(243) (r-return m)]
       ;; --- Style-B (same helper the Redex rule escapes to) -----------
       [(8)  (stack-op m 3 8 (lambda (a b n) (list (u-addmod a b n))))]
       [(9)  (stack-op m 3 8 (lambda (a b n) (list (u-mulmod a b n))))]
       [(10) (exp-op m)]
       [(11) (stack-op m 2 5 (lambda (b x)  (list (u-signextend b x))))]
       [(26) (stack-op m 2 3 (lambda (i x)  (list (u-byte i x))))]
       [(27) (stack-op m 2 3 (lambda (sh v) (list (u-shl sh v))))]
       [(28) (stack-op m 2 3 (lambda (sh v) (list (u-shr sh v))))]
       [(29) (stack-op m 2 3 (lambda (sh v) (list (u-sar sh v))))]
       [(32) (keccak-op m)]
       [(48) (stack-op m 0 GAS-BASE (lambda () (list (current-target m))))]
       [(49) (extaccount-op m (lambda (w a) (acct-balance (world-ref w a))))]
       [(50) (stack-op m 0 GAS-BASE (lambda () (list (tx-origin m))))]
       [(51) (stack-op m 0 GAS-BASE (lambda () (list (msg-caller m))))]
       [(52) (stack-op m 0 GAS-BASE (lambda () (list (msg-value m))))]
       [(53) (stack-op m 1 GAS-VERY-LOW (lambda (off) (list (list-load32 (msg-data m) off))))]
       [(54) (stack-op m 0 GAS-BASE (lambda () (list (length (msg-data m)))))]
       [(55) (copy3-op m (lambda (off len) (code-slice (msg-data m) off len)))]
       [(56) (stack-op m 0 GAS-BASE (lambda () (list (length (m-code m)))))]
       [(57) (copy3-op m (lambda (off len) (code-slice (m-code m) off len)))]
       [(58) (stack-op m 0 GAS-BASE (lambda () (list (tx-gasprice m))))]
       [(59) (extaccount-op m (lambda (w a) (length (acct-code (world-ref w a)))))]
       [(60) (extcodecopy-op m)]
       [(61) (stack-op m 0 GAS-BASE (lambda () (list (length (m-retdata m)))))]
       [(62) (returndatacopy-op m)]
       [(63) (extaccount-op m extcodehash-project)]
       [(64) (stack-op m 1 GAS-BLOCK-HASH (lambda (num) (list 0)))]
       [(65) (stack-op m 0 GAS-BASE (lambda () (list (blk-ref m 3))))]
       [(66) (stack-op m 0 GAS-BASE (lambda () (list (blk-ref m 2))))]
       [(67) (stack-op m 0 GAS-BASE (lambda () (list (blk-ref m 1))))]
       [(68) (stack-op m 0 GAS-BASE (lambda () (list (blk-ref m 6))))]
       [(69) (stack-op m 0 GAS-BASE (lambda () (list (blk-ref m 7))))]
       [(70) (stack-op m 0 GAS-BASE (lambda () (list (blk-ref m 8))))]
       [(71) (stack-op m 0 GAS-LOW (lambda () (list (acct-balance (world-ref (m-world m) (current-target m))))))]
       [(72) (stack-op m 0 GAS-BASE (lambda () (list (blk-ref m 4))))]
       [(73) (stack-op m 1 GAS-VERY-LOW (lambda (i) (list (let ([bs (tx-blobs m)]) (if (< i (length bs)) (list-ref bs i) 0)))))]
       [(74) (stack-op m 0 GAS-BASE (lambda () (list (blk-ref m 5))))]
       [(83) (mstore8-op m)]
       [(84) (sload-op m)]
       [(85) (sstore-op m)]
       [(89) (stack-op m 0 GAS-BASE (lambda () (list (mem-size (m-mem m)))))]
       [(90) (gas-op m)]
       [(92) (tload-op m)]
       [(93) (tstore-op m)]
       [(94) (mcopy-op m)]
       [(95) (stack-op m 0 GAS-BASE (lambda () (list 0)))]
       [(160) (log-op m 0)]
       [(161) (log-op m 1)]
       [(162) (log-op m 2)]
       [(163) (log-op m 3)]
       [(164) (log-op m 4)]
       [(240) (do-create m 'create)]
       [(241) (do-call m 'call)]
       [(242) (do-call m 'callcode)]
       [(244) (do-call m 'delegatecall)]
       [(245) (do-create m 'create2)]
       [(250) (do-call m 'staticcall)]
       [(253) (revert-op m)]
       [(255) (selfdestruct-op m)]
       ;; --- anything else: invalid (matches catch-all 412) ------------
       [else (set-halt m (list 'invalid-opcode op))])]))

;; --- backend selection + differential oracle --------------------------
;; Read from the ENVIRONMENT at module load (NOT a make-parameter): the
;; conformance runner executes in racket/place workers, and parameters do not
;; cross the place boundary, but each worker re-instantiates this module and
;; re-reads the environment.
;;   EVM_EXEC_BACKEND = racket (default) | spec
;;   EVM_ORACLE       = 1  -> per-step self-differential vs the Redex spec
;; A PARAMETER (not a plain define) so in-process callers -- e.g. the PBT layer
;; -- can pin the fast executor regardless of the environment.  Its default
;; still comes from the env at module load, which is what the racket/place
;; conformance workers rely on (each worker re-instantiates the module).
(define current-exec-backend
  (make-parameter (if (equal? (getenv "EVM_EXEC_BACKEND") "spec") 'spec 'racket)))
(define exec-oracle? (equal? (getenv "EVM_ORACLE") "1"))

;; the Redex `exec` metafunction as a plain Racket function (reference spec)
(define (exec-spec op m) (term (exec ,op ,m)))

;; opcodes safe to run through BOTH executors on the same machine: the
;; side-effect-free Style-A set.  They never touch the tx-level boxes (38-52),
;; so re-running the spec is idempotent (no double-apply).  Style-B opcodes are
;; the identical Racket helper in both paths, so differencing them is vacuous.
(define (pure-styleA-op? op)
  (or (= op 0) (and (>= op 1) (<= op 7)) (and (>= op 16) (<= op 25))
      (= op 80) (= op 81) (= op 82) (= op 86) (= op 87) (= op 88) (= op 91)
      (and (>= op 96) (<= op 159)) (= op 243)))

;; run exec-racket and the Redex spec on the same machine; assert they agree
;; (both raise, or both return equal? machines).  Returns the racket result.
(define (exec-oracle-step op m)
  (define (cap thunk)
    (with-handlers ([exn:fail? (lambda (_) 'raised)]) (box (thunk))))
  (define r1 (cap (lambda () (exec-racket op m))))
  (define r2 (cap (lambda () (term (exec ,op ,m)))))
  (cond
    [(and (eq? r1 'raised) (eq? r2 'raised)) (exec-racket op m)] ; re-raise
    [(or  (eq? r1 'raised) (eq? r2 'raised))
     (error 'exec-oracle "raise mismatch at op ~a: racket ~s spec ~s\n ~s" op r1 r2 m)]
    [(equal? (unbox r1) (unbox r2)) (unbox r1)]
    [else
     (error 'exec-oracle "value divergence at op ~a\n racket: ~s\n spec:   ~s\n from:   ~s"
            op (unbox r1) (unbox r2) m)]))

;; ---- execution tracing hook ---------------------------------------------
;; The one thing an outside tool cannot reconstruct for itself: which (pc, op)
;; each frame actually executed.  `current-frame-tracer`, when set, is called
;; ONCE PER FRAME with the frame's code address, its code, and whether the code
;; is the one installed at that address ('runtime) or a constructor's init code
;; ('init).  It returns either #f (don't trace this frame) or a per-step
;; procedure, which is then called with the pc, the opcode, and the machine
;; AFTER the step (enough to see which way a JUMPI went).
;;
;; It lives HERE, in the driver loop, rather than in either executor, because
;; this is the one place every frame passes through — the top frame, each
;; CALL/DELEGATECALL/STATICCALL child, and a CREATE's init code all re-enter
;; `run` — and it sees the pc and the opcode before dispatch.  So a tracer is
;; oblivious to which backend ran the step (identical under
;; EVM_EXEC_BACKEND=spec) and is not called twice by the EVM_ORACLE
;; double-execution.  Nothing is added to the machine term, so the Redex/Racket
;; differential oracle and the `exec` metafunction cache are untouched.
;;
;; The parameter is read ONCE PER FRAME, not per step: with no tracer installed
;; the per-step cost is a single test of a local `#f`.
;;
;; The coverage measure built on this hook lives in the companion
;; evm-redex-tests project (coverage/coverage.rkt).
(define current-frame-tracer (make-parameter #f))

;; ---- call observer ------------------------------------------------------
;; A coarser companion to the per-step frame tracer: when set, it is called at
;; each nested CALL/CREATE boundary — once on ENTER, once on EXIT — which is
;; exactly a call tree, with the caller/callee/value/gas each event carries.
;; The simulator's call trace is built on it; the coverage/step-trace path uses
;; the finer current-frame-tracer instead.  Off by default, so the per-call cost
;; is a single test of a local #f.  Entry point (only ever a `run` boundary),
;; not the machine term, so backends and the Redex oracle are unaffected.
;;   (obs 'enter kind caller callee value gas depth)
;;   (obs 'exit  success? gas-left output)
;; Enter/exit nest as a well-formed tree; pre-execution failures (depth limit,
;; insufficient balance) that never spawn a child frame are not reported.
(define current-call-observer (make-parameter #f))

(define (frame-tracer m code)
  (define t (current-frame-tracer))
  (and t
       (let* ([code-addr (list-ref (m-msg m) 5)]
              ;; a frame whose code is NOT the code installed at its own address
              ;; is a constructor (CREATE runs the init code at an address whose
              ;; account is still empty).  resolve-code also sees through an
              ;; EIP-7702 delegation, which is a runtime frame, not an init one.
              [kind (if (equal? code (resolve-code (m-world m) code-addr))
                        'runtime
                        'init)])
         (t code-addr code kind))))

;; run : drive the machine to a halt (deterministic single-step loop).
;; The frame's code never changes while stepping, so index it through a vector
;; computed once (O(1) opcode fetch instead of O(pc) list-ref per step).
(define (run m #:fuel [fuel 10000000])
  (define code (car (machine-field m 'code)))
  (define cvec (list->vector code))
  (define clen (vector-length cvec))
  (define trace! (frame-tracer m code))
  (let loop ([m m] [n fuel])
    (define h (machine-halt m))
    (cond [(not (equal? h 'running)) m]
          [(zero? n) (error 'run "fuel exhausted")]
          [else
           (define pc (car (machine-field m 'pc)))
           (define m2 (if (< pc clen)
                          (fork-step m (vector-ref cvec pc))
                          (term (with-halt ,m (stop)))))
           (when (and trace! (< pc clen))
             (trace! pc (vector-ref cvec pc) m2))
           (loop m2 (sub1 n))])))

;; step one instruction, honouring the active fork: an opcode not yet activated
;; in `current-fork` is INVALID.  Gating here (uncached Racket) rather than in
;; the cached `exec` metafunction keeps it fork-correct despite Redex's
;; term-keyed metafunction cache.  (M12)
;; `run`/`run-trace` already fetched the opcode (O(1) via the code vector), so
;; invoke the `exec` semantics DIRECTLY with it, rather than `(step m)` which
;; would re-derive the opcode through the `op@` metafunction (an O(code) length +
;; list-ref) and add two extra whole-Machine metafunction dispatches per step.
;; (exec op m) == (step m) whenever op = code[pc], which is exactly this call.
;; The Racket executor (exec-racket) is the hot path; the Redex `exec`
;; metafunction remains the reference spec, reachable via EVM_EXEC_BACKEND=spec
;; and used as the differential oracle under EVM_ORACLE=1.
(define (fork-step m op)
  (if (opcode-available? op)
      (cond
        [(eq? (current-exec-backend) 'spec)   (term (exec ,op ,m))]
        [(and exec-oracle? (pure-styleA-op? op)) (exec-oracle-step op m)]
        [else                                 (exec-racket op m)])
      (set-halt m (list 'invalid-opcode op))))

;; run-trace : like run, but collect every intermediate machine
(define (run-trace m #:fuel [fuel 100000])
  (define code (car (machine-field m 'code)))
  (define cvec (list->vector code))
  (define clen (vector-length cvec))
  (define trace! (frame-tracer m code))
  (let loop ([m m] [n fuel] [acc (list m)])
    (define h (machine-halt m))
    (cond [(not (equal? h 'running)) (reverse acc)]
          [(zero? n) (error 'run-trace "fuel exhausted")]
          [else
           (define pc (car (machine-field m 'pc)))
           (define m2 (if (< pc clen)
                          (fork-step m (vector-ref cvec pc))
                          (term (with-halt ,m (stop)))))
           (when (and trace! (< pc clen))
             (trace! pc (vector-ref cvec pc) m2))
           (loop m2 (sub1 n) (cons m2 acc))])))
