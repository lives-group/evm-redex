#lang racket/base

;; Multi-fork configuration (M12).
;;
;; A single linear ordering of hardforks Frontier -> Prague, plus a dynamic
;; `current-fork` parameter and a `fork-at-least?` predicate that the gas
;; schedule, transaction layer, and interpreter consult to gate EIP-specific
;; behaviour.  Everything defaults to 'Prague, so with no `parameterize` the
;; semantics are exactly the (Prague) behaviour the rest of the code already
;; implements — selecting an older fork only *removes* newer EIPs.
;;
;; Note: the pre-Berlin gas schedule (flat SLOAD/CALL costs, EIP-1283/2200
;; net-metered SSTORE variants) is NOT fully modelled here; the reused warm/cold
;; (EIP-2929) gas engine is correct from Berlin onward.  Berlin..Prague are the
;; forks this table fully parameterizes; older forks get the correct EIP *gates*
;; (opcode availability, refunds, base fee) but share the Berlin+ access-gas
;; model.  Completing the pre-Berlin schedule is deferred to M13.

(require racket/list)

(provide (all-defined-out))

;; Linear fork ordering (index = age).  Glacier forks are difficulty-bomb-only
;; and share the execution semantics of the preceding fork; they are included so
;; fixture fork names resolve, but gate identically to their predecessor.
(define FORKS
  '(Frontier Homestead Tangerine Spurious Byzantium Constantinople Petersburg
    Istanbul MuirGlacier Berlin London ArrowGlacier GrayGlacier Paris
    Shanghai Cancun Prague))

;; Map the fork names that appear in execution-spec-tests fixtures to our symbols.
(define FORK-ALIASES
  (hash "Frontier"            'Frontier
        "Homestead"           'Homestead
        "EIP150"              'Tangerine
        "TangerineWhistle"    'Tangerine
        "EIP158"              'Spurious
        "SpuriousDragon"      'Spurious
        "Byzantium"           'Byzantium
        "Constantinople"      'Constantinople
        "ConstantinopleFix"   'Petersburg
        "Petersburg"          'Petersburg
        "Istanbul"            'Istanbul
        "MuirGlacier"         'MuirGlacier
        "Berlin"              'Berlin
        "London"              'London
        "ArrowGlacier"        'ArrowGlacier
        "GrayGlacier"         'GrayGlacier
        "Merge"               'Paris
        "Paris"               'Paris
        "Shanghai"            'Shanghai
        "Cancun"              'Cancun
        "Prague"              'Prague))

;; resolve a fixture fork string (or symbol) to a canonical fork symbol, or #f.
(define (fork-from-name name)
  (define s (if (symbol? name) (symbol->string name) name))
  (hash-ref FORK-ALIASES s #f))

;; the fork whose semantics are currently active
(define current-fork (make-parameter 'Prague))

(define (fork-index f)
  (or (index-of FORKS f) (error 'fork-index "unknown fork: ~a" f)))

;; is the active fork at least `f` (i.e. has EIPs up to and including `f`)?
(define (fork-at-least? f)
  (>= (fork-index (current-fork)) (fork-index f)))

;; ---- derived EIP gates / parameters -----------------------------------

;; EIP-2929 warm/cold access lists (Berlin)
(define (access-lists?)         (fork-at-least? 'Berlin))
;; EIP-1559 fee market + BASEFEE opcode (London)
(define (base-fee?)             (fork-at-least? 'London))
;; EIP-3607 reject tx from an account with code (London)
(define (reject-sender-code?)   (fork-at-least? 'London))
;; EIP-3541 reject new contract code starting with 0xEF (London)
(define (reject-ef-code?)       (fork-at-least? 'London))
;; EIP-3651 warm coinbase (Shanghai)
(define (warm-coinbase?)        (fork-at-least? 'Shanghai))
;; EIP-3860 init code word cost + size limit (Shanghai)
(define (init-code-metering?)   (fork-at-least? 'Shanghai))
;; EIP-7623 calldata floor cost (Prague)
(define (calldata-floor?)       (fork-at-least? 'Prague))

;; EIP-3529 (London) lowered the refund cap divisor 2 -> ... actually raised it,
;; making refunds a smaller fraction: max_refund = gas_used // quotient.
(define (refund-quotient)       (if (fork-at-least? 'London) 5 2))
;; EIP-3529 (London) lowered the storage clear refund 15000 -> 4800.
(define (sstore-clear-refund-amount) (if (fork-at-least? 'London) 4800 15000))
;; EIP-3529 (London) removed the SELFDESTRUCT refund (was 24000).
(define (selfdestruct-refund-amount) (if (fork-at-least? 'London) 0 24000))

;; ---- opcode availability ----------------------------------------------
;; The fork in which each fork-gated opcode became available.  Opcodes not
;; listed are assumed available in all supported forks.
(define OPCODE-FORK
  (hash #x1b 'Constantinople   ; SHL
        #x1c 'Constantinople   ; SHR
        #x1d 'Constantinople   ; SAR
        #x3d 'Byzantium        ; RETURNDATASIZE
        #x3e 'Byzantium        ; RETURNDATACOPY
        #x3f 'Constantinople   ; EXTCODEHASH
        #x46 'Istanbul         ; CHAINID
        #x47 'Istanbul         ; SELFBALANCE
        #x48 'London           ; BASEFEE
        #x49 'Cancun           ; BLOBHASH
        #x4a 'Cancun           ; BLOBBASEFEE
        #x5c 'Cancun           ; TLOAD
        #x5d 'Cancun           ; TSTORE
        #x5e 'Cancun           ; MCOPY
        #x5f 'Shanghai         ; PUSH0
        #xf5 'Constantinople   ; CREATE2
        #xfa 'Byzantium        ; STATICCALL
        #xfd 'Byzantium))      ; REVERT

;; is opcode `byte` available in the active fork?
(define (opcode-available? byte)
  (define f (hash-ref OPCODE-FORK byte #f))
  (or (not f) (fork-at-least? f)))
