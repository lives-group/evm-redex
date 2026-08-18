#lang scribble/manual

@(require (for-label racket/base racket/contract))

@title[#:tag "sim"]{@tt{#lang evm-redex/sim} — simulating transactions}

@defmodulelang[evm-redex/sim]

Where @secref["asm"] runs a single code @emph{fragment}, @tt{#lang evm-redex/sim}
runs @emph{transactions}: its source is a @deftech{scenario} that declares
accounts and initial state and then submits a sequence of transactions against an
evolving chain. Running the module executes the whole scenario — validating
nonces, charging gas, threading the world from one transaction to the next — and
prints a receipt per transaction plus the final state root and a diff.

The engine underneath is also usable directly, @racket[(require evm-redex/sim)];
the scenario language is a thin front-end over it.

@margin-note{Honest limits, inherited from the semantics: the active hard fork is
the @racket[current-fork] parameter (the scenario's @litchar{.fork} sets it), not
derived from the block; EIP-7702 authorizations and EIP-4844 blobs are the
library's simplified forms (signature recovery assumed already done). The state
root is the library's own Merkle-Patricia trie.}

@section{A first scenario}

@codeblock|{
#lang evm-redex/sim
.fork Prague
.account ALICE balance=10eth
.account BOB
.deploy TOKEN from=ALICE code=@Token.json:Token

tx from=ALICE to=TOKEN sig="mint(address,uint256)" args=(ALICE, 1000)
tx from=ALICE to=TOKEN sig="transfer(address,uint256)" args=(BOB, 100)
call from=BOB to=TOKEN sig="balanceOf(address)" args=(BOB) returns=uint256
}|

Running it prints a receipt for each @tt{tx}, the decoded result of the @tt{call},
and the final state:

@verbatim|{
deployed TOKEN at 0x479a82c754d9b3e2bfebf618dad979ccadc5cce7
tx ALICE -> TOKEN
  status:    success
  gas used:  68764
  log:       Transfer(from=0x0, to=0xacc0…0001, value=0x3e8)
  return:    0x
tx ALICE -> TOKEN
  status:    success
  gas used:  52122
  log:       Transfer(from=0xacc0…0001, to=0xacc0…0002, value=0x64)
  return:    0x0000…0001
call TOKEN.balanceOf(address) = (100)

--- final state ---
state root: 0x81a5f2e3…4882
0x479a82…cce7  nonce 0->1
    slot 0x0: 0x0 -> 0x3e8
    slot 0x25d8…1e3c: 0x0 -> 0x64
    slot 0x6e05…b3bb: 0x0 -> 0x384
0xacc0…0001  nonce 0->3
}|

@section{The scenario language}

One command per line; @litchar{;} starts a comment. A command is a head followed
by @deftech{positional}s and @tt{key=value} @deftech{field}s. Values are: numbers
(@tt{0x…} or decimal, optional @tt{eth}/@tt{gwei}/@tt{wei} unit), account names
(bare identifiers, bound by the setup directives), quoted @tt{"signatures"},
tuples @tt{(a, b, …)} for call arguments, storage maps @tt{{slot:val, …}},
code references @tt{@"@"file:Contract}, and relative @tt{+N} for block fields.

@subsection{Setup directives}

@itemlist[
@item{@litchar{.fork} @tt{@italic{Name}} — the hard fork (default @tt{Prague}).}
@item{@litchar{.account} @tt{@italic{NAME}} @tt{[0x@italic{addr}]}
      @tt{[balance=…] [nonce=…]} — an externally-owned account. With no explicit
      address the account's address is @bold{derived from its name} (the low 20
      bytes of @tt{keccak(@italic{NAME})}), so it is deterministic and reproducible
      across runs. @tt{balance} takes a unit (@tt{10eth}).}
@item{@litchar{.contract} @tt{@italic{NAME} 0x@italic{addr}} @tt{code=…}
      @tt{[balance=…] [storage={0x0:0x7b, …}]} — a pre-existing
      contract: runtime code and initial storage, installed directly.}
@item{@litchar{.deploy} @tt{@italic{NAME}} @tt{from=… code=… [value=…]} — run a
      creation transaction and bind @italic{NAME} to the created address. With a
      @tt{@"@"file.json} code reference the contract's ABI is remembered, so its
      events decode in later receipts.}
@item{@litchar{.block} @tt{[number=… timestamp=… coinbase=… basefee=…]} — set the
      current block environment (absolute, or @tt{+N} relative).}
@item{@litchar{.trace} @tt{call|step|off} — the default trace for later @tt{tx}s.}
]

@subsection{Actions}

@itemlist[
@item{@tt{tx} @tt{from=… to=… [value=…] [gas=…]} @tt{[sig="…" args=(…) | data=0x…]}
      @tt{[gasprice=… | maxfee=… maxpriority=…]} @tt{[trace=call|step]} — submit a
      transaction and print its receipt. The tx type follows the fee fields
      (@tt{gasprice} → legacy, @tt{maxfee}/@tt{maxpriority} → EIP-1559). @tt{sig} +
      @tt{args} are ABI-encoded for you; @tt{args} may name accounts.}
@item{@tt{call} @tt{from=… to=… [sig="…" args=(…)] [returns=@italic{types}]} — a
      read-only frame call (no state change); prints the decoded return.}
@item{@tt{mine} @tt{[number=… timestamp=… coinbase=…]} — close the current block
      (EIP-4788/2935 system calls, then withdrawals) and advance to the next
      (number @tt{+1}, timestamp @tt{+12} unless overridden).}
@item{@tt{withdrawal} @tt{to=… amount=…} — a withdrawal credited on the next
      @tt{mine}.}
]

@bold{Code references.} @tt{code=@"@"file.json:Contract} reads a solc/Foundry
artifact (creation bytecode + ABI) via @racket[read-artifact]; @tt{code=@"@"file}
reads raw hex; @tt{code=0x…} is inline bytecode. Paths resolve relative to the
scenario file.

@section{The four observations}

@subsection{Receipt (with decoded logs and revert reasons)}

Every @tt{tx} prints a receipt: @tt{status} (@tt{success}/@tt{revert}/@tt{error}),
gas used, each log, and the return data. Logs emitted by a contract whose ABI is
known (deployed from a @tt{.json} artifact) are decoded to
@tt{EventName(arg=…, …)}; a revert is decoded to its reason —
@tt{Error("…")}, @tt{Panic(0x…: …)}, a custom-error selector, or raw bytes:

@verbatim|{
tx ALICE -> TOKEN
  status:    revert  (Error("insufficient balance"))
  gas used:  23742
  return:    0x08c379a0…
}|

@subsection{Call trace}

@tt{trace=call} prepends the tree of nested @tt{CALL}/@tt{DELEGATECALL}/
@tt{STATICCALL}/@tt{CREATE}s — caller, callee, value, gas, and success — built on
the interpreter's @racket[current-call-observer] hook:

@verbatim|{
  call trace:
    call 0x…a11ce -> 0x…token  value=0 gas=…  ✓
      staticcall 0x…token -> 0x…oracle  value=0 gas=…  ✗  Error("stale price")
}|

@subsection{Step trace}

@tt{trace=step} prepends one line per opcode (pc, mnemonic, resulting stack),
nested frames indented, from @racket[current-frame-tracer]:

@verbatim|{
  step trace:
     0  PUSH1  stack (0x80)
     2  PUSH1  stack (0x40 0x80)
     4  MSTORE stack ()
}|

@subsection{State diff and root}

After the last command the scenario prints the state root and a diff from the
initial state — balance deltas, nonce changes, and per-account storage slots that
moved (shown in the first example above).

@section{The engine API}

The same simulator is available as functions through @racket[(require
evm-redex/sim)] — a mutable chain that the scenario language drives, and that you
can drive yourself.

@defproc[(make-simulator [#:fork fork symbol? 'Prague]
                         [#:coinbase coinbase exact-nonnegative-integer? 0]
                         [#:number number exact-nonnegative-integer? 0]
                         [#:timestamp timestamp exact-nonnegative-integer? 0]
                         [#:basefee basefee exact-nonnegative-integer? 0]
                         [#:gas gas exact-positive-integer? 10000000]
                         [#:gas-price gas-price exact-nonnegative-integer? 0]) simulator?]{
A fresh chain: an empty world, a block at the given environment, and the given
transaction defaults.}

@deftogether[(
@defproc[(sim-account! [s simulator?] [name (or/c #f symbol?)]
                       [#:address address (or/c #f exact-nonnegative-integer?) #f]
                       [#:balance balance exact-nonnegative-integer? 0]
                       [#:nonce nonce exact-nonnegative-integer? 0]
                       [#:code code (listof byte?) '()]
                       [#:storage storage list? '()]
                       [#:abi abi any/c #f]) exact-nonnegative-integer?]
@defproc[(sim-fund! [s simulator?] [who (or/c symbol? exact-nonnegative-integer?)]
                    [wei exact-nonnegative-integer?]) void?]
@defproc[(name->address [name symbol?]) exact-nonnegative-integer?]
@defproc[(eth [n real?]) exact-nonnegative-integer?]
)]{
Declare an account (binding @racket[name] for later use) and top up a balance.
With no @racket[#:address], the account's address is @racket[(name->address name)]
— the low 20 bytes of @racket[keccak(name)], deterministic across runs.
@racket[eth] is the wei in @racket[n] ether.}

@defproc[(sim-deploy! [s simulator?] [creation (listof byte?)]
                      [#:from from (or/c symbol? exact-nonnegative-integer?)]
                      [#:value value exact-nonnegative-integer? 0]
                      [#:gas gas (or/c #f exact-positive-integer?) #f]
                      [#:name name (or/c #f symbol?) #f]
                      [#:abi abi any/c #f]) exact-nonnegative-integer?]{
Run a creation transaction, bind @racket[name] to the created address, and
remember @racket[abi] (so the contract's events decode in receipts).}

@defproc[(sim-send! [s simulator?]
                    [#:from from (or/c symbol? exact-nonnegative-integer?)]
                    [#:to to (or/c symbol? exact-nonnegative-integer?)]
                    [#:value value exact-nonnegative-integer? 0]
                    [#:data data (or/c #f (listof byte?)) #f]
                    [#:sig sig (or/c #f string?) #f]
                    [#:args args list? '()]
                    [#:gas gas (or/c #f exact-positive-integer?) #f]
                    [#:gas-price gas-price (or/c #f exact-nonnegative-integer?) #f]
                    [#:max-fee max-fee (or/c #f exact-nonnegative-integer?) #f]
                    [#:max-priority max-priority (or/c #f exact-nonnegative-integer?) #f]
                    [#:trace trace (or/c #f 'call 'step) #f]) receipt?]{
Submit a transaction from @racket[from]'s current nonce and return a
@racket[receipt]. Give either @racket[#:data] or @racket[#:sig] + @racket[#:args]
(ABI-encoded, names resolved). The world advances (even on revert — gas is still
spent and the nonce still bumps).}

@defproc[(sim-view [s simulator?]
                   [#:from from (or/c symbol? exact-nonnegative-integer?) 0]
                   [#:to to (or/c symbol? exact-nonnegative-integer?)]
                   [#:sig sig (or/c #f string?) #f]
                   [#:args args list? '()]
                   [#:data data (or/c #f (listof byte?)) #f]
                   [#:returns returns (or/c #f string? (listof string?)) #f]) any/c]{
A read-only frame call; returns the decoded values when @racket[#:returns] is
given, else the raw return bytes.}

@deftogether[(
@defproc[(sim-mine! [s simulator?] [#:number number (or/c #f exact-nonnegative-integer?) #f]
                    [#:timestamp timestamp (or/c #f exact-nonnegative-integer?) #f]
                    [#:coinbase coinbase (or/c #f exact-nonnegative-integer?) #f]) void?]
@defproc[(sim-withdraw! [s simulator?] [who (or/c symbol? exact-nonnegative-integer?)]
                        [wei exact-nonnegative-integer?]) void?]
)]{
Close the current block — the EIP-4788/EIP-2935 system calls (no-ops unless those
contracts exist) then the queued withdrawals — and advance the block.}

@deftogether[(
@defproc[(sim-balance [s simulator?] [who any/c]) exact-nonnegative-integer?]
@defproc[(sim-nonce   [s simulator?] [who any/c]) exact-nonnegative-integer?]
@defproc[(sim-code    [s simulator?] [who any/c]) (listof byte?)]
@defproc[(sim-storage [s simulator?] [who any/c] [slot exact-nonnegative-integer?]) exact-nonnegative-integer?]
@defproc[(sim-address [s simulator?] [name symbol?]) (or/c #f exact-nonnegative-integer?)]
@defproc[(sim-state-root [s simulator?]) bytes?]
@defproc[(sim-diff [s simulator?]) list?]
)]{
Read the current state, the state root, and the diff from the initial snapshot:
each diff entry is @racket[(list addr balance-delta nonce-before nonce-after
storage-changes)], where a storage change is @racket[(list slot before after)].}

@defstruct*[receipt ([status (or/c 'success 'revert 'error)]
                     [gas-used exact-nonnegative-integer?]
                     [output (listof byte?)]
                     [revert-reason (or/c #f string?)]
                     [logs list?] [logs-text (listof string?)]
                     [calls any/c] [steps any/c] [err (or/c #f string?)])
            #:transparent]{
What @racket[sim-send!] returns; @racket[print-receipt] renders it.
@racket[logs-text] is the decoded one-line-per-log rendering; @racket[calls] /
@racket[steps] are the traces when requested, else @racket[#f].}
