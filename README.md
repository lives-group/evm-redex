# An executable specification of the Ethereum Virtual Machine

`evm-redex` models the EVM as a formal small-step semantics (a Redex reduction
relation over an explicit machine/world/transaction grammar) and runs that
semantics directly against the official Ethereum test vectors. It is derived
from [`ethereum/execution-specs`](https://github.com/ethereum/execution-specs)
and validated against [`ethereum/tests`](https://github.com/ethereum/tests) /
EEST.

It is not a re-implementation of a client in Racket for speed; it is a
_specification you can execute and test_ — every opcode, gas rule, and
precompile is expressed as an inspectable rewrite rule or metafunction, and
conformance is established by computing real post-state roots and log hashes and
comparing them to the reference fixtures.

## What it covers

- **Full opcode semantics** — arithmetic/stack/memory/storage, `CALL`/`CREATE`
  family, `SELFDESTRUCT`, transient storage, `MCOPY`, `BLOBHASH`, `PUSH0`, …
- **Transaction processing** — legacy + typed transactions (access-list, 1559,
  blob, set-code/EIP-7702), intrinsic gas, refunds, validation of invalid txs.
- **Block-level processing** — system calls (beacon-root, history), ordered
  transactions, withdrawals, state/receipts roots (own Keccak, RLP, and
  Merkle-Patricia trie).
- **All precompiles `0x01`–`0x11`** — ecrecover, SHA-256, RIPEMD-160, identity,
  modexp, bn254 (ecadd/ecmul/ecpairing), BLS12-381 (EIP-2537), KZG point
  evaluation, blake2f.
- **Multi-fork semantics** — a per-fork EIP/gas/opcode configuration from
  Frontier through **Prague** (the default target); the harness runs every fork
  present in each fixture.
- **Optional native crypto accelerators** — opt-in FFI (GMP, libsecp256k1, blst,
  mcl) that fall back to the pure Racket implementations byte-identically.

## Requirements

Core (needed to run the spec and the whole pure-Racket test battery):

- **[Racket](https://racket-lang.org/) 8+** — provides `racket` and `raco`.
- The **`redex-lib`** package — the semantics is written in PLT Redex. The full
  Racket distribution already bundles it; on `racket-minimal` install it with
  `raco pkg install redex-lib`.
- **libgmp** — already a dependency of Racket, so it is present wherever Racket
  is. It is used automatically to accelerate big modular exponentiation.

Optional (only for the extra native crypto accelerators — the spec and all tests
run without them, in pure Racket):

- **[Nix](https://nixos.org/)** with flakes enabled — the easiest and fully
  reproducible way to get _everything_, including the native libraries.
- Or, without Nix: **libsecp256k1** (with the recovery module), **blst**, and
  **herumi/mcl** on your `LD_LIBRARY_PATH`. These are Linux-only extras.

## Installation

### Option A — Nix flake (recommended, reproducible)

The provided `flake.nix` pins nixpkgs (via `flake.lock`) and provisions the
complete environment: Racket (with redex), libgmp, libsecp256k1, a shared `blst`
(built from the static archive nixpkgs ships), and herumi/mcl (built from source
via `mcl.nix`, since it is not packaged in nixpkgs). Entering the shell puts all
of them on `LD_LIBRARY_PATH`:

```sh
nix develop            # builds/fetches everything, enters the dev shell
```

Then run any of the test commands below; the native accelerators will be active.
This is the way to reproduce the reported results bit-for-bit — the same nixpkgs
revision yields the same toolchain and libraries. You can also build the native
libraries on their own:

```sh
nix build .#mcl          # herumi/mcl (libmclbn384_256.so, bn254)
nix build .#blst-shared  # libblst.so
```

> Note: `mcl.nix` fetches external cryptography source (herumi/mcl, pinned by
> commit + hash). Review it before building. Without flakes, `nix-shell` reads
> the equivalent `shell.nix`.

### Option B — manual (pure Racket, any platform)

Install Racket (the [official installer](https://download.racket-lang.org/),
`nix profile install nixpkgs#racket`, or your distro's package), ensure
`redex-lib` is available, and you're done — the full test battery runs in pure
Racket with no native libraries. To additionally enable the native accelerators,
install libsecp256k1 / blst / mcl and put their `.so` files on `LD_LIBRARY_PATH`
(see the accelerator table at the end); each is opt-in and falls back to pure
Racket when absent.

## Documentation

In-depth documentation — a general description of the library and the reference
for its public interface — is written in
[Scribble](https://docs.racket-lang.org/scribble/) under `scribblings/` and
pre-rendered to HTML in [`html/`](html/). A **step-by-step tutorial** — writing,
compiling, and testing a small Solidity contract — is rendered alongside it, in
[`tutorial/`](tutorial/), in English (`tutorial-en.scrbl`) and Portuguese
(`tutorial-pt.scrbl`), with its runnable examples next to it (`tutorial/*.rkt`).

The test-suite documentation lives with the tests, in the companion
[`evm-redex-tests`](https://github.com/lives-group/evm-redex-tests) package.

**📖
[Read the rendered documentation](https://htmlpreview.github.io/?https://github.com/lives-group/evm-redex/blob/main/html/evm-redex.html)**

> GitHub serves a committed `.html` file as source text, not as a page, so the
> link above goes through
> [htmlpreview.github.io](https://htmlpreview.github.io/), which fetches
> `html/evm-redex.html` and renders it. Alternatively, enable GitHub Pages for
> the `html/` folder and browse
> [lives-group.github.io/evm-redex](https://lives-group.github.io/evm-redex/)
> (it redirects to the docs via `html/index.html`).

Rebuild the HTML after editing the `.scrbl` sources with (all documents in one
invocation, so cross-references between them are linked relative to `html/`):

```sh
raco scribble --html --dest html \
  ++xref-in setup/xref load-collections-xref \
  --redirect-main https://docs.racket-lang.org/ \
  scribblings/evm-redex.scrbl tutorial/tutorial-en.scrbl tutorial/tutorial-pt.scrbl

# The docs are published single-page (one .html each), but section cross-references
# between them resolve against the multi-page *installed* layout; collapse
# `../doc/<name>/x.html` onto the single page `<name>.html` (anchors preserved).
for f in html/*.html; do
  sed -i 's#\.\./doc/\([a-zA-Z0-9_-]*\)/[a-zA-Z0-9._-]*\.html#\1.html#g' "$f"
done
```

The `++xref-in` / `--redirect-main` flags load the installed documentation
cross-reference index and point core-Racket links (`require`, `racket/base`, …)
at the online docs, so the build is warning-free.

## Running the tests

The library itself carries only the runnable tutorial examples, checked with
`raco test tutorial`. The full test suite — the unit suites, the bundled
`ethereum/tests` corpora under rackunit, the parallel full-conformance runner (a
full run reports **1274 pass / 0 fail**), the property-based worked examples, and
the equivalence/coverage harnesses — lives in the companion
[`evm-redex-tests`](https://github.com/lives-group/evm-redex-tests) package, which
declares a dependency on this one. Install both (by local link during
development), then run the suite from that package:

```sh
raco pkg install --link ../evm-redex      # the library
raco pkg install --link .                 # this test package
raco test -p evm-redex-tests              # the whole battery
raco test unit/                           # fast unit suites
racket conformance/runner.rkt             # full parallel conformance runner
```

Every test module imports the specification only through its public surface
(`(require evm-redex)`, `evm-redex/pbt`, `evm-redex/crypto`). See that package's
`README.md` and its rendered documentation for the full layer-by-layer guide.

### Native accelerators (optional)

The pure Racket build passes the whole battery on its own. To additionally
exercise the native crypto paths (and speed up crypto-heavy fixtures), enter the
Nix environment (`nix develop` for the flake, or `nix-shell`), which provisions
GMP, libsecp256k1, blst, and mcl and puts them on `LD_LIBRARY_PATH`:

```sh
nix develop
racket -e '(require evm-redex) native-status'
;; => ((keccak256 . pure) (secp256k1-recover . native) (modular-expt . native)
;;     (blst . native) (mcl-bn254 . native))
```

The native paths are **oracle-gated**: each accelerator is adopted only after it
reproduces the pure result, so results are identical to the pure build — the
whole battery reports the same `1274 pass / 0 fail` in either mode.

## Writing EVM assembly: `#lang evm-redex/asm`

The library ships a small language whose source **is** an EVM program in
assembly. Running the module assembles it to bytecode, executes it on this
semantics, prints a summary, and `provide`s the result.

```
#lang evm-redex/asm
PUSH1 0x05
PUSH1 0x03
ADD
STOP
```

```
$ racket add.rkt
outcome:     stop
gas used:    9
stack:       [0x8]
returndata:  0x
```

The dialect has labels for jumps (`loop:` … `PUSH loop`, resolved to the
JUMPDEST's pc), auto-sized `PUSH` (`PUSH 0x1234` → `PUSH2`), directives `.gas`
and `.calldata`, comments with `;`, and a raw-hex fallback for pasting `solc`
output. The same assembler/disassembler/runner are also available as functions
via `(require evm-redex/asm)` — `assemble`, `disassemble`, `run-source`. See the
[rendered docs](#documentation) ("#lang evm-redex/asm"); examples and tests live
in the companion [`evm-redex-tests`](https://github.com/lives-group/evm-redex-tests) package.

## Simulating transactions: `#lang evm-redex/sim`

Where `#lang evm-redex/asm` runs a single code fragment, `#lang evm-redex/sim`
runs **transactions**: its source is a scenario that declares accounts and state
and submits a sequence of transactions against an evolving chain. Running it
prints a receipt per transaction and the final state.

```
#lang evm-redex/sim
.account ALICE balance=10eth
.account BOB
.deploy TOKEN from=ALICE code=@Token.json:Token

tx from=ALICE to=TOKEN sig="mint(address,uint256)" args=(ALICE, 1000)
tx from=ALICE to=TOKEN sig="transfer(address,uint256)" args=(BOB, 100) trace=call
call from=BOB to=TOKEN sig="balanceOf(address)" args=(BOB) returns=uint256
```

```
tx ALICE -> TOKEN
  status:    success
  gas used:  52122
  log:       Transfer(from=0xacc0…0001, to=0xacc0…0002, value=0x64)
  return:    0x0000…0001
...
--- final state ---
state root: 0x81a5f2e3…4882
0x479a82…cce7  nonce 0->1
    slot 0x0: 0x0 -> 0x3e8
```

It threads the world from one transaction to the next (nonces auto-increment,
gas is charged, reverts roll back), and each receipt can carry a decoded event
log, a decoded revert reason (`Error("…")` / `Panic`), a **call trace**
(`trace=call`) or a **step trace** (`trace=step`); the run ends with the state
root and a diff. The engine is also usable as functions via
`(require
evm-redex/sim)` — `make-simulator`, `sim-deploy!`, `sim-send!`,
`sim-view`, `sim-mine!`, `sim-diff`. See the [rendered docs](#documentation)
("#lang evm-redex/sim"); examples and tests live in the companion
[`evm-redex-tests`](https://github.com/lives-group/evm-redex-tests) package.

## Property-based testing of EVM programs

`evm-redex/pbt` (under [`pbt/`](pbt/)) is a small DSL for **validating
properties of a fixed EVM program** — a routine of opcodes or a whole
contract/transaction — by running it through the semantics on many generated
concrete inputs (property-based testing via
[rackcheck](https://docs.racket-lang.org/rackcheck/)). You write a Hoare-style
contract — `#:pre` / `#:post`, per-step `#:invariant`, and `#:revert-when` /
`#:succeed-when` obligations — and the engine searches for a counterexample,
shrinking it to a minimal failing input.

```racket
(require evm-redex/pbt)

(define-evm-property add-wraps
  #:code  (list 1)                       ; ADD
  #:given ([a gen-word] [b gen-word])
  #:stack (list a b) #:gas 100
  #:post  (lambda (m0 m1) (= (stack-top m1) (u-add a b))))

(check-evm-property add-wraps)           ; passes; drop the wrap and it shrinks
                                         ; the counterexample to (MAX-U256, 1)
```

It is property-based testing, so it finds bugs rather than proving their
absence, but it places no restriction on the program (precompiles, non-linear
arithmetic and loops all just execute). Its own tests live in the companion
`evm-redex-tests` package (under `pbt/`).

Full documentation of the DSL, the observation vocabulary, and the generators is
in the [rendered docs](#documentation) ("Property-based testing of EVM
programs"). Requires the `rackcheck` package (`raco pkg install rackcheck`, or
it is pulled in as a dependency by `raco pkg install` of this package).

### Testing real Solidity contracts

The DSL closes the loop from Solidity to a checked property: `read-artifact`
extracts the creation/runtime bytecode from a `solc`/Foundry/Hardhat artifact,
`deploy` runs the constructor, `encode-call` / `call` / `abi-decode` build
calldata and read getters (static _and_ dynamic ABI types), and
`define-evm-property` states the invariant. Worked examples (in the companion `evm-redex-tests` package):

- `erc20/`, `erc721/` — the minimal ERC-20 / ERC-721 from _solidity-by-example_,
  compiled with `solc`.
- `open-zeppelin/` — property specifications for a curated set of
  **OpenZeppelin** contracts (ERC20, Ownable, AccessControl, ERC721, ERC1155),
  using the verbatim OZ sources plus a concrete instance per contract. Each
  `properties/<Contract>.rkt` attests the invariant classes the OZ audits
  emphasise — supply/ownership conservation, allowance and access-control
  semantics, no unauthorized mint, ERC-conformant reverts (and OZ specifics such
  as the infinite-allowance optimisation). See `open-zeppelin/README.md` there.

```sh
raco test open-zeppelin/properties/ERC20.rkt   # one OZ contract
raco test open-zeppelin/properties/            # all of them (slow)
```

These runs are **slow** — every trial executes the contract through the pure
interpreter — so trial counts are modest. Running the ERC-721 example even
surfaced a genuine interpreter bug (a `LOG` that failed to pop its stack
operands, which the conformance fixtures masked): PBT over real contracts
exercises code paths the reference vectors do not. These examples, and a
benchmark reproducing real Code4rena bugs, live in the companion `evm-redex-tests` package.

The same has happened from outside: the sibling `yul-redex` project, whose EVM
dialect delegates here, runs the Solidity compiler's own Yul interpreter corpus
(`test/libyul/yulInterpreterTests`), and two of those files caught a
**zero-length memory copy expanding memory**. `execution-specs` keeps two things
apart that this specification had merged — `calculate_gas_extend_memory` skips
zero-size extensions, so `expand_by` is 0, and `memory_write` of an empty value
writes nothing — whereas here the expansion was a side effect of `mem-write`,
which rounded `dest + 0` up to a 32-byte boundary. Memory grew for free (the gas
was already correctly zero) and `MSIZE` was wrong, for `CALLDATACOPY`,
`CODECOPY`, `EXTCODECOPY`, `RETURNDATACOPY` and `MCOPY` alike. Fixed in
`private/mem.rkt`; regression test in `evm-redex-tests`, `unit/zero-length-copy-test.rkt`.
Like the `LOG` bug, the conformance fixtures never exercised it.

### The tracing hook

`(require evm-redex)` exports `current-frame-tracer`, the one hook into the
execution loop: it reports which instruction each frame actually executed, which
is the one fact a tool outside the library cannot reconstruct for itself. Set,
it is called **once per frame** (with the code address, the code, and whether
that code is `'runtime` or a constructor's `'init`) and returns a procedure
called **once per step** (with the pc, the opcode, and the machine after the
step — enough to see which way a `JUMPI` went).

It sits in the driver loop rather than in either executor, so a trace sees
nested `CALL`/`CREATE` frames for free, is identical under the Redex reference
backend, and is not double-reported by the `EVM_ORACLE` differential. Nothing is
added to the machine term. With no tracer installed the per-step cost is a
single test of a local `#f` — not measurable.

Two harnesses built on this library's public surface live in the companion
`evm-redex-tests` package, since each is a way of _using_ the semantics rather
than part of them:

- **Coverage** (`coverage/`) — how much of a contract a property executed:
  instructions, `JUMPI` branches (both sides?), and Solidity lines via the solc
  source map. Built on the hook above.
- **Contract equivalence** (`equivalence/`) — twin worlds, generated call
  sequences, ABI-level comparison of two contracts.

## Repository layout

```
private/            the specification
  lang.rkt          Redex grammar (machine, world, block, tx)
  interpreter.rkt   opcode semantics (step), CALL/CREATE/SELFDESTRUCT, run loop
  transaction.rkt   tx validation, intrinsic gas, refunds, tx-type parsing
  gas.rkt           gas schedule (EIP-2929 warm/cold, SSTORE, memory)
  state.rkt mem.rkt words.rkt   world/account, memory, 256-bit words
  forks.rkt         multi-fork config: fork order, EIP gates, opcode gating
  keccak.rkt hashes.rkt rlp.rkt mpt.rkt   crypto + encoding + Merkle-Patricia trie
  ec.rkt fields.rkt pairing.rkt bls.rkt   secp256k1 / bn254 / BLS12-381 / KZG
  precompiles.rkt   0x01–0x11 precompiled contracts
  native.rkt        optional native crypto accelerators (FFI) with pure fallback

pbt/                property-based testing DSL (evm-redex/pbt)
  vocab.rkt         observation vocabulary for pre/post/invariant predicates
  execute.rkt       run-fragment / run-txn / call, uniform outcome classification
  gen.rkt           edge-biased rackcheck generators (words, storage, calldata…)
  property.rkt      define-evm-property / check-evm-property / run-evm-property
  main.rkt          the assembled DSL
asm/ sim/            the #lang evm-redex/asm and #lang evm-redex/sim languages
main.rkt            the whole specification         (require evm-redex)
pbt.rkt             property-based testing DSL       (require evm-redex/pbt)
asm.rkt             EVM assembler / runner           (require evm-redex/asm)
sim.rkt             transaction simulator engine     (require evm-redex/sim)
crypto.rkt          curve/field primitives           (require evm-redex/crypto)

flake.nix flake.lock  reproducible dev environment (Racket + all native libs)
shell.nix           non-flake dev shell enabling the native accelerators
mcl.nix             build recipe for herumi/mcl (bn254; not in nixpkgs)
```

The test suites, worked examples, bundled conformance corpora, and the Web3Bugs
reproduction benchmark live in the companion `evm-redex-tests` package, which
depends on this library.

## Multi-fork semantics

`private/forks.rkt` holds the linear fork list (Frontier→Prague), fixture-name
aliases (`Merge`→Paris, `EIP150`→Tangerine, …), a dynamic `current-fork`
parameter, and the derived EIP gates (`access-lists?`, `base-fee?`,
`init-code-metering?`, `calldata-floor?`, …) plus refund/opcode gating.
Everything defaults to Prague, so with no `parameterize` the behaviour is
byte-identical to a Prague-only build. The warm/cold (EIP-2929) gas engine is
exact for **Berlin→Prague**; older forks get the correct EIP _gates_ but share
that access-gas model.

## Native crypto accelerators

Pure-Racket crypto is correct but slow. `private/native.rkt` binds fast native
libraries when present and adopts each accelerator **only if it reproduces the
pure result** (the pure implementation is the oracle). When a library is absent
it falls back to pure Racket, so nothing here is required to run the spec.

| accelerator         | library           | used by                                                                       | speedup           |
| ------------------- | ----------------- | ----------------------------------------------------------------------------- | ----------------- |
| `modular-expt`      | libgmp (mpz_powm) | modexp precompile (0x05), big exponents                                       | ~10×              |
| `secp256k1-recover` | libsecp256k1      | ecrecover precompile (0x01)                                                   | native ECDSA      |
| `blst`              | blst              | BLS12-381 add·mul + pairing (0x0b–0x0f), map-to-curve (0x10/0x11), KZG (0x0a) | ~2500× on pairing |
| `mcl-bn254`         | herumi/mcl        | bn254/alt_bn128 ecadd·ecmul·ecpairing (0x06/0x07/0x08)                        | ~1100× on pairing |

`libgmp` is a Racket dependency, so the modexp accelerator is active out of the
box; the rest need to be on `LD_LIBRARY_PATH`, which `shell.nix` arranges (it
builds `libblst.so` from the static archive nixpkgs ships, and builds herumi/mcl
via `mcl.nix` — mcl is not in nixpkgs, so review that recipe before building, as
it fetches external source).
