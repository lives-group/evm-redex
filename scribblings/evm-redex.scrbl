#lang scribble/manual

@(require (for-label racket/base racket/contract))

@title{evm-redex: An Executable EVM Specification in PLT Redex}
@author{Rodrigo Ribeiro}

@margin-note{This document describes the library and its @emph{public interface}.
For a quick-start and installation, see the repository @filepath{README.md}. A
bilingual @secref["tutorial-en" #:doc '(lib "evm-redex/tutorial/tutorial-en.scrbl")]
ships with the library; the worked examples,
test suites, and conformance corpora live in the companion
@hyperlink["https://github.com/lives-group/evm-redex-tests"]{@tt{evm-redex-tests}}
package.}

@bold{evm-redex} is an executable specification of the Ethereum Virtual Machine,
written in @hyperlink["https://redex.racket-lang.org/"]{PLT Redex} and Racket. It
models the EVM as a formal small-step semantics — a Redex reduction relation over
an explicit machine / world / transaction grammar — and runs that semantics
directly against the official Ethereum test vectors
(@hyperlink["https://github.com/ethereum/tests"]{@tt{ethereum/tests}} / EEST),
from which it derives its structure alongside
@hyperlink["https://github.com/ethereum/execution-specs"]{@tt{ethereum/execution-specs}}.

The default target hard fork is @bold{Prague}, and the semantics is
@bold{multi-fork} (Frontier through Prague). The project is designed to be read
and tested: every opcode, gas rule, and precompile is an inspectable rewrite rule
or metafunction, and conformance is established by computing real post-state
roots and log hashes and comparing them against the reference fixtures.

The library's public interface is three modules: @racket[(require evm-redex)] —
the whole specification (grammar, interpreter, world and transaction machinery,
fork gates, precompiles); @racket[(require evm-redex/pbt)] — the property-based
testing DSL, documented in full below; and @racket[(require evm-redex/crypto)] —
the curve and field primitives. Nothing else is public: @filepath{private/} is an
implementation detail and should never be imported directly.

@bold{New here?} Start with the step-by-step tutorial that writes, compiles, and
tests a small Solidity contract:
@other-doc['(lib "evm-redex/tutorial/tutorial-en.scrbl")] (English) /
@other-doc['(lib "evm-redex/tutorial/tutorial-pt.scrbl")] (português).

The worked examples and the test suites live in the companion
@hyperlink["https://github.com/lives-group/evm-redex-tests"]{@tt{evm-redex-tests}}
package (which depends on this one) and are documented there.

The library also ships two languages: @tt{#lang evm-redex/asm}, whose source is an
EVM program in assembly (@secref["asm"]), and @tt{#lang evm-redex/sim}, a
transaction-@tech{scenario} language that submits transactions against an evolving
chain and reports receipts, traces, and a state diff (@secref["sim"]).

@table-of-contents[]

@include-section["asm.scrbl"]

@include-section["sim.scrbl"]

@include-section["pbt.scrbl"]

@section[#:tag "tracing"]{Tracing hook}

@declare-exporting[evm-redex]

@defparam[current-frame-tracer tracer
          (or/c #f (-> exact-nonnegative-integer?
                       (listof byte?)
                       (or/c 'runtime 'init)
                       (or/c #f (-> exact-nonnegative-integer? byte? any/c any))))
          #:value #f]{
Exported by @racket[(require evm-redex)]. The one hook into the execution loop:
it reports which instruction each frame actually executed, which is the one fact a
tool outside the library cannot reconstruct for itself. Coverage measurement is
built on it — the harness lives in the companion @tt{evm-redex-tests} package,
since it is a way of @emph{using} the semantics rather than part of them.

When set, the tracer is called @bold{once per frame} with the frame's code
address, its code, and whether that code is the one installed at the address
(@racket['runtime]) or a constructor's init code (@racket['init]). It returns
either @racket[#f] (do not trace this frame) or a procedure, which is then called
@bold{once per step} with the pc, the opcode, and the machine @emph{after} the
step — enough to see which way a @tt{JUMPI} went.

It sits in the driver loop (@racket[run] / @racket[run-trace]) rather than in
either executor, and that placement is the whole design:

@itemlist[
@item{Every frame passes through it — the top frame, each
      @tt{CALL}/@tt{DELEGATECALL}/@tt{STATICCALL} child, and a @tt{CREATE}'s init
      code all re-enter @racket[run] — so nested calls are traced for free.}
@item{It is oblivious to which backend ran the step, so a trace is identical under
      @tt{EVM_EXEC_BACKEND=spec}, and the @tt{EVM_ORACLE} differential (which runs
      a step through both executors) does not report it twice.}
@item{Nothing is added to the machine term, so the Redex/Racket differential
      oracle and the @racket[exec] metafunction cache are untouched.}
]

The parameter is read once per frame, not per step: with no tracer installed the
per-step cost is a single test of a local @racket[#f], which is not measurable.}

@defparam[current-call-observer observer
          (or/c #f procedure?) #:value #f]{
Exported by @racket[(require evm-redex)]. A coarser companion to
@racket[current-frame-tracer], fired at each nested @tt{CALL}/@tt{CREATE}
@bold{boundary} rather than each step — which is exactly a call tree. When set it
is called on @racket['enter] as @racket[(observer 'enter kind caller callee value
gas depth)] and on @racket['exit] as @racket[(observer 'exit success? gas-left
output)]; enter/exit nest as a well-formed tree. The simulator's call trace
(@secref["sim"]) is built on it. Like the frame tracer it lives at the @racket[run]
boundary — backend-agnostic, off by default (cost is one test of a local
@racket[#f] per call), and outside the machine term. Pre-execution failures (depth
limit, insufficient balance) that never spawn a child frame are not reported.}

@section{Repository layout}

@verbatim|{
private/            the specification
  lang.rkt          Redex grammar (machine, world, block, tx)
  interpreter.rkt   opcode semantics (step), CALL/CREATE/SELFDESTRUCT, run loop
  transaction.rkt   tx validation, intrinsic gas, refunds, tx-type parsing
  gas.rkt           gas schedule (EIP-2929 warm/cold, SSTORE, memory)
  state.rkt mem.rkt words.rkt   world/account, memory, 256-bit words
  forks.rkt         multi-fork config: fork order, EIP gates, opcode gating
  keccak.rkt hashes.rkt rlp.rkt mpt.rkt   crypto + encoding + Merkle-Patricia trie
  ec.rkt fields.rkt pairing.rkt bls.rkt   secp256k1 / bn254 / BLS12-381 / KZG
  precompiles.rkt   0x01-0x11 precompiled contracts
  native.rkt        optional native crypto accelerators (FFI) w/ pure fallback
                    (the run loop also carries current-frame-tracer, the hook
                     coverage tools are built on)

pbt/                property-based testing DSL (require evm-redex/pbt)
  property.rkt      define-evm-property / check- / run-
  vocab.rkt gen.rkt observation vocabulary + EVM-tuned generators
  execute.rkt deploy.rkt solc.rkt   deploy/run/call, artifact reader

asm/                the #lang evm-redex/asm language + programmatic API
  opcodes.rkt       canonical mnemonic <-> byte table
  parse.rkt assemble.rkt   assembler (labels, auto-PUSH) + disassembler
  execute.rkt       run assembled bytecode -> evm-result
  runtime.rkt lang/reader.rkt   the module language and its reader

sim/                the #lang evm-redex/sim transaction simulator + engine
  engine.rkt        the stateful chain (deploy / send / view / mine / diff)
  state-root.rkt block.rkt   state commitments; block-boundary processing
  abi.rkt receipt.rkt trace.rkt   revert/event decode, receipts, call/step traces
  parse.rkt runtime.rkt lang/reader.rkt   the scenario language and its reader
main.rkt            the whole specification (require evm-redex)
asm.rkt             assembler / runner as functions (require evm-redex/asm)
sim.rkt             the simulator engine as functions (require evm-redex/sim)
crypto.rkt          curve/field primitives (require evm-redex/crypto)

tutorial/           the step-by-step tutorial (EN + PT) and its runnable examples
  contracts/*.sol   the example contracts, with their compiled *.json artifacts
  counter.rkt token.rkt ballot.rkt auction.rkt purchase.rkt explore.rkt
  tutorial-en.scrbl tutorial-pt.scrbl  the two-language documents

flake.nix flake.lock  reproducible dev environment (Racket + all native libs)
shell.nix mcl.nix     non-flake dev shell / herumi-mcl build recipe
}|

The test suites, worked examples, bundled conformance corpora, and the Web3Bugs
reproduction benchmark live in the companion
@hyperlink["https://github.com/lives-group/evm-redex-tests"]{@tt{evm-redex-tests}}
package, which depends on this library.
}
