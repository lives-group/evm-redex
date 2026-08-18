#lang scribble/manual

@(require (for-label racket/base racket/contract))

@title[#:tag "asm"]{@tt{#lang evm-redex/asm} — writing and running EVM assembly}

@defmodulelang[evm-redex/asm]

@tt{#lang evm-redex/asm} is a small language whose source @emph{is} an EVM
program, written as assembly. Running the module — @tt{racket file.rkt}, or
@bold{Run} in DrRacket — assembles the text to bytecode, executes it on the
library's interpreter (the same semantics as the rest of @tt{evm-redex}), prints
a summary, and provides the result for inspection.

@margin-note{This is a @emph{fragment} runner, not a transaction simulator: no
nonce checks, no intrinsic-gas charge, an empty starting world (so @tt{CALL} /
@tt{SLOAD} against other contracts execute, but there is nothing there to reach).
It is the shortest path from "here is some EVM code" to "here is what it does".}

@section{A first program}

@codeblock|{
#lang evm-redex/asm
PUSH1 0x05
PUSH1 0x03
ADD
STOP
}|

Running it prints:

@verbatim|{
outcome:     stop
gas used:    9
stack:       [0x8]
returndata:  0x
}|

@tt{outcome} is the halt reason (@tt{stop}, @tt{return}, @tt{revert},
@tt{out-of-gas}, @tt{invalid-opcode}, …); @tt{stack} is the final stack, top
element first, each a 256-bit word in hex; @tt{returndata} is the bytes of a
@tt{RETURN} / @tt{REVERT}.

@section{The assembly dialect}

One instruction per token, one or more per line; mnemonics are
case-insensitive. A @litchar{;} begins a comment that runs to the end of the
line.

@bold{Opcodes.} Every opcode the interpreter implements is a mnemonic:
@tt{ADD}, @tt{MUL}, @tt{SSTORE}, @tt{JUMPI}, @tt{KECCAK256}, @tt{CALLDATALOAD},
@tt{DUP1}…@tt{DUP16}, @tt{SWAP1}…@tt{SWAP16}, @tt{LOG0}…@tt{LOG4}, @tt{RETURN},
@tt{REVERT}, and so on. An unknown mnemonic is a compile error.

@bold{PUSH.} Three forms:

@itemlist[
@item{@bold{Explicit} — @tt{PUSH1 0x05} … @tt{PUSH32 0x..}: the operand is padded
      to exactly @italic{k} bytes; an operand too wide for @italic{k} is an error.}
@item{@bold{Auto-sized} — a bare @tt{PUSH} with a literal chooses the minimal
      width: @tt{PUSH 0x1234} becomes @tt{PUSH2}. (@tt{PUSH 0} is @tt{PUSH1 0x00};
      the zero-byte @tt{PUSH0} opcode is written @tt{PUSH0}.)}
@item{@bold{Label} — @tt{PUSH @italic{name}} references a label (below) and is
      always emitted as @tt{PUSH2}.}
]

Operands are hexadecimal (@tt{0x..}) or decimal.

@bold{Labels.} A token @tt{name:} defines a label at the current position; a bare
@tt{name} as a @tt{PUSH} operand references it, and the assembler resolves it to
that position's program counter. This is how jumps are written without counting
bytes:

@codeblock|{
#lang evm-redex/asm
.gas 100000

      PUSH1 0x00        ; counter = 0
loop: JUMPDEST
      PUSH1 0x01
      ADD               ; counter += 1
      DUP1              ; [counter, counter]
      PUSH1 0x0a        ; [10, counter, counter]
      GT                ; 10 > counter ?
      PUSH loop         ; the JUMPDEST's pc, resolved for you
      JUMPI             ; if so, loop
      STOP              ; leaves 10 on the stack
}|

@verbatim|{
outcome:     stop
gas used:    293
stack:       [0xa]
}|

@bold{Directives} configure the run; they must come before any instruction:

@itemlist[
@item{@litchar{.gas} @italic{N} — the gas budget (default 30,000,000).}
@item{@litchar{.calldata} @tt{0x..} — the transaction calldata, for
      @tt{CALLDATALOAD} / @tt{CALLDATASIZE} / @tt{CALLDATACOPY}.}
]

@codeblock|{
#lang evm-redex/asm
.calldata 0x00000000000000000000000000000000000000000000000000000000000000ff

PUSH1 0x00
CALLDATALOAD      ; the first 32-byte word of calldata
PUSH1 0x00
MSTORE
PUSH1 0x20
PUSH1 0x00
RETURN            ; return memory[0..32)
}|

@verbatim|{
outcome:     return
gas used:    21
stack:       []
returndata:  0x00000000000000000000000000000000000000000000000000000000000000ff
}|

@bold{Raw bytecode.} If the source begins with @tt{0x}, the whole thing is taken
as a hex bytecode blob — handy for pasting @tt{solc} output and disassembling or
running it as-is. (A mnemonic program never starts with @tt{0x}, so there is no
ambiguity.)

@codeblock|{
#lang evm-redex/asm
0x6005600301
}|

@section{Using the result from other modules}

A @tt{#lang evm-redex/asm} module @racket[provide]s three bindings, so another
module can @racket[require] it and inspect what the program did — this is how the
example programs are tested:

@itemlist[
@item{@racket[program-bytes] — the assembled bytecode, a list of bytes.}
@item{@racket[result] — an @racket[evm-result] (outcome, gas, stack, return
      data, and the final machine).}
@item{@racket[final-machine] — the final machine term, for reading anything else
      (memory, logs, world) with @racket[machine-field].}
]

@racketblock[
(require "arithmetic.rkt")
(evm-result-stack result)     (code:comment "=> '(8)")
program-bytes                 (code:comment "=> '(96 5 96 3 1 0)")
]

@section{The programmatic API}

The same assembler, disassembler, and runner are available as ordinary functions
through @racket[(require evm-redex/asm)] — no @tt{#lang} needed. This is what the
language is built on, and what the unit tests exercise directly.

@defproc[(assemble [src (or/c string? input-port?)])
         (values (listof byte?) asm-config?)]{
Parse and assemble EVM assembly text, returning the bytecode and the
@racket[asm-config] the directives asked for. Raises
@racket[exn:fail:evm-asm?] on a syntax or assembly error (unknown mnemonic,
undefined/duplicate label, an operand too wide for an explicit @tt{PUSHk}), with
the source line and column in the message.

@racketblock[
(define-values (code cfg) (assemble "PUSH1 0x05\nPUSH1 0x03\nADD\nSTOP"))
(code:comment "code => '(96 5 96 3 1 0)")
]}

@defproc[(run-source [code (listof byte?)] [config asm-config?]) evm-result?]{
Run assembled bytecode on the interpreter and package the outcome. The wrapper
pins the fast Racket executor and folds any Racket-level failure (e.g. fuel
exhaustion) into an @racket['error] outcome.}

@defstruct*[evm-result ([outcome symbol?]
                        [gas-used exact-nonnegative-integer?]
                        [gas-left exact-nonnegative-integer?]
                        [stack (listof exact-nonnegative-integer?)]
                        [returndata (listof byte?)]
                        [machine any/c]
                        [err (or/c #f string?)]) #:transparent]{
What a run produced. @racket[stack] is top-first; @racket[machine] is the final
machine term (read more from it with @racket[machine-field] from
@racket[(require evm-redex)]).}

@defproc[(disassemble [code (listof byte?)]) string?]{
The inverse of @racket[assemble] for the byte level: one instruction per line,
@tt{PUSH} immediates shown inline as hex, immediates skipped the way the
interpreter skips them. Re-assembling the result reproduces the same bytes.

@racketblock[
(disassemble (list #x61 #x00 #x56 #x00))
(code:comment "=> \"PUSH2 0x0056\\nSTOP\"")
]}

@defstruct*[asm-config ([gas exact-nonnegative-integer?]
                        [calldata (listof byte?)]) #:transparent]{
The environment the directives select: the gas budget and the calldata.}

@defproc[(parse-evm [input (or/c string? input-port?)]
                    [#:source source any/c 'evm]) parsed?]{
The parser alone: assembly text @tt{->} an AST (@racket[parsed], carrying
@racket[dir] directives and a stream of @racket[label] / @racket[instr] items
with source locations). Exposed for tools that want to walk a program without
assembling it; @racket[assemble] is @racket[parse-evm] followed by
@racket[assemble-parsed].}
