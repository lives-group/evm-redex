#lang scribble/manual

@(require (for-label racket/base racket/contract))

@title[#:tag "pbt"]{Library reference: the property-based testing DSL}

@defmodule[evm-redex/pbt]

The @racketmodname[evm-redex/pbt] library lets you @emph{validate properties of
a fixed EVM program} — a routine of opcodes or a whole contract/transaction — by
running it through the semantics on many generated concrete inputs. You write the
property as a Hoare-style contract (pre/post conditions), optionally with per-step
invariants and revert/success obligations, and the engine searches for a
counterexample, shrinking it to a minimal failing input.

@margin-note{This is property-based testing, so it is @bold{incomplete}: it finds
counterexamples, it does not prove their absence. In exchange it runs on concrete
inputs and so places no restriction on the program — cryptographic precompiles,
non-linear arithmetic and loops all simply execute. It builds on
@hyperlink["https://docs.racket-lang.org/rackcheck/"]{rackcheck} (generation and
shrinking) and Redex's own @racket[redex-check].}

This chapter documents every exported binding. For a hands-on, step-by-step
walkthrough that applies the library to real Solidity contracts, read the
@secref["tutorial-en" #:doc '(lib "evm-redex/tutorial/tutorial-en.scrbl")] first;
this reference explains each piece it uses.

@racketblock[(require evm-redex/pbt)]

@; ======================================================================
@section{Defining properties}

@defform[#:literals (: unquote)
         (define-evm-property name clause ...)
         #:grammar
         [(clause (code:line #:code bytes)
                  (code:line #:call tx)
                  (code:line #:contract bytes)
                  (code:line #:address addr)
                  (code:line #:given ([x gen] ...))
                  (code:line #:world world)
                  (code:line #:block block)
                  (code:line #:stack stack)
                  (code:line #:gas gas)
                  (code:line #:memory memory)
                  (code:line #:msg msg)
                  (code:line #:orig-storage store)
                  (code:line #:pre expr)
                  (code:line #:post proc)
                  (code:line #:invariant proc)
                  (code:line #:revert-when expr)
                  (code:line #:succeed-when expr)
                  (code:line #:fuel n)
                  (code:line #:trials n)
                  (code:line #:seed n))]]{

Binds @racket[name] to an @racket[evm-property]. The clauses fall into four
groups.

@bold{Program} (choose exactly one). @racket[#:code] gives @deftech{fragment
mode}: @racket[bytes] is a list of opcode bytes, run from a fresh machine with
@racket[run]. @racket[#:call] gives @deftech{transaction mode}: @racket[tx] is
built with @racket[make-tx] and run with the full transaction machinery; pair it
with @racket[#:world] (a pre-state), or with @racket[#:contract] and
@racket[#:address] to install a contract's runtime code in a minimal world.

@bold{Inputs and setup}. @racket[#:given] lists the generated inputs; each
@racket[x] is bound (to a concrete value) in every other clause. In fragment mode
the initial machine is configured with @racket[#:stack] (top first),
@racket[#:gas], @racket[#:memory], @racket[#:world], @racket[#:msg] and
@racket[#:orig-storage]; in transaction mode the environment is @racket[#:world]
and @racket[#:block].

@bold{Obligations} (any subset). @racket[#:pre] is the Hoare antecedent over the
inputs — when it is false the trial passes vacuously, so it gates the other
obligations. @racket[#:post] is @racket[(lambda (m0 m1) ....)] in fragment mode or
@racket[(lambda (w0 w1 result) ....)] in transaction mode, where @racket[m0] /
@racket[w0] is the @emph{pre}-state (this is how you refer to ``old'' values) and
@racket[result] is a @racket[txn-run]. @racket[#:invariant] is a predicate checked
at @bold{every step} in fragment mode (via @racket[run-trace]) and at the
start/end boundary in transaction mode. @racket[#:revert-when] /
@racket[#:succeed-when] demand that, under the given condition, the run reverts /
succeeds.

@bold{Budget}. @racket[#:fuel] caps steps; @racket[#:trials] sets the number of
generated cases (default 1000); @racket[#:seed] fixes the RNG for reproducibility.

@racketblock[
(define-evm-property add-wraps
  #:code   (list 1)                     (code:comment "ADD")
  #:given  ([a gen-word] [b gen-word])
  #:stack  (list a b)                   (code:comment "initial stack, top first")
  #:gas    100
  #:post   (lambda (m0 m1) (= (stack-top m1) (u-add a b))))]}

@defstruct*[evm-property ([prop any/c] [trials exact-nonnegative-integer?]
                          [seed (or/c #f exact-integer?)]) #:transparent]{
The compiled property produced by @racket[define-evm-property]. You normally pass
it straight to a runner rather than inspecting its fields.}

@; ======================================================================
@section{Running properties}

@defproc[(check-evm-property [p evm-property?]
                             [#:trials trials (or/c #f exact-nonnegative-integer?) #f]
                             [#:seed seed (or/c #f exact-integer?) #f]
                             [#:deadline deadline (or/c #f real?) #f]) void?]{
Runs @racket[p] as a @racketmodname[rackunit] check, registering a pass or
failure with the enclosing test suite. Use it inside a @racketmodname[rackunit]
test module. The
keyword arguments override the property's own @racket[#:trials] / @racket[#:seed];
@racket[#:deadline] is a wall-clock budget in @bold{seconds} (default 60) — a
contract-level check runs the full semantics per trial, so raise it when running
many trials. Hitting the deadline yields an inconclusive @racket['timed-out], not
a failure.}

@defproc[(run-evm-property [p evm-property?]
                           [#:trials trials (or/c #f exact-nonnegative-integer?) #f]
                           [#:seed seed (or/c #f exact-integer?) #f]
                           [#:deadline deadline (or/c #f real?) #f]) evm-result?]{
The functional runner: instead of registering a rackunit check it returns an
@racket[evm-result] you can inspect programmatically. Use it to test the tester,
or to react to a counterexample in code.}

@defproc[(evm-property-holds? [p evm-property?]
                              [#:trials trials (or/c #f exact-nonnegative-integer?) #f]
                              [#:seed seed (or/c #f exact-integer?) #f]
                              [#:deadline deadline (or/c #f real?) #f]) boolean?]{
The boolean shortcut: @racket[#t] when @racket[p] passed all trials.}

@defstruct*[evm-result ([status (or/c 'passed 'falsified 'timed-out)]
                        [counterexample (or/c #f list?)]
                        [tests exact-nonnegative-integer?]) #:transparent]{
The outcome of @racket[run-evm-property]. @racket[counterexample] is the shrunk
list of generated argument values (in @racket[#:given] order) when
@racket[status] is @racket['falsified], else @racket[#f]. Because the seed and
the shrunk inputs are recorded, a falsifying case is reproducible: feed the same
inputs back through @racket[run-fragment] / @racket[run-txn] to re-observe the
failure.}

@; ======================================================================
@section{Worlds, accounts, and transactions}

A @deftech{world} is an association of addresses to accounts; an account is
@racket[(list 'account nonce balance code storage transient)], where
@racket[code] is a byte list and @racket[storage] is an association of slot to
value. Most of the time you obtain a world from @racket[deploy] rather than
writing one by hand.

@defproc[(make-tx [#:sender sender exact-nonnegative-integer?]
                  [#:gas-limit gas-limit exact-nonnegative-integer?]
                  [#:nonce nonce exact-nonnegative-integer? 0]
                  [#:to to (or/c #f exact-nonnegative-integer?) #f]
                  [#:value value exact-nonnegative-integer? 0]
                  [#:data data (listof byte?) '()]
                  [#:gas-price gas-price (or/c #f exact-nonnegative-integer?) #f]
                  [#:max-fee max-fee exact-nonnegative-integer? 0]
                  [#:max-priority max-priority exact-nonnegative-integer? 0]
                  [#:access-list access-list list? '()]
                  [#:auth-list auth-list list? '()]
                  [#:blob-hashes blob-hashes list? '()]
                  [#:max-blob-fee max-blob-fee exact-nonnegative-integer? 0]) any/c]{
Builds a transaction for @racket[#:call]. @racket[#:to] is the callee, or
@racket[#f] for a contract-creation transaction (with the init code in
@racket[#:data]). Set @racket[#:gas-price] to @racket[0] to avoid deducting a fee
from the sender's balance, which keeps value-conservation properties simple. The
EIP-1559 (@racket[#:max-fee] / @racket[#:max-priority]), access-list, EIP-7702
authorization, and blob fields default to empty/zero.}

@defproc[(install-contract [code (listof byte?)]
                           [addr exact-nonnegative-integer?]
                           [balance exact-nonnegative-integer? 0]) list?]{
A one-line @tech{world} holding a single account at @racket[addr] with the given
runtime @racket[code] and @racket[balance]. Handy for @racket[#:world] when you
want to skip deployment and test a known runtime directly.}

@defthing[DEFAULT-BLOCK list?]{
The default block environment (number/timestamp 0, 30M gas limit, chain id 1,
Prague fork) used when a property omits @racket[#:block].}

@; ======================================================================
@section{Deploying contracts}

A Solidity contract compiles to two bytecodes: the @bold{creation} (init) code,
which runs the constructor and @emph{returns} the runtime code, and the
@bold{runtime} (deployed) code that is stored at the address. Running the creation
code is the faithful path — it applies constructor storage writes and patches
@tt{immutable}s — whereas installing the runtime directly with @racket[#:contract]
skips the constructor.

@defproc[(deploy [creation (listof byte?)]
                 [#:from from exact-nonnegative-integer? DEFAULT-DEPLOYER]
                 [#:value value exact-nonnegative-integer? 0]
                 [#:gas gas exact-nonnegative-integer? 30000000]
                 [#:nonce nonce (or/c #f exact-nonnegative-integer?) #f]
                 [#:world world list? '()]
                 [#:block block list? DEFAULT-BLOCK]) deploy-result?]{
Runs @racket[creation] as a real contract-creation transaction and returns the
post-deployment world with the runtime installed and all constructor effects
applied. The created address is derived from @racket[#:from] and the deployer's
@racket[#:nonce]; @racket[deploy] ensures the deployer exists with enough balance
to cover @racket[#:value]. Append ABI-encoded constructor arguments to
@racket[creation] before calling (see @racket[abi-encode]).

@racketblock[
(define art (read-artifact "storage/storage.json" #:contract "SimpleStorage"))
(define dep (deploy (artifact-creation art)))
(deploy-result-ok? dep)                          (code:comment "#t")
(sload (deploy-result-world dep) (deploy-result-address dep) 0)]}

@defstruct*[deploy-result ([world list?]
                           [address exact-nonnegative-integer?]
                           [code (or/c #f (listof byte?))]
                           [ok? boolean?]
                           [gas-used exact-nonnegative-integer?]
                           [err (or/c #f string?)]) #:transparent]{
The result of @racket[deploy]. On success @racket[ok?] is @racket[#t] and
@racket[code] is the installed runtime; on failure @racket[ok?] is @racket[#f],
@racket[code] is @racket[#f] and @racket[err] carries a message. @racket[world]
is always the resulting world (unchanged from the input on failure).}

@; ======================================================================
@section{Solidity artifacts and the ABI}

@defstruct*[artifact ([name (or/c #f string?)]
                      [creation (or/c #f (listof byte?))]
                      [runtime (or/c #f (listof byte?))]
                      [abi any/c]
                      [raw any/c]
                      [srcmap (or/c #f string?)]
                      [sources (or/c #f (listof string?))]) #:transparent]{
The two bytecodes (and the parsed ABI) pulled from a build artifact.
@racket[creation] feeds @racket[deploy]; @racket[runtime] feeds
@racket[#:contract].

@racket[srcmap] is the @emph{runtime} source map, verbatim, and @racket[sources]
the file list its indices refer to; both are @racket[#f] unless the artifact was
built with them. They are what lifts bytecode coverage to Solidity lines (see the
coverage harness in the companion @tt{evm-redex-tests} package); nothing in the
library itself reads them.}

@deftogether[(
@defproc[(read-artifact [path path-string?] [#:contract name (or/c #f string?) #f]) artifact?]
@defproc[(read-artifact/string [str string?] [#:contract name (or/c #f string?) #f]) artifact?]
)]{
Read an @racket[artifact] from a solc / Foundry / Hardhat JSON file (or string).
Recognises Foundry (@tt{out/C.sol/C.json}), Hardhat,
@tt{solc --combined-json bin,bin-runtime,abi} and @tt{solc --standard-json}
shapes. When the file holds several contracts, pass @racket[#:contract] to select
one by name (the part after the last @tt{:} or @tt{/}, or the whole key). Build
the artifacts with the dev-shell toolchain, e.g.
@tt{solc --combined-json bin,bin-runtime,abi Token.sol}.

Add @tt{srcmap-runtime} to that list if you want line-level coverage:
@tt{solc --combined-json bin,bin-runtime,abi,srcmap-runtime Token.sol}. It leaves
the bytecode byte-for-byte identical — nothing else in a suite moves — and simply
adds the map (and solc's @tt{sourceList}) to the JSON.}

@defproc[(function-selector [sig string?]) exact-nonnegative-integer?]{
The 4-byte Contract-ABI selector for a function signature, i.e. the first four
bytes of the Keccak-256 of @racket[sig], as an integer.
@racket[(function-selector "transfer(address,uint256)")] is @racket[#xa9059cbb].}

@defproc[(encode-call [sig string?] [arg any/c] ...) (listof byte?)]{
Full calldata for a call: the 4-byte @racket[function-selector] of @racket[sig]
followed by @racket[abi-encode] of the arguments. Use it for @racket[#:data].

@racketblock[
(encode-call "transfer(address,uint256)" recipient amount)
(code:comment "dynamic types too, matching the canonical Solidity vector:")
(encode-call "sam(bytes,bool,uint256[])" (list 100 97 118 101) #t (list 1 2 3))]}

@deftogether[(
@defproc[(abi-encode [types (listof string?)] [values list?]) (listof byte?)]
@defproc[(abi-decode [types (listof string?)] [bs (listof byte?)]) list?]
)]{
Encode / decode an argument tuple by ABI type strings, @emph{without} a selector.
@racket[abi-decode] is the inverse of @racket[abi-encode] and is what you use to
read a getter's return bytes. The supported types and the Racket values they map
to:

@itemlist[
@item{@tt{uint<M>} / @tt{int<M>} (bare @tt{uint}/@tt{int} = 256), @tt{address} —
      an integer (signed for @tt{int}); @tt{bool} — @racket[#t]/@racket[#f] (or
      1/0).}
@item{@tt{bytes<M>} (fixed, 1–32) and @tt{bytes} (dynamic) — a byte list or a
      Racket @racket[bytes]; @tt{string} — a Racket string (or byte list).}
@item{@tt{T[]} (dynamic) and @tt{T[k]} (fixed) arrays — a list of element values;
      tuples @tt{(T1,...)} — a list of the tuple's element values.}
]}

@defproc[(parse-arg-types [sig string?]) (listof string?)]{
The argument type strings of a function signature — e.g.
@racket[(parse-arg-types "transfer(address,uint256)")] is
@racket['("address" "uint256")]. Used internally by @racket[encode-call]; exposed
for building @racket[abi-encode] / @racket[abi-decode] type lists from a
signature.}

@defproc[(hex->bytes [s string?]) (or/c #f (listof byte?))]{
Parse a hex string (with or without a @tt{0x} prefix, odd length tolerated) into a
byte list; @racket[#f] on a non-string.}

@; ======================================================================
@section{Calling into a world}

A transaction discards a function's return value, so transaction-mode
@racket[#:post] sees the world, outcome and logs but not the returned bytes. To
read a getter or @tt{view} function, use @racket[call], a raw frame execution that
exposes the output.

@defproc[(call [world list?] [addr exact-nonnegative-integer?]
               [#:from from exact-nonnegative-integer? DEFAULT-CALLER]
               [#:value value exact-nonnegative-integer? 0]
               [#:data data (listof byte?) '()]
               [#:gas gas exact-nonnegative-integer? 30000000]
               [#:block block list? CALL-BLOCK]
               [#:static static? boolean? #f]) call-result?]{
Runs a message call to @racket[addr] at the frame level, with @bold{no} transaction
validation or gas charging, and returns the call's outcome and return data. Set
@racket[#:static] to forbid state changes (a @tt{staticcall}).

@racketblock[
(define r (call world addr #:data (encode-call "total()")))
(call-result-outcome r)                                  (code:comment "'return | 'revert | ...")
(car (abi-decode (list "uint256") (call-result-return r)))]}

@defstruct*[call-result ([outcome symbol?]
                         [return (listof byte?)]
                         [world list?]
                         [gas-left exact-nonnegative-integer?]
                         [err (or/c #f string?)]
                         [logs list?]) #:transparent]{
@racket[outcome] is @racket['return], @racket['stop], @racket['revert],
@racket['out-of-gas], another exception tag, or @racket['error]. @racket[return]
is the returned or reverted bytes (empty for @racket['stop]); @racket[world] is
the post-call world (unchanged on a static call or revert); @racket[logs] holds
the events the call emitted, and is empty unless it succeeded — a reverted call
emits none.}

@; ======================================================================
@section{Lower-level execution}

@racket[define-evm-property] builds on two run functions and their result structs.
You can call them directly to reproduce a counterexample or to script an ad-hoc
run.

@defproc[(run-fragment [code (listof byte?)]
                       [#:stack stack list? '()]
                       [#:gas gas exact-nonnegative-integer? 1000000]
                       [#:memory memory list? '()]
                       [#:world world list? '()]
                       [#:msg msg any/c #f]
                       [#:block block any/c #f]
                       [#:tx tx any/c #f]
                       [#:orig-storage orig list? '()]
                       [#:fuel fuel exact-nonnegative-integer? 1000000]
                       [#:trace? trace? boolean? #f]) frag-run?]{
Runs a fixed opcode @racket[code] fragment from a generated initial state (the
engine behind @tech{fragment mode}). With @racket[#:trace?] it uses
@racket[run-trace] so per-step @racket[#:invariant]s can be checked.}

@defproc[(run-txn [tx any/c] [world list?] [block list?]) txn-run?]{
Runs a whole transaction against @racket[world] (the engine behind @tech{transaction
mode}), wrapping @racket[process-transaction] and catching an invalid-transaction
error into a clean @racket['error] outcome.}

@defstruct*[frag-run ([pre any/c] [post any/c] [trace (or/c #f list?)]
                      [outcome symbol?] [err (or/c #f string?)]) #:transparent]{
A fragment run. @racket[pre] / @racket[post] are the machine terms before and
after; @racket[trace] is the list of every intermediate machine (when
@racket[#:trace?] was set) else @racket[#f]; @racket[outcome] is the halt tag
(@racket['stop], @racket['return], @racket['revert], an exception tag, or
@racket['error]).}

@defstruct*[txn-run ([world0 list?] [world1 list?] [ok? boolean?]
                     [gas-used exact-nonnegative-integer?] [logs list?]
                     [outcome (or/c 'success 'revert 'error)]
                     [err (or/c #f string?)]) #:transparent]{
A transaction run. @racket[world0] / @racket[world1] are the pre- and post-states;
@racket[ok?] and @racket[outcome] classify the result; @racket[gas-used] and
@racket[logs] are the consumed gas and emitted logs. This is the @racket[result]
passed to a transaction-mode @racket[#:post].}

@deftogether[(
@defproc[(machine-with-field [m any/c] [tag symbol?] [arg any/c] ...) any/c]
@defthing[CALL-BLOCK list?]
)]{
@racket[machine-with-field] functionally overrides one field (by its @racket[tag],
e.g. @racket['stack]) of a machine term. @racket[CALL-BLOCK] is the default block
environment used by @racket[call].}

@; ======================================================================
@section{Observation vocabulary}

Pure readers for @racket[#:pre] / @racket[#:post] / @racket[#:invariant] bodies.
All are total on well-formed machines / worlds.

@subsection{Machine fields}

@deftogether[(
@defproc[(pc [m any/c]) exact-nonnegative-integer?]
@defproc[(gas [m any/c]) exact-nonnegative-integer?]
@defproc[(the-stack [m any/c]) list?]
@defproc[(mem-of [m any/c]) any/c]
@defproc[(code-of [m any/c]) list?]
@defproc[(world-of [m any/c]) list?]
@defproc[(return-data [m any/c]) list?]
@defproc[(logs-of [m any/c]) list?]
@defproc[(refund-of [m any/c]) exact-integer?]
@defproc[(halt-of [m any/c]) any/c]
)]{
Read the corresponding field of a machine @racket[m]: program counter, remaining
gas, the stack (top first), memory, code, world, return data, logs, gas-refund
counter, and the raw halt value.}

@subsection{Stack}

@deftogether[(
@defproc[(stack [m any/c] [i exact-nonnegative-integer?]) exact-nonnegative-integer?]
@defproc[(stack-top [m any/c]) exact-nonnegative-integer?]
@defproc[(stack-depth [m any/c]) exact-nonnegative-integer?]
@defproc[(stack-empty? [m any/c]) boolean?]
)]{
@racket[(stack m i)] is the @racket[i]th word from the top (@racket[(stack-top m)]
= @racket[(stack m 0)]).}

@subsection{Outcome}

@deftogether[(
@defproc[(running? [m any/c]) boolean?]
@defproc[(halted? [m any/c]) boolean?]
@defproc[(stopped? [m any/c]) boolean?]
@defproc[(returned? [m any/c]) boolean?]
@defproc[(reverted? [m any/c]) boolean?]
@defproc[(out-of-gas? [m any/c]) boolean?]
@defproc[(exception? [m any/c]) boolean?]
@defproc[(halt-tag [m any/c]) symbol?]
)]{
Classify a machine's halt status. @racket[halt-tag] returns the leading symbol of
the halt value (@racket['running], @racket['stop], @racket['return],
@racket['revert], or an exception tag); @racket[exception?] is true for any tag in
@racket[EXN-TAGS].}

@defthing[EXN-TAGS (listof symbol?)]{
The exceptional halt tags: @racket['out-of-gas], @racket['stack-underflow],
@racket['stack-overflow], @racket['invalid-opcode], @racket['invalid-jump],
@racket['out-of-bounds], @racket['stack-depth-limit], @racket['write-in-static].}

@subsection{Return data and memory}

@deftogether[(
@defproc[(return-bytes [m any/c]) (listof byte?)]
@defproc[(return-word [m any/c]) exact-nonnegative-integer?]
@defproc[(return-size [m any/c]) exact-nonnegative-integer?]
@defproc[(mem-word [m any/c] [off exact-nonnegative-integer?]) exact-nonnegative-integer?]
@defproc[(mem-byte [m any/c] [off exact-nonnegative-integer?]) byte?]
@defproc[(mem-bytes [m any/c] [off exact-nonnegative-integer?] [len exact-nonnegative-integer?]) (listof byte?)]
@defproc[(memory-size [m any/c]) exact-nonnegative-integer?]
)]{
@racket[return-word] reads the (first 32 bytes of the) return data as a word;
@racket[mem-word] reads 32 bytes at @racket[off] as a word. The @racket[mem-*]
readers zero-extend past the current memory size.}

@subsection{World, accounts, and storage}

@deftogether[(
@defproc[(account-of [world list?] [addr exact-nonnegative-integer?]) list?]
@defproc[(exists? [world list?] [addr exact-nonnegative-integer?]) boolean?]
@defproc[(balance-of [world list?] [addr exact-nonnegative-integer?]) exact-nonnegative-integer?]
@defproc[(nonce-of [world list?] [addr exact-nonnegative-integer?]) exact-nonnegative-integer?]
@defproc[(code-at [world list?] [addr exact-nonnegative-integer?]) (listof byte?)]
@defproc[(storage-at [world list?] [addr exact-nonnegative-integer?]) list?]
@defproc[(empty-account? [world list?] [addr exact-nonnegative-integer?]) boolean?]
@defproc[(sload [world list?] [addr exact-nonnegative-integer?] [key exact-nonnegative-integer?]) exact-nonnegative-integer?]
)]{
Read an account and its fields out of a @tech{world}. @racket[sload] reads storage
slot @racket[key] of @racket[addr] (0 when unset) — the workhorse for asserting on
contract state without going through a getter.}

@deftogether[(
@defproc[(m-balance [m any/c] [addr exact-nonnegative-integer?]) exact-nonnegative-integer?]
@defproc[(m-nonce [m any/c] [addr exact-nonnegative-integer?]) exact-nonnegative-integer?]
@defproc[(m-sload [m any/c] [addr exact-nonnegative-integer?] [key exact-nonnegative-integer?]) exact-nonnegative-integer?]
@defproc[(m-storage-at [m any/c] [addr exact-nonnegative-integer?]) list?]
)]{
The same world readers relative to a @emph{machine}'s current world — convenient
inside a fragment-mode @racket[#:invariant] or @racket[#:post].}

@subsection{Constants}

@deftogether[(@defthing[UINT256-MAX exact-nonnegative-integer?]
              @defthing[WORD-MOD exact-nonnegative-integer?])]{
@racket[2^256 - 1] and @racket[2^256] — the maximum word and the wrap-around
modulus.}

@; ======================================================================
@section{Generators}

EVM-tuned, edge-biased, shrinkable @racketmodname[rackcheck] generators for
@racket[#:given]. Edge bias matters: bugs cluster at @racket[0], @racket[1],
@racket[2^255], @racket[MAX-U256] and byte/word boundaries, so the defaults sample
those heavily while still covering the uniform range.

@deftogether[(
@defthing[gen-word gen?]
@defthing[gen-word-edge gen?]
@defthing[gen-word-small gen?]
@defthing[gen-word-uniform gen?]
@defproc[(gen-word-in [lo exact-nonnegative-integer?] [hi exact-nonnegative-integer?]) gen?]
)]{
256-bit word generators. @racket[gen-word] is the balanced default (edges +
small + a uniform tail); @racket[gen-word-edge] draws only boundary values,
@racket[gen-word-small] only @racket[0]–@racket[255], @racket[gen-word-uniform]
uniformly over the whole range. @racket[gen-word-in] draws uniformly from an
inclusive range — the idiom for satisfying a precondition @emph{by construction}
(e.g. an amount in @racket[(gen-word-in 0 balance)]) instead of a strict
@racket[#:pre] that discards samples.}

@deftogether[(
@defthing[gen-byte gen?]
@defproc[(gen-bytes [#:max-length n exact-nonnegative-integer? 64]) gen?]
@defthing[gen-address gen?]
)]{
A byte, a variable-length byte list, and a 160-bit address (edge-biased).}

@deftogether[(
@defproc[(gen-stack [#:max-depth depth exact-nonnegative-integer? 8]) gen?]
@defproc[(gen-stack-of [k exact-nonnegative-integer?]) gen?]
@defthing[gen-gas gen?]
)]{
@racket[gen-stack] draws up to @racket[depth] words; @racket[gen-stack-of] exactly
@racket[k]. @racket[gen-gas] is biased toward small/tight budgets so runs actually
reach out-of-gas.}

@deftogether[(
@defproc[(gen-store [#:max-entries n exact-nonnegative-integer? 6]) gen?]
@defproc[(gen-account [#:code code (listof byte?) '()]) gen?]
@defproc[(gen-world-with [contract (listof byte?)] [addr exact-nonnegative-integer?]
                         [#:storage storage gen? (gen-store)]
                         [#:caller caller exact-nonnegative-integer? 0]
                         [#:caller-balance caller-balance gen? gen-word]) gen?]
)]{
Storage, account, and world generators. @racket[gen-store] uses a small key space
so collisions are likely; @racket[gen-world-with] generates a world holding
@racket[contract] at @racket[addr] (with generated storage) plus a funded caller —
useful for exercising a runtime directly without deployment.}

@deftogether[(
@defthing[gen-selector gen?]
@defproc[(gen-calldata [#:max-args max-args exact-nonnegative-integer? 3]) gen?]
@defproc[(abi [selector (listof byte?)] [arg any/c] ...) (listof byte?)]
)]{
Calldata generators: a random 4-byte @racket[gen-selector] and
@racket[gen-calldata] (selector + word-args). @racket[abi] assembles calldata from
an explicit selector and word arguments (each padded to 32 bytes).}

@margin-note{All of @racketmodname[rackcheck]'s combinators
(@racket[gen:integer-in], @racket[gen:one-of], @racket[gen:map],
@racket[gen:frequency], @racket[gen:tuple], …) are re-exported, so you can build
custom generators inline.}

@; ======================================================================
@section{Running the DSL's own tests}

The DSL's tests — correct properties passing, buggy ones falsified and shrunk,
each clause form, a transaction property, reproducibility by seed — live in the
companion @tt{evm-redex-tests} package, under @filepath{pbt/}:

@racketblock[(code:comment "raco test pbt/   (in the evm-redex-tests package)")]
