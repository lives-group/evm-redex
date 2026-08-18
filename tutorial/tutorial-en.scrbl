#lang scribble/manual

@(require (for-label racket/base racket/contract evm-redex/pbt))

@title[#:tag "tutorial-en"]{Tutorial: testing Solidity contracts with evm-redex}
@author{rodrigo}

@margin-note{Versão em português: @other-doc['(lib "evm-redex/tutorial/tutorial-pt.scrbl")].}

This tutorial walks, from scratch, through @bold{writing a small Solidity
contract, compiling it, and testing it} with @racketmodname[evm-redex/pbt] — the
property-based testing DSL. Every snippet below is the real, runnable code under
@filepath{tutorial/} — @filepath{counter.rkt}, @filepath{token.rkt},
@filepath{ballot.rkt}, @filepath{auction.rkt}, @filepath{purchase.rkt}, and
@filepath{explore.rkt}; run them all with @exec{raco test tutorial}.

You will need @tt{solc} (the Solidity compiler) to turn a @tt{.sol} file into the
JSON artifact the library reads; the committed artifacts under @filepath{tutorial/}
were built with @tt{solc 0.8.33}, so you can follow along without recompiling.

@table-of-contents[]

@section{The idea in one paragraph}

The library runs a contract's @emph{bytecode} on an executable specification of
the EVM. So the loop is always the same: compile the @tt{.sol} to bytecode with
@tt{solc}, load that artifact with @racket[read-artifact], @racket[deploy] it into
a fresh world, and then either @racket[call] into it (to read state or send a
message) or state a @deftech{property} with @racket[define-evm-property] and let
the engine try to falsify it over many generated inputs.

@section{A first contract: @tt{Counter}}

Here is the "hello world" of stateful contracts — a counter, in
@filepath{tutorial/contracts/Counter.sol}:

@filebox["Counter.sol"]{
@verbatim|{
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract Counter {
    uint256 public count;                    // public -> free getter `count()`

    function increment() public { count += 1; }
    function add(uint256 n) public { count += n; }
    function decrement() public {
        require(count > 0, "underflow");     // a guard we can test for reverting
        count -= 1;
    }
}
}|
}

@subsection{Compile it}

Ask @tt{solc} for the bytecode and the ABI, as one JSON file:

@commandline{solc --combined-json bin,bin-runtime,abi contracts/Counter.sol > Counter.json}

@subsection{Load and deploy}

@racket[read-artifact] pulls the creation and runtime bytecode (and the ABI) out
of the JSON; @racket[deploy] runs the constructor and gives you the deployed
address and the resulting world.

@racketblock[
(require evm-redex/pbt)

(define ART (read-artifact "Counter.json" #:contract "Counter"))

(define DEPLOYER #x00000000000000000000000000000000A11CE001)
(define DEP     (deploy (artifact-creation ART) #:from DEPLOYER))
(define COUNTER (deploy-result-address DEP))
(define BASE    (deploy-result-world DEP))   (code:comment "the world just after deployment")
]

@subsection{Read state}

@tt{count} is a public variable, so Solidity generates a @tt{count()} getter. A
@racket[call] runs a message; @racket[call-result-return] is its return data,
which @racket[abi-decode] turns back into a number:

@racketblock[
(define (count-of world)
  (car (abi-decode (list "uint256")
                   (call-result-return
                    (call world COUNTER #:data (encode-call "count()"))))))

(count-of BASE)   (code:comment "=> 0")
]

@subsection{Send transactions}

@racket[encode-call] builds the calldata for a function; @racket[call] returns a
@racket[call-result], and @racket[call-result-world] is the world after the call.
Thread that world through a couple of calls and read the count back:

@racketblock[
(define CALLER #x00000000000000000000000000000000B0B00001)
(define (send world sig . args)
  (call-result-world
   (call world COUNTER #:from CALLER #:data (apply encode-call sig args))))

(let* ([w (send BASE "increment()")]
       [w (send w "add(uint256)" 5)])
  (count-of w))    (code:comment "=> 6")
]

@subsection{State a property}

Concrete calls are fine for a sanity check, but the point of the library is to
test a @tech{property} over @emph{many} generated inputs. @racket[define-evm-property]
in @emph{transaction mode} (the @racket[#:call] clause) runs a real transaction;
@racket[#:given] lists the generated inputs, and @racket[#:post] receives the world
before (@racket[w0]) and after (@racket[w1]) plus the transaction result:

@racketblock[
(define (tx sig . args)
  (make-tx #:sender CALLER #:to COUNTER #:gas-limit 200000 #:gas-price 0
           #:data (apply encode-call sig args)))

(define-evm-property add-raises-count-by-n
  #:given ([n (gen-word-in 0 (expt 2 200))])
  #:world BASE
  #:call  (tx "add(uint256)" n)
  #:post  (lambda (w0 w1 r) (= (count-of w1) (+ (count-of w0) n))))

(check-evm-property add-raises-count-by-n #:trials 50)
]

Running it prints:

@verbatim|{
  ✓ property add-raises-count-by-n passed 50 tests.
}|

A property can also assert that a call @emph{reverts}. @racket[#:revert-when] says
"under this condition, the transaction must revert"; from a count of zero,
@tt{decrement()} always does:

@racketblock[
(define-evm-property decrement-reverts-at-zero
  #:given ()                     (code:comment "no generated inputs — a plain assertion")
  #:world BASE                   (code:comment "count is 0 here")
  #:call  (tx "decrement()")
  #:revert-when #t)

(check-evm-property decrement-reverts-at-zero #:trials 1)
]

@section{A richer contract: @tt{Token}}

A minimal token adds @emph{balances}, a @emph{guarded} transfer, and an
@emph{event} — @filepath{tutorial/contracts/Token.sol}:

@filebox["Token.sol"]{
@verbatim|{
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract Token {
    mapping(address => uint256) public balanceOf;   // -> `balanceOf(address)`
    uint256 public totalSupply;

    event Transfer(address indexed from, address indexed to, uint256 value);

    function mint(address to, uint256 amount) public {
        totalSupply += amount;
        balanceOf[to] += amount;
        emit Transfer(address(0), to, amount);
    }

    function transfer(address to, uint256 amount) public returns (bool) {
        require(balanceOf[msg.sender] >= amount, "insufficient balance");
        balanceOf[msg.sender] -= amount;
        balanceOf[to] += amount;
        emit Transfer(msg.sender, to, amount);
        return true;
    }
}
}|
}

Deploy it and, this time, build a world where @tt{ALICE} already holds 1000
tokens (mint is unrestricted here, so any account can call it):

@racketblock[
(define ART   (read-artifact "Token.json" #:contract "Token"))
(define ALICE #x000000000000000000000000000000000000A11CE)
(define BOB   #x0000000000000000000000000000000000000B0B00)

(define DEP    (deploy (artifact-creation ART) #:from #x00000000000000000000000000000000DEB00001))
(define TOKEN  (deploy-result-address DEP))

(define (u256 r) (car (abi-decode (list "uint256") (call-result-return r))))
(define (bal w a) (u256 (call w TOKEN #:data (encode-call "balanceOf(address)" a))))
(define (total w) (u256 (call w TOKEN #:data (encode-call "totalSupply()"))))

(define (send w from sig . args)
  (call-result-world (call w TOKEN #:from from #:data (apply encode-call sig args))))

(define MINTED (send (deploy-result-world DEP) ALICE "mint(address,uint256)" ALICE 1000))
(bal MINTED ALICE)   (code:comment "=> 1000")
]

@subsection{The property that matters: conservation}

The invariant a token must never break is that a transfer @emph{moves} value
without creating or destroying any. This one property captures both the success
and the revert paths, over amounts that straddle ALICE's balance:

@racketblock[
(define (tx from sig . args)
  (make-tx #:sender from #:to TOKEN #:gas-limit 200000 #:gas-price 0
           #:data (apply encode-call sig args)))

(define-evm-property transfer-conserves-supply
  #:given ([amount (gen-word-in 0 2000)])       (code:comment "straddles the 1000 balance")
  #:world MINTED
  #:call  (tx ALICE "transfer(address,uint256)" BOB amount)
  #:post  (lambda (w0 w1 r)
            (and (= (total w1) (total w0))       (code:comment "supply never changes")
                 (if (eq? (txn-run-outcome r) 'revert)
                     (= (bal w1 ALICE) (bal w0 ALICE))          (code:comment "rolled back")
                     (and (= (bal w1 ALICE) (- (bal w0 ALICE) amount))
                          (= (bal w1 BOB)   (+ (bal w0 BOB) amount)))))))

(check-evm-property transfer-conserves-supply #:trials 100)
]

And a focused revert property — a transfer of more than you own must fail:

@racketblock[
(define-evm-property transfer-reverts-when-insufficient
  #:given ([amount (gen-word-in 1001 100000)])  (code:comment "always more than ALICE has")
  #:world MINTED
  #:call  (tx ALICE "transfer(address,uint256)" BOB amount)
  #:revert-when #t)

(check-evm-property transfer-reverts-when-insufficient #:trials 50)
]

@margin-note{When a property @emph{fails}, the engine shrinks the random inputs to
a minimal counterexample and prints it — that shrunk case is the payoff of
property-based testing. Try weakening @tt{transfer}'s @tt{require} in the contract
and re-running: the conservation property falsifies with a tiny transfer.}

@section{More examples from @emph{Solidity by Example}}

The two contracts above cover the whole loop; the rest of this section applies it
to the classic contracts from the Solidity documentation's
@hyperlink["https://docs.soliditylang.org/en/v0.8.36/solidity-by-example.html"]{Solidity
by Example}. Each is a full runnable module under @filepath{tutorial/} — the
Solidity source, the deployment, and the property that pins its central rule.

@subsection{Voting}

@filepath{tutorial/ballot.rkt} tests @tt{Ballot}, the delegated-voting contract.
Two things are new here: the @bold{constructor takes arguments} (an array of
@tt{bytes32} proposal names), which you ABI-encode and append to the creation
bytecode; and a getter can return a @bold{struct}, which you decode as a tuple.

@racketblock[
(define ART (read-artifact "Ballot.json" #:contract "Ballot"))
(define CHAIR #x00000000000000000000000000000000C4A19001)

(code:comment "proposal names are bytes32 — pad each to 32 bytes")
(define NAMES (list (name->b32 "alpha") (name->b32 "beta") (name->b32 "gamma")))

(code:comment "constructor args are ABI-encoded and appended to the creation code")
(define DEP    (deploy (append (artifact-creation ART)
                               (abi-encode (list "bytes32[]") (list NAMES)))
                       #:from CHAIR))
(define BALLOT (deploy-result-address DEP))
]

The chairperson (the deployer) grants a right to vote, the voter casts it, and the
tally updates — and the rule worth checking is that @bold{only the chairperson may
grant voting rights}, so a call from anyone else must revert:

@racketblock[
(define OUTSIDER #x00000000000000000000000000000000BADA55001)
(define-evm-property only-chair-grants-rights
  #:given ([who gen-address])
  #:world BASE
  #:call  (make-tx #:sender OUTSIDER #:nonce 0 #:to BALLOT #:gas-limit 300000 #:gas-price 0
                   #:data (encode-call "giveRightToVote(address)" who))
  #:revert-when #t)                 (code:comment "a non-chairperson caller always reverts")

(check-evm-property only-chair-grants-rights #:trials 30)
]

@subsection{An open auction}

@filepath{tutorial/auction.rkt} tests @tt{SimpleAuction}. Its calls carry
@bold{ether}: @racket[make-tx]'s @racket[#:value] funds the bid. Accounts that
bid need a balance, so we top a couple up by hand — the same thing @racket[deploy]
does for the deployer:

@racketblock[
(require (only-in evm-redex world-ref world-set acct-balance acct-with-balance))
(define (fund w a wei)
  (world-set w a (acct-with-balance (world-ref w a) (+ (acct-balance (world-ref w a)) wei))))
]

The auction's central rule is that a bid is accepted @bold{exactly when} it beats
the current highest, and then becomes the new highest — one property captures both
the accept and the reject path:

@racketblock[
(define-evm-property bid-raises-the-highest
  #:given ([v (gen-word-in 0 500)])
  #:world W1                          (code:comment "highest bid is 100 here")
  #:call  (make-tx #:sender BOB #:nonce (nonce-of W1 BOB) #:to AUCTION #:value v
                   #:gas-limit 300000 #:gas-price 0 #:data (encode-call "bid()"))
  #:post  (lambda (w0 w1 r)
            (if (> v (highest-bid w0))
                (and (eq? (txn-run-outcome r) 'success) (= (highest-bid w1) v))
                (eq? (txn-run-outcome r) 'revert))))

(check-evm-property bid-raises-the-highest #:trials 60)
]

@subsection{Safe remote purchase}

@filepath{tutorial/purchase.rkt} tests @tt{Purchase}, a four-state escrow
(@tt{Created → Locked → Release → Inactive}). The @bold{constructor is payable} —
the seller locks twice the item value — so you deploy it with @racket[#:value]:

@racketblock[
(define DEP (deploy (artifact-creation ART) #:from SELLER #:value 200))  (code:comment "value = 100")
]

The happy path is buyer @tt{confirmPurchase} (matching the deposit) then
@tt{confirmReceived}. The rule worth pinning is that from the @tt{Locked} state
@bold{only the buyer} can confirm receipt:

@racketblock[
(define-evm-property only-buyer-confirms-receipt
  #:given ()
  #:world LOCKED-W
  #:call  (make-tx #:sender OUTSIDER #:nonce 0 #:to PURCHASE #:gas-limit 300000 #:gas-price 0
                   #:data (encode-call "confirmReceived()"))
  #:revert-when #t)

(check-evm-property only-buyer-confirms-receipt #:trials 1)
]

@margin-note{The fourth @emph{Solidity by Example} contract, the @bold{Micropayment
Channel}, is @emph{not} included: it verifies an off-chain ECDSA signature with
@tt{ecrecover}, and producing that signature happens @emph{outside} the EVM this
library models — so testing it would need an off-chain signer, beyond the scope of
this tutorial.}

@section{Exploring by hand with @tt{#lang evm-redex/sim}}

Properties are for @emph{checking}; when you just want to @emph{poke} at a
contract, the transaction simulator reads like a script.
@filepath{tutorial/explore.rkt} is a whole session — run it with
@exec{racket tutorial/explore.rkt}:

@filebox["explore.rkt"]{
@verbatim|{
#lang evm-redex/sim
.account ALICE balance=1eth
.account BOB
.deploy TOKEN from=ALICE code=@Token.json:Token

tx from=ALICE to=TOKEN sig="mint(address,uint256)" args=(ALICE, 1000)
tx from=ALICE to=TOKEN sig="transfer(address,uint256)" args=(BOB, 100)
tx from=BOB   to=TOKEN sig="transfer(address,uint256)" args=(ALICE, 5000)  ; reverts
call from=ALICE to=TOKEN sig="balanceOf(address)" args=(BOB) returns=uint256
}|
}

It prints a receipt per transaction — with the @tt{Transfer} event and the revert
reason decoded for you — and a final state diff:

@verbatim|{
tx ALICE -> TOKEN
  status:    success
  log:       Transfer(from=0x0, to=0x4e8a…8f7, value=0x3e8)
tx ALICE -> TOKEN
  status:    success
  log:       Transfer(from=0x4e8a…8f7, to=0x28e4…4c38, value=0x64)
tx BOB -> TOKEN
  status:    revert  (Error("insufficient balance"))
call TOKEN.balanceOf(address) = (100)

--- final state ---
state root: 0x4b40…1730
…
}|

@section{Where to go next}

@itemlist[
@item{The complete @racketmodname[evm-redex/pbt] reference — every clause of
      @racket[define-evm-property], the observation vocabulary, and the
      generators — is in @secref["pbt" #:doc '(lib "evm-redex/scribblings/evm-redex.scrbl")].}
@item{The @tt{#lang evm-redex/sim} scenario language is documented in
      @secref["sim" #:doc '(lib "evm-redex/scribblings/evm-redex.scrbl")].}
@item{Larger worked examples — real ERC-20/721, OpenZeppelin contracts, and a
      benchmark reproducing real audit bugs — live in the companion
      @hyperlink["https://github.com/lives-group/evm-redex-tests"]{@tt{evm-redex-tests}}
      package, which depends on this library.}
]
