# evm-redex tutorial

A step-by-step introduction to **writing, compiling, and testing a small Solidity
contract** with the `evm-redex` library — available in two languages:

- **English** — [`tutorial-en.scrbl`](tutorial-en.scrbl)
- **Português** — [`tutorial-pt.scrbl`](tutorial-pt.scrbl)

Both are [Scribble](https://docs.racket-lang.org/scribble/) documents, rendered to
HTML alongside the rest of the package docs (see the top-level `README.md` for the
rebuild command and the htmlpreview link).

Every snippet in the tutorial is real, runnable code that lives here:

| File | What it shows |
|------|---------------|
| [`contracts/`](contracts/) | the example contracts (`Counter`, `Token`, and the *Solidity by Example* set) |
| `*.json` | their `solc --combined-json bin,bin-runtime,abi` artifacts |
| [`counter.rkt`](counter.rkt) | load / deploy / call, and two properties |
| [`token.rkt`](token.rkt) | ABI getters, a conservation property, a revert property |
| [`ballot.rkt`](ballot.rkt) | Voting: constructor arguments and struct getters |
| [`auction.rkt`](auction.rkt) | an open auction: payable calls (`#:value`) and funding accounts |
| [`purchase.rkt`](purchase.rkt) | Safe Remote Purchase: a payable constructor and a state machine |
| [`explore.rkt`](explore.rkt) | the token, poked at with `#lang evm-redex/sim` |

The last three are the classic contracts from
[Solidity by Example](https://docs.soliditylang.org/en/v0.8.36/solidity-by-example.html)
(the fourth, the Micropayment Channel, needs an off-chain ECDSA signer and is out
of scope).

Run the examples with `raco` (from the package root):

```sh
raco test tutorial              # all of them
raco test tutorial/purchase.rkt # just one
racket tutorial/explore.rkt     # the #lang evm-redex/sim scenario
```

Recompile the contracts (only needed if you edit the `.sol` sources; `solc` is on
`PATH` in the Nix dev shell):

```sh
for c in Counter Token Ballot SimpleAuction Purchase; do
  solc --combined-json bin,bin-runtime,abi "contracts/$c.sol" > "$c.json"
done
```
