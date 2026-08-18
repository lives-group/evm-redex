#lang evm-redex/sim
;; Tutorial example — poke at Token.sol by hand with the transaction simulator.
;; Run it with:  racket tutorial/explore.rkt
.account ALICE balance=1eth
.account BOB
.deploy TOKEN from=ALICE code=@Token.json:Token

tx from=ALICE to=TOKEN sig="mint(address,uint256)" args=(ALICE, 1000)
tx from=ALICE to=TOKEN sig="transfer(address,uint256)" args=(BOB, 100)
tx from=BOB   to=TOKEN sig="transfer(address,uint256)" args=(ALICE, 5000)  ; reverts
call from=ALICE to=TOKEN sig="balanceOf(address)" args=(BOB) returns=uint256
