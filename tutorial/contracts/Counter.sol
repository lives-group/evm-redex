// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

/// The "hello world" of stateful contracts: a counter you can bump up and down.
contract Counter {
    uint256 public count;                       // public -> free getter `count()`

    function increment() public { count += 1; }

    function add(uint256 n) public { count += n; }

    function decrement() public {
        require(count > 0, "underflow");        // a guard we can test for reverting
        count -= 1;
    }
}
