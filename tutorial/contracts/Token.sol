// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

/// A minimal token: balances, a guarded transfer, and an event.
contract Token {
    mapping(address => uint256) public balanceOf; // public -> `balanceOf(address)`
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
