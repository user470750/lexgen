# Changelog Next

This CHANGELOG describes the merged but unreleased changes.

## Syntax

* Skip rules and token rules may be declared in any order.

## Runtime API

* **Breaking:** `Lexable.next` takes a proof that the input is shorter than `maxInputSize`
  (2^48 bytes).
* `Lexer.new` panics on an input of `maxInputSize` bytes or more.
