# Changelog Next

This CHANGELOG describes the merged but unreleased changes.

## Syntax

* Skip rules and token rules may be declared in any order.
* A rule `subpattern name := r"…"` declares a named pattern for the other patterns to use: token
  and skip rules may use any subpattern, declared before or after them, and a subpattern only the
  subpatterns declared before it. A pattern refers to a subpattern as `(?&name)`.

## Runtime API

* **Breaking:** `Lexable.next` takes a proof that the input is shorter than `maxInputSize`
  (2^48 bytes).
* `Lexer.new` panics on an input of `maxInputSize` bytes or more.
