/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public section

/-!
# Regex CST

Defines the concrete syntax tree (`ReSyntax`) for regular expressions.

Also defines `Quantity`, used to represent repetition bounds.
-/

/--
A quantity bounded, inclusively, by a minimum and, optionally, a maximum.

It can be used in certain parsers to specify
how many times an item is expected to appear.
-/
structure Quantity where
  /--
  Minimum number of occurrences.
  -/
  minimum : Nat
  /--
  Optional maximum number of occurrences.
  -/
  maximum : Option Nat

/--
Creates a `Quantity` without bounds.
-/
def zeroOrMore : Quantity := { minimum := 0, maximum := none }

/--
Creates a `Quantity` with a lower bound of one and no upper bound.
-/
def oneOrMore : Quantity := { minimum := 1, maximum := none }

/--
Creates a `Quantity` from zero to one.
-/
def optionalOne : Quantity := { minimum := 0, maximum := some 1 }

/--
Creates a `Quantity` with the given lower and upper bounds.
-/
def between (minimum : Nat) (maximum : Nat) : Quantity :=
  { minimum, maximum }

/--
Creates a `Quantity` with only a lower bound.
-/
def atLeast (minimum : Nat) : Quantity := { minimum, maximum := none }

/--
Creates a `Quantity` from zero to the given upper bound.
-/
def atMost (maximum : Nat) : Quantity := { minimum := 0, maximum }

/--
Creates a `Quantity` requiring an exact number of occurrences.
-/
def exactly (n : Nat) : Quantity := { minimum := n, maximum := n }

/--
Checks whether a `Quantity`'s bounds are well-formed,
i.e. `minimum <= maximum`.
-/
def inOrder : Quantity → Bool
  | { minimum := _, maximum := none }   => true
  | { minimum,      maximum := some m } => minimum <= m

/--
The CST representation of regular expressions.
-/
inductive ReSyntax where
  /--
  Matches an alternation of regular expressions.
  -/
  -- Left-associative due to the parser: `a|b|c` is `alt (alt a b) c`.
  | alt (left : ReSyntax) (right : ReSyntax)
  /--
  Matches a concatenation of regular expressions.
  -/
  -- Left-associative due to the parser: `abc` is `concat (concat a b) c`.
  | concat (first : ReSyntax) (rest : ReSyntax)
  /--
  Matches repetition of a regular expression.
  -/
  | repeatRe (quantity : Quantity) (re : ReSyntax)
  /--
  Matches the literal character `c`.
  -/
  | symbol (c : Char)
  /--
  Matches any single character.
  -/
  | dot
  /--
  Matches the empty string.
  -/
  | ε
