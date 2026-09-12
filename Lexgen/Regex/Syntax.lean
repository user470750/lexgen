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
  min : Nat
  /--
  Optional maximum number of occurrences.
  -/
  max : Option Nat

/--
Creates a `Quantity` without bounds.
-/
def zeroOrMore : Quantity := { min := 0, max := none }

/--
Creates a `Quantity` with a lower bound of one and no upper bound.
-/
def oneOrMore : Quantity := { min := 1, max := none }

/--
Creates a `Quantity` from zero to one.
-/
def optionalOne : Quantity := { min := 0, max := some 1 }

/--
Creates a `Quantity` with the given lower and upper bounds.
-/
def between (min : Nat) (max : Nat) : Quantity := { min, max }

/--
Creates a `Quantity` with only a lower bound.
-/
def atLeast (min : Nat) : Quantity := { min, max := none }

/--
Creates a `Quantity` from zero to the given upper bound.
-/
def atMost (max : Nat) : Quantity := { min := 0, max }

/--
Creates a `Quantity` requiring an exact number of occurrences.
-/
def exactly (n : Nat) : Quantity := { min := n, max := n }

/--
Checks whether a `Quantity`'s bounds are well-formed, i.e. `min <= max`.
-/
def inOrder : Quantity → Bool
  | { min := _, max := none }   => true
  | { min,      max := some m } => min <= m

/--
The CST representation of regular expressions.
-/
inductive ReSyntax where
  /--
  Matches an alternation of regular expressions.
  -/
  | alt (left : ReSyntax) (right : ReSyntax)
  /--
  Matches a concatenation of regular expressions.
  -/
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
