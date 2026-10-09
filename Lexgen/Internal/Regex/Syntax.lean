/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public import Lexgen.Internal.Regex.Quantity

public section

/-!
# Regex CST

Defines the concrete syntax tree (`RegexSyntax`) for regular expressions.
-/

namespace Lexgen.Internal

/--
A named character class. The classes are ASCII.
-/
inductive NamedClass where
  /--
  `\d`: a digit.
  -/
  | digit
  /--
  `\D`: any character but a digit.
  -/
  | nonDigit
  /--
  `\w`: a word character.
  -/
  | word
  /--
  `\W`: any character but a word character.
  -/
  | nonWord
  /--
  `\s`: a whitespace character.
  -/
  | space
  /--
  `\S`: any character but a whitespace character.
  -/
  | nonSpace
deriving Repr, DecidableEq

/--
An item of a character class in the CST.
-/
inductive ClassItem where
  /--
  Matches the literal character `character`.
  -/
  | single (character : Char)
  /--
  Matches a character from `lower` to `upper`, inclusive.
  -/
  | range (lower upper : Char) (wellFormed : lower ≤ upper)
  /--
  Matches a character of the named class `kind`.
  -/
  | named (kind : NamedClass)
deriving Repr, DecidableEq

/--
The CST representation of regular expressions.
-/
inductive RegexSyntax where
  /--
  Matches an alternation of regular expressions.
  -/
  -- Left-associative due to the parser: `a|b|c` is `alt (alt a b) c`.
  | alt (left right : RegexSyntax)
  /--
  Matches a concatenation of regular expressions.
  -/
  -- Left-associative due to the parser: `abc` is `concat (concat a b) c`.
  | concat (first rest : RegexSyntax)
  /--
  Matches repetition of a regular expression.
  -/
  | repeated (quantity : Quantity) (re : RegexSyntax)
  /--
  Matches the literal character `c`.
  -/
  | symbol (c : Char)
  /--
  Matches a character from `ranges`, or, if `negate`, a character outside them.
  -/
  | charClass (negate : Bool) (ranges : Array ClassItem)
  /--
  Matches a character of the named class `kind`.
  -/
  | namedClass (kind : NamedClass)
  /--
  Matches any character but `\n`.
  -/
  | dot
  /--
  Matches the empty string.
  -/
  | ε
deriving Repr, DecidableEq

/--
Estimates the size of `re` after desugaring.
-/
def RegexSyntax.countNodes : RegexSyntax → Nat
  | .ε | .dot | .charClass .. | .namedClass _ | .symbol _ => 1
  | .alt left right                => countNodes left + countNodes right
  | .concat first second           => countNodes first + countNodes second
  | .repeated (.atLeast n) re      => (n + 1) * countNodes re
  -- Each of the `m - n` optional copies is an `alt` with an `ε` leaf.
  | .repeated (.between n m _) re  => m * countNodes re + (m - n)

end Lexgen.Internal
