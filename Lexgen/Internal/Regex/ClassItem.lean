/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public section

/-!
# Character class items

Defines `ClassItem`, an item of a character class in the CST, and `NamedClass`, the named classes
it can refer to.
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

end Lexgen.Internal
