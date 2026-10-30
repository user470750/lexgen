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

/--
Returns the ranges of the named character class `kind`.
-/
def NamedClass.ranges (kind : NamedClass) : Array ClassItem :=
  let max := Char.ofNat 0x10FFFF
  match kind with
  | .digit => #[.range '0' '9' (by decide)]
  | .nonDigit =>
    #[
      .range '\x00' '/' (by decide),
      .range ':' max (by decide)
    ]
  | .word =>
    #[
      .range '0' '9' (by decide),
      .range 'A' 'Z' (by decide),
      .single '_',
      .range 'a' 'z' (by decide)
    ]
  | .nonWord =>
    #[
      .range '\x00' '/' (by decide),
      .range ':' '@' (by decide),
      .range '[' '^' (by decide),
      .single '`',
      .range '{' max (by decide)
    ]
  | .space =>
    #[
      .range '\t' '\r' (by decide),
      .single ' '
    ]
  | .nonSpace =>
    #[
      .range '\x00' '\x08' (by decide),
      .range '\x0e' '\x1f' (by decide),
      .range '!' max (by decide)
    ]

end Lexgen.Internal
