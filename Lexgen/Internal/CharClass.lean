/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public section

/-!
# Character classes

Defines `CharClass`, an item of a character class.
-/

/--
An item of a character class: a literal character or a range of characters.
-/
inductive CharClass where
  /--
  Matches the literal character `character`.
  -/
  | single (character : Char)
  /--
  Matches a character from `lower` to `upper`, inclusive.
  -/
  | range (lower upper : Char)
deriving Repr, DecidableEq, Hashable

/--
Checks whether a `CharClass` matches the character `c`.
-/
def CharClass.contains (c : Char) : CharClass → Bool
  | .single s          => c == s
  | .range lower upper => lower <= c && c <= upper
