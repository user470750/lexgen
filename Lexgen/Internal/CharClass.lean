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
deriving Repr, DecidableEq

/--
Checks whether a `CharClass` is well-formed, i.e. a range's `lower <= upper` by code point.
-/
def CharClass.isWellFormed : CharClass → Bool
  | .single _          => true
  | .range lower upper => lower <= upper
