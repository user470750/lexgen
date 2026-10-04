/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public section

/-!
# DFA intervals

Defines `DFA.Interval`, the label type for `DFA` transitions.
-/

namespace Lexgen.Internal

/--
The label type for `DFA` transitions: an interval of characters, which is a single character or a
range of characters.
-/
inductive DFA.Interval where
  /--
  Matches the literal character `character`.
  -/
  | single (character : Char)
  /--
  Matches a character from `lower` to `upper`, inclusive.
  -/
  | range (lower upper : Char)
deriving Repr, BEq

/--
Returns the interval of the characters from `lower` to `upper`, inclusive: a `single` if they are
the same character.
-/
def DFA.Interval.ofBounds (lower upper : Char) : DFA.Interval :=
  if lower == upper then .single lower else .range lower upper

/--
Returns the first character of `interval`.
-/
def DFA.Interval.lower : DFA.Interval → Char
  | .single character => character
  | .range lower _    => lower

/--
Returns the last character of `interval`.
-/
def DFA.Interval.upper : DFA.Interval → Char
  | .single character => character
  | .range _ upper    => upper

end Lexgen.Internal
