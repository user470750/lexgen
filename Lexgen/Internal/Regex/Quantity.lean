/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public section

/-!
# Quantity

Defines `Quantity`, used to represent repetition bounds.
-/

namespace Lexgen.Internal

/--
Repetition bounds.
-/
inductive Quantity where
  /--
  At least `minimum` occurrences, with no upper bound: `*`, `+` and `{n,}`.
  -/
  | atLeast (minimum : Nat)
  /--
  From `minimum` to `maximum` occurrences, inclusive: `?`, `{n}` and `{n,m}`.
  -/
  | between (minimum maximum : Nat) (inOrder : minimum ≤ maximum)
deriving Repr, DecidableEq

/--
A `Quantity` without bounds.
-/
def Quantity.zeroOrMore : Quantity := .atLeast 0

/--
A `Quantity` with a lower bound of one and no upper bound.
-/
def Quantity.oneOrMore : Quantity := .atLeast 1

/--
A `Quantity` from zero to one.
-/
def Quantity.optionalOne : Quantity := .between 0 1 (Nat.zero_le 1)

/--
Creates a `Quantity` requiring an exact number of occurrences.
-/
def Quantity.exactly (n : Nat) : Quantity := .between n n (Nat.le_refl n)

end Lexgen.Internal
