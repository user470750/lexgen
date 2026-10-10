/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public section

/-!
# Instances

Defines `inRange`, the test of a transition on a range of characters in the generated code. Its own
`Decidable` instance lets the elaboration of the generated `if`s find one in a single step, instead
of the three that `lo ≤ c ∧ c ≤ hi` would need.
-/

namespace Lexgen.Internal

/--
Checks whether `c` lies between `lo` and `hi`, inclusive.
-/
-- Exposed, so that the instance below can see that `inRange c lo hi` is `lo ≤ c ∧ c ≤ hi`: public
-- declarations, such as instances, do not see the body of a definition unless it is exposed.
@[expose]
def inRange (c lo hi : Char) : Prop := lo ≤ c ∧ c ≤ hi

@[inline]
instance (c lo hi : Char) : Decidable (inRange c lo hi) :=
  instDecidableAnd

end Lexgen.Internal
