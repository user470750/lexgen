/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public section

/-!
# Instances

Defines `inRange`, the test of a transition on a range of bytes in the generated code. Its own
`Decidable` instance lets the elaboration of the generated `if`s find one in a single step, instead
of the three that `lo ≤ b ∧ b ≤ hi` would need.
-/

namespace Lexgen.Internal

/--
Checks whether `b` lies between `lo` and `hi`, inclusive.
-/
-- Exposed, so that the instance below can see that `inRange b lo hi` is `lo ≤ b ∧ b ≤ hi`: public
-- declarations, such as instances, do not see the body of a definition unless it is exposed.
@[expose]
def inRange (b lo hi : UInt8) : Prop := lo ≤ b ∧ b ≤ hi

@[inline]
instance (b lo hi : UInt8) : Decidable (inRange b lo hi) :=
  instDecidableAnd

end Lexgen.Internal
