/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public section

/-!
# Instances

Defines `inRange` and `inTable`, the tests of a transition in the generated code. Their own
`Decidable` instances let the elaboration of the generated `if`s find one in a single step.
`inTable` also proves once that its index is in bounds, instead of a proof in every generated test.
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

/--
Checks whether the entry of `table` indexed by `b` has the bit `mask` set.
-/
-- Exposed for the instance below, as `inRange`.
@[expose]
def inTable (b : UInt8) (hb : b < 0x80) (table : ByteArray) (ht : table.size = 0x80)
    (mask : UInt8) : Prop :=
  table[b.toNat]'(ht ▸ hb) &&& mask ≠ 0

@[inline]
instance (b : UInt8) (hb : b < 0x80) (table : ByteArray) (ht : table.size = 0x80) (mask : UInt8) :
    Decidable (inTable b hb table ht mask) :=
  instDecidableNot

end Lexgen.Internal
