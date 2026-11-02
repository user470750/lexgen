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
The label type for `DFA` transitions: an interval of bytes, from `lower` to `upper`, inclusive.
-/
structure DFA.Interval where
  /--
  The lower bound of the interval.
  -/
  lower : UInt8
  /--
  The upper bound of the interval.
  -/
  upper : UInt8
deriving Repr, BEq

end Lexgen.Internal
