/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public section

/-!
# Packed matches

Defines how the state functions of a generated lexer pass on and return the longest match: packed
into a `UInt64`, so that no object is allocated for it. The low `ruleBits` bits are its rule plus
one, or zero if there is no match, and the bits above are the byte offset of its end in the input.
-/

namespace Lexgen.Internal

/--
The number of the low bits of a packed match that hold its rule.
-/
private abbrev ruleBits : UInt64 := 16

/--
The packed match of no rule.
-/
def noMatch : UInt64 := 0

/--
Returns the packed match of rule `rule` that stops right before `stop`.
-/
@[inline]
def packMatch {input : String.Slice} (rule : Nat) (stop : input.Pos) : UInt64 :=
  (stop.offset.byteIdx.toUInt64 <<< ruleBits) ||| (rule + 1).toUInt64

/--
Returns the rule of the packed match `packed`, or a number that is no rule, `2 ^ 64 - 1`, if there
is no match.
-/
@[inline]
def unpackRule (packed : UInt64) : UInt64 :=
  (packed &&& ((1 <<< ruleBits) - 1)) - 1

/--
Returns the end of the packed match `packed`, as a position of the input `input` it comes from.
-/
@[inline]
def unpackEnd (input : String.Slice) (packed : UInt64) : input.Pos :=
  -- The end always comes from a position of `input`, so `pos!` never panics.
  input.pos! ⟨(packed >>> ruleBits).toNat⟩

end Lexgen.Internal
