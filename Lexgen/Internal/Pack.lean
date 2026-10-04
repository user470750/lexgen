/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public section

/-!
# Packed matches

Defines how the state functions of a generated lexer pass on and return the longest match: its rule
and the position where it stops, packed into a `UInt64` in `Packed`, so that no object is allocated
for them.
-/

namespace Lexgen.Internal

/--
The number of the low bits of a `Packed` that hold the rule.
-/
abbrev ruleBits : Nat := 16

/--
The rule of a match in the input `input` and the position where the match stops, packed into one
number. Only `Packed.noMatch` and `Packed.ofMatch` build one, so that the position is always one of
`input`.
-/
structure Packed (input : String.Slice) where
  private mk ::
  /--
  The rule and the position, packed: the low `ruleBits` bits are the rule plus one, or zero if there
  is no match, and the bits above are the byte offset of the position in the input.
  -/
  private bits : UInt64
deriving Inhabited

/--
The `Packed` of no match.
-/
def Packed.noMatch {input : String} : Packed input := ⟨0⟩

/--
Packs the rule `rule` of a match and the position `stop` right before which it stops.
-/
@[inline]
def Packed.ofMatch {input : String} (rule : Nat) (stop : input.Pos)
    -- The rule must not reach the bits of the position.
    (_ : rule + 1 < 2 ^ ruleBits := by decide) : Packed input :=
  ⟨(stop.offset.byteIdx.toUInt64 <<< ruleBits.toUInt64) ||| (rule + 1).toUInt64⟩

/--
Returns the rule packed in `packed`, or a number that is no rule, `2 ^ 64 - 1`, if there is no
match.
-/
@[inline]
def Packed.rule {input : String} (packed : Packed input) : UInt64 :=
  (packed.bits &&& ((1 <<< ruleBits.toUInt64) - 1)) - 1

/--
Returns the position packed in `packed` without checking that it is one of its input: the
implementation of `Packed.stop`.
-/
@[inline]
private unsafe def Packed.stopUnsafe {input : String} (packed : Packed input) : input.Pos :=
  -- At run time a position is its byte offset.
  unsafeCast (packed.bits >>> ruleBits.toUInt64).toNat

/--
Returns the position packed in `packed`, as a position of its input.

Only `Packed.noMatch` and `Packed.ofMatch` build a `Packed`, so the position in it is always one of
the input, as long as the input is shorter than `2 ^ (64 - ruleBits)` bytes. Compiled code calls
`Packed.stopUnsafe` instead, which skips the check.
-/
@[implemented_by Packed.stopUnsafe, inline]
def Packed.stop {input : String} (packed : Packed input) : input.Pos :=
  input.pos! ⟨(packed.bits >>> ruleBits.toUInt64).toNat⟩

end Lexgen.Internal
