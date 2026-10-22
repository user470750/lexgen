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
The mask of the low `ruleBits` bits of a `Packed`, which hold the rule.
-/
abbrev ruleMask : UInt64 := (1 <<< ruleBits.toUInt64) - 1

/--
The rule of a match in the input `input` and the position where the match stops, packed into one
number. The position is valid if there is a match.
-/
-- The rule and the position are packed into a `UInt64` only because the `@[unbox]` attribute does
-- not work: otherwise `Packed` would keep the position and the rule in separate fields.
structure Packed (input : String) where
  private mk ::
  /--
  The rule and the position, packed: the low `ruleBits` bits are the rule plus one, or zero if there
  is no match, and the bits above are the byte offset of the position in the input.
  -/
  private bits : UInt64
  /--
  Proves that the position can be unpacked: there is no match, or the position is valid.
  -/
  private safeUnpack : bits &&& ruleMask = 0 ∨
    String.Pos.Raw.IsValid input ⟨(bits >>> ruleBits.toUInt64).toNat⟩

/--
Unpacking a position packed into a `UInt64` does not change it, as long as the position and the
rule fit in their bits.
-/
private theorem toNat_shiftLeft_or_shiftRight {rule pos : Nat} (hp : pos < 2 ^ 48)
    (hr : rule + 1 < 65536) :
    (((pos.toUInt64 <<< ruleBits.toUInt64) ||| (rule + 1).toUInt64) >>>
      ruleBits.toUInt64).toNat = pos := by
  simp only [
    UInt64.toNat_shiftRight,
    UInt64.toNat_or,
    UInt64.toNat_shiftLeft,
    UInt64.toNat_ofNat',
    Nat.shiftRight_eq_div_pow,
    Nat.shiftLeft_eq,
    Nat.or_div_two_pow
  ]
  have h₁ : 2 ^ (ruleBits % 2 ^ 64 % 64) = 2 ^ 16 := rfl
  have h₂ : (rule + 1) % 2 ^ 64 / 2 ^ 16 = 0 := by omega
  rw [h₁, h₂, Nat.or_zero]
  omega

/--
A position packed into a `UInt64` stays valid after unpacking, as long as the position and the rule
fit in their bits.
-/
private theorem packed_position_isValid {rule : Nat} {input : String} {stop : input.Pos}
    (hp : input.utf8ByteSize < 2 ^ (64 - ruleBits)) (hr : rule + 1 < 2 ^ ruleBits) :
    String.Pos.Raw.IsValid
      input
      ⟨((stop.offset.byteIdx.toUInt64 <<< ruleBits.toUInt64 ||| (rule + 1).toUInt64) >>>
        ruleBits.toUInt64).toNat⟩ := by
  rw [
    toNat_shiftLeft_or_shiftRight
      (Nat.lt_of_le_of_lt stop.byteIdx_offset_le_utf8ByteSize hp)
      hr
  ]
  exact stop.isValid

/--
The `Packed` of no match.
-/
def Packed.noMatch {input : String} : Packed input := ⟨0, Or.inl rfl⟩

/--
Packs the rule `rule` of a match and the position `stop` right before which it stops. `hp` and `hr`
prove that the position and the rule fit in their bits.
-/
-- In the generated code `rule` is a literal, so `decide` proves `hr`. The `lexer` command rejects a
-- lexer with more rules.
@[inline]
def Packed.ofMatch {input : String} (rule : Nat) (stop : input.Pos)
    (hp : input.utf8ByteSize < 2 ^ (64 - ruleBits)) (hr : rule + 1 < 2 ^ ruleBits := by decide) :
    Packed input :=
  ⟨
    (stop.offset.byteIdx.toUInt64 <<< ruleBits.toUInt64) ||| (rule + 1).toUInt64,
    Or.inr (packed_position_isValid hp hr)
  ⟩

/--
Returns the rule packed in `packed`, or a number that is no rule, `2 ^ 64 - 1`, if there is no
match.
-/
@[inline]
def Packed.rule {input : String} (packed : Packed input) : UInt64 :=
  (packed.bits &&& ruleMask) - 1

/--
If `packed` holds a match, the bits of its rule are not zero.
-/
private theorem Packed.bits_and_ruleMask_ne_zero {input : String} {packed : Packed input}
    (h₁ : packed.rule ≠ UInt64.ofNat (2 ^ 64 - 1)) : packed.bits &&& ruleMask ≠ 0 :=
  fun h₂ : packed.bits &&& ruleMask = 0 => h₁ (congrArg (· - 1) h₂)

/--
Returns the position packed in `packed`, as a position of its input. `h` proves that `packed` holds
a match, so its position is valid.
-/
@[inline]
def Packed.stop {input : String} (packed : Packed input)
    (h : packed.rule ≠ UInt64.ofNat (2 ^ 64 - 1)) : input.Pos :=
  input.pos
    ⟨(packed.bits >>> ruleBits.toUInt64).toNat⟩
    (packed.safeUnpack.resolve_left (Packed.bits_and_ruleMask_ne_zero h))

end Lexgen.Internal
