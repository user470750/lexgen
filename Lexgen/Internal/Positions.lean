/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public import Lexgen.Internal.Instances

public section

/-!
# Positions

Defines the lemmas that let the generated lexer move from character to character without checking
positions at runtime.
-/

namespace Lexgen.Internal

/--
The bounds of the first bytes that start a sequence of `n + 1` bytes, inclusive.
-/
-- Exposed, so that `decide` in the generated code, which is in another module, can compute the
-- bounds.
@[expose]
def leadBounds : Fin 4 → UInt8 × UInt8
  | 0 => (0x00, 0x7F)
  | 1 => (0xC2, 0xDF)
  | 2 => (0xE0, 0xEF)
  | 3 => (0xF0, 0xF4)

/--
A first byte from `l` to `u`, within the bounds `leadBounds n`, starts a sequence of `n + 1` bytes.
-/
theorem utf8ByteSize_eq {b : UInt8} {hb : b.IsUTF8FirstByte} {l u : UInt8} (n : Fin 4)
    (h : inRange b l u) (hl : (leadBounds n).1 ≤ l := by decide)
    (hu : u ≤ (leadBounds n).2 := by decide) :
    b.utf8ByteSize hb = n + 1 := by
  have h₁ := UInt8.le_trans hl h.1
  have h₂ := UInt8.le_trans h.2 hu
  clear h hl hu
  -- A `UInt8` is a `BitVec 8` inside, and core decides `∀` over `BitVec n`.
  cases b
  decide +revert

/--
The position after the character at `pos` is valid, written as the position after its first byte
followed by the `n` remaining bytes of the character.
-/
-- This is the validity of `pos.next h`, whose offset is `pos` moved on by `utf8ByteSize` of the
-- first byte by definition.
theorem isValid_next {input : String} {pos : input.Pos} {h : pos ≠ input.endPos} {n : Nat}
    (hn : (pos.byte h).utf8ByteSize String.Pos.isUTF8FirstByte_byte = n + 1) :
    (⟨pos.offset.byteIdx + (n + 1)⟩ : String.Pos.Raw).IsValid input :=
  hn ▸ (pos.next h).isValid

end Lexgen.Internal
