/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
import Lexgen

/-!
# Helpers for the tests
-/

open Lexgen

/--
The tokens of type `α` of `input` with their slices and byte offsets, if it lexes without errors.
-/
def lex (α : Type) [Lexable α] (input : String) : Option (Array (α × String × Nat × Nat)) :=
  (Lexer.new input).spanned.toOption.map
    (·.map fun t => (t.token, t.slice.toString, t.startOffset, t.stopOffset))
