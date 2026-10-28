/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
import Lexgen
import LexgenTest.Basic

/-!
# Tests on subpatterns

Token rules use subpatterns declared before and after them, a subpattern uses one declared before
it, and a subpattern may match the empty string.
-/

open Lexgen

lexer Token where
  skip r" +"
  subpattern digit := r"[0-9]"
  -- Matches the empty string.
  subpattern sign  := r"[+\-]?"
  | int   := r"(?&sign)(?&digit)+"
  | float := r"(?&sign)(?&digit)+(?&frac)"
  -- Tests visibility: used by `float` above it; uses `digit`.
  subpattern frac  := r"\.(?&digit)+"
deriving BEq, Repr

#guard lex Token "12 -3.45 +6" == some #[
  (.int,   "12",    0, 2),
  (.float, "-3.45", 3, 8),
  (.int,   "+6",    9, 11)
]
