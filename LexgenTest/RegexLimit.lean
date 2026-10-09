/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
import Lexgen

/-!
# Tests on regex size limits

Errors on quantifiers with too many repetitions and on patterns that are too large once their
repetitions are expanded.
-/

/--
error: offset 6: invalid quantifier `{1001`: more than 1000 repetitions
---
error: offset 8: invalid quantifier `{0,1001`: more than 1000 repetitions
---
error: this rule is too large: its repetitions expand into more than 2000 AST leaves
-/
#guard_msgs in
lexer Token where
  | rule₁ := r"a{1001}"
  | rule₂ := r"a{0,1001}"
  | rule₃ := r"(a{50}){50}"  -- nested repetitions
