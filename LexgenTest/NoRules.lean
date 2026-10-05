/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
import Lexgen

/-!
# Tests on a lexer without rules

Error on a lexer declared without any rule.
-/

/-- error: a lexer needs at least one rule -/
#guard_msgs in
lexer Token where
