/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
import Lexgen

/-!
# Tests on late skip rules

An error on a skip rule written after the token rules.
-/

/--
error: skip rules must come before the token rules
---
error: skip rules must come before the token rules
-/
#guard_msgs in
lexer Token where
  | abc := r"abc"
  skip r" +"
  skip r"\s"
