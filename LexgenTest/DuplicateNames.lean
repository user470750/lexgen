/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
import Lexgen

/-!
# Tests on duplicate names

Errors on a token name used more than once in a lexer and on a lexer declared twice.
-/

/--
error: duplicate token name `a`
---
error: duplicate token name `b`
---
error: duplicate token name `a`
-/
#guard_msgs in
lexer Token where
  | a := r"a"
  | b := r"b"
  | a := r"c"
  | b := r"d"
  | a := r"e"

lexer Twice where
  | a := r"a"

/-- error: `Twice` has already been declared -/
#guard_msgs in
lexer Twice where
  | b := r"b"
