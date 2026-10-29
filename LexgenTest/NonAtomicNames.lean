/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
import Lexgen

/-!
# Tests on non-atomic names

Errors on token names and subpattern names that contain `.`.
-/

/--
error: token name `a.b` cannot contain `.`
---
error: token name `c.d.e` cannot contain `.`
-/
#guard_msgs in
lexer Tokens where
  | a.b                  := r"a"
  | f                    := r"f"
  | c.d.e : String.Slice := r"c" => id

/--
error: subpattern name `a.b` cannot contain `.`
---
error: subpattern name `c.d.e` cannot contain `.`
-/
#guard_msgs in
lexer Subpatterns where
  subpattern a.b   := r"a"
  subpattern f     := r"f"
  subpattern c.d.e := r"c"
  | g := r"g"
