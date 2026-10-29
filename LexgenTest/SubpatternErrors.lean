/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
import Lexgen

/-!
# Tests on subpattern errors

Errors on subpattern names, on references to unknown subpatterns, and on invalid or too large
subpatterns, which stop the conversion before the rules are checked.
-/

/--
error: duplicate name `a`
---
error: duplicate name `b`
---
error: subpattern name `c.d` cannot contain `.`
-/
#guard_msgs in
lexer Names where
  | a := r"a"
  subpattern a := r"x"  -- the name of a token
  subpattern b := r"y"
  subpattern b := r"z"
  subpattern c.d := r"w"

/--
error: unknown subpattern name: `digit`
---
error: offset 3: missing subpattern name after `(?&`
-/
#guard_msgs in
lexer UnknownInRules where
  | digits := r"(?&digit)+"
  | empty  := r"(?&)"

/-- error: unknown subpattern name: `b` -/
#guard_msgs in
lexer DeclaredLater where
  subpattern a := r"(?&b)"
  subpattern b := r"b"
  | c := r"(?&a)"

/-- error: unknown subpattern name: `a` -/
#guard_msgs in
lexer SelfReference where
  subpattern a := r"a(?&a)?"
  | b := r"(?&a)"

/-- error: offset 2: missing `]`, unterminated character class -/
#guard_msgs in
lexer Invalid where
  subpattern a := r"[a"
  -- Not checked: `b` would refer to an unknown subpattern, and `c` is invalid.
  | b := r"(?&a)"
  | c := r"["

/--
error: this subpattern is too large: its repetitions expand into more than 2000 AST leaves
-/
#guard_msgs in
lexer LargeSubpattern where
  subpattern a := r"(a{50}){50}"
  | b := r"b"

/--
error: this rule is too large: its repetitions expand into more than 2000 AST leaves
-/
#guard_msgs in
lexer LargeRule where
  subpattern a := r"a{1000}"
  | b := r"(?&a)(?&a)(?&a)"  -- each use of `a` counts
