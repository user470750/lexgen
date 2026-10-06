/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
import Lexgen
import LexgenTest.Basic

/-!
# Tests on a lexer that accepts no input

A rule matching the empty language, as opposed to the empty string, is not rejected: it never
matches any text, so it cannot make the lexer loop. The start state is then equivalent to the trap.
-/

open Lexgen

/-- warning: this rule never produces a match: other rules always win over it -/
#guard_msgs in
lexer Nothing where
  | a := r"[^\s\S]"
deriving BEq, Repr

-- The empty input lexes to no tokens; anything else fails, since no rule can ever match it.
#guard lex Nothing "" == some #[]
#guard lex Nothing "x" == none

/-- info: Except.error "offset 0: no rule matches the input" -/
#guard_msgs in
#eval (Lexer.new "x" : Lexer Nothing).tokens
