/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
import Lexgen
import LexgenTest.Basic

/-!
# Tests without DFA minimization

The lexer is built with `lexgen.minimization` off, so its DFA keeps redundant states.
-/

open Lexgen

-- For `(a|b)*abb` the subset construction gives one state more than the minimal DFA.
set_option lexgen.minimization false in
lexer Token where
  skip r" +"
  | abb   := r"(a|b)*abb"
  | ident := r"[a-z]+"
deriving BEq, Repr

#guard lex Token "abb babb abab ab" == some #[
  (.abb,   "abb",  0,  3),
  (.abb,   "babb", 4,  8),
  (.ident, "abab", 9,  13),
  (.ident, "ab",   14, 16)
]
