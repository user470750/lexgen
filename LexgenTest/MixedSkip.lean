/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
import Lexgen
import LexgenTest.Basic

/-!
# Tests on rule order

Skip rules may come anywhere among the token rules. On a tie the earlier rule wins.
-/

lexer Token where
  skip   r" +"
  | kwIf := r"if"
  skip   r"[a-z]+"
  skip   r"--"
  | op   := r"-+"
deriving BEq, Repr

#guard lex Token "if iff - -- ---" == some #[
  (.kwIf, "if",  0,  2),
  (.op,   "-",   7,  8),
  (.op,   "---", 12, 15)
]
