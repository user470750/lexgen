/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
import Lexgen
import LexgenTest.Basic

/-!
# Tests on skip rules

The lexer skips whitespace and three kinds of comments: `//…//`, which may span several lines, and
`--` and `#` up to the end of the line.
-/

open Lexgen

lexer Token where
  skip r"\s+"
  skip r"//([^/]|/[^/])*//"
  skip r"--[^\n]*"
  skip r"#[^\n]*"
  | ident := r"[a-zA-Z_][a-zA-Z0-9_]*"
deriving BEq, Repr

#guard lex Token "a // b // c -- d\ne # f\ng" == some #[
  (.ident, "a", 0,  1),
  (.ident, "c", 10, 11),
  (.ident, "e", 17, 18),
  (.ident, "g", 23, 24)
]

#guard lex Token "a //b\nc// d" == some #[
  (.ident, "a", 0,  1),
  (.ident, "d", 10, 11)
]

#guard lex Token "# // a\nb" == some #[
  (.ident, "b", 7, 8)
]

#guard lex Token "// -- a\n# b // c" == some #[
  (.ident, "c", 15, 16)
]

#guard lex Token "a -- b" == some #[
  (.ident, "a", 0, 1)
]

/-- info: Except.error "offset 2: no rule matches the input" -/
#guard_msgs in
#eval (Lexer.new "a // b" : Lexer Token).tokens
