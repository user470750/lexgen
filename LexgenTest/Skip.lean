/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
import Lexgen

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

/--
The tokens of `input` with their slices and byte offsets, if it lexes without errors.
-/
def lex (input : String) : Option (Array (Token × String × Nat × Nat)) :=
  (Lexer.new input).spanned.toOption.map
    (·.map fun t => (t.token, t.slice.toString, t.startOffset, t.stopOffset))

#guard lex "a // b // c -- d\ne # f\ng" == some #[
  (.ident, "a", 0,  1),
  (.ident, "c", 10, 11),
  (.ident, "e", 17, 18),
  (.ident, "g", 23, 24)
]

#guard lex "a //b\nc// d" == some #[
  (.ident, "a", 0,  1),
  (.ident, "d", 10, 11)
]

#guard lex "# // a\nb" == some #[
  (.ident, "b", 7, 8)
]

#guard lex "// -- a\n# b // c" == some #[
  (.ident, "c", 15, 16)
]

#guard lex "a -- b" == some #[
  (.ident, "a", 0, 1)
]

/-- info: Except.error "offset 2: no rule matches the input" -/
#guard_msgs in
#eval (Lexer.new "a // b" : Lexer Token).tokens
