/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
import Lexgen

/-!
# Tests on a CSS lexer

Ported from logos's `tests/tests/css.rs`.
-/

open Lexgen

lexer Token where
  skip r"[ \t\n\f]+"
  | relativeLength    := r"em|ex|ch|rem|vw|vh|vmin|vmax"
  | absoluteLength    := r"cm|mm|Q|in|pc|pt|px"
  | number            := r"[+\-]?[0-9]*[.]?[0-9]+([eE][+\-]?[0-9]+)?"
  | ident             := r"[\-a-zA-Z_][a-zA-Z0-9_\-]*"
  | curlyBracketOpen  := r"\{"
  | curlyBracketClose := r"\}"
  | colon             := r":"
deriving BEq

/--
The tokens of `input` with their slices and byte offsets, if it lexes without errors.
-/
def lex (input : String) : Option (Array (Token × String × Nat × Nat)) :=
  (Lexer.new input : Lexer Token).spanned.toOption.map
    (·.map fun t => (t.token, t.slice.toString, t.startOffset, t.stopOffset))

#guard lex "h2 { line-height: 3cm }" == some #[
  (.ident,             "h2",          0,  2),
  (.curlyBracketOpen,  "{",           3,  4),
  (.ident,             "line-height", 5,  16),
  (.colon,             ":",           16, 17),
  (.number,            "3",           18, 19),
  (.absoluteLength,    "cm",          19, 21),
  (.curlyBracketClose, "}",           22, 23)
]

#guard lex "h3 { word-spacing: 4mm }" == some #[
  (.ident,             "h3",           0,  2),
  (.curlyBracketOpen,  "{",            3,  4),
  (.ident,             "word-spacing", 5,  17),
  (.colon,             ":",            17, 18),
  (.number,            "4",            19, 20),
  (.absoluteLength,    "mm",           20, 22),
  (.curlyBracketClose, "}",            23, 24)
]

#guard lex "h3 { letter-spacing: 42em }" == some #[
  (.ident,             "h3",             0,  2),
  (.curlyBracketOpen,  "{",              3,  4),
  (.ident,             "letter-spacing", 5,  19),
  (.colon,             ":",              19, 20),
  (.number,            "42",             21, 23),
  (.relativeLength,    "em",             23, 25),
  (.curlyBracketClose, "}",              26, 27)
]
