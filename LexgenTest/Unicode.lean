/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
import Lexgen
import LexgenTest.Basic

/-!
# Tests on non-ASCII characters

Slices and byte offsets of tokens with characters of 1 to 4 bytes in UTF-8.
-/

open Lexgen

lexer Token where
  skip r"[ \t\n\f]+"
  | ascii    := r"[a-zA-Z]+"
  | cyrillic := r"[а-яА-ЯёЁ]+"
  | chinese  := r"[一-鿿]+"
  | other    := r"."
deriving BEq

-- A Cyrillic letter takes 2 bytes.
#guard lex Token "До свидания" == some #[
  (.cyrillic, "До",       0, 4),
  (.cyrillic, "свидания", 5, 21)
]

#guard lex Token "abcабв" == some #[
  (.ascii,    "abc", 0, 3),
  (.cyrillic, "абв", 3, 9)
]

-- A Chinese character takes 3 bytes.
#guard lex Token "你好 世界" == some #[
  (.chinese, "你好", 0, 6),
  (.chinese, "世界", 7, 13)
]

-- An emoji takes 4 bytes.
#guard lex Token "a😀b" == some #[
  (.ascii, "a",  0, 1),
  (.other, "😀", 1, 5),
  (.ascii, "b",  5, 6)
]

#guard lex Token "😀 € é λ" == some #[
  (.other, "😀", 0,  4),
  (.other, "€",  5,  8),
  (.other, "é",  9,  11),
  (.other, "λ",  12, 14)
]

#guard lex Token "👍🏽" == some #[
  (.other, "👍",                          0, 4),
  (.other, (Char.ofNat 0x1F3FD).toString, 4, 8)
]

lexer Word where
  skip r"[ \t\n\f]+"
  | cyrillic := r"[а-яА-ЯёЁ]+"
deriving Repr

/-- info: Except.error "offset 13: no rule matches the input" -/
#guard_msgs in
#eval (Lexer.new "привет ?" : Lexer Word).tokens
