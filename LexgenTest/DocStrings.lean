/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
import Lexgen

/-!
# Tests on docstrings

The docstring of a `lexer` declaration goes to the token type, and the docstring of a token rule
to its constructor.
-/

open Lean

/--
Tokens of arithmetic on Roman numerals.
-/
lexer Roman where
  /--
  Whitespaces.
  -/
  skip r" +"
  /--
  A Roman numeral.
  -/
  | numeral := r"[IVXLCDM]+"
  /--
  Exponentiation.
  -/
  | power  := r"\*\*"
  | times  := r"\*"

/--
info: Roman: Tokens of arithmetic on Roman numerals.
Roman.numeral: A Roman numeral.
Roman.power: Exponentiation.
-/
#guard_msgs in
#eval show CoreM Unit from do
  let env ← getEnv
  for name in [``Roman, ``Roman.numeral, ``Roman.power] do
    let doc? ← findDocString? env name
    IO.println s!"{name}: {(doc?.map (·.trimAscii)).get!}"
