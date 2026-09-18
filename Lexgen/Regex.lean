/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module
public import Lexgen.Regex.Syntax
public import Lexgen.Regex.Ast
public import Lexgen.Regex.Parser
public import Lexgen.Regex.Desugar

public section

/--
Parses a regular expression `s` and desugars it into a `RegularExprAST`.

Returns an error message if `s` is not a valid regular expression.
-/
def parse (s : String) : Except String RegularExprAST := do
  let syn ← parseRe.run s
  pure $ desugar syn
