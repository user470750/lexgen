/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
import Lexgen.Regex.Syntax
import Lexgen.Regex.Ast
import Lexgen.Regex.Parser
import Lexgen.Regex.Desugar

/--
Parses a regular expression `s` and desugars it into a `RegularExprAST`.

Returns an error message if `s` is not a valid regular expression.
-/
def parse (s : String) : Except String RegularExprAST := do
  let syn ← parseRe.run s
  pure $ desugar syn

#eval parse "ab|a*|b+|c?|e{3,4}"
