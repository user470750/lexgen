/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public import Lexgen.Internal.Regex.Ast

import Lexgen.Internal.Regex.Desugar
import Lexgen.Internal.Regex.Parser
import Lexgen.Internal.Regex.Syntax

public section

/--
Parses a regular expression `s` and desugars it into a `RegularExprAST`.

Returns an error message if `s` is not a valid regular expression.
-/
def parse (s : String) : Except String RegularExprAST := do
  let syn ← parseRe.run s
  pure syn.desugar
