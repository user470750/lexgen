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
-- TODO: Run the parser directly instead of `Parser.run`, so that the error keeps
-- the offset as data rather than inside a message.
def parse (s : String) : Except String RegularExprAST := do
  let syn ← parseRe.run s
  return syn.desugar
