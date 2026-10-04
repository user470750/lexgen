/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public import Lexgen.Internal.Regex.Ast
public import Lexgen.Internal.Regex.Parser

import Lexgen.Internal.Regex.Desugar
import Lexgen.Internal.Regex.Syntax

public section

/-!
# Regex

Defines `parse`, the entry point of the regex stage: parsing and desugaring.
-/

namespace Lexgen.Internal

/--
Parses a regular expression `s` and desugars it into a `RegexAST`.
-/
def parse (s : String) : Except ParseRegexError RegexAST :=
  (parseRegex s).map RegexSyntax.desugar

end Lexgen.Internal
