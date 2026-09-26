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
An error of `parse`.
-/
structure ParseRegexError where
  /--
  The byte offset in the pattern where parsing failed.
  -/
  offset : Nat
  /--
  The description of the error.
  -/
  msg    : String

/--
Parses a regular expression `s` and desugars it into a `RegularExprAST`.
-/
def parse (s : String) : Except ParseRegexError RegularExprAST :=
  match parseRe ⟨s, s.startPos⟩ with
  | .success _ syn => pure syn.desugar
  | .error it err  => throw { offset := it.2.offset.byteIdx, msg := toString err }
