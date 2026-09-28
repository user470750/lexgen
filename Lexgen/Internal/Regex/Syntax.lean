/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public import Lexgen.Internal.Regex.Quantity

public section

/-!
# Regex CST

Defines the concrete syntax tree (`RegexSyntax`) for regular expressions.
-/

/--
The CST representation of regular expressions.
-/
inductive RegexSyntax where
  /--
  Matches an alternation of regular expressions.
  -/
  -- Left-associative due to the parser: `a|b|c` is `alt (alt a b) c`.
  | alt (left right : RegexSyntax)
  /--
  Matches a concatenation of regular expressions.
  -/
  -- Left-associative due to the parser: `abc` is `concat (concat a b) c`.
  | concat (first rest : RegexSyntax)
  /--
  Matches repetition of a regular expression.
  -/
  | repeated (quantity : Quantity) (re : RegexSyntax)
  /--
  Matches the literal character `c`.
  -/
  | symbol (c : Char)
  /--
  Matches any single character.
  -/
  | dot
  /--
  Matches the empty string.
  -/
  | ε
deriving Repr, DecidableEq
