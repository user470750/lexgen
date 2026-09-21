/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public section

/-!
# Regex AST

Defines the abstract syntax tree (`RegularExprAST`)
for regular expressions.
-/

/--
The AST representation of regular expressions.

AST covers only basic regex constructs, while
others are desugared into them.
-/
inductive RegularExprAST where
  /--
  Matches an alternation of regular expressions.
  -/
  -- Left-associative: `desugar` keeps the parser's `alt (alt a b) c`.
  | alt (left right : RegularExprAST)
  /--
  Matches a concatenation of regular expressions.
  -/
  -- Left-associative: `desugar` keeps the parser's `concat (concat a b) c`.
  | concat (first rest : RegularExprAST)
  /--
  Matches repetition of a regular expression.
  -/
  | repeated (re : RegularExprAST)
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
deriving Repr

/--
Wraps `concat` to avoid useless concatenation with ε.
-/
def RegularExprAST.normalizedConcat : RegularExprAST → RegularExprAST → RegularExprAST
  | .ε,   rest  => rest
  | first, .ε   => first
  | first, rest => .concat first rest
