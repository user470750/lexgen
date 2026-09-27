/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public section

/-!
# Regex AST

Defines the abstract syntax tree (`RegexAST`)
for regular expressions.
-/

/--
The AST representation of regular expressions.

AST covers only basic regex constructs, while
others are desugared into them.
-/
inductive RegexAST where
  /--
  Matches an alternation of regular expressions.
  -/
  -- Left-associative: `RegexSyntax.desugar` keeps the parser's `alt (alt a b) c`.
  | alt (left right : RegexAST)
  /--
  Matches a concatenation of regular expressions.
  -/
  -- Left-associative: `RegexSyntax.desugar` keeps the parser's `concat (concat a b) c`.
  | concat (first rest : RegexAST)
  /--
  Matches repetition of a regular expression.
  -/
  | repeated (re : RegexAST)
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
def RegexAST.normalizedConcat : RegexAST → RegexAST → RegexAST
  | .ε,    rest => rest
  | first, .ε   => first
  | first, rest => .concat first rest

/--
Checks whether `re` matches the empty string.
-/
def RegexAST.matchesEmpty : RegexAST → Bool
  | .ε          => true
  | .symbol _   => false
  | .dot        => false
  | .alt l r    => l.matchesEmpty || r.matchesEmpty
  | .concat f s => f.matchesEmpty && s.matchesEmpty
  | .repeated _ => true
