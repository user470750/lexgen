/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public import Lexgen.Regex.Ast
public import Lexgen.Regex.Syntax

public section

/-!
# Regular expression desugaring

Defines `ReSyntax.desugar` for translating a CST into an AST.
-/

/--
Builds a chain of `n` copies of `re` concatenated together.

Produces `.ε` (the empty match) if `n` (repetitions) is zero.

Used for the required part of a `{n,m}` range.
-/
private def repeatConcat (n : Nat) (re : RegularExprAST) : RegularExprAST :=
  match n with
  | 0     => .ε
  | m + 1 => (List.replicate m re).foldl .concat re

/--
Builds a chain of `n` optional copies of `re`, allowing to match
at most `n` times.

Used for the optional part of a `{n,m}` range.
-/
private def optionalTail (n : Nat) (re : RegularExprAST) : RegularExprAST :=
  repeatConcat n (.alt re .ε)

/--
Desugars CST (`ReSyntax`) to AST (`RegularExprAST`).

Expresses complex constructs (e.g. `+`, `?`, `{n,m}`) in terms of the
basic AST constructors.
-/
def ReSyntax.desugar : ReSyntax → RegularExprAST
  | .alt left right =>
    .alt left.desugar right.desugar
  | .concat first rest =>
    .concat first.desugar rest.desugar
  | .repeatRe { minimum := n, maximum := none } re =>
    let desugared := re.desugar
    .normalizedConcat (repeatConcat n desugared) (.repeated desugared)
  | .repeatRe { minimum := n, maximum := some m } re =>
    let desugared := re.desugar
    .normalizedConcat
      (repeatConcat n desugared)
      (optionalTail (m - n) desugared)
  | .symbol c => .symbol c
  | .dot      => .dot
  | .ε        => .ε
