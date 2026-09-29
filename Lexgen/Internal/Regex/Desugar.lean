/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public import Lexgen.Internal.Regex.Ast
public import Lexgen.Internal.Regex.Syntax

public section

/-!
# Regular expression desugaring

Defines `RegexSyntax.desugar` for translating a CST into an AST.
-/

/--
Builds a chain of `n` copies of `re` concatenated together.

Produces `.ε` (the empty match) if `n` (repetitions) is zero.

Used for the required part of a `{n,m}` range.
-/
private def repeatConcat (n : Nat) (re : RegexAST) : RegexAST :=
  match n with
  | 0     => .ε
  | m + 1 => (List.replicate m re).foldl .concat re

/--
Builds a chain of `n` optional copies of `re`, allowing to match
at most `n` times.

Used for the optional part of a `{n,m}` range.
-/
private def optionalTail (n : Nat) (re : RegexAST) : RegexAST :=
  repeatConcat n (.alt re .ε)

/--
Returns the ranges of the named character class `kind`.
-/
private def namedClassRanges (kind : NamedClass) : Array CharClass :=
  let max := Char.ofNat 0x10FFFF
  match kind with
  | .digit => #[.range '0' '9']
  | .nonDigit =>
    #[
      .range '\x00' '/',
      .range ':' max
    ]
  | .word =>
    #[
      .range '0' '9',
      .range 'A' 'Z',
      .single '_',
      .range 'a' 'z'
    ]
  | .nonWord =>
    #[
      .range '\x00' '/',
      .range ':' '@',
      .range '[' '^',
      .single '`',
      .range '{' max
    ]
  | .space =>
    #[
      .range '\t' '\r',
      .single ' '
    ]
  | .nonSpace =>
    #[
      .range '\x00' '\x08',
      .range '\x0e' '\x1f',
      .range '!' max
    ]

/--
Desugars an item of a character class into the ranges it matches.
-/
private def ClassItem.desugar : ClassItem → Array CharClass
  | ClassItem.single c          => #[CharClass.single c]
  | ClassItem.range lower upper => #[CharClass.range lower upper]
  | ClassItem.named kind        => namedClassRanges kind

/--
Desugars CST (`RegexSyntax`) to AST (`RegexAST`).

Expresses complex constructs (e.g. `+`, `?`, `{n,m}`) in terms of the
basic AST constructors.
-/
def RegexSyntax.desugar : RegexSyntax → RegexAST
  | RegexSyntax.alt left right =>
    RegexAST.alt left.desugar right.desugar
  | RegexSyntax.concat first rest =>
    RegexAST.concat first.desugar rest.desugar
  | RegexSyntax.repeated { minimum := n, maximum := none } re =>
    let desugared := re.desugar
    RegexAST.normalizedConcat
      (repeatConcat n desugared)
      (RegexAST.repeated desugared)
  | RegexSyntax.repeated { minimum := n, maximum := some m } re =>
    let desugared := re.desugar
    RegexAST.normalizedConcat
      (repeatConcat n desugared)
      (optionalTail (m - n) desugared)
  | RegexSyntax.symbol c => RegexAST.charClass false #[.single c]
  | RegexSyntax.charClass negate items =>
    RegexAST.charClass negate (items.flatMap ClassItem.desugar)
  | RegexSyntax.namedClass kind =>
    RegexAST.charClass false (namedClassRanges kind)
  | RegexSyntax.dot      => RegexAST.charClass true #[.single '\n']
  | RegexSyntax.ε        => RegexAST.ε
