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
# Regex desugaring

Defines `RegexSyntax.desugar` for translating a CST into an AST.
-/

namespace Lexgen.Internal

/--
Builds a chain of `n` copies of `re` concatenated together.

Produces `.ε` if `n` is zero.

Used for the required part of a `{n,m}` range.
-/
private def repeatConcat (n : Nat) (re : RegexAST) : RegexAST :=
  match n with
  | 0     => .ε
  | m + 1 => (List.replicate m re).foldl .concat re

/--
Builds a chain of `n` optional copies of `re`.

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
  | .digit => #[.range '0' '9' (by decide)]
  | .nonDigit =>
    #[
      .range '\x00' '/' (by decide),
      .range ':' max (by decide)
    ]
  | .word =>
    #[
      .range '0' '9' (by decide),
      .range 'A' 'Z' (by decide),
      .single '_',
      .range 'a' 'z' (by decide)
    ]
  | .nonWord =>
    #[
      .range '\x00' '/' (by decide),
      .range ':' '@' (by decide),
      .range '[' '^' (by decide),
      .single '`',
      .range '{' max (by decide)
    ]
  | .space =>
    #[
      .range '\t' '\r' (by decide),
      .single ' '
    ]
  | .nonSpace =>
    #[
      .range '\x00' '\x08' (by decide),
      .range '\x0e' '\x1f' (by decide),
      .range '!' max (by decide)
    ]

/--
Desugars an item of a character class into the ranges it matches.
-/
private def ClassItem.desugar : ClassItem → Array CharClass
  | ClassItem.single c => #[CharClass.single c]
  | ClassItem.range lower upper wellFormed =>
    #[CharClass.range lower upper wellFormed]
  | ClassItem.named kind => namedClassRanges kind

/--
Desugars CST (`RegexSyntax`) to AST (`RegexAST`).

Expresses complex constructs (e.g. `+`, `?`, `{n,m}`) in terms of the basic AST constructors.
-/
def RegexSyntax.desugar : RegexSyntax → RegexAST
  | RegexSyntax.alt left right =>
    RegexAST.alt left.desugar right.desugar
  | RegexSyntax.concat first rest =>
    RegexAST.concat first.desugar rest.desugar
  | RegexSyntax.repeated (.atLeast n) re =>
    let desugared := re.desugar
    RegexAST.normalizedConcat
      (repeatConcat n desugared)
      (RegexAST.repeated desugared)
  | RegexSyntax.repeated (.between n m ..) re =>
    let desugared := re.desugar
    -- `inOrder` keeps `m - n` from truncating to zero.
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

end Lexgen.Internal
