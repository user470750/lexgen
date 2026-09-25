/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public meta import Lean
public meta import Lexgen.Internal.Codegen
public meta import Lexgen.Internal.Pipeline

public section

open Lean Elab Command

/-!
# `lexer` syntax

Defines the `lexer` command: its syntax and its elaboration.
-/

/--
Declares a lexer: an inductive type with a constructor for each rule, and a function
`lex` in its namespace, which splits a string into an array of tokens.

Each rule pairs a constructor with the regex it matches, written as a raw string
literal. `lex` always takes the longest match; when several rules match the same
text, the one declared earlier wins. If no rule matches, `lex` returns an error with
the byte offset of the failure.

An optional `deriving` clause after the rules derives these instances for the
token type.
-/
syntax "lexer" ident "where" ("|" ident str)* ("deriving" ident,+)? : command

elab_rules : command
  | `(
      lexer $typeName:ident where
        $[| $tokName:ident $pattern:str]*
      $[deriving $[$derivings:ident],*]?
    ) => do
    let dfa ← ofExcept <| rulesToDFA (pattern.toList.map TSyntax.getString)
    for cmd in ← buildLexer typeName tokName dfa derivings do
      elabCommand cmd
