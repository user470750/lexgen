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

/--
Checks the names of a `lexer` declaration before any code is generated, so that errors
are reported on the names themselves rather than on the generated code: the type must
not be declared yet, and token names must be distinct and differ from `lex`.
-/
private meta def checkNames (typeName : Ident) (tokNames : Array Ident) : CommandElabM Unit := do
  -- Resolves the type name as `inductive` does and fails if it is already declared.
  discard <| withRef typeName <| mkDeclName (← getCurrNamespace) {} typeName.getId

  let mut seen : Std.HashSet Name := {}
  let mut failed := false

  for tok in tokNames do
    if tok.getId == `lex then
      logErrorAt tok m!"`lex` is taken by the generated function `{typeName}.lex`"
      failed := true
    else if seen.contains tok.getId then
      logErrorAt tok m!"duplicate token name `{tok}`"
      failed := true
    seen := seen.insert tok.getId
  if failed then
    throwAbortCommand

elab_rules : command
  | `(
      lexer $typeName:ident where
        $[| $tokName:ident $pattern:str]*
      $[deriving $[$derivings:ident],*]?
    ) => do
    checkNames typeName tokName
    let dfa ← ofExcept <| rulesToDFA (pattern.toList.map TSyntax.getString)
    for cmd in ← buildLexer typeName tokName dfa derivings do
      elabCommand cmd
