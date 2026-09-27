/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public section

/-!
# Lexer

Defines `Lexer`, which encapsulates lexing a string and provides functions to get the result,
and `Lexable`, implemented for every token type declared with `lexer`.
-/

namespace Lexgen

/--
A token together with its slice of the source.
-/
structure Spanned (α : Type) where
  /--
  The token.
  -/
  token : α
  /--
  The token's slice of the source, which also gives its position.
  -/
  slice : String.Slice

-- `String.Slice` has no `Repr`, so the slice is shown by its text.
instance [Repr α] : Repr (Spanned α) where
  reprPrec s _ := "{ token := " ++ repr s.token ++ ", slice := " ++ repr s.slice.toString ++ " }"

/--
A type of tokens that can be lexed. The `lexer` command implements it for the type it declares.
-/
class Lexable (α : Type) where
  /--
  Returns the first token of the input that is not matched by a skip rule, with its slice and
  the rest of the input; `none` if there is no such token (the input is empty, or only skip
  rules matched); or an error if no rule matches.
  -/
  next : String.Slice → Except String (Option (Spanned α × String.Slice))

/--
Encapsulates lexing a string into tokens of type `α`.
-/
structure Lexer (α : Type) [Lexable α] where
  private mk ::
  /--
  The input not lexed yet.
  -/
  private rest : String.Slice

/--
Creates a lexer for `source`.
-/
def Lexer.new [Lexable α] (source : String) : Lexer α :=
  ⟨source.toSlice⟩

/--
Returns the results of applying `f` to every token of the whole input with its slice, if there
are no errors.
-/
def Lexer.collectMap [Lexable α] (lexer : Lexer α) (f : Spanned α → β) :
    Except String (Array β) := do
  let mut s := lexer.rest
  let mut acc := #[]
  while !s.isEmpty do
    let some (item, rest) ← Lexable.next s
      | return acc
    s   := rest
    acc := acc.push (f item)
  return acc

/--
Returns the tokens of the whole input, if there are no errors.
-/
def Lexer.tokens [Lexable α] (lexer : Lexer α) : Except String (Array α) :=
  lexer.collectMap (·.token)

/--
Returns the tokens of the whole input with their slices, if there are no errors.
-/
def Lexer.spanned [Lexable α] (lexer : Lexer α) : Except String (Array (Spanned α)) :=
  lexer.collectMap id

end Lexgen
