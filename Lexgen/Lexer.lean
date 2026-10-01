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
The result of matching the start of an input.
-/
inductive Step (α : Type) where
  /--
  The token `token`, followed by the input `rest`, which is a suffix of the input.
  -/
  | token (token : α) (rest : String.Slice)
  /--
  A skip rule matched, followed by the input `rest`.
  -/
  | skip (rest : String.Slice)
  /--
  The input is empty.
  -/
  | done
  /--
  No rule matches the input at the byte offset `offset` of the source.
  -/
  | error (offset : Nat)

/--
A type of tokens that can be lexed. The `lexer` command implements it for the type it declares.
-/
class Lexable (α : Type) where
  /--
  Returns the result of the longest match at the start of the input.
  -/
  next : String.Slice → Step α

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
Returns the tokens of the whole input, if there are no errors.
-/
partial def Lexer.tokens [Lexable α] (lexer : Lexer α) : Except String (Array α) :=
  collect lexer.rest #[]
where
  /--
  Returns `acc` followed by the tokens of `s`.
  -/
  collect (s : String.Slice) (acc : Array α) : Except String (Array α) :=
    match Lexable.next s with
    | .token token rest => collect rest (acc.push token)
    | .skip rest        => collect rest acc
    | .done             => .ok acc
    | .error offset     => .error s!"offset {offset}: no rule matches the input"

/--
Returns the tokens of the whole input with their slices, if there are no errors.
-/
partial def Lexer.spanned [Lexable α] (lexer : Lexer α) : Except String (Array (Spanned α)) :=
  collect lexer.rest #[]
where
  /--
  Returns `acc` followed by the tokens of `s` with their slices.
  -/
  collect (s : String.Slice) (acc : Array (Spanned α)) : Except String (Array (Spanned α)) :=
    match Lexable.next s with
    | .token token rest =>
      -- The token is the part of `s` before `rest`, which is a suffix of `s`.
      collect rest (acc.push ⟨token, s.sliceTo (s.pos! (s.rawEndPos - rest))⟩)
    | .skip rest        => collect rest acc
    | .done             => .ok acc
    | .error offset     => .error s!"offset {offset}: no rule matches the input"

end Lexgen
