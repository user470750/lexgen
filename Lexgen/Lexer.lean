/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public section

/-!
# Lexer

Defines `Lexer`, which encapsulates lexing a string and provides functions to get the result, and
`Lexable`, implemented for every token type declared with `lexer`.
-/

namespace Lexgen

/--
A token together with its position in the source.
-/
structure Spanned (α : Type) where
  private mk ::
  /--
  The token.
  -/
  token : α
  /--
  The whole input the token comes from.
  -/
  private source : String.Slice
  /--
  The position in `source` where the token starts.
  -/
  private start : source.Pos
  /--
  The position in `source` right after the token.
  -/
  private stop : source.Pos

/--
Returns the token's slice of the source.
-/
def Spanned.slice (s : Spanned α) : String.Slice :=
  -- Only the lexer builds a `Spanned`, and a match never stops before it starts, so `slice!` never
  -- panics.
  s.source.slice! s.start s.stop

/--
Returns the byte offset in the source string where the token starts.
-/
def Spanned.startOffset (s : Spanned α) : Nat :=
  s.source.startInclusive.offset.byteIdx + s.start.offset.byteIdx

/--
Returns the byte offset in the source string right after the token.
-/
def Spanned.stopOffset (s : Spanned α) : Nat :=
  s.source.startInclusive.offset.byteIdx + s.stop.offset.byteIdx

-- `String.Slice` has no `Repr`, so the slice is shown by its text.
instance [Repr α] : Repr (Spanned α) where
  reprPrec s _ := "{ token := " ++ repr s.token ++ ", slice := " ++ repr s.slice.toString ++ " }"

/--
The result of matching the input `input` from a position.
-/
inductive Step (α : Type) (input : String.Slice) where
  /--
  The token `token`, which stops right before `stop`.
  -/
  | token (token : α) (stop : input.Pos)
  /--
  A skip rule matched, and the match stops right before `stop`.
  -/
  | skip (stop : input.Pos)
  /--
  The position is the end of the input.
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
  Returns the result of the longest match in the input from the position `start`.
  -/
  next : (input : String.Slice) → (start : input.Pos) → Step α input

/--
Encapsulates lexing a string into tokens of type `α`.
-/
structure Lexer (α : Type) [Lexable α] where
  private mk ::
  /--
  The input to lex.
  -/
  private source : String.Slice

/--
Creates a lexer for `source`.
-/
def Lexer.new [Lexable α] (source : String) : Lexer α :=
  ⟨source.toSlice⟩

/--
Returns the tokens of the whole input, if there are no errors.
-/
partial def Lexer.tokens [Lexable α] (lexer : Lexer α) : Except String (Array α) :=
  collect lexer.source lexer.source.startPos #[]
where
  /--
  Returns `acc` followed by the tokens of `input` from `start`.
  -/
  collect (input : String.Slice) (start : input.Pos) (acc : Array α) :
      Except String (Array α) :=
    match Lexable.next input start with
    | .token token stop => collect input stop (acc.push token)
    | .skip stop        => collect input stop acc
    | .done             => .ok acc
    | .error offset     => .error s!"offset {offset}: no rule matches the input"

/--
Returns the tokens of the whole input with their positions, if there are no errors.
-/
partial def Lexer.spanned [Lexable α] (lexer : Lexer α) : Except String (Array (Spanned α)) :=
  collect lexer.source lexer.source.startPos #[]
where
  /--
  Returns `acc` followed by the tokens of `input` from `start` with their positions.
  -/
  collect (input : String.Slice) (start : input.Pos) (acc : Array (Spanned α)) :
      Except String (Array (Spanned α)) :=
    match Lexable.next input start with
    | .token token stop => collect input stop (acc.push ⟨token, input, start, stop⟩)
    | .skip stop        => collect input stop acc
    | .done             => .ok acc
    | .error offset     => .error s!"offset {offset}: no rule matches the input"

end Lexgen
