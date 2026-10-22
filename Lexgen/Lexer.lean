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
  private source : String
  /--
  The position in `source` where the token starts.
  -/
  private start : source.Pos
  /--
  The position in `source` right after the token.
  -/
  private stop : source.Pos
  /--
  Proves that the token is not empty.
  -/
  private h : start < stop

/--
Returns the token's slice of the source.
-/
def Spanned.slice (s : Spanned α) : String.Slice :=
  s.source.slice s.start s.stop (String.Pos.le_of_lt s.h)

/--
Returns the byte offset in the source string where the token starts.
-/
def Spanned.startOffset (s : Spanned α) : Nat :=
  s.start.offset.byteIdx

/--
Returns the byte offset in the source string right after the token.
-/
def Spanned.stopOffset (s : Spanned α) : Nat :=
  s.stop.offset.byteIdx

instance [Repr α] : Repr (Spanned α) where
  reprPrec s _ := "{ token := " ++ repr s.token ++ ", slice := " ++ repr s.slice.toString ++ " }"

/--
A lexing error.
-/
structure LexError where
  /--
  The byte offset in the source string where no rule matches.
  -/
  offset : Nat
deriving Repr, BEq

instance : ToString LexError where
  toString e := s!"offset {e.offset}: no rule matches the input"

/--
The result of matching the input `input` from the position `start`.
-/
inductive Step (α : Type) (input : String) (start : input.Pos) where
  /--
  The token `token`, which stops right before `stop`: `h` proves that the match moves the position
  forward.
  -/
  | token (token : α) (stop : input.Pos) (h : start < stop)
  /--
  A match of a skip rule, which stops right before `stop`: `h` proves that the match moves the
  position forward.
  -/
  | skip (stop : input.Pos) (h : start < stop)
  /--
  The end of the input: `h` proves that `start` is the last position of `input`.
  -/
  | done (h : start = input.endPos)
  /--
  A failure: no rule matches `input` at the position `start`.
  -/
  | error

/--
The size in bytes from which an input cannot be lexed.
-/
-- The value follows from the bit layout of `Lexgen.Internal.Packed`, which keeps a position in 48
-- bits. A mismatch does not go unnoticed: the generated code passes a proof of `< maxInputSize`
-- where `Lexgen.Internal.Packed.ofMatch` expects one of its own bound, so it would fail to
-- elaborate.
abbrev maxInputSize : Nat := 2 ^ 48

/--
A type of tokens that can be lexed. The `lexer` command implements it for the type it declares.
-/
class Lexable (α : Type) where
  /--
  Returns the result of the longest match in the input from the position `start`. The input size
  must be less than `maxInputSize`.
  -/
  next : (input : String) → (start : input.Pos) → (input.utf8ByteSize < maxInputSize) →
    Step α input start

/--
A lexer for a string, with the token type `α`.
-/
structure Lexer (α : Type) [Lexable α] where
  private mk ::
  /--
  The input to lex.
  -/
  private source : String
  /--
  Proves that `source` is short enough to lex.
  -/
  private inBounds : source.utf8ByteSize < maxInputSize

private instance [Lexable α] : Inhabited (Lexer α) where
  default := ⟨"", by decide⟩

/--
Creates a lexer for `source`.
-/
def Lexer.new [Lexable α] (source : String) : Lexer α :=
  if inBounds : source.utf8ByteSize < maxInputSize then
    ⟨source, inBounds⟩
  else
    -- Not reached in practice: the input would have to take 256 TiB.
    panic! "input out of bounds"

/--
Returns the tokens of the whole input, if there are no errors.
-/
def Lexer.tokens [Lexable α] (lexer : Lexer α) : Except LexError (Array α) :=
  collect lexer.source.startPos #[]
where
  /--
  Returns `acc` followed by the tokens of the input from `start`.
  -/
  collect (start : lexer.source.Pos) (acc : Array α) :
      Except LexError (Array α) :=
    match Lexable.next lexer.source start lexer.inBounds with
    | .token token stop _ => collect stop (acc.push token)
    | .skip stop _        => collect stop acc
    | .done _             => .ok acc
    | .error =>
      .error ⟨start.offset.byteIdx⟩
  termination_by start

/--
Returns the tokens of the whole input with their positions, if there are no errors.
-/
def Lexer.spanned [Lexable α] (lexer : Lexer α) :
    Except LexError (Array (Spanned α)) :=
  collect lexer.source.startPos #[]
where
  /--
  Returns `acc` followed by the tokens of the input from `start` with their positions.
  -/
  collect (start : lexer.source.Pos) (acc : Array (Spanned α)) :
      Except LexError (Array (Spanned α)) :=
    match Lexable.next lexer.source start lexer.inBounds with
    | .token token stop h => collect stop (acc.push ⟨token, lexer.source, start, stop, h⟩)
    | .skip stop _        => collect stop acc
    | .done _             => .ok acc
    | .error =>
      .error ⟨start.offset.byteIdx⟩
  termination_by start

end Lexgen
