/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

-- Public, although no public signature needs it: `buildLexer` takes the instances of its monad, so
-- the compiler specializes it to the monad of each module that calls it, and rejects it if the
-- `asciiTests` that it calls is not imported publicly.
public import Lexgen.Internal.Codegen.Ascii
public import Lexgen.Internal.DFA.Automaton

import Lean
-- The quotations below name declarations of these modules: without the imports, the names stay
-- hygienic, and the generated code reports them as unknown identifiers.
import Lexgen.Internal.Instances
import Lexgen.Internal.Pack
import Lexgen.Internal.Positions
import Lexgen.Lexer

import Std.Data.HashMap

public section

open Lean

/-!
# Lexer code generation

Defines `buildLexer`: translating a `DFA` into the code of the generated lexer.
-/

namespace Lexgen.Internal

/--
The token type of a `lexer` declaration.
-/
structure TypeInfo where
  /--
  The name of the type.
  -/
  name : Ident
  /--
  The docstring of the type, if any.
  -/
  doc? : Option (TSyntax ``Parser.Command.docComment)
  /--
  The instances to derive for the type, if any.
  -/
  derivings? : Option (Array Ident)

/--
A rule of a `lexer` declaration.
-/
inductive RuleInfo where
  /--
  A token named `name`, without a value, with the docstring `doc?`, if any.
  -/
  | simple (name : Ident) (doc? : Option (TSyntax ``Parser.Command.docComment))
  /--
  A token named `name` with a value of type `valueType`, which `func` computes from the token's
  slice, with the docstring `doc?`, if any.
  -/
  | converted (name : Ident) (valueType : Term) (func : Term)
      (doc? : Option (TSyntax ``Parser.Command.docComment))
  /--
  A skip rule.
  -/
  | skip

/--
Returns the name of the token, or `none` for a skip rule.
-/
def RuleInfo.name : RuleInfo → Option Ident
  | .simple name _     => some name
  | .converted name .. => some name
  | .skip              => none

variable [Monad m] [MonadQuotation m]

/--
Returns the name of the generated function for state `state`.
-/
private def stateName (state : Nat) : m Ident := do
  -- The name is hygienic: it cannot clash with a name of the user, such as a token called `state0`,
  -- or with the states of another lexer, and it is not visible outside the generated code.
  return mkIdent (← MonadQuotation.addMacroScope (Name.mkSimple s!"state{state}"))

/--
Returns the name of the generated lookup table numbered `table`.
-/
private def tableName (table : Nat) : m Ident := do
  -- Hygienic, as the names of the states.
  return mkIdent (← MonadQuotation.addMacroScope (Name.mkSimple s!"table{table}"))

/--
Returns the name of the generated theorem that the size of the lookup table numbered `table` is
`0x80`.
-/
private def tableSizeName (table : Nat) : m Ident := do
  -- Hygienic, as the names of the states.
  return mkIdent (← MonadQuotation.addMacroScope (Name.mkSimple s!"table{table}_size"))

/--
Builds the lookup tables `tables`: for every table, its definition and the theorem that its size is
`0x80`, which `inTable` takes.
-/
private def buildTables (tables : Array ByteArray) : m (Array Command) :=
  tables.zipIdx.flatMapM fun (table, i) => do
    let entries : Array Term := table.data.map (quote ·.toNat)
    return #[
      ← `(def $(← tableName i) : ByteArray := ⟨#[$entries,*]⟩),
      -- `rfl` as a term exceeds the maximum recursion depth on a table of 128 entries.
      ← `(theorem $(← tableSizeName i) : ($(← tableName i)).size = 0x80 := by rfl)
    ]

/--
Builds the function of state `state`. The generated function takes `input`, the current position in
it, the packed longest match so far `best` (an older one for an accepting state, which ignores it)
and the proof `hi` that `input` is short enough to lex, and returns the packed longest match.

At a character boundary, the current position is `pos : input.Pos`, and the function tests an ASCII
byte with `tests`. Inside a character, the current position is `pos : String.Pos.Raw`, followed by
`hk : (⟨pos.byteIdx + count⟩ : String.Pos.Raw).IsValid input`, where `count` is the pending count of
`state`.
-/
private def buildStateFunc (dfa : DFA) (state : Nat) (trans : Array (DFA.Interval × Nat))
    (tests : Array (Nat × AsciiTest)) (recursive : Bool) (pendings : Std.HashMap Nat Nat) :
    m Command := do
  let funcName ← stateName state

  let count := pendings[state]!

  -- The longest match so far, passed on to a non-accepting state and returned when there is nowhere
  -- to go.
  let found : Term ← if let some rule := dfa.accepting[state]? then
    -- The match of `rule` up to `pos` is longer than `best`, so it replaces `best`.
    `(Lexgen.Internal.Packed.ofMatch $(quote rule) pos hi)
  else
    `(best)

  -- No test passed: the longest match is the one so far.
  let ascii ← tests.foldrM (init := found) fun (next, test) rest => do
    let cond ← match test with
    | .inRange interval =>
      `(Lexgen.Internal.inRange c
        $(quote interval.lower.toNat)
        $(quote interval.upper.toNat))
    | .inTable table mask =>
      `(Lexgen.Internal.inTable c hc
        $(← tableName table)
        $(← tableSizeName table)
        $(quote mask.toNat))
    let target ← call count next found
    `(if $cond then $target else $rest)

  -- No interval matched: the longest match is the one so far.
  let nonAscii ← (splitAscii trans).2.foldrM
    (init := found)
    fun (interval, next) rest => do
      -- The trap never leads to a match, so going there is the same as no transition.
      if next == DFA.trap then
        return rest
      let target ← call count next found
      let lower := quote interval.lower.toNat
      let upper := quote interval.upper.toNat
      -- `stateArgs` refers to `hc` from a character boundary: the class of the first byte gives
      -- the length of the character.
      `(if hc : Lexgen.Internal.inRange c $lower $upper then $target else $rest)

  -- `hp` bounds `pos` for `decreasing_by`.
  let body ← if count == 0 then
    `(
      if h : pos ≠ input.endPos then
        have hp : pos.offset < input.rawEndPos := String.Pos.offset_lt_rawEndPos_iff.mpr h
        let c := pos.byte h
        if hc : c < 0x80 then
          -- An ASCII byte is a whole character: one proof for all ASCII transitions.
          let next := input.pos ⟨pos.offset.byteIdx + 1⟩ (Lexgen.Internal.isValid_next_ascii hc)
          $ascii
        else
          $nonAscii
      else
        -- At the end of the input, the longest match is also the one so far.
        $found
    )
  else
    -- The input cannot end inside a character, so the byte at `pos` is read without checking for
    -- the end of the input. A byte inside a character is at least `0x80`, so only `nonAscii` can
    -- match it.
    `(
      have hp : pos < input.rawEndPos :=
        Nat.lt_of_lt_of_le (Nat.lt_add_of_pos_right (by decide)) hk.le_rawEndPos
      let c := input.getUTF8Byte pos hp
      $nonAscii
    )

  let byteIdx : Term ← if count == 0 then `(pos.offset.byteIdx) else `(pos.byteIdx)
  let termination? := if recursive then some byteIdx else none
  let decreasing? ← if recursive then
    some <$> `(tacticSeq| all_goals exact Nat.sub_lt_sub_left hp (Nat.lt_add_one _))
  else
    pure none
  `(
    def $funcName (input : String) $(← posBinders count)*
        (best : Lexgen.Internal.Packed input)
        (hi : input.utf8ByteSize < Lexgen.maxInputSize) :
        Lexgen.Internal.Packed input :=
      $body
    -- The measure is the number of bytes after `pos`.
    $[termination_by input.utf8ByteSize - $termination?]?
    $[decreasing_by $decreasing?]?
  )
where
  /--
  Returns the call of the function of state `next` on the position after the byte.
  -/
  call (count next : Nat) (found : Term) : m Term := do
    let nextArgs : Array Term ← stateArgs count pendings[next]!
    if dfa.accepting.contains next then
      -- An accepting state replaces the match it gets with its own, so it gets `best`: building
      -- `found` for it is unnecessary.
      `($(← stateName next) input $nextArgs* best hi)
    else
      `($(← stateName next) input $nextArgs* $found hi)

  /--
  Returns the position arguments that a state passes to the function of the next state, depending
  on the pending counts of the two states.
  -/
  stateArgs (count nextCount : Nat) : m (Array Term) := do
    match count, nextCount with
    -- From a character boundary to a character boundary.
    | 0, 0 => return #[← `(next)]
    -- From a character boundary to inside a character.
    | 0, _ => return #[
      ← `(⟨pos.offset.byteIdx + 1⟩),
      -- The end of the character that the byte `c` starts is valid.
      ← `(Lexgen.Internal.isValid_next (Lexgen.Internal.utf8ByteSize_eq $(quote nextCount) hc))
    ]
    -- From inside a character to a character boundary.
    | 1, _ => return #[← `(input.pos ⟨pos.byteIdx + 1⟩ hk)]
    -- From inside a character to inside the same character.
    | _, _ => return #[← `(⟨pos.byteIdx + 1⟩), ← `(hk)]

  /--
  Returns the position parameters of the function of a state, depending on its pending count.
  -/
  posBinders : Nat → m (Array (TSyntax ``Parser.Term.bracketedBinderF))
    | 0     => return #[← `(Parser.Term.bracketedBinderF| (pos : String.Pos input))]
    | count => return #[
        ← `(Parser.Term.bracketedBinderF| (pos : String.Pos.Raw)),
        ← `(Parser.Term.bracketedBinderF|
          (hk : (⟨pos.byteIdx + $(quote count)⟩ : String.Pos.Raw).IsValid input))]

/--
Builds the functions of all states of `dfa` but the trap as one `mutual` block, since they call each
other.
-/
private def buildStateFuncs (dfa : DFA) (tests : Array (Array (Nat × AsciiTest))) :
    m Command := do
  let recursiveStates := dfa.recursiveStates
  let pendings := dfa.pending
  -- The trap needs no function: no branch calls it. It comes before the start state, and every
  -- other state comes after it.
  let stateFuncs ← (dfa.trans.extract DFA.start).mapIdxM
    fun i trans =>
      let state := DFA.start + i
      buildStateFunc dfa state trans tests[state]! (recursiveStates.contains state) pendings
  `(mutual $stateFuncs* end)

/--
Builds the inductive type named `typeInfo.name`, with a constructor for each of `rules` but the skip
rules, carrying a value if the token has one. The type and its constructors get the docstrings of
the declaration and of their rules, if any. The type derives the instances in `typeInfo.derivings?`,
if any.
-/
private def buildTokenType (typeInfo : TypeInfo) (rules : Array RuleInfo) : m Command := do
  let ctors ← rules.filterMapM fun
    | .simple name doc? =>
      some <$> `(Lean.Parser.Command.ctor| $[$doc?:docComment]? | $name:ident)
    | .converted name valueType _ doc? =>
      some <$> `(Lean.Parser.Command.ctor| $[$doc?:docComment]? | $name:ident (value : $valueType))
    | .skip => pure none
  `(
    $[$(typeInfo.doc?):docComment]?
    inductive $(typeInfo.name) where
      $ctors*
    $[deriving $[$(typeInfo.derivings?):ident],*]?
  )

/--
Builds the function `lexer` in the namespace of the type named `typeName`, which creates a `Lexer`
for a string.
-/
private def buildLexerFunc (typeName : Ident) : m Command := do
  -- `lexer` is called by the user, so its name is not hygienic.
  let lexerName := mkIdent (typeName.getId.str "lexer")
  `(
    def $lexerName (source : String) : Lexgen.Lexer $typeName :=
      Lexgen.Lexer.new source
  )

/--
Builds the `Lexable` instance of the type named `typeName`.
-/
private def buildLexableImpl (typeName : Ident) (rules : Array RuleInfo) : m Command := do
  let startState ← stateName DFA.start
  let slice      ← `(input.slice start stopAt (String.Pos.le_of_lt h))

  -- The state functions return the rule of the longest match only as a number, packed, so the
  -- generated code acts on it with a `match`: `ruleNums` are the rule numbers as literals, and
  -- `branches` are what is done for them.
  let ruleNums : Array Term := rules.mapIdx fun i _ => quote i
  let mut branches ← rules.mapM fun
    | .simple name _ =>
      `(Lexgen.Step.token .$name stopAt h)
    | .converted name valueType func _ => do
      -- The ascription takes the position of the function, so that a type error points at it.
      let typedFunc ← withRef func `(($func : String.Slice → $valueType))
      -- Only a converted token needs its slice here.
      `(Lexgen.Step.token (.$name ($typedFunc $slice)) stopAt h)
    | .skip => `(Lexgen.Step.skip stopAt h)
  branches ← branches.mapM fun step =>
    `(
      let stopAt := packed.stop (hr ▸ by decide)
      if h : start < stopAt then $step else Lexgen.Step.error
    )

  `(
    instance : Lexgen.Lexable $typeName where
      next input start hi :=
        let packed := $startState input start .noMatch hi
        match hr : packed.rule with
        $[| $ruleNums => $branches]*
        -- No match: `Packed.rule` returns a number that no rule has. No rule matches the empty
        -- string, so this is also the case at the end of the input, which is checked only here.
        | _ =>
          if h : start = input.endPos then
            Lexgen.Step.done h
          else
            Lexgen.Step.error
  )

/--
Generates the code of a lexer for `dfa`, as commands to elaborate in order:

* the lookup tables, with the theorems of their sizes, hidden from the user;
* an inductive type named `typeInfo.name`, with a constructor for each of `rules` but the skip
  rules, with their docstrings, if any, deriving the instances in `typeInfo.derivings?`, if any;
* a `mutual` block with a function per `DFA` state but the trap, hidden from the user;
* the `Lexable` instance of that type;
* a function `lexer` in the namespace of that type, which creates a `Lexer` for a string.

Tokens are matched to rules by position, so `rules` must be in the same order as the rules `dfa` was
built from.
-/
def buildLexer (typeInfo : TypeInfo) (rules : Array RuleInfo) (dfa : DFA) : m (Array Command) := do
  let ascii := asciiTests dfa.trans
  return (← buildTables ascii.tables) ++ #[
    ← buildTokenType typeInfo rules,
    ← buildStateFuncs dfa ascii.tests,
    ← buildLexableImpl typeInfo.name rules,
    ← buildLexerFunc typeInfo.name
  ]

end Lexgen.Internal
