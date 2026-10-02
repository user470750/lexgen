/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public import Lexgen.Internal.DFA.Automaton

import Lean
import Lexgen.Internal.Pack
import Lexgen.Lexer

public section

open Lean

/-!
# Lexer code generation

Defines the translation of a `DFA` into the code of the generated lexer.
-/

namespace Lexgen.Internal

/--
A rule of a `lexer` declaration.
-/
inductive RuleInfo where
  /--
  A token named `name`, without a value.
  -/
  | simple (name : Ident)
  /--
  A token named `name` with a value of type `valueType`, which `func` computes from the
  token's slice.
  -/
  | converted (name : Ident) (valueType : Term) (func : Term)
  /--
  A skip rule: its matches are dropped, and it has no constructor in the generated token type.
  -/
  | skip

/--
Returns the name of the token, or `none` for a skip rule.
-/
def RuleInfo.name : RuleInfo → Option Ident
  | .simple name        => some name
  | .converted name _ _ => some name
  | .skip               => none

variable [Monad m] [MonadQuotation m]

/--
Returns the name of the generated function for state `state`.
-/
private def stateName (state : Nat) : m Ident := do
  -- The name is hygienic: it cannot clash with a name of the user, such as a token called
  -- `state0`, or with the states of another lexer, and it is not visible outside the
  -- generated code.
  return mkIdent (← MonadQuotation.addMacroScope (Name.mkSimple s!"state{state}"))

/--
Builds the chain of `if`s on the character of the slice `input` at `pos`, for a state with the
transitions `trans`. `best` is the longest match so far, packed as the state functions return
it: it is passed on to the next state, and returned when there is nowhere to go.
-/
private def buildTrans (input pos : Ident) (best : Term)
    (trans : Array (DFA.Interval × Nat)) : m Term := do
  let c ← `(ident| c)
  -- The proof that `pos` is not the end of `input`, needed to read and skip its character.
  let h ← `(ident| h)
  -- No interval matched: the longest match is the one so far.
  let body ← trans.foldrM (init := best) fun (interval, next) rest => do
    -- The trap never leads to a match, so going there is the same as no transition.
    if next == DFA.trap then
      return rest
    -- A character of `interval` leads to a call of the function of state `next` on the
    -- position after the character.
    let target ← `($(← stateName next) $input ($(pos).next $h) $best)
    match interval with
    | .single character =>
      `(if $c:ident == $(quote character) then $target else $rest)
    | .range lower upper =>
      `(if $(quote lower) ≤ $c:ident && $c:ident ≤ $(quote upper) then $target else $rest)
  -- At the end of the input, the longest match is also the one so far.
  `(if $h:ident : $pos ≠ $(input).endPos then
      let $c:ident := $(pos).get $h
      $body
    else
      $best)

/--
Builds the function of state `state`. The generated function takes the input slice `input` and
the current position `pos` in it, together with the packed longest match so far `best`, and
returns the packed longest match.
-/
private def buildStateFunc (dfa : DFA) (state : Nat)
    (trans : Array (DFA.Interval × Nat)) : m Command := do
  let funcName ← stateName state
  let input ← `(ident| input)
  let pos   ← `(ident| pos)
  let best  ← `(ident| best)
  let body  ← if let some rule := dfa.accepting[state]? then
    -- An accepting state is the longest match so far: going further can only replace it.
    let here  ← `(ident| here)
    let trans ← buildTrans input pos here trans
    `(let $here:ident := Lexgen.Internal.Packed.ofMatch $(quote rule) $pos
      $trans)
  else
    buildTrans input pos best trans
  -- `partial` is needed for now: Lean cannot see that the position grows with every call.
  `(partial def $funcName ($input : String.Slice) ($pos : String.Slice.Pos $input)
      ($best : Lexgen.Internal.Packed $input) : Lexgen.Internal.Packed $input :=
    $body)

/--
Builds the functions of all states of `dfa` but the trap as one `mutual` block, since
they call each other.
-/
private def buildStateFuncs (dfa : DFA) : m Command := do
  -- The trap needs no function: no branch calls it. It comes before the start state,
  -- and every other state comes after it.
  let stateFuncs ← (dfa.trans.extract DFA.start).mapIdxM
    fun i => (buildStateFunc dfa (DFA.start + i))
  `(mutual $stateFuncs* end)

/--
Builds the inductive type named `typeName`, with a constructor for each of `rules` but the
skip rules, carrying a value if the token has one, deriving the instances in `derivings`, if
any.
-/
private def buildTokenType (typeName : Ident) (rules : Array RuleInfo)
    (derivings : Option (Array Ident)) : m Command := do
  let ctors ← rules.filterMapM fun
    | .simple name                => some <$> `(Lean.Parser.Command.ctor| | $name:ident)
    | .converted name valueType _ =>
      some <$> `(Lean.Parser.Command.ctor| | $name:ident (value : $valueType))
    | .skip => pure none
  `(
    inductive $typeName where
      $ctors*
    $[deriving $[$derivings:ident],*]?
  )

/--
Builds the function `lexer` in the namespace of the type named `typeName`, which creates a
`Lexer` for a string.
-/
private def buildLexerFunc (typeName : Ident) : m Command := do
  -- `lexer` is called by the user, so its name is not hygienic.
  let lexerName := mkIdent (typeName.getId.str "lexer")
  let source ← `(ident| source)
  `(
    def $lexerName ($source : String) : Lexgen.Lexer $typeName :=
      Lexgen.Lexer.new $source
  )

/--
Builds the `Lexable` instance of the type named `typeName`.
-/
private def buildLexableImpl (typeName : Ident) (rules : Array RuleInfo) : m Command := do
  let startState ← stateName DFA.start
  -- The rule number is only known when the lexer runs, so the generated code acts on it
  -- with a `match`: `ruleNums` are the rule numbers as literals, and `branches` are what
  -- is done for them.
  let ruleNums : Array Term := rules.mapIdx fun i _ => quote i
  let input  ← `(ident| input)
  let start  ← `(ident| start)
  let rule   ← `(ident| rule)
  let stopAt ← `(ident| stopAt)
  -- A match never stops before it starts, so `slice!` never panics.
  let slice  ← `($(input).slice! $start $stopAt)
  let branches ← rules.mapM fun
    | .simple name =>
      `(Lexgen.Step.token .$name $stopAt)
    | .converted name valueType func => do
      -- The ascription takes the position of the function, so that a type error points at it.
      let typedFunc ← withRef func `(($func : String.Slice → $valueType))
      -- Only a converted token needs its slice here.
      `(Lexgen.Step.token (.$name ($typedFunc $slice)) $stopAt)
    | .skip => `(Lexgen.Step.skip $stopAt)
  `(
    instance : Lexgen.Lexable $typeName where
      next := Lexgen.Internal.nextWith $startState fun $input $start $stopAt $rule =>
        match $rule:ident with
        $[| $ruleNums => $branches]*
        -- No match: `Packed.rule` returns a number that no rule has. No rule matches the empty
        -- string, so this is also the case at the end of the input, which is checked only here.
        | _ =>
          if $start = $(input).endPos then
            Lexgen.Step.done
          else
            Lexgen.Step.error ($(input).startInclusive.offset.byteIdx + $(start).offset.byteIdx)
  )

/--
Generates the code of a lexer for `dfa`, as commands to elaborate in order:

* an inductive type named `typeName`, with a constructor for each of `rules` but the skip
  rules, deriving the instances in `derivings`, if any;
* a `mutual` block with a function per `DFA` state but the trap, hidden from the user;
* the `Lexable` instance of that type;
* a function `lexer` in the namespace of that type, which creates a `Lexer` for a string.

Tokens are matched to rules by position, so `rules` must be in the same order as the
rules `dfa` was built from.
-/
def buildLexer (typeName : Ident) (rules : Array RuleInfo) (dfa : DFA)
    (derivings : Option (Array Ident)) : m (Array Command) := do
  return #[
    ← buildTokenType typeName rules derivings,
    ← buildStateFuncs dfa,
    ← buildLexableImpl typeName rules,
    ← buildLexerFunc typeName
  ]

end Lexgen.Internal
