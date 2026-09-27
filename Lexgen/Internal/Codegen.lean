/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public import Lexgen.Internal.DFA.Automaton

import Lean
import Lexgen.Lexer

public section

open Lean

/-!
# Lexer code generation

Defines the translation of a `DFA` into the code of the generated lexer.
-/

/--
A token of a `lexer` declaration.
-/
inductive TokenInfo where
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
Returns the name of the token.
-/
def TokenInfo.name : TokenInfo → Ident
  | .simple name        => name
  | .converted name _ _ => name

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
Builds a branch of the generated `match`.
-/
private def buildBranch (inputArg : Ident) (pat : Term) (next : Nat) :
    m (TSyntax ``Lean.Parser.Term.matchAlt) := do
  -- The trap never leads to a match, so going there fails at once.
  if next == DFA.trap then
    return ← `(Lean.Parser.Term.matchAltExpr| | $pat => none)
  let funcName ← stateName next
  -- A character matching `pat` leads to a call of the function of state `next` on the
  -- input without its first character.
  `(Lean.Parser.Term.matchAltExpr| | $pat => $funcName ($(inputArg).drop 1))

/--
Builds the `match` on the next character of the input named `inputArg`, for a
state with the transitions `trans`.
-/
private def buildTrans (inputArg : Ident) (trans : List (DFA.Symbol × Nat)) : m Term := do
  -- Without a `dot` transition, the catch-all branch returns `none`: no character of the
  -- alphabet matched, and there is nowhere to go.
  let mut dot : TSyntax ``Lean.Parser.Term.matchAlt ← `(Lean.Parser.Term.matchAltExpr| | _ => none)
  let mut chars := #[]
  for (sym, next) in trans do
    match sym with
    | .char c => chars := chars.push (← buildBranch inputArg (quote c) next)
    | .dot    => dot ← buildBranch inputArg (← `(_)) next
  let branches := chars.push dot
  let c ← `(ident| c)
  -- `bind` returns `none` if the input is empty.
  `($(inputArg).front?.bind fun $c => match $c:ident with $branches:matchAlt*)

/--
Builds the function of state `state`. The generated function takes the rest of
the input and returns the rule it matched, together with the input left after the match,
or `none` if no rule matches.
-/
private def buildStateFunc (dfa : DFA) (state : Nat)
    (trans : List (DFA.Symbol × Nat)) : m Command := do
  let funcName   ← stateName state
  -- One name for the input, passed to every builder: otherwise nothing guarantees that
  -- the other functions refer to the argument by the same name.
  let inputArg   ← `(ident| str)
  let transMatch ← buildTrans inputArg trans
  let body       ← if let some rule := dfa.accepting[state]? then
    -- An accepting state succeeds with its own rule even when going further fails, so
    -- that the longest match wins.
    `(($transMatch) <|> some ($(quote rule), $inputArg))
  else
    pure transMatch
  -- `partial` is needed for now: Lean cannot see that the input gets shorter with every
  -- call.
  `(partial def $funcName ($inputArg : String.Slice) : Option (Nat × String.Slice) := $body)

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
Builds the inductive type named `typeName`, with a constructor for each of `tokens`,
carrying a value if the token has one, deriving the instances in `derivings`, if any.
-/
private def buildTokenType (typeName : Ident) (tokens : Array TokenInfo)
    (derivings : Option (Array Ident)) : m Command := do
  let ctors ← tokens.mapM fun
    | .simple name                => `(Lean.Parser.Command.ctor| | $name:ident)
    | .converted name valueType _ =>
      `(Lean.Parser.Command.ctor| | $name:ident (value : $valueType))
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
private def buildLexableImpl (typeName : Ident) (tokens : Array TokenInfo) : m Command := do
  let startState ← stateName DFA.start
  -- The rule number is only known when the lexer runs, so the generated code turns it
  -- into a constructor with a `match`: `ruleNums` are the rule numbers as literals, and
  -- `ctors` are the tokens they turn into.
  let ruleNums : Array Term := tokens.mapIdx fun i _ => quote i
  let input ← `(ident| input)
  let rule  ← `(ident| rule)
  let rest  ← `(ident| rest)
  let slice ← `(ident| slice)
  let token ← `(ident| token)
  let ctors : Array Term ← tokens.mapM fun tok => do
    let ctor := mkIdent (typeName.getId ++ tok.name.getId)
    match tok with
    | .simple _ => return ctor
    | .converted _ valueType func =>
      -- The ascription takes the position of the function, so that a type error points at it.
      let typedFunc ← withRef func `(($func : String.Slice → $valueType))
      `($ctor ($typedFunc $slice))
  `(
    instance : Lexgen.Lexable $typeName where
      next $input:ident := do
        let some ($rule, $rest) := $startState $input
          | throw s!"offset {$(input).startInclusive.offset.byteIdx}: no rule matches the input"
        -- The token is the part of `input` before `rest`, which is a suffix of `input`.
        let $slice:ident := $(input).sliceTo ($(input).pos! ($(input).rawEndPos - $rest))
        let some $token := (match $rule:ident with $[| $ruleNums => some $ctors]* | _ => none)
          | throw "unknown rule"
        return (⟨$token, $slice⟩, $rest)
  )

/--
Generates the code of a lexer for `dfa`, as commands to elaborate in order:

* an inductive type named `typeName`, with a constructor for each of `tokens`,
  deriving the instances in `derivings`, if any;
* a `mutual` block with a function per `DFA` state but the trap, hidden from the user;
* the `Lexable` instance of that type;
* a function `lexer` in the namespace of that type, which creates a `Lexer` for a string.

Tokens are matched to rules by position, so `tokens` must be in the same order as the
rules `dfa` was built from.
-/
def buildLexer (typeName : Ident) (tokens : Array TokenInfo) (dfa : DFA)
    (derivings : Option (Array Ident)) : m (Array Command) := do
  return #[
    ← buildTokenType typeName tokens derivings,
    ← buildStateFuncs dfa,
    ← buildLexableImpl typeName tokens,
    ← buildLexerFunc typeName
  ]
