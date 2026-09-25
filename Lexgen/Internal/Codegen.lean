/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public import Lexgen.Internal.DFA.Automaton

import Lean

public section

open Lean

/-!
# Lexer code generation

Defines the translation of a `DFA` into the code of the generated lexer.
-/

variable [Monad m] [MonadQuotation m]

/--
Returns the name of the generated function for state `state` of the lexer whose type is
named `typeName`.
-/
private def stateName (typeName : Ident) (state : Nat) : m Ident := do
  -- The name is hygienic: it cannot clash with a name of the user, such as a token called
  -- `state0`, and it is not visible outside the generated code. It lies in the namespace
  -- of `typeName`.
  return mkIdent (← MonadQuotation.addMacroScope (typeName.getId.str s!"state{state}"))

/--
Builds a branch of the generated `match`.
-/
private def buildBranch (typeName inputArg : Ident) (pat : Term) (next : Nat) :
    m (TSyntax ``Lean.Parser.Term.matchAlt) := do
  -- The trap never leads to a match, so going there fails at once.
  if next == DFA.trap then
    return ← `(Lean.Parser.Term.matchAltExpr| | $pat => none)
  let funcName ← stateName typeName next
  -- A character matching `pat` leads to a call of the function of state `next` on the
  -- input without its first character.
  `(Lean.Parser.Term.matchAltExpr| | $pat => $funcName ($(inputArg).drop 1))

/--
Builds the `match` on the next character of the input named `inputArg`, for a
state with the transitions `trans`.
-/
private def buildTrans (typeName inputArg : Ident) (trans : List (DFA.Symbol × Nat)) : m Term := do
  -- Without a `dot` transition, the catch-all branch returns `none`: no character of the
  -- alphabet matched, and there is nowhere to go.
  let mut dot : TSyntax ``Lean.Parser.Term.matchAlt ← `(Lean.Parser.Term.matchAltExpr| | _ => none)
  let mut chars := #[]
  for (sym, next) in trans do
    match sym with
    | .char c => chars := chars.push (← buildBranch typeName inputArg (quote c) next)
    | .dot    => dot ← buildBranch typeName inputArg (← `(_)) next
  let branches := chars.push dot
  let c ← `(ident| c)
  -- `bind` returns `none` if the input is empty.
  `($(inputArg).front?.bind fun $c => match $c:ident with $branches:matchAlt*)

/--
Builds the function of state `state`. The generated function takes the rest of
the input and returns the rule it matched, together with the input left after the match,
or `none` if no rule matches.
-/
private def buildStateFunc (typeName : Ident) (dfa : DFA) (state : Nat)
    (trans : List (DFA.Symbol × Nat)) : m Command := do
  let funcName   ← stateName typeName state
  -- One name for the input, passed to every builder: otherwise nothing guarantees that
  -- the other functions refer to the argument by the same name.
  let inputArg   ← `(ident| str)
  let transMatch ← buildTrans typeName inputArg trans
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
private def buildStateFuncs (typeName : Ident) (dfa : DFA) : m Command := do
  -- The trap needs no function: no branch calls it. It comes before the start state,
  -- and every other state comes after it.
  let stateFuncs ← (dfa.trans.extract DFA.start).mapIdxM
    fun i => (buildStateFunc typeName dfa (DFA.start + i))
  `(mutual $stateFuncs* end)

/--
Builds the inductive type named `typeName`, with a constructor for each name in
`tokNames`.
-/
private def buildTokenType (typeName : Ident) (tokNames : Array Ident) : m Command :=
  `(inductive $typeName where $[| $tokNames:ident]*)

/--
Builds the function `lex` in the namespace of the type named `typeName`,
which splits a string into tokens.

The generated function returns an error if no rule matches at some point of the input.
-/
private def buildRunner (typeName : Ident) (tokNames : Array Ident) : m Command := do
  let startState ← stateName typeName DFA.start
  -- The rule number is only known when the lexer runs, so the generated code turns it
  -- into a constructor with a `match`: `ruleNums` are the rule numbers as literals, and
  -- `ctors` are the constructors they turn into.
  let ruleNums : Array Term := tokNames.mapIdx fun i _ => quote i
  let ctors := tokNames.map fun tok => mkIdent (typeName.getId ++ tok.getId)
  -- `lex` is called by the user, so its name is not hygienic. The local names are.
  let runnerName := mkIdent (typeName.getId.str "lex")
  let inputArg ← `(ident| str)
  let s        ← `(ident| s)
  let acc      ← `(ident| acc)
  let rule     ← `(ident| rule)
  let rest     ← `(ident| rest)
  let token    ← `(ident| token)
  `(
    def $runnerName ($inputArg : String) : Except String (Array $typeName) := do
      let mut $s   := $(inputArg).toSlice
      let mut $acc := #[]
      while !$(s).isEmpty do
        let some ($rule, $rest) := $startState $s
          | throw "no rule matches the input"
        let some $token := (match $rule:ident with $[| $ruleNums => some $ctors]* | _ => none)
          | throw "unknown rule"
        $s:ident   := $rest
        $acc:ident := $(acc).push $token
      return $acc
  )

/--
Generates the code of a lexer for `dfa`, as commands to elaborate in order:

* an inductive type named `typeName`, with a constructor for each name in `tokNames`;
* a `mutual` block with a function per `DFA` state but the trap, hidden from the user;
* a function `lex` in the namespace of that type, which splits a string into an array
  of tokens, always taking the longest match.

Tokens are matched to rules by position, so `tokNames` must be in the same order as the
rules `dfa` was built from.
-/
def buildLexer (typeName : Ident) (tokNames : Array Ident) (dfa : DFA) :
    m (Array Command) := do
  return #[
    ← buildTokenType typeName tokNames,
    ← buildStateFuncs typeName dfa,
    ← buildRunner typeName tokNames
  ]
