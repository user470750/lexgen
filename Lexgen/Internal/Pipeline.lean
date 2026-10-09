/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public import Lexgen.Internal.DFA.Automaton

import Lexgen.Internal.DFA.Construction
import Lexgen.Internal.DFA.Merge
import Lexgen.Internal.DFA.Moore
import Lexgen.Internal.NFA.Thompson
import Lexgen.Internal.Regex

public section

/-!
# Pipeline

Defines `rulesToDFA`, the entry point of the regex → NFA → DFA pipeline, and its error type
`ConversionError`.
-/

namespace Lexgen.Internal

/--
An error of `rulesToDFA`. Rules are given by their numbers, so that the `lexer` command can report
each error on the rule it concerns.
-/
inductive ConversionError where
  /--
  There are no rules at all.
  -/
  | noRules
  /--
  Rule `rule` is not a valid regular expression: parsing failed at the byte `offset` of its pattern.
  -/
  | invalidPattern (rule : Nat) (offset : Nat) (msg : String)
  /--
  Rule `rule` matches the empty string. Such a rule would make the lexer loop, since it accepts a
  token of length zero.
  -/
  | matchesEmpty (rule : Nat)

/--
Translates the rules of a `lexer` declaration, given as regular expressions, into a single `DFA`.

Rules are numbered by position: when several of them match the same text, the one declared earlier
wins. The `DFA` is minimized if `minimization` is set, and the neighbouring intervals of a state
that lead to the same state are merged.

Returns the errors of all rules: an error for each rule that is not a valid regular expression or
matches the empty string, or a single error if there are no rules at all.
-/
def rulesToDFA (patterns : List String) (minimization : Bool) :
    Except (Array ConversionError) DFA := do
  let checked := patterns.zipIdx.map checkRule
  let errors := checked.filterMap (if let .error err := · then some err else none)
  unless errors.isEmpty do
    throw errors.toArray
  match checked.filterMap (·.toOption) with
  | [] => throw #[.noRules]
  | first :: rest =>
    let dfa := DFA.ofNFA (NFA.ofRules first rest)
    pure (if minimization then dfa.minimize.merge else dfa.merge)
where
  /--
  Parses the pattern of a rule, paired with the rule number, and checks that it does not match the
  empty string, tagging an error with the rule number.
  -/
  checkRule : String × Nat → Except ConversionError RegexAST
    | (pattern, rule) => do
      let regex ← (parse pattern).mapError fun err => .invalidPattern rule err.offset err.msg
      if regex.matchesEmpty then
        throw (.matchesEmpty rule)
      return regex

end Lexgen.Internal
