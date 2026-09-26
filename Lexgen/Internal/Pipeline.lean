/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public import Lexgen.Internal.DFA.Automaton

import Lexgen.Internal.DFA.Construction
import Lexgen.Internal.NFA.Thompson
import Lexgen.Internal.Regex

public section

/-!
# Pipeline

Defines the entry point of the regex → NFA → DFA pipeline: `rulesToDFA`,
turning the rules of a `lexer` declaration into a single `DFA`, and its error type
`ConversionError`.
-/

/--
An error of `rulesToDFA`. Rules are given by their numbers, so that the `lexer` command
can report each error on the rule it concerns.
-/
inductive ConversionError where
  /--
  There are no rules at all.
  -/
  | noRules
  /--
  Rule `rule` is not a valid regular expression: parsing failed at the byte `offset` of
  its pattern.
  -/
  | invalidPattern (rule : Nat) (offset : Nat) (msg : String)
  /--
  The rules `rules` match the empty string. Such a rule would make the lexer loop, since
  it accepts a token of length zero.
  -/
  | matchesEmpty (rules : List Nat)

/--
Translates the rules of a `lexer` declaration, given as regular expressions,
into a single `DFA`.

Rules are numbered by position: when several of them match the same text, the
one declared earlier wins.

Returns an error if there are no rules at all, if a rule is not a valid regular
expression, or if a rule matches the empty string.
-/
def rulesToDFA : List String → Except ConversionError DFA
  | []            => throw .noRules
  | first :: rest => do
    let firstRegex  ← parseRule 0 first
    let restRegexes ← (rest.zipIdx 1).mapM fun (pattern, rule) => parseRule rule pattern
    let emptyRules := rulesMatchingEmpty (firstRegex :: restRegexes)
    unless emptyRules.isEmpty do
      throw (.matchesEmpty emptyRules)
    return DFA.ofNFA (NFA.ofRules firstRegex restRegexes)
where
  /--
  Parses the pattern of rule `rule`, tagging a parse error with the rule number.
  -/
  parseRule (rule : Nat) (pattern : String) : Except ConversionError RegularExprAST :=
    (parse pattern).mapError fun err => .invalidPattern rule err.offset err.msg
  /--
  Returns the numbers of the rules that match the empty string.
  -/
  rulesMatchingEmpty (rules : List RegularExprAST) : List Nat :=
    rules.zipIdx.filterMap
      (fun (regex, rule) => if regex.matchesEmpty then some rule else none)
