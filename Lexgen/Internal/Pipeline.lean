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
turning the rules of a `lexer` declaration into a single `DFA`.

It is the only declaration of the pipeline that `Codegen` needs, and once code
generation is implemented, `Codegen` will be the only module importing this one.
-/

/--
Translates the rules of a `lexer` declaration, given as regular expressions,
into a single `DFA`.

Rules are numbered by position: when several of them match the same text, the
one declared earlier wins.

Returns an error message if a rule is not a valid regular expression, if there
are no rules at all, or if a rule matches the empty string: such a rule would
make the lexer loop, since it accepts a token of length zero.
-/
def rulesToDFA : List String → Except String DFA
  | []            => throw "a lexer needs at least one rule"
  | first :: rest => do
    let firstRegex  ← parse first
    let restRegexes ← rest.mapM parse
    let emptyRules := existEmpty (firstRegex :: restRegexes)
    unless emptyRules.isEmpty do
      -- TODO: Return the rule numbers as data instead of putting them into a
      -- message. The `lexer` command knows the name of every rule, so it could
      -- then report the names instead of the numbers.
      throw s!"rules matching the empty string: {emptyRules}"
    pure (DFA.ofNFA (NFA.ofRules firstRegex restRegexes))
where
  /--
  Returns the numbers of the rules that match the empty string.
  -/
  existEmpty (rules : List RegularExprAST) : List Nat :=
    rules.zipIdx.filterMap
      (fun (regex, rule) => if regex.matchesEmpty then some rule else none)
