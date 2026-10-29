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
import Lexgen.Internal.Regex.Ast
import Lexgen.Internal.Regex.Desugar
import Lexgen.Internal.Regex.Parser
import Lexgen.Internal.Regex.Syntax

public section

/-!
# Pipeline

Defines `rulesToDFA`, the entry point of the regex → NFA → DFA pipeline, and its error type
`ConversionError`.
-/

namespace Lexgen.Internal

/--
The largest number of nodes a pattern may have once its repetitions are expanded.
-/
def maxRegexSize : Nat := 2000

/--
The location of an error of `rulesToDFA`: a rule or a subpattern, given by its position among the
declarations of rules or of subpatterns respectively.
-/
inductive Location where
  /--
  The rule at position `rule` among the rules, which is its number.
  -/
  | rule (rule : Nat)
  /--
  The subpattern at position `subpattern` among the subpatterns.
  -/
  | subpattern (subpattern : Nat)

/--
An error of `rulesToDFA`. Rules (and subpatterns) are given by their numbers, so that the `lexer`
command can report each error on the rule it concerns.
-/
inductive ConversionError where
  /--
  There are no rules at all.
  -/
  | noRules
  /--
  The rule or subpattern at `location` is not a valid regular expression: parsing failed at the
  byte `offset` of its pattern.
  -/
  | invalidPattern (location : Location) (offset : Nat) (msg : String)
  /--
  Rule `rule` matches the empty string. Such a rule would make the lexer loop, since it accepts a
  token of length zero.
  -/
  | matchesEmpty (rule : Nat)
  /--
  The rule or subpattern at `location` is too large: once its repetitions are expanded, it has more
  than `maxRegexSize` nodes.
  -/
  | tooLarge (location : Location)

/--
Translates the rules of a `lexer` declaration, given as regular expressions, into a single `DFA`.

Rules are numbered by position: when several of them match the same text, the one declared earlier
wins. The `DFA` is minimized if `minimization` is set, and the neighbouring intervals of a state
that lead to the same state are merged.

Subpatterns are processed first, also in the order of declaration. An error in a subpattern stops
the conversion, since otherwise a pattern that refers to it could panic in `get!`.

Returns the errors of all rules: an error for each rule that is not a valid regular expression, is
too large or matches the empty string, or a single error if there are no rules at all.
-/
def rulesToDFA (patterns : List String) (subpatterns : List (String × String))
    (minimization : Bool) : Except (Array ConversionError) DFA := do
  let desugared ← subpatterns.zipIdx.foldlM
    (init := {})
    fun desugared ((name, pattern), subpattern) => do
      let regex ← (checkSubpattern desugared (pattern, subpattern)).mapError (#[·])
      return desugared.insert name regex
  let checked := patterns.zipIdx.map (checkRule desugared)
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
  Parses the pattern of a rule, paired with the rule number, checks its size before desugaring it,
  and checks that it does not match the empty string, tagging an error with the rule number.
  -/
  checkRule (subpatterns : Std.HashMap String RegexAST) :
      String × Nat → Except ConversionError RegexAST
    | (pattern, rule) => do
      let parsed ← (parseRegex pattern).mapError fun err =>
        .invalidPattern (.rule rule) err.offset err.msg
      if parsed.countNodes (subpatterns.map fun _ regex => regex.countNodes) > maxRegexSize then
        throw (.tooLarge (.rule rule))
      let regex := parsed.desugar subpatterns
      if regex.matchesEmpty then
        throw (.matchesEmpty rule)
      return regex
  /--
  Parses the pattern of a subpattern, paired with its index among the subpatterns, checks its size
  before desugaring it, tagging an error with the index. Unlike a rule, a subpattern may match the
  empty string.
  -/
  checkSubpattern (subpatterns : Std.HashMap String RegexAST) :
      String × Nat → Except ConversionError RegexAST
    | (pattern, index) => do
      let parsed ← (parseRegex pattern).mapError fun err =>
        .invalidPattern (.subpattern index) err.offset err.msg
      if parsed.countNodes (subpatterns.map fun _ regex => regex.countNodes) > maxRegexSize then
        throw (.tooLarge (.subpattern index))
      return parsed.desugar subpatterns

end Lexgen.Internal
