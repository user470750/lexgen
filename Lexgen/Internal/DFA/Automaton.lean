/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public import Std.Data.HashMap
public import Std.Data.HashSet

public section

/-!
# DFA

Defines the DFA representation (`DFA`).
-/

namespace Lexgen.Internal

/--
The label type for `DFA` transitions: an interval of characters, which is a single character
or a range of characters.
-/
inductive DFA.Interval where
  /--
  Matches the literal character `character`.
  -/
  | single (character : Char)
  /--
  Matches a character from `lower` to `upper`, inclusive.
  -/
  | range (lower upper : Char)
deriving Repr, BEq

/--
The deterministic finite automaton representation.

States are numbered from zero. State `DFA.trap` is the trap, from which no match can
be reached, and state `DFA.start` is the start state.
-/
structure DFA where
  /--
  Transition table: for every state, the intervals of characters leaving it together with
  the states they lead to. Indexed by state. No `NFA` edge tells apart two characters of one
  interval.
  -/
  trans     : Array (Array (DFA.Interval × Nat))
  /--
  Accepting (final) states, each mapped to the rule it accepts.
  -/
  accepting : Std.HashMap Nat Nat
deriving Repr, BEq

namespace DFA

/--
The number of the trap state: the empty set of `NFA` states, reached when no edge
matches, and looping back to itself on every interval.
-/
abbrev trap : Nat := 0

/--
The number of the start state.
-/
abbrev start : Nat := 1

/--
Returns the rules that some state of `dfa` accepts.
-/
def liveRules (dfa : DFA) : Std.HashSet Nat :=
  Std.HashSet.ofList dfa.accepting.values

end DFA

end Lexgen.Internal
