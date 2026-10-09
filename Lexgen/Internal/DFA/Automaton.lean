/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public import Lexgen.Internal.DFA.Interval
public import Std.Data.HashMap
public import Std.Data.HashSet

import Lean.Util.SCC

public section

/-!
# DFA

Defines the DFA representation (`DFA`).
-/

namespace Lexgen.Internal

/--
The deterministic finite automaton representation.

States are numbered from zero. State `DFA.trap` is the trap, from which no match can be reached, and
state `DFA.start` is the start state.
-/
structure DFA where
  /--
  Transition table: for every state, sorted, disjoint intervals covering every character, each with
  the state it leads to. Indexed by state.
  -/
  trans     : Array (Array (DFA.Interval × Nat))
  /--
  Accepting (final) states, each mapped to the rule it accepts.
  -/
  accepting : Std.HashMap Nat Nat
deriving Repr, BEq

namespace DFA

/--
The number of the trap state: the empty set of `NFA` states, reached when no edge matches, and
looping back to itself on every interval.
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

/--
Returns the states of `dfa` that lie on a cycle of transitions.
-/
def recursiveStates (dfa : DFA) : Std.HashSet Nat :=
  let components := Lean.SCC.scc
    (List.range dfa.trans.size)
    (Array.toList ∘ successorsOf)
  components.foldl (init := {}) addComponent
where
  /--
  Returns the states that `state` has transitions to.
  -/
  successorsOf (state : Nat) : Array Nat := dfa.trans[state]!.map (·.2)

  /--
  Checks whether `state` has a transition to itself.
  -/
  isSelfRecursive (state : Nat) : Bool := (successorsOf state).contains state

  /--
  Adds the states of a strongly connected component to `acc` if they lie on a cycle: a component of
  several states always does, a single state only if it has a transition to itself.
  -/
  addComponent (acc : Std.HashSet Nat) : List Nat → Std.HashSet Nat
  | [state] =>
    if isSelfRecursive state then
      acc.insert state
    else
      acc
  | states => acc.insertMany states

end DFA

end Lexgen.Internal
