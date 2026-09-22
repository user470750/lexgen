/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public import Lexgen.DFA.Automaton
public import Lexgen.NFA.Automaton

import Std.Data.HashMap
import Std.Data.HashSet

public section

/-!
# DFA construction

Defines `NFA.toDFA`: translating an `NFA` into a `DFA` using the
subset construction.
-/

/--
Returns `visited` extended with the ε-closure of `node`: the states reachable
from `node` (including `node` itself) via ε-transitions.

States already in `visited` are not explored again, so `visited` is assumed to
contain the ε-closure of each of its states.
-/
-- TODO: Prove termination. An `Array` may be needed instead of a `HashSet`.
private partial def NFA.εClosure (nfa : NFA) (visited : Std.HashSet Nat) (node : Nat) :
    Std.HashSet Nat :=
  if visited.contains node then
    visited
  else
    -- TODO: Can we prove indexing?
    let state      : Node            := nfa.nodes[node]!
    let newVisited : Std.HashSet Nat := visited.insert node
    match state with
    | .done _            => newVisited
    | .edge (.char _) _  => newVisited
    | .edge .dot _       => newVisited
    | .edge .ε next      => nfa.εClosure newVisited next
    -- When constructing the ε-closure for `next₂`,
    -- the states in the ε-closure of `next₁`
    -- have already been marked as visited.
    | .split next₁ next₂ => nfa.εClosure (nfa.εClosure newVisited next₁) next₂

/--
Returns all `NFA` states reachable by following
a single edge with label `c` from state `s`.
-/
private def edge : (c : NFA.Edge) → (s : NFA.Node) → List Nat
  | .char c₁ , .edge (.char c₂) next =>
    if c₁ == c₂ then [next] else []
  | .char _, .edge .dot next         => [next]
  | .dot, .edge .dot next            => [next]
  | .ε, .edge .ε next                => [next]
  | .ε, .split next₁ next₂           => [next₁, next₂]
  | _, _                             => []

/--
Maps a set of states to the new set of states reachable via
a transition on edge value `c` (`char`/`dot`/`ε`) followed by an
unbounded number of ε-transitions.
-/
private def NFA.edgeDFA (nfa : NFA) (states : Std.HashSet Nat) (c : Edge) : Std.HashSet Nat :=
  let raw := states.toList.flatMap (fun state => edge c nfa.nodes[state]!)
  raw.foldl nfa.εClosure {}

/--
Returns the alphabet of `nfa`.
-/
private def NFA.getAlphabet (nfa : NFA) : Std.HashSet Edge :=
  Std.HashSet.ofArray
    (nfa.nodes.filterMap
      fun state =>
        match state with
        | .edge (.char c) _ => some (.char c)
        | .edge .dot      _ => some .dot
        | _                 => none)

/--
Returns the rule accepted by a `DFA` state (a set of `NFA` states), or `none`
if none of its `NFA` states is accepting.

When several rules are accepted, the one with the smallest number
(rule declared earlier) takes priority.
-/
private def NFA.acceptingRule? (nfa : NFA) (states : Std.HashSet Nat) : Option Nat :=
  let rules := states.toList.filterMap fun state =>
    if let .done rule := nfa.nodes[state]! then
      some rule
    else
      none
  rules.min?

/--
Translates an `NFA` into a `DFA` using the subset construction.
-/
def NFA.toDFA (nfa : NFA) : DFA :=
  -- The subset construction is an established imperative algorithm:
  -- `states`, `trans` and `accepting` all need to be mutated while building
  -- the `DFA`.
  -- `while`/`for` loops are simpler here than splitting the logic into a
  -- bunch of recursive helper functions, which would be more verbose.
  -- TODO: Consider a functional rewrite for consistency with the rest of
  -- the codebase.
  Id.run do
    let alphabet : List Edge := nfa.getAlphabet.toList

    let mut states    : Array (Std.HashSet Nat)            := #[nfa.εClosure {} 0]
    let mut trans     : Std.HashMap (Nat × DFA.Symbol) Nat := {}
    let mut accepting : Std.HashMap Nat Nat                := {}

    if let some rule := nfa.acceptingRule? states[0]! then
      accepting := accepting.insert 0 rule

    let mut lastState  := 0
    let mut current    := 0

    -- `lastState` stops growing once every reached state for `current` is already in `states`.
    while current ≤ lastState do
      for edgeLabel in alphabet do
        let reached := nfa.edgeDFA states[current]! edgeLabel
        let dfaSymbol : DFA.Symbol :=
          match edgeLabel with
          | NFA.char ch => .char ch
          | NFA.dot     => .dot
          -- TODO: Can we prove this branch is unreachable instead of panicking?
          | NFA.ε       => panic! "alphabet should never contain ε"
        match states.findIdx? (· == reached) with
        | some existingId => trans := trans.insert (current, dfaSymbol) existingId
        | none            =>
          lastState := lastState + 1
          states := states.push reached
          trans := trans.insert (current, dfaSymbol) lastState
          if let some rule := nfa.acceptingRule? reached then
            accepting := accepting.insert lastState rule
      current := current + 1

    return { trans, accepting }
