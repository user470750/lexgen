/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public import Lexgen.Internal.DFA.Automaton
public import Lexgen.Internal.NFA.Automaton

import Std.Data.HashMap
import Std.Data.HashSet

public section

/-!
# DFA construction

Defines `DFA.ofNFA`: translating an `NFA` into a `DFA` using the
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
    | .done _                 => newVisited
    | .edge (.charClass ..) _ => newVisited
    | .edge .ε next           => nfa.εClosure newVisited next
    -- When constructing the ε-closure for `next₂`,
    -- the states in the ε-closure of `next₁`
    -- have already been marked as visited.
    | .split next₁ next₂      => nfa.εClosure (nfa.εClosure newVisited next₁) next₂

/--
Returns all `NFA` states reachable from a node by following a single edge matching `interval`.
-/
private def step (interval : CharClass) : NFA.Node → List Nat
  | .edge (.charClass negate ranges) next =>
    let c := match interval with | .single c | .range c _ => c
    if negate != ranges.any (·.contains c) then [next] else []
  | _ => []

namespace DFA

/--
Maps a set of states to the new set of states reachable via a transition
on `interval` followed by an unbounded number of ε-transitions.
-/
private def reachedOn (nfa : NFA) (states : Std.HashSet Nat) (interval : CharClass) :
    Std.HashSet Nat :=
  let raw := states.toList.flatMap (fun state => step interval nfa.nodes[state]!)
  raw.foldl nfa.εClosure {}

/--
Returns the alphabet of `nfa`: sorted, disjoint intervals covering every character, such that
no edge of `nfa` tells apart two characters of one interval.

The intervals are cut where some character class starts or stops matching, at `0` and
`0x110000`, which bound all characters, and at `0xD800` and `0xE000`, which bound the
surrogates.
-/
private def alphabet (nfa : NFA) : Array CharClass :=
  let bounds :=
    ((nfa.nodes.filterMap nodeBounds).flatten ++ #[0, 0xD800, 0xE000, 0x110000])
    |>.mergeSort
    |>.eraseReps
  Array.zipWith (·, · - 1) bounds (bounds.extract 1)
  |>.filter (·.1 != 0xD800)
  |>.map formClass
where
  /--
  Returns the code points at which the character class on the edge of a node starts and stops
  matching, or `none` if the node has no such edge.
  -/
  nodeBounds : NFA.Node → Option (Array Nat)
    | .edge (.charClass _ ranges) _ =>
      some (ranges.flatMap fun
        | .single c          => #[c.toNat, c.toNat + 1]
        | .range lower upper => #[lower.toNat, upper.toNat + 1])
    | _ => none
  /--
  Returns the `CharClass` of the characters from the code point `lower` to `upper`, inclusive:
  a `single` if there is only one.
  -/
  formClass : Nat × Nat → CharClass
    | (lower, upper) =>
      if lower == upper then
        .single (Char.ofNat lower)
      else
        .range (Char.ofNat lower) (Char.ofNat upper)

/--
Returns the rule accepted by a `DFA` state (a set of `NFA` states), or `none`
if none of its `NFA` states is accepting.

When several rules are accepted, the one with the smallest number
(rule declared earlier) takes priority.
-/
private def acceptingRule? (nfa : NFA) (states : Std.HashSet Nat) : Option Nat :=
  let rules := states.toList.filterMap fun state =>
    if let .done rule := nfa.nodes[state]! then
      some rule
    else
      none
  rules.min?

/--
Translates an `NFA` into a `DFA` using the subset construction.
-/
def ofNFA (nfa : NFA) : DFA :=
  -- The subset construction is an established imperative algorithm:
  -- `states`, `trans` and `accepting` all need to be mutated while building
  -- the `DFA`.
  -- `while`/`for` loops are simpler here than splitting the logic into a
  -- bunch of recursive helper functions, which would be more verbose.
  -- TODO: Consider a functional rewrite for consistency with the rest of
  -- the codebase.
  Id.run do
    let alphabet : Array CharClass := alphabet nfa

    -- The trap goes first and the start state second.
    let mut states    : Array (Std.HashSet Nat)         := #[{}, nfa.εClosure {} 0]
    let mut trans     : Array (Array (CharClass × Nat)) := #[]
    let mut accepting : Std.HashMap Nat Nat             := {}

    if let some rule := acceptingRule? nfa states[DFA.start]! then
      accepting := accepting.insert DFA.start rule

    let mut lastState := 1
    let mut current   := 0

    -- `lastState` stops growing once every reached state for `current` is already in `states`.
    while current ≤ lastState do
      let mut row : Array (CharClass × Nat) := #[]
      for interval in alphabet do
        let reached := reachedOn nfa states[current]! interval
        match states.findIdx? (· == reached) with
        | some existingId => row := row.push (interval, existingId)
        | none            =>
          lastState := lastState + 1
          states := states.push reached
          row := row.push (interval, lastState)
          if let some rule := acceptingRule? nfa reached then
            accepting := accepting.insert lastState rule
      trans := trans.push row
      current := current + 1

    return { trans, accepting }

end DFA
