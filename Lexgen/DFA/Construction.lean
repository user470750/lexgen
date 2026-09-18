/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module
public import Std.Data.HashSet
public import Std.Data.HashMap
public import Lexgen.NFA.Automaton
public import Lexgen.DFA.Automaton
meta import Lexgen.NFA.Automaton
meta import Lexgen.DFA.Automaton

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
private partial def NFA.εClosure (nfa : NFA) (visited : Std.HashSet Nat) (node : Nat) : Std.HashSet Nat :=
  if visited.contains node then
    visited
  else
    -- TODO: Can we prove indexing?
    let state      : Node            := nfa.nodes[node]!
    let newVisited : Std.HashSet Nat := visited.insert node
    match state with
    | .done              => newVisited
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
private def edge : (c : Edge) → (s : Node) → List Nat
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
Predicate checking whether a `DFA` state (a set of `NFA` states) is accepting.

A `DFA` state is accepting if at least one of the `NFA` states it includes
is accepting.
-/
private def NFA.isAccepting (nfa : NFA) (states : Std.HashSet Nat) : Bool :=
  states.toList.any (.done == nfa.nodes[·]!)

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

    let mut states    : Array (Std.HashSet Nat)        := #[nfa.εClosure {} 0]
    let mut trans     : Std.HashMap (Nat × Symbol) Nat := {}
    let mut accepting : Std.HashSet Nat                := {}

    if isAccepting nfa states[0]! then
      accepting := accepting.insert 0

    let mut lastState  := 0
    let mut current    := 0

    -- `lastState` stops growing once every reached state for `current` is already in `states`.
    while current ≤ lastState do
      for edgeLabel in alphabet do
        let reached := nfa.edgeDFA states[current]! edgeLabel
        let dfaSymbol : Symbol :=
          match edgeLabel with
          | .char ch => .char ch
          | .dot     => .dot
          -- TODO: Can we prove this branch is unreachable instead of panicking?
          | .ε       => panic! "alphabet should never contain ε"
        match states.findIdx? (· == reached) with
        | some existingId => trans := trans.insert (current, dfaSymbol) existingId
        | none            =>
          lastState := lastState + 1
          states := states.push reached
          trans := trans.insert (current, dfaSymbol) lastState
          if isAccepting nfa reached then
            accepting := accepting.insert lastState
      current := current + 1

    return { trans, accepting }

-- The `NFA` inputs are the ones produced by Thompson's construction for the
-- regexes given in the comments.

-- Each table is total: one entry per state and alphabet symbol. Most tables
-- therefore contain a trap state — the empty set of `NFA` states, reached
-- when no edge matches — looping back to itself.

-- "a"
#guard
NFA.toDFA { nodes := #[.edge (.char 'a') 1, .done] } ==
{
  trans := Std.HashMap.ofList [
    ((0, .char 'a'), 1),
    ((1, .char 'a'), 2),
    ((2, .char 'a'), 2)
  ],
  accepting := Std.HashSet.ofList [1]
}

-- "."
#guard
NFA.toDFA { nodes := #[.edge .dot 1, .done] } ==
{
  trans := Std.HashMap.ofList [((0, .dot), 1), ((1, .dot), 2), ((2, .dot), 2)],
  accepting := Std.HashSet.ofList [1]
}

-- The `NFA` has no `char`/`dot` edges, so the alphabet is empty and no
-- transitions are produced at all. State 0 accepts because its ε-closure
-- reaches `done`.
#guard
NFA.toDFA { nodes := #[.edge .ε 1, .done] } ==
{ trans := {}, accepting := Std.HashSet.ofList [0] }

-- "ab"
#guard
NFA.toDFA { nodes := #[.edge (.char 'a') 1, .edge (.char 'b') 2, .done] } ==
{
  trans := Std.HashMap.ofList [
    ((0, .char 'a'), 1), ((0, .char 'b'), 2),
    ((1, .char 'a'), 2), ((1, .char 'b'), 3),
    ((2, .char 'a'), 2), ((2, .char 'b'), 2),
    ((3, .char 'a'), 2), ((3, .char 'b'), 2)
  ],
  accepting := Std.HashSet.ofList [3]
}

-- "a|b"
#guard
NFA.toDFA {
  nodes := #[
    .split 1 3,
    .edge (.char 'a') 2,
    .edge .ε 5,
    .edge (.char 'b') 4,
    .edge .ε 5,
    .done
  ]
} ==
{
  trans := Std.HashMap.ofList [
    ((0, .char 'a'), 1), ((0, .char 'b'), 2),
    ((1, .char 'a'), 3), ((1, .char 'b'), 3),
    ((2, .char 'a'), 3), ((2, .char 'b'), 3),
    ((3, .char 'a'), 3), ((3, .char 'b'), 3)
  ],
  accepting := Std.HashSet.ofList [1, 2]
}

-- "a*"
#guard
NFA.toDFA { nodes := #[.split 1 3, .edge (.char 'a') 2, .split 1 3, .done] } ==
{
  trans := Std.HashMap.ofList [((0, .char 'a'), 1), ((1, .char 'a'), 1)],
  accepting := Std.HashSet.ofList [0, 1]
}

-- The two cases below mix a literal with `.`, so reading `'a'` has to follow
-- the `dot` edge as well, not only the `char` one.

-- ".a"
#guard
NFA.toDFA { nodes := #[.edge .dot 1, .edge (.char 'a') 2, .done] } ==
{
  trans := Std.HashMap.ofList [
    ((0, .dot), 1), ((0, .char 'a'), 1),
    ((1, .dot), 2), ((1, .char 'a'), 3),
    ((2, .dot), 2), ((2, .char 'a'), 2),
    ((3, .dot), 2), ((3, .char 'a'), 2)
  ],
  accepting := Std.HashSet.ofList [3]
}

-- "a."
#guard
NFA.toDFA { nodes := #[.edge (.char 'a') 1, .edge .dot 2, .done] } ==
{
  trans := Std.HashMap.ofList [
    ((0, .dot), 1), ((0, .char 'a'), 2),
    ((1, .dot), 1), ((1, .char 'a'), 1),
    ((2, .dot), 3), ((2, .char 'a'), 3),
    ((3, .dot), 1), ((3, .char 'a'), 1)
  ],
  accepting := Std.HashSet.ofList [3]
}
