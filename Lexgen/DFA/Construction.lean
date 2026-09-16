import Std.Data.HashSet
import Lexgen.NFA.Automaton
import Lexgen.DFA.Automaton

/--
ε-closure of `node`.

TODO: Prove termination. An `Array` may be needed instead of a `HashSet`.
-/
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

/-
Returns all `NFA` states reachable by following
a single edge with label `c` from state `s`.
-/
private def edge : (c : Edge) → (s : Node) → List Nat
  | .char c₁ , .edge (.char c₂) next =>
    if c₁ == c₂ then [next] else []
  | .dot, .edge .dot next            => [next]
  | .ε, .edge .ε next                => [next]
  | .ε, .split next₁ next₂           => [next₁, next₂]
  | _, _                             => []

/-
Maps a set of states to the new set of states reachable via
a transition on edge value `c` (`char`/`dot`/`ε`) followed by an
unbounded number of ε-transitions.
-/
private def NFA.edgeDFA (nfa : NFA) (states : Std.HashSet Nat) (c : Edge) : Std.HashSet Nat :=
  let raw := states.toList.flatMap (fun state => edge c nfa.nodes[state]!)
  raw.foldl nfa.εClosure {}

/-
Returns the alphabet of `nfa`.
-/
private def NFA.getAlphabet (nfa : NFA) : Std.HashSet Char :=
  Std.HashSet.ofArray
    (nfa.nodes.filterMap
      fun state =>
        if let .edge (.char c) _ := state then
          some c
        else
          none)
