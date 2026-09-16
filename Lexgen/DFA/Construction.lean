import Std.Data.HashSet
import Lexgen.NFA.Automaton

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
