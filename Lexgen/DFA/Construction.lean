import Std.Data.HashSet
import Lexgen.NFA.Automaton

/--
ε-closure of `node`.

TODO: Prove termination. An `Array` may be needed instead of a `HashSet`.
-/
private partial def NFA.εClosure (nfa : NFA) (node : Nat) : Std.HashSet Nat :=
  let rec closure (visited : Std.HashSet Nat) (node : Nat) : Std.HashSet Nat :=
    if visited.contains node then
      visited
    else
      -- TODO: Can we prove indexing?
      let state   : Node            := nfa.nodes[node]!
      let newVisited : Std.HashSet Nat := visited.insert node
      match state with
      | .done              => newVisited
      | .char _ _          => newVisited
      | .dot _             => newVisited
      | .ε next            => closure newVisited next
      -- When constructing the ε-closure for `next₂`,
      -- the states in the ε-closure of `next₁`
      -- have already been marked as visited.
      | .split next₁ next₂ => closure (closure newVisited next₁) next₂
  closure {} node
