/-!
# NFA

Defines the NFA representation (`NFA`).
-/

/--
A state of the `NFA`.
-/
inductive Edge where
  /--
  Accept state of `NFA`. There are no transitions from it.
  -/
  | done
  /--
  The literal character `c` label for transition to `next` state.
  -/
  | char (c : Char) (next : Nat)
  /--
  Any single character label for transition to `next` state.
  -/
  | ε
deriving Repr, DecidableEq, Hashable

/--
A state of the `NFA`.
-/
inductive Node where
  /--
  Accept state of `NFA`. There are no transitions from it.
  -/
  | done
  /--
  Node labeled by a real edge (`char`/`dot`/`ε`), not a control node.
  -/
  | edge (e : Edge) (next : Nat)
  /--
  Two ε-transition labels, to `next₁` and `next₂`.
  -/
  | split (next₁ next₂ : Nat)
deriving Repr, DecidableEq, Inhabited

/--
The non-deterministic finite automaton representation.
-/
structure NFA where
  /--
  The states of the `NFA`, indexed by state id.
  -/
  nodes : Array Node
deriving Repr, DecidableEq
