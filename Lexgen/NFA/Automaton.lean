/-!
# NFA

Defines the NFA representation (`NFA`).
-/

/--
A state of the `NFA`.
-/
inductive Node where
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
  | dot (next : Nat)
  /--
  The empty string label for transition to `next` state.
  -/
  | ε (next : Nat)
  /--
  Two ε-transition labels, to `next₁` and `next₂`.
  -/
  | split (next₁ next₂ : Nat)
deriving Repr, DecidableEq

/--
The non-deterministic finite automaton representation.
-/
structure NFA where
  /--
  The states of the `NFA`, indexed by state id.
  -/
  nodes : Array Node
deriving Repr, DecidableEq
