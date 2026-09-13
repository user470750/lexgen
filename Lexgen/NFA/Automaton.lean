/-!
# NFA

Defines the NFA representation (`NFA`).
-/

/--
The label type for `NFA` transitions.
-/
inductive Symbol where
  /--
  Matches the literal character `c`.
  -/
  | char (c : Char)
  /--
  Matches any single character.
  -/
  | dot
  /--
  Matches the empty string.
  -/
  | ε
deriving Repr

/--
The non-deterministic finite automaton representation.
-/
structure NFA where
  /--
  The start state.
  -/
  start  : Nat
  /--
  The transitions table.
  -/
  trans  : List (Nat × Symbol × Nat) -- TODO: choose better table representation
  /--
  The accept state.
  -/
  accept : Nat
deriving Repr
