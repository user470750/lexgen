/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public section

/-!
# NFA

Defines the NFA representation (`NFA`).
-/

/--
The label type for `NFA` transitions.
-/
inductive Edge where
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
deriving Repr, DecidableEq, Hashable

/--
A state of the `NFA`.
-/
inductive Node where
  /--
  Accept state of `NFA` for rule `rule`. There are no transitions from it.
  -/
  | done (rule : Nat)
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
