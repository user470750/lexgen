/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public import Std.Data.HashMap

public section

/-!
# DFA

Defines the DFA representation (`DFA`).
-/

/--
The label type for `DFA` transitions.

Unlike `Edge`, it has no `ε`: a `DFA` has no ε-transitions.
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
deriving Repr, DecidableEq, Hashable, Inhabited

/--
The deterministic finite automaton representation.
-/
structure DFA where
  /--
  Transition table: maps a state and an edge label to the
  resulting state.
  -/
  trans     : Std.HashMap (Nat × Symbol) Nat
  /--
  Accepting (final) states, each mapped to the rule it accepts.
  -/
  accepting : Std.HashMap Nat Nat
deriving Repr, BEq
