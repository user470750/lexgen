/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
import Std.Data.HashSet
import Std.Data.HashMap

/-!
# DFA

Defines the DFA representation (`DFA`).
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
  Accepting (final) states.
  -/
  accepting : Std.HashSet Nat
deriving Repr, BEq
