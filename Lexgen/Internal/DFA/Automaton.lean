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

Unlike `NFA.Edge`, it has no `ε`: a `DFA` has no ε-transitions.
-/
inductive DFA.Symbol where
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

States are numbered from zero. State `DFA.trap` is the trap, from which no match can
be reached, and state `DFA.start` is the start state.
-/
structure DFA where
  /--
  Transition table: for every state, the symbols leaving it together with
  the states they lead to. Indexed by state.
  -/
  trans     : Array (List (DFA.Symbol × Nat))
  /--
  Accepting (final) states, each mapped to the rule it accepts.
  -/
  accepting : Std.HashMap Nat Nat
deriving Repr, BEq

namespace DFA

/--
The number of the trap state: the empty set of `NFA` states, reached when no edge
matches, and looping back to itself on every symbol.
-/
abbrev trap : Nat := 0

/--
The number of the start state.
-/
abbrev start : Nat := 1

/-
Short names for `Symbol` constructors, so that `DFA.dot` tells the `DFA`
symbol apart from the `NFA` edge `NFA.dot`.
-/
export Symbol (char dot)

end DFA
