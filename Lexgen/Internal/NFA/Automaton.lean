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

namespace Lexgen.Internal

/--
An item on an `NFA` edge: a literal character or a range of characters.
-/
inductive NFA.Range where
  /--
  Matches the literal character `character`.
  -/
  | single (character : Char)
  /--
  Matches a character from `lower` to `upper`, inclusive.
  -/
  | range (lower upper : Char)
deriving Repr, DecidableEq, Hashable

/--
Checks whether an `NFA.Range` matches the character `c`.
-/
def NFA.Range.contains (c : Char) : NFA.Range → Bool
  | .single s          => c == s
  | .range lower upper => lower <= c && c <= upper

/--
The label type for `NFA` transitions.
-/
inductive NFA.Edge where
  /--
  Matches a character from `ranges`, or, if `negate`, a character outside them.
  -/
  | charClass (negate : Bool) (ranges : Array Range)
  /--
  Matches the empty string.
  -/
  | ε
deriving Repr, DecidableEq, Hashable

/--
A state of the `NFA`.
-/
inductive NFA.Node where
  /--
  Accepting state of `NFA` for rule `rule`. There are no transitions from it.
  -/
  | done (rule : Nat)
  /--
  Node labeled by a real edge (`charClass`/`ε`), not a control node.
  -/
  | edge (e : NFA.Edge) (next : Nat)
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
  nodes : Array NFA.Node
deriving Repr, DecidableEq

namespace NFA

/--
The number of the start state.
-/
abbrev start : Nat := 0

/-
Short names for `Edge` constructors, so that `NFA.charClass` tells the `NFA` edge apart from the
`RegexAST` node `RegexAST.charClass`.
-/
export Edge (charClass ε)

end NFA

end Lexgen.Internal
