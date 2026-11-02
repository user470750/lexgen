/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public import Lexgen.Internal.DFA.Automaton

public section

/-!
# Merging intervals

Defines `DFA.merge`: merging the neighbouring intervals of a state that lead to the same state, so
that the generated lexer tests fewer intervals for each byte.
-/

namespace Lexgen.Internal

/--
Returns `dfa` with the neighbouring intervals of every state that lead to the same state merged
into one.
-/
-- The intervals of a state are in order and cover every byte, so two neighbours have no other
-- interval between them, and their merge leaves out no byte.
def DFA.merge (dfa : DFA) : DFA :=
  { dfa with trans := dfa.trans.map fun row =>
      row.foldl (init := #[]) fun merged (interval, target) =>
        match merged.back? with
        | some (last, lastTarget) =>
          if lastTarget == target then
            merged.pop.push (⟨last.lower, interval.upper⟩, target)
          else
            merged.push (interval, target)
        | none => merged.push (interval, target) }

end Lexgen.Internal
