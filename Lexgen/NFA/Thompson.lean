/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public import Lexgen.NFA.Automaton
public import Lexgen.Regex.Ast

public section

/-!
# Thompson's construction

Defines Thompson's construction: translating a
`RegularExprAST` into an `NFA`.
-/

/--
Internal implementation of Thompson's algorithm.

`offset` is passed explicitly at every step.
-/
private def translate (offset : Nat) : RegularExprAST → NFA
  -- The single-node fragments below exit to the state right after them.
  | .ε =>
    { nodes := #[.edge .ε (offset + 1)] }
  | .symbol c =>
    { nodes := #[.edge (.char c) (offset + 1)] }
  | .dot =>
    { nodes := #[.edge .dot (offset + 1)] }
  | .concat first rest =>
    let fstNFA := translate offset first
    -- The second fragment starts right after the first one.
    let sndNFA := translate (offset + fstNFA.nodes.size) rest
    { nodes := fstNFA.nodes ++ sndNFA.nodes }
  | .repeated regex =>
    -- `+ 1` skips the entry `split` at `offset`.
    let subStart := offset + 1
    let subNFA   := translate subStart regex
    -- `+ 1` skips the loop-back `split` after the fragment.
    let endState := subStart + subNFA.nodes.size + 1
    {
      nodes :=
        #[.split subStart endState] ++
        subNFA.nodes ++
        #[.split subStart endState]
    }
  | .alt left right =>
    -- `+ 1` skips the entry `split` at `offset`.
    let leftStart  := offset + 1
    let leftNFA    := translate leftStart left
    -- `+ 1` skips the `ε`-edge that exits the left fragment.
    let rightStart := leftStart + leftNFA.nodes.size + 1
    let rightNFA   := translate rightStart right
    -- `+ 1` skips the `ε`-edge that exits the right fragment.
    let endState   := rightStart + rightNFA.nodes.size + 1
    {
      nodes :=
      #[.split leftStart rightStart] ++
      leftNFA.nodes ++
      #[.edge .ε endState] ++
      rightNFA.nodes ++
      #[.edge .ε endState]
    }

/--
Translates the rules `first :: rest` into an `NFA` whose states are numbered
from `offset` and whose rules are numbered from `rule`.

Each rule's fragment ends in a `done` state labeled with its rule, and a chain of
`split` states chooses between the rules.
-/
private def translateRules (rule offset : Nat) (first : RegularExprAST) :
    List RegularExprAST → NFA
  | [] =>
    -- The last rule needs no `split`: its fragment starts right at `offset`.
    let translated := translate offset first
    { nodes := translated.nodes ++ #[.done rule] }
  | next :: rest =>
    -- `+ 1` skips the `split` at `offset` that chooses between this rule and the rest.
    let translated := translate (offset + 1) first
    -- `+ 2` skips the `split` and this rule's `done` state.
    let restStart  := offset + translated.nodes.size + 2
    {
      nodes :=
      -- Chooses between this rule and the remaining ones.
      #[.split (offset + 1) restStart] ++
      translated.nodes ++
      -- The fragment exits here, to the state right after it.
      #[.done rule] ++
      -- The remaining rules, starting right after this rule's `done` state.
      (translateRules (rule + 1) restStart next rest).nodes
    }

/--
Translates the rules `first :: rest` into a single `NFA` using Thompson's
construction.

Rules are numbered by position, starting from `0` for `first`,
and each accept state is labeled with its rule.
-/
-- Taking `first` separately guarantees there is at least one rule.
def rulesToNFA (first : RegularExprAST) (rest : List RegularExprAST) : NFA :=
  translateRules 0 0 first rest
