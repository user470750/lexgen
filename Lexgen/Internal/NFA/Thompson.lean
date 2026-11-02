/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public import Lexgen.Internal.NFA.Automaton
public import Lexgen.Internal.Regex.Ast

import Lexgen.Internal.NFA.UTF8

public section

/-!
# Thompson's construction

Defines Thompson's construction: translating a `RegexAST` into an `NFA`.
-/

namespace Lexgen.Internal

/--
Internal implementation of Thompson's algorithm.

Translates a regular expression into an `NFA` fragment whose states are numbered from `offset`.
-/
-- The fragment exits to the state right after it.
private def translate (offset : Nat) : RegexAST → NFA
  -- A single-node fragment, which exits to the state right after it.
  | RegexAST.ε =>
    { nodes := #[.edge NFA.ε (offset + 1)] }
  | RegexAST.charClass ranges =>
    let seqs := ranges.toList.flatMap fun ⟨lo, hi, _⟩ => UTF8.split lo.val hi.val
    classFragment offset (offset + classSize seqs) seqs
  | RegexAST.concat first rest =>
    let fstNFA := translate offset first
    -- The second fragment starts right after the first one.
    let sndNFA := translate (offset + fstNFA.nodes.size) rest
    { nodes := fstNFA.nodes ++ sndNFA.nodes }
  | RegexAST.repeated regex =>
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
  | RegexAST.alt left right =>
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
        #[.edge NFA.ε endState] ++
        rightNFA.nodes ++
        #[.edge NFA.ε endState]
    }
where
  /--
  Builds the fragment of a character class from its byte sequences, numbering its states from
  `offset`: a chain of `split`s chooses a sequence, and every sequence exits to `endState`, the
  state right after the whole fragment.
  -/
  classFragment (offset endState : Nat) : List UTF8.Sequence → NFA
    | []          => edgeFragment offset none
    | seq :: []   => sequenceFragment offset seq
    | seq :: seqs =>
      -- `+ 1` skips the `split` at `offset`.
      let seqStart  := offset + 1
      let seqNFA    := sequenceFragment seqStart seq
      -- `+ 1` skips the `ε`-edge that exits the sequence.
      let restStart := seqStart + seqNFA.nodes.size + 1
      let restNFA   := classFragment restStart endState seqs
      {
        nodes :=
          #[.split seqStart restStart] ++
          seqNFA.nodes ++
          #[.edge NFA.ε endState] ++
          restNFA.nodes
      }

  /--
  Returns the number of states in the fragment of a class from its byte sequences.
  -/
  -- It must agree with the layout of `classFragment`.
  classSize : List UTF8.Sequence → Nat
    -- An empty class: an edge that matches nothing.
    | []          => 1
    | seq :: []   => seq.val.length
    -- A `split` and an `ε`-edge for every sequence but the last.
    | seq :: seqs => seq.val.length + 2 + classSize seqs

  /--
  Builds the fragment of a byte sequence, numbering its states from `offset`: an edge for each
  range, each leading to the next.
  -/
  sequenceFragment (offset : Nat) : UTF8.Sequence → NFA
    | ⟨r :: [], _⟩        => edgeFragment offset (some r)
    | ⟨r₁ :: r₂ :: rs, _⟩ =>
      let edgeNFA : NFA := edgeFragment offset (some r₁)
      let restNFA :=
        sequenceFragment
          (offset + edgeNFA.nodes.size)
          ⟨r₂ :: rs, List.cons_ne_nil _ _⟩
      { nodes := edgeNFA.nodes ++ restNFA.nodes }

  /--
  Builds a fragment of one edge that matches a byte from `r?`, or nothing if it is `none`,
  numbering its state `offset`.
  -/
  edgeFragment (offset : Nat) (r? : Option NFA.Range) : NFA :=
    { nodes := #[.edge (NFA.charClass r?) (offset + 1)] }

/--
Translates the rule `first`, followed by the rules of the list, into an `NFA` whose states are
numbered from `offset` and whose rules are numbered from `rule`.

Each rule's fragment ends in a `done` state labeled with its rule, and a chain of `split` states
chooses between the rules.
-/
private def translateRules (rule offset : Nat) (first : RegexAST) :
    List RegexAST → NFA
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
        -- The fragment exits to the state right after it, so this rule's `done` state goes here.
        #[.done rule] ++
        -- The remaining rules, starting right after this rule's `done` state.
        (translateRules (rule + 1) restStart next rest).nodes
    }

/--
Translates the rules `first :: rest` into a single `NFA` using Thompson's construction.

Rules are numbered by position, starting from `0` for `first`, and each accepting state is labeled
with its rule.
-/
-- Taking `first` separately guarantees there is at least one rule.
def NFA.ofRules (first : RegexAST) (rest : List RegexAST) : NFA :=
  translateRules 0 0 first rest

end Lexgen.Internal
