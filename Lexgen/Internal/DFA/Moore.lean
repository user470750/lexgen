/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public import Lexgen.Internal.DFA.Automaton

public section

/-!
# Minimizing states

Defines `DFA.minimize`: merging the equivalent states of a `DFA` with Moore's algorithm, so that the
generated lexer has fewer state functions.
-/

namespace Lexgen.Internal

/--
Splits the states of `dfa` into the groups that Moore's algorithm starts from: the states that
accept no rule and the accepting states of each rule.
-/
private def initialGroups (dfa : DFA) : Array (Array Nat) :=
  let states := Array.range dfa.trans.size
  -- Group `0` holds the states that accept no rule, group `rule + 1` the states that accept `rule`,
  -- up to the largest rule that some state accepts.
  -- Hence `+ 2`: one because rules are numbered from `0`, and one for group `0`.
  let emptyGroups := Array.replicate (dfa.accepting.values.max?.getD 0 + 2) #[]
  (states.foldl addState emptyGroups).filter (!·.isEmpty)
where
  /--
  Adds `state` to its group in `groups`: group `rule + 1` if it accepts `rule`, group `0` otherwise.
  -/
  addState (groups : Array (Array Nat)) (state : Nat) : Array (Array Nat) :=
    let group := (dfa.accepting[state]?.map (· + 1)).getD 0
    groups.modify group (·.push state)

/--
Returns the number of the group of every state in `groups`.
-/
private def groupOfStates (groups : Array (Array Nat)) : Array Nat :=
  groups.mapIdx (fun i group => group.map (·, i))
    |>.flatten
    |>.mergeSort (fun a b => a.1 ≤ b.1)
    |>.map (·.2)

/--
Splits every group of `groups` into the states that, on each interval, lead into the same group.
-/
private def refineGroups (dfa : DFA) (groups : Array (Array Nat)) : Array (Array Nat) :=
  let groupOf := groupOfStates groups
  groups.flatMap fun group => (split groupOf group).values.toArray
where
  /--
  Groups the states of `group` by the groups their transitions lead into, given the group of every
  state in `groupOf`.
  -/
  split (groupOf : Array Nat) (group : Array Nat) : Std.HashMap (Array Nat) (Array Nat) :=
    group.foldl (init := {}) fun subgroups state =>
      let targets := dfa.trans[state]!.map fun (_, target) => groupOf[target]!
      let states :=
        if let some states := subgroups[targets]? then
          states.push state
        else
          #[state]
      subgroups.insert targets states

/--
Refines `groups` until they stop changing, into the groups of states that no input tells apart.
-/
-- TODO: Prove termination. Every refinement adds groups, and there are no more groups than states.
private partial def stableGroups (dfa : DFA) (groups : Array (Array Nat)) : Array (Array Nat) :=
  let refined := refineGroups dfa groups
  if refined.size == groups.size then
    groups
  else
    stableGroups dfa refined

/--
Returns the minimal `DFA` of `dfa`, with one state for each group of states that no input tells
apart. That state takes the transitions of the first state of the group, each leading to the group
of its target, and the rule that the states of the group accept, if any.

All states of `dfa` must have their transitions on the same intervals in the same order, as after
`DFA.ofNFA`, so `dfa` must not be merged yet.
-/
-- TODO: Prove that all states share their intervals, together with the other invariants of `DFA`.
def DFA.minimize (dfa : DFA) : DFA :=
  -- Sorted by their first state, the group of the trap comes first and that of the start state
  -- second, so that their states keep the numbers `DFA.trap` and `DFA.start`.
  -- TODO: Prove that every group lists its states in ascending order, as `initialGroups` and
  -- `refineGroups` build them, so that its first state is its smallest.
  let ordered := (stableGroups dfa (initialGroups dfa)).qsort (·[0]! < ·[0]!)
  -- The start state is equivalent to the trap only if no input is accepted at all. Then there is
  -- nothing to minimize, and `DFA.start` still needs a state apart from `DFA.trap`.
  if ordered[0]!.contains DFA.start then
    dfa
  else
    let groupOf         := groupOfStates ordered
    let representatives := ordered.map (·[0]!)
    let trans  := representatives.map fun state =>
      dfa.trans[state]!.map fun (interval, target) => (interval, groupOf[target]!)
    -- Every state of a group accepts the same rule, since the first groups are split by rule.
    let accepting := Std.HashMap.ofList
      (dfa.accepting.toList.map fun (state, rule) => (groupOf[state]!, rule))
    { trans, accepting }

end Lexgen.Internal
