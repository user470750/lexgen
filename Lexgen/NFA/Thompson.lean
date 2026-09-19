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

`startState` is passed explicitly at every step.
-/
private def translate (startState : Nat) : RegularExprAST → NFA
  | .ε =>
    { nodes := #[.edge .ε (startState + 1)] }
  | .symbol c =>
    { nodes := #[.edge (.char c) (startState + 1)] }
  | .dot =>
    { nodes := #[.edge .dot (startState + 1)] }
  | .concat first rest =>
    let fstNFA := translate startState first
    let sndNFA := translate (startState + fstNFA.nodes.size) rest
    { nodes := fstNFA.nodes ++ sndNFA.nodes }
  | .repeated regex =>
    let subStart := startState + 1
    let subNFA   := translate subStart regex
    let endState := subStart + subNFA.nodes.size + 1
    {
      nodes :=
        #[.split subStart endState] ++
        subNFA.nodes ++
        #[.split subStart endState]
    }
  | .alt left right =>
    let leftStart  := startState + 1
    let leftNFA    := translate leftStart left
    let rightStart := leftStart + leftNFA.nodes.size + 1
    let rightNFA   := translate rightStart right
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
Translates a `RegularExprAST` into an `NFA` using Thompson's construction.
-/
def reToNFA (regex : RegularExprAST) : NFA :=
  let translated := translate 0 regex
  { nodes := translated.nodes ++ #[.done] }
