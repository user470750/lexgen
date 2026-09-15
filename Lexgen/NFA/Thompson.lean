import Lexgen.NFA.Automaton
import Lexgen.Regex.Ast

/-!
# Thompson's construction

Defines Thompson's construction: translating a
`RegularExprAST` into an `NFA`.
-/

/-
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

-- "a"
#guard
reToNFA (.symbol 'a') = { nodes := #[.edge (.char 'a') 1, .done] }

-- "."
#guard
reToNFA .dot = { nodes := #[.edge .dot 1, .done] }

-- ""
#guard
reToNFA .ε = { nodes := #[.edge .ε 1, .done] }

-- "ab"
#guard
reToNFA (.concat (.symbol 'a') (.symbol 'b')) =
{ nodes := #[.edge (.char 'a') 1, .edge (.char 'b') 2, .done] }

-- "a|b"
#guard
reToNFA (.alt (.symbol 'a') (.symbol 'b')) =
{
  nodes := #[
    .split 1 3,
    .edge (.char 'a') 2,
    .edge .ε 5,
    .edge (.char 'b') 4,
    .edge .ε 5,
    .done
  ]
}

-- "a*"
#guard
reToNFA (.repeated (.symbol 'a')) =
{ nodes := #[.split 1 3, .edge (.char 'a') 2, .split 1 3, .done] }
