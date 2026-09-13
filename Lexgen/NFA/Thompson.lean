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
    {
      start  := startState,
      trans  := [(startState, .ε, startState + 1)],
      accept := startState + 1
    }
  | .symbol c =>
    {
      start  := startState,
      trans  := [(startState, .char c, startState + 1)],
      accept := startState + 1
    }
  | .dot =>
    {
      start  := startState,
      trans  := [(startState, .dot, startState + 1)],
      accept := startState + 1
    }
  | .concat first rest =>
    let fstNFA := translate startState first
    let sndNFA := translate fstNFA.accept rest
    {
      start  := fstNFA.start,
      trans  := fstNFA.trans ++ sndNFA.trans,
      accept := sndNFA.accept
    }
  | .repeated regex =>
    let subNFA   := translate (startState + 1) regex
    let endState := subNFA.accept + 1
    {
      start := startState,
      trans :=
        subNFA.trans ++
        [ (startState, .ε, subNFA.start),
          (subNFA.accept, .ε, endState),
          (subNFA.accept, .ε, subNFA.start),
          (startState, .ε, endState)
        ],
      accept := endState
    }
  | .alt left right =>
    let leftNFA  := translate (startState + 1) left
    let rightNFA := translate (leftNFA.accept + 1) right
    let endState := rightNFA.accept + 1
    {
      start := startState,
      trans :=
        leftNFA.trans ++
        rightNFA.trans ++
        [ (startState, .ε, leftNFA.start),
          (startState, .ε, rightNFA.start),
          (leftNFA.accept, .ε, endState),
          (rightNFA.accept, .ε, endState)
        ],
      accept := endState
    }

/--
Translates a `RegularExprAST` into an `NFA` using Thompson's construction.
-/
def reToNFA (regex : RegularExprAST) : NFA := translate 0 regex
