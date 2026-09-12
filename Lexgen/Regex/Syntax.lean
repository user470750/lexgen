structure Quantity where
  min : Nat
  max : Option Nat

def zeroOrMore : Quantity := { min := 0, max := none }

def oneOrMore : Quantity := { min := 1, max := none }

def optionalOne : Quantity := { min := 0, max := some 1 }

def between (min : Nat) (max : Nat) : Quantity := { min, max }

def atLeast (min : Nat) : Quantity := { min, max := none }

def atMost (max : Nat) : Quantity := { min := 0, max }

def exactly (n : Nat) : Quantity := { min := n, max := n }

def inOrder : Quantity → Bool
  | { min := _, max := none }   => true
  | { min,      max := some m } => min <= m

inductive ReSyntax where
  | alt (left : ReSyntax) (right : ReSyntax)
  | concat (first : ReSyntax) (rest : ReSyntax)
  | repeatRe (quantity : Quantity) (re : ReSyntax)
  | symbol (c : Char)
  | dot
  | ε
