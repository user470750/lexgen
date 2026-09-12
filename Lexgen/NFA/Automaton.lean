inductive Symbol where
  | ε
  | dot
  | char (c : Char)
deriving Repr

structure NFA where
  start  : Nat
  trans  : List (Nat × Symbol × Nat)
  accept : Nat
deriving Repr
