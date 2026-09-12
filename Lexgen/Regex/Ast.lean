inductive RegularExprAST where
  | alt (left : RegularExprAST) (right : RegularExprAST)
  | concat (first : RegularExprAST) (rest : RegularExprAST)
  | repeated (re : RegularExprAST)
  | symbol (c : Char)
  | dot
  | ε
deriving Repr

def RegularExprAST.normalizedConcat : RegularExprAST → RegularExprAST → RegularExprAST
  | .ε,   right => right
  | left, .ε    => left
  | left, right => .concat left right
