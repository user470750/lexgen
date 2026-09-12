/-!
# Regex AST

Defines the abstract syntax tree (`RegularExprAST`)
for regular expressions.
-/

/--
The AST representation of regular expressions.

AST covers only basic regex constructs, while
others are desugared into them.
-/
inductive RegularExprAST where
  /--
  Matches an alternation of regular expressions.
  -/
  | alt (left : RegularExprAST) (right : RegularExprAST)
  /--
  Matches a concatenation of regular expressions.
  -/
  | concat (first : RegularExprAST) (rest : RegularExprAST)
  /--
  Matches repetition of a regular expression.
  -/
  | repeated (re : RegularExprAST)
  /--
  Matches the literal character `c`.
  -/
  | symbol (c : Char)
  /--
  Matches any single character.
  -/
  | dot
  /--
  Matches the empty string.
  -/
  | ε
deriving Repr

/--
Wraps `concat` to avoid useless concatenation with ε.
-/
def RegularExprAST.normalizedConcat : RegularExprAST → RegularExprAST → RegularExprAST
  | .ε,   rest  => rest
  | first, .ε   => first
  | first, rest => .concat first rest
