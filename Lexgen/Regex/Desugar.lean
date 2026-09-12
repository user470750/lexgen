import Lexgen.Regex.Syntax
import Lexgen.Regex.Ast

private def repeatConcat (n : Nat) (re : RegularExprAST) : RegularExprAST :=
  match n with
  | 0     => .ε
  | m + 1 => (List.replicate m re).foldl .concat re

private def optionalTail (n : Nat) (re : RegularExprAST) : RegularExprAST :=
  repeatConcat n (.alt re .ε)

def desugar : ReSyntax → RegularExprAST
  | .alt left right =>
    .alt (desugar left) (desugar right)
  | .concat first rest =>
    .concat (desugar first) (desugar rest)
  | .repeatRe { min := n, max := none } re =>
    let desugared := desugar re
    .normalizedConcat (repeatConcat n desugared) (.repeated desugared)
  | .repeatRe { min := n, max := some m } re =>
    let desugared := desugar re
    .normalizedConcat
      (repeatConcat n desugared)
      (optionalTail (m - n) desugared)
  | .symbol c => .symbol c
  | .dot      => .dot
  | .ε        => .ε
