import Lexgen.Regex.Syntax
import Lexgen.Regex.Ast
import Lexgen.Regex.Parser
import Lexgen.Regex.Desugar

def parse (s : String) : Except String RegularExprAST := do
  let syn ← parseRe.run s
  pure $ desugar syn

#eval parse "ab|a*|b+|c?|e{3,4}"
