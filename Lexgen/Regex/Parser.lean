import Std.Internal.Parsec
import Std.Internal.Parsec.String
import Lexgen.Regex.Syntax

open Std.Internal.Parsec Std.Internal.Parsec.String

private def altSep             : Parser Char := pchar '|'
private def starQuantifier     : Parser Char := pchar '*'
private def plusQuantifier     : Parser Char := pchar '+'
private def questionQuantifier : Parser Char := pchar '?'
private def leftParen          : Parser Char := pchar '('
private def rightParen         : Parser Char := pchar ')'
private def leftBrace          : Parser Char := pchar '{'
private def rightBrace         : Parser Char := pchar '}'

private def dot : Parser ReSyntax :=
  pchar '.' *> pure ReSyntax.dot

private def escapedMeta : Parser Char := do
  skipChar '\\'
  satisfy ("|.*+?(){}".contains ·)

private def simpleEscape : Parser Char := do
  skipChar '\\'
  let c ← satisfy ("nrtfv0ae".contains ·)
  pure $ match c with
    | 'n' => '\n'
    | 'r' => '\r'
    | 't' => '\t'
    | 'f' => Char.ofNat 12   -- form feed
    | 'v' => Char.ofNat 11   -- vertical tab
    | '0' => Char.ofNat 0    -- null
    | 'a' => Char.ofNat 7    -- bell
    | 'e' => Char.ofNat 27   -- escape
    | _   => c               -- impossible due to satisfy predicate

private def literalChar : Parser Char := satisfy (not $ "\\|.*+?(){}".contains ·)

private def symbol : Parser ReSyntax := do
  let sym ← escapedMeta.attempt <|> simpleEscape.attempt <|> literalChar <|>
    (skipChar '\\' *> fail "bad escape (end of pattern or unknown escape)")
  pure $ ReSyntax.symbol sym

private def rangeQuantifier : Parser Quantity := do
  skipChar '{'
  let n ← digits
  let spec ← (do
      skipChar ','
      (do
        let m ← digits
        let q := between n m
        if inOrder q then pure q
        else fail s!"invalid range \{{n},{m}}: maximum less than minimum"
      ) <|> pure (atLeast n)
    ) <|> pure (exactly n)
  skipChar '}' <|> fail s!"missing }, unterminated quantifier"
  pure spec

private def nothingToRepeat : Parser ReSyntax :=
  (starQuantifier     <|>
   plusQuantifier     <|>
   questionQuantifier <|>
   rangeQuantifier *> pure ' ') *> fail "nothing to repeat"

mutual
private partial def altRe : Parser ReSyntax := do
  let left ← concatRe <|> (pure ReSyntax.ε)
  let alts ← many (altSep *> (concatRe <|> pure ReSyntax.ε))
  pure $ alts.foldl ReSyntax.alt left

private partial def concatRe : Parser ReSyntax := do
  let first ← quantified
  let rest ← many quantified
  pure $ rest.foldl ReSyntax.concat first

private partial def buildQuantity (re : ReSyntax) : Parser ReSyntax := do
  let quantity ← rangeQuantifier
  pure $ ReSyntax.repeatRe quantity re

private partial def quantified : Parser ReSyntax := do
  let re ← atom
  starQuantifier     *> (pure $ ReSyntax.repeatRe zeroOrMore re)  <|>
  plusQuantifier     *> (pure $ ReSyntax.repeatRe oneOrMore re)   <|>
  questionQuantifier *> (pure $ ReSyntax.repeatRe optionalOne re) <|>
  buildQuantity re                                                <|>
  pure re

private partial def atom : Parser ReSyntax :=
  symbol  <|>
  dot     <|>
  subExpr <|>
  nothingToRepeat

private partial def subExpr : Parser ReSyntax :=
  leftParen *> altRe <* (rightParen <|> fail "missing ), unterminated subpattern")
end

partial def parseRe : Parser ReSyntax := altRe <* eof
