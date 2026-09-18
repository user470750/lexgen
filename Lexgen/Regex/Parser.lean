/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module
public import Std.Internal.Parsec
public import Std.Internal.Parsec.String
public import Lexgen.Regex.Syntax

public section

open Std.Internal.Parsec Std.Internal.Parsec.String

/-!
# Regular expression parser

Defines a recursive-descent parser for regular expressions,
built using parser combinators.
-/

/-
Parsers for special characters in the regular expression grammar.
-/
private def altSep             : Parser Unit := skipChar '|'
private def starQuantifier     : Parser Unit := skipChar '*'
private def plusQuantifier     : Parser Unit := skipChar '+'
private def questionQuantifier : Parser Unit := skipChar '?'
private def leftParen          : Parser Unit := skipChar '('
private def rightParen         : Parser Unit := skipChar ')'
private def leftBrace          : Parser Unit := skipChar '{'
private def rightBrace         : Parser Unit := skipChar '}'

/--
`dot := "."`

Parser for the dot metacharacter, matching any character.
-/
private def dot : Parser ReSyntax :=
  pchar '.' *> pure .dot

/--
String containing all metacharacters in regular expression grammar.
-/
private def metaChars : String := "\\|.*+?(){}"

/--
`escapedMeta := "\" metaChar`

Parser for escaped metacharacters.
-/
private def escapedMeta : Parser Char := do
  skipChar '\\'
  satisfy (metaChars.contains ·)

/--
Parser for escape sequences.
-/
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

/--
Parser for literal characters (except escaped).
-/
private def literalChar : Parser Char := satisfy (not $ metaChars.contains ·)

/--
`symbol := escapedMeta | simpleEscape | literalChar`

Parser for literal characters, escape sequences and escaped metacharacters.
-/
private def symbol : Parser ReSyntax := do
  let sym ← escapedMeta.attempt <|> simpleEscape.attempt <|> literalChar <|>
    (skipChar '\\' *> fail "bad escape (end of pattern or unknown escape)")
  pure $ .symbol sym

/--
`quantity := "{" digits ("," digits?)? "}"`

Parser for a quantity.
-/
private def rangeQuantifier : Parser Quantity := do
  leftBrace
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
  rightBrace <|> fail s!"missing }, unterminated quantifier"
  pure spec

/--
Takes the already-parsed atom `re`, parses a `Quantity`, and produces
a `repeatRe` CST node.
-/
private def buildQuantity (re : ReSyntax) : Parser ReSyntax := do
  let quantity ← rangeQuantifier
  pure $ .repeatRe quantity re

/--
Produces a descriptive error when a quantifier (`*`, `+`, `?`, `{...}`)
appears with no preceding atom to repeat.
-/
private def nothingToRepeat : Parser ReSyntax :=
  (discard starQuantifier     <|>
   discard plusQuantifier     <|>
   discard questionQuantifier <|>
   discard rangeQuantifier)
  *> fail "nothing to repeat"

mutual
/--
`alt := concat? ("|" concat?)*`

Parser for alternatives in the regular expression grammar. Top-level rule.
-/
private partial def altRe : Parser ReSyntax := do
  let left ← concatRe <|> (pure .ε)
  let alts ← many (altSep *> (concatRe <|> pure .ε))
  pure $ alts.foldl .alt left

/--
`concat := quantified+`

Parser for concatenation in the regular expression grammar.
-/
private partial def concatRe : Parser ReSyntax := do
  let first ← quantified
  let rest ← many quantified
  pure $ rest.foldl .concat first

/--
`quantified := atom ("*" | "+" | "?" | quantity)?`

Parser for a quantified atom.
-/
private partial def quantified : Parser ReSyntax := do
  let re ← atom
  starQuantifier     *> (pure $ .repeatRe zeroOrMore re)  <|>
  plusQuantifier     *> (pure $ .repeatRe oneOrMore re)   <|>
  questionQuantifier *> (pure $ .repeatRe optionalOne re) <|>
  buildQuantity re                                        <|>
  pure re

/--
`atom := symbol | dot | subExpr`

Parser for atoms in the regular expression grammar.
-/
private partial def atom : Parser ReSyntax :=
  symbol  <|>
  dot     <|>
  subExpr <|>
  nothingToRepeat

/--
`subExpr := "(" alt ")"`

Parser for an expression in parenthesis.
-/
private partial def subExpr : Parser ReSyntax :=
  leftParen *> altRe <* (rightParen <|> fail "missing ), unterminated subpattern")
end

/--
A recursive-descent parser for regular expressions.

Built using parser combinators. Produces a concrete syntax tree (`ReSyntax`).
-/
partial def parseRe : Parser ReSyntax := altRe <* eof
