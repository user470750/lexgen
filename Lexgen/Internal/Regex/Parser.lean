/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public import Lexgen.Internal.Regex.Syntax

import Std.Internal.Parsec
import Std.Internal.Parsec.String

public section

open Std.Internal.Parsec Std.Internal.Parsec.String

/-!
# Regular expression parser

Defines a recursive-descent parser for regular expressions, built using parser combinators.
-/

namespace Lexgen.Internal

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
private def leftBracket        : Parser Unit := skipChar '['
private def rightBracket       : Parser Unit := skipChar ']'
private def rangeSep           : Parser Unit := skipChar '-'

/--
`dot := "."`

Parser for the dot metacharacter, matching any character but `\n`.
-/
private def dot : Parser RegexSyntax :=
  pchar '.' *> pure .dot

/--
String containing all metacharacters in regular expression grammar.
-/
private def metaChars : String := "\\|.*+?()[]{}"

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
  return match c with
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
`classEscape := "\" ("d" | "D" | "w" | "W" | "s" | "S")`

Parser for escape sequences of named character classes.
-/
private def classEscape : Parser NamedClass := do
  skipChar '\\'
  let c ← satisfy ("dDwWsS".contains ·)
  return match c with
    | 'd' => .digit
    | 'D' => .nonDigit
    | 'w' => .word
    | 'W' => .nonWord
    | 's' => .space
    | _   => .nonSpace   -- `S`, due to the satisfy predicate

/--
Parser for literal characters (except escaped).
-/
private def literalChar : Parser Char := satisfy (!metaChars.contains ·)

/--
`symbol := escapedMeta | simpleEscape | literalChar`

Parser for literal characters, escape sequences and escaped metacharacters.
-/
private def symbol : Parser RegexSyntax := do
  let sym ← escapedMeta.attempt <|> simpleEscape.attempt <|> literalChar <|>
    (skipChar '\\' *> fail "bad escape (end of pattern or unknown escape)")
  return .symbol sym

/--
String containing all metacharacters in character class grammar. Other metacharacters are literal
inside a character class.
-/
private def classMetaChars : String := "\\[]^-"

/--
`escapedClassMeta := "\" (metaChar | classMetaChar)`

Parser for escaped metacharacters inside a character class.
-/
private def escapedClassMeta : Parser Char := do
  skipChar '\\'
  satisfy fun c => metaChars.contains c || classMetaChars.contains c

/--
Parser for literal characters inside a character class (except escaped).
-/
private def classLiteralChar : Parser Char := satisfy (!classMetaChars.contains ·)

/--
`classChar := escapedClassMeta | simpleEscape | classLiteralChar`

Parser for a character inside a character class.
-/
private def classChar : Parser Char :=
  escapedClassMeta.attempt <|> simpleEscape.attempt <|> classLiteralChar <|>
    satisfy ("[^-".contains ·) >>=
      (fun (c : Char) => (fail s!"unescaped {c} in character class"))    <|>
    (skipChar '\\' *> fail "bad escape (end of pattern or unknown escape)")

/--
Takes the already-parsed lower bound `lower`, parses `-` and the upper bound, and produces a `range`
item.
-/
private def charClassRange (lower : Char) : Parser ClassItem := do
  rangeSep
  let upper ←
    (classEscape.attempt *>
      fail s!"invalid range {lower.quoteCore}-: a named class cannot bound a range") <|>
    classChar <|>
    fail s!"missing upper bound of range {lower.quoteCore}-"
  let range := ClassItem.range lower upper
  if range.isWellFormed then
    pure range
  else
    fail s!"invalid range {lower.quoteCore}-{upper.quoteCore}: upper bound less than lower bound"

/--
`charClassItem := classChar ("-" classChar)?`

Parser for an item of a character class.
-/
private def charClassItem : Parser ClassItem := do
  let c ← classChar
  charClassRange c <|> pure (.single c)

/--
`classNegation := "^"?`

Parser for the optional negation of a character class: returns whether it is negated.
-/
private def classNegation : Parser Bool :=
  (skipChar '^' *> pure true) <|> pure false

/--
`namedClass := classEscape`

Parser for a named character class outside a character class, such as `\d`.
-/
private def namedClass : Parser RegexSyntax :=
  RegexSyntax.namedClass <$> classEscape.attempt

/--
`charClass := "[" classNegation (classEscape | charClassItem)+ "]"`

Parser for a character class.
-/
private def charClass : Parser RegexSyntax := do
  leftBracket
  let negate ← classNegation
  let items ← many ((ClassItem.named <$> classEscape.attempt) <|> charClassItem)
  rightBracket <|> fail "missing ], unterminated character class"
  if items.isEmpty then
    fail "empty character class"
  return .charClass negate items

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
        let q : Quantity := .between n m
        if q.inOrder then pure q
        else fail s!"invalid range \{{n},{m}}: maximum less than minimum"
      ) <|> pure (.atLeast n)
    ) <|> pure (.exactly n)
  rightBrace <|> fail s!"missing }, unterminated quantifier"
  return spec

/--
Takes the already-parsed atom `re`, parses a `Quantity`, and produces a `repeated` CST node.
-/
private def quantifiedByRange (re : RegexSyntax) : Parser RegexSyntax := do
  let quantity ← rangeQuantifier
  return .repeated quantity re

/--
Produces a descriptive error when a quantifier (`*`, `+`, `?`, `{...}`) appears with no preceding
atom to repeat.
-/
private def nothingToRepeat : Parser RegexSyntax :=
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
private partial def alt : Parser RegexSyntax := do
  let left ← concat <|> (pure .ε)
  let alts ← many (altSep *> (concat <|> pure .ε))
  -- `foldl` makes alternation left-associative: `a|b|c` is `alt (alt a b) c`.
  return alts.foldl .alt left

/--
`concat := quantified+`

Parser for concatenation in the regular expression grammar.
-/
private partial def concat : Parser RegexSyntax := do
  let first ← quantified
  let rest  ← many quantified
  -- `foldl` makes concatenation left-associative: `abc` is `concat (concat a b) c`.
  return rest.foldl .concat first

/--
`quantified := atom ("*" | "+" | "?" | quantity)?`

Parser for a quantified atom.
-/
private partial def quantified : Parser RegexSyntax := do
  let re ← atom
  starQuantifier     *> (pure $ .repeated .zeroOrMore re)  <|>
  plusQuantifier     *> (pure $ .repeated .oneOrMore re)   <|>
  questionQuantifier *> (pure $ .repeated .optionalOne re) <|>
  quantifiedByRange re                                     <|>
  pure re

/--
`atom := namedClass | symbol | dot | charClass | subExpr`

Parser for atoms in the regular expression grammar.
-/
private partial def atom : Parser RegexSyntax :=
  namedClass <|>
  symbol     <|>
  dot        <|>
  charClass  <|>
  subExpr    <|>
  nothingToRepeat

/--
`subExpr := "(" alt ")"`

Parser for an expression in parenthesis.
-/
private partial def subExpr : Parser RegexSyntax :=
  leftParen *> alt <* (rightParen <|> fail "missing ), unterminated subpattern")
end

/--
A recursive-descent parser for regular expressions.

Built using parser combinators. Produces a concrete syntax tree (`RegexSyntax`).
-/
private def regex : Parser RegexSyntax :=
  alt <*
    (eof <|>
    satisfy ("])}".contains ·) >>=
      (fun (c : Char) => fail s!"unmatched {c}"))

/--
An error of `parseRegex`.
-/
structure ParseRegexError where
  /--
  The byte offset in the pattern where parsing failed.
  -/
  offset : Nat
  /--
  The description of the error.
  -/
  msg    : String
deriving Repr

/--
Parses a regular expression `s` into a concrete syntax tree (`RegexSyntax`).
-/
def parseRegex (s : String) : Except ParseRegexError RegexSyntax :=
  match regex ⟨s, s.startPos⟩ with
  | .success _ syn => pure syn
  | .error it err  => throw { offset := it.2.offset.byteIdx, msg := toString err }

end Lexgen.Internal
