# Lexgen

## Description

Lexgen is a compile-time, declarative, regex-based lexer generator for the Lean programming language.

## Goals

* Simplify lexer creation.
* Make generated lexers faster than hand-written solutions.

## Installation

Add Lexgen as a dependency in `lakefile.toml`:

```toml
[[require]]
name = "lexgen"
git = "https://github.com/user470750/lexgen"
rev = "main"
```

or in `lakefile.lean`:

```lean
require lexgen from git "https://github.com/user470750/lexgen" @ "main"
```

Make sure that your project uses the same Lean toolchain as Lexgen, given in its
[`lean-toolchain`](lean-toolchain). Then import the library:

```lean
import Lexgen
```

## Usage

Lexers are declared with the `lexer` command, which is in essence a small DSL for
lexers embedded in Lean. It pairs each constructor with the regex pattern it
corresponds to (and, for constructors carrying a value, a user-defined function
converting the matched text), and expands into an ordinary Lean inductive together
with the lexer itself. Patterns are written as raw string literals, so regex escapes
need no extra backslashes. Text matching a `skip` rule, such as whitespace, is
dropped. An optional `deriving` clause derives instances for the token type, as for
any inductive. As an example, tokens for a JSON lexer:

```lean
lexer Token where
  skip r"[ \t\n\r]+"
  | lbrace          := r"\{"
  | rbrace          := r"\}"
  | lbracket        := r"\["
  | rbracket        := r"\]"
  | colon           := r":"
  | comma           := r","
  | jtrue           := r"true"
  | jfalse          := r"false"
  | null            := r"null"
  -- `stringToFloat` and `unquote` are user-defined conversions of the matched text
  | number : Float  := r"-?(0|[1-9]\d*)(\.\d+)?([eE][+\-]?\d+)?" => stringToFloat
  | string : String := r#""([^"\\]|\\(["\\/bfnrt]|u[0-9a-fA-F]{4}))*""# => unquote
deriving Repr, BEq
```

Such a declaration makes Lexgen generate a function `Token.lexer`, which creates a lexer
for a string. Its `tokens` function splits the input into an array of tokens, or returns
an error if some part of the input matches no rule:

```lean
#eval (Token.lexer r#"{"a": 1, "b": true}"#).tokens
-- Except.ok #[lbrace, string "a", colon, number 1, comma, string "b", colon, jtrue, rbrace]
```
