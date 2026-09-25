# Lexgen

## Overview

Lexgen is a compile-time, declarative, regex-based lexer generator for the Lean programming language.

## Goals

* Simplify lexer creation.
* Make generated lexers faster than hand-written solutions.

## Intended Usage

Lexers will be declared with the `lexer` command, which is in essence a small DSL for
lexers embedded in Lean. It pairs each constructor with the regex pattern it
corresponds to (and, for constructors carrying a value, a user-defined function
converting the matched text), and expands into an ordinary Lean inductive together
with the lexer itself. Patterns are written as raw string literals, so regex escapes
need no extra backslashes. Text matching a `skip` pattern, such as whitespace, is
dropped. As an example, tokens for a JSON lexer:

```lean
lexer Token where
  skip                                r"[ \t\n\r]+"
  | lbrace                            r"\{"
  | rbrace                            r"\}"
  | lbracket                          r"\["
  | rbracket                          r"\]"
  | colon                             r":"
  | comma                             r","
  | jtrue                             r"true"
  | jfalse                            r"false"
  | null                              r"null"
  -- `stringToFloat` and `unquote` are user-defined conversions of the matched text
  | number (n : Float)  stringToFloat r"-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?"
  | string (s : String) unquote       r#""([^"\\]|\\(["\\/bfnrt]|u[0-9a-fA-F]{4}))*""#
```

Once the `lexer` command is implemented, such a declaration will make Lexgen generate a
function that turns an input string into an array of tokens, or returns an error if
some part of the input matches no rule:

```lean
#eval Token.lex r#"{"a": 1, "b": true}"#
-- Except.ok #[lbrace, string "a", colon, number 1, comma, string "b", colon, jtrue, rbrace]
```

## Alternatives

* [lean-regex](https://github.com/pandaman64/lean-regex) — a formally verified regular expression engine. It has a broader scope than Lexgen but lacks a specialized API for generating lexers.
* [Regex](https://github.com/Applimu/Regex) — a model for regular expressions and string matching based on the Brzozowski derivative. Still in progress; suffers from the same limitation as the previous entry.
* Lexer-less parsers — in some cases, a separate lexer is unnecessary.
* [Flex](https://github.com/westes/flex) — a tool for generating scanners: programs that recognize lexical patterns in text. Lexgen provides a much more native solution for Lean 4.
* [logos](https://github.com/maciejhirsz/logos) — a lexer generator for Rust. Included here because it is closest in spirit to Lexgen and serves as a primary point of reference.

## Architecture

The project is organized around the stages of the regex → NFA → DFA → lexer pipeline.
All of them live under `Lexgen/Internal/`, since only the `lexer` command is meant for
users of the library:

```
Regex/        -- AST, regex parser, desugaring
NFA/          -- NFA construction (Thompson's construction)
DFA/          -- NFA -> DFA conversion (subset construction)
              -- DFA optimization (Hopcroft's algorithm)
Pipeline.lean -- entry point of the pipeline, from rules to a single DFA
Codegen.lean  -- lexer code generation from the DFA
```

* **`Regex/`** — parses a regular expression into an AST and desugars extended syntax
  (e.g. quantifiers) down to a small core of primitive constructs.
* **`NFA/`** — builds an NFA from the desugared AST via Thompson's construction.
  The patterns of all rules are combined into one NFA.
* **`DFA/`** — determinizes the NFA into a DFA via subset construction, and will also
  cover DFA optimization via Hopcroft's algorithm (see Implementation Details).
  When several rules match the same text, the rule listed first wins.
* **`Codegen.lean`** — turns the DFA into the code of the generated lexer. The `lexer`
  command, built on Lean 4's macro system, will run the pipeline and this step at
  compile time (see Implementation Details).

Tests live in a separate library, `LexgenTest/`, where each module mirrors the path of
the module it checks.

## Implementation Roadmap

- [x] Regex parsing
- [x] NFA construction (Thompson's construction)
- [x] NFA → DFA conversion (subset construction)
- [ ] Extending regex syntax with additional syntactic sugar (e.g. character classes)
- [ ] Lexer code generation
- [ ] Formal verification of pipeline correctness
- [ ] DFA optimization (minimization)

## Implementation Details

Lexgen is intended to be built on top of Lean 4's macro system: once code generation
is implemented, token definitions will be processed at elaboration time
(`Meta`/`TermElab`), so the regex → NFA → DFA pipeline will run during compilation,
and the resulting lexer will be emitted as ordinary Lean code with no runtime
dependency on Lexgen itself. This part (`Codegen/`) is not implemented yet — see
Implementation Roadmap.

Internally, the pipeline relies on two classical automata-theoretic constructions:

* **Thompson's construction** — builds an NFA from a regular expression by structural
  induction, using ε-transitions to compose sub-automata for concatenation, alternation,
  and Kleene star.
* **Subset construction** — determinizes the resulting NFA into a DFA by treating each
  DFA state as a set of NFA states, eliminating ε-transitions and non-determinism.

DFA optimization is not implemented yet. One standard approach is being considered
as a possible future direction for this step:

* **Hopcroft's algorithm** — minimizes the DFA by iteratively refining a partition of
  states into equivalence classes, in O(n log n).
