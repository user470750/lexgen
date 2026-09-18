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
need no extra backslashes. As an example, tokens for a JSON lexer:

```lean
lexer Token where
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

Once code generation is implemented, such a declaration will make Lexgen generate a
function that turns an input string into a list of tokens:

```lean
#eval Token.lex r#"{"a": 1, "b": true}"#
-- [lbrace, string "a", colon, number 1, comma, string "b", colon, jtrue, rbrace]
```

## Alternatives

* [lean-regex](https://github.com/pandaman64/lean-regex) — a formally verified regular expression engine. It has a broader scope than Lexgen but lacks a specialized API for generating lexers.
* [Regex](https://github.com/Applimu/Regex) — a model for regular expressions and string matching based on the Brzozowski derivative. Still in progress; suffers from the same limitation as the previous entry.
* Lexer-less parsers — in some cases, a separate lexer is unnecessary.
* [Flex](https://github.com/westes/flex) — a tool for generating scanners: programs that recognize lexical patterns in text. Lexgen provides a much more native solution for Lean 4.
* [logos](https://github.com/maciejhirsz/logos) — a lexer generator for Rust. Included here because it is closest in spirit to Lexgen and serves as a primary point of reference.

## Architecture

The project is organized around the stages of the regex → NFA → DFA → lexer pipeline:

```
Regex/    -- AST, regex parser, desugaring                              [implemented]
NFA/      -- NFA construction (Thompson's construction)                 [implemented]
DFA/      -- NFA -> DFA conversion (subset construction)                [implemented]
          -- DFA optimization (Hopcroft's algorithm)                    [not implemented yet]
Codegen/  -- lexer code generation from the optimized DFA                [not implemented yet]
```

* **`Regex/`** — parses a regular expression into an AST and desugars extended syntax
  (e.g. character classes, quantifiers) down to a small core of primitive constructs.
  Implemented.
* **`NFA/`** — builds an NFA from the desugared AST via Thompson's construction.
  Implemented.
* **`DFA/`** — determinizes the NFA into a DFA via subset construction (implemented),
  and will also cover DFA optimization via Hopcroft's algorithm (see Implementation
  Details) — this part is not implemented yet.
* **`Codegen/`** — not implemented yet. Once built, this module will turn the
  (optimized) DFA into the actual generated lexer code, driven by Lean 4's macro
  system (see Implementation Details).

## Implementation Roadmap

- [x] Regex parsing
- [x] NFA construction (Thompson's construction)
- [x] NFA → DFA conversion (subset construction) (currently only in the `dfa-conversion` branch)
- [ ] Extending regex syntax with additional syntactic sugar (e.g. character classes, quantifiers)
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

## AI Contribution

AI is used in this project to assist with code review, documentation edits, and
commit messages. However, when submitting a pull request, a contributor is expected
to fully understand their own code rather than relying on AI.
