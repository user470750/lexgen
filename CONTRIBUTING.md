# Contributing

## Code Style and Documentation

**Follow the style of the module you are editing.** The conventions below are the
common ground, but when a module already does something in its own consistent way,
match it rather than introducing a new one.

**Documentation.**

* Every module with definitions starts with a module header:

  ```lean
  /-!
  # Thompson's construction

  Defines Thompson's construction: translating a
  `RegularExprAST` into an `NFA`.
  -/
  ```

* All declarations are documented with docstrings `/-- ... -/`: required for public
  ones (checked by `linter.missingDocs`), recommended for private ones whose purpose
  isn't obvious. Plain comments are for notes that don't describe a single
  declaration.
* Non-obvious steps inside a definition are explained with `--` comments; open
  questions are marked with `TODO:`.
* Identifiers and type names in comments are wrapped in backticks, e.g. `NFA`.

**Alignment.** Aligning `:=`, `:` and `=>` across adjacent `let` bindings, structure
fields and `match` arms is encouraged:

```lean
let leftStart  := offset + 1
let leftNFA    := translate leftStart left
let rightStart := leftStart + leftNFA.nodes.size + 1
```

```lean
| DFA.char c₁, .edge (NFA.char c₂) next => if c₁ == c₂ then [next] else []
| DFA.char _, .edge NFA.dot next        => [next]
| DFA.dot, .edge NFA.dot next           => [next]
```

When an arm's body goes on the next line, no padding is needed.

**Pattern matching.** When only one pattern matters and all other cases share a
fallback, `if let` is preferred over a `match` with a `_` arm:

```lean
if let .done rule := nfa.nodes[state]! then
  some rule
else
  none
```

Use `match` when several patterns need their own branches.

**Similar types.** Some types share constructor names: `char` and `dot` belong to
both `NFA.Edge` and `DFA.Symbol`, and `ReSyntax` repeats the names of
`RegularExprAST`. When two such types meet in one function, do not leave both to the
leading dot: name the type, so that it is clear which one is meant.

**Imports.** Use a plain `import` wherever possible, so that importing a module does
not pull in its dependencies. After `module`, leave a blank line, list the
`public import`s, leave another blank line, then list the plain `import`s. Sort each
group alphabetically.

**Tests.** New features and bug fixes are welcome to come with tests. Tests live in
the `LexgenTest` library, in a module whose path mirrors the module it checks:
`LexgenTest/Internal/NFA/Thompson.lean` checks `Lexgen/Internal/NFA/Thompson.lean`.
They are written as `#guard` commands:

```lean
-- "ab"
#guard
NFA.ofRules (.concat (.symbol 'a') (.symbol 'b')) [] =
{ nodes := #[.edge (.char 'a') 1, .edge (.char 'b') 2, .done 0] }
```

## Commit Messages

Commit messages follow [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/).

**Header.** `type(scope): summary`, where:

* `type` is one of `feat`, `fix`, `refactor`, `docs`, `style`, `test`, `ci`;
* `scope` names the affected module or part of the project, e.g. `DFA`, `parser`,
  `readme`;
* `summary` is a short imperative phrase in lowercase without a trailing period;
  identifiers are wrapped in backticks.

A breaking change is marked with `!` after the scope, e.g. `refactor(NFA)!: ...`, and
described in a `BREAKING CHANGE:` footer. Until the first release, breaking changes
are not marked.

**Body.** When a commit makes more than one change, list them explicitly, one per `*`
item. Where the reason for a change is not obvious, briefly explain it. The text
should not be redundant: don't restate the diff or add filler.

```
test(DFA): add `#guard` tests for the subset construction

* add tests for the same regexes as in `Lexgen.NFA.Thompson`, plus `".a"`
  and `"a."`
* derive `BEq` for `DFA`, needed for `#guard`; `DecidableEq` cannot be
  derived, since `Std.HashMap` has no instance
```

**Rebase merges.** Prefer rebase when merging pull requests. Squash merging was used
before, but it put too many changes into one commit.

**AI assistance.** If AI assisted with a commit, this must be stated explicitly: say
in the body what AI was used for, and add a `Co-Authored-By:` trailer naming the
model. See also [AI Contribution](#ai-contribution).

## AI Contribution

AI is used in this project to assist with code review, documentation edits, and
commit messages. However, when submitting a pull request, a contributor is expected
to fully understand their own code rather than relying on AI.

In addition, contributions follow the [AI policy of the Lean 4 project](https://github.com/leanprover/lean4/blob/1ae7988353baa92c0aab03617f49e2f6fe4aab1c/CONTRIBUTING.md#L61-L63):

> **AI Contributions**: Any assistance by Generative AI contributing to the final PR must be noted in the PR description.
> Authors are responsible for manually checking these contributions before opening a PR.
> PRs authored solely by AI are not welcome and may be closed without further comment.
