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

* Public declarations (definitions, structures, inductives, their constructors and
  fields) get a docstring `/-- ... -/`.
* Private declarations get a plain comment `/- ... -/` instead of a docstring.
* Non-obvious steps inside a definition are explained with `--` comments; open
  questions are marked with `TODO:`.
* Identifiers and type names in comments are wrapped in backticks, e.g. `NFA`.

**Alignment.** Aligning `:=`, `:` and `=>` across adjacent `let` bindings, structure
fields and `match` arms is encouraged:

```lean
let leftStart  := startState + 1
let leftNFA    := translate leftStart left
let rightStart := leftStart + leftNFA.nodes.size + 1
```

```lean
match edgeLabel with
| .char ch => .char ch
| .dot     => .dot
```

When an arm's body goes on the next line, no padding is needed.

**Tests.** New features and bug fixes are welcome to come with tests. Tests live next
to the code they check as `#guard` commands:

```lean
-- "ab"
#guard
reToNFA (.concat (.symbol 'a') (.symbol 'b')) =
{ nodes := #[.edge (.char 'a') 1, .edge (.char 'b') 2, .done] }
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
described in a `BREAKING CHANGE:` footer.

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

**Squash merges.** Pull requests are squash-merged. The squash commit's header follows
the same format, with the PR number appended by GitHub, e.g.
`feat(DFA): add DFA conversion (#2)`; its body lists the branch's commits.

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
