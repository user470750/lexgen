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

## AI Contribution

AI is used in this project to assist with code review, documentation edits, and
commit messages. However, when submitting a pull request, a contributor is expected
to fully understand their own code rather than relying on AI.

In addition, contributions follow the [AI policy of the Lean 4 project](https://github.com/leanprover/lean4/blob/1ae7988353baa92c0aab03617f49e2f6fe4aab1c/CONTRIBUTING.md#L61-L63):

> **AI Contributions**: Any assistance by Generative AI contributing to the final PR must be noted in the PR description.
> Authors are responsible for manually checking these contributions before opening a PR.
> PRs authored solely by AI are not welcome and may be closed without further comment.
