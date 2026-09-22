/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
import Lexgen.Internal.DFA.Construction

/-!
# Tests for the subset construction

Checks the `DFA` produced by `NFA.toDFA` for basic regular expressions.
-/

-- The `NFA` inputs are the ones produced by Thompson's construction for the
-- regexes given in the comments.

-- Each table is total: one entry per state and alphabet symbol. Most tables
-- therefore contain a trap state — the empty set of `NFA` states, reached
-- when no edge matches — looping back to itself.

-- "a"
#guard
NFA.toDFA { nodes := #[.edge (.char 'a') 1, .done 0] } ==
{
  trans := Std.HashMap.ofList [
    ((0, .char 'a'), 1),
    ((1, .char 'a'), 2),
    ((2, .char 'a'), 2)
  ],
  accepting := Std.HashMap.ofList [(1, 0)]
}

-- "."
#guard
NFA.toDFA { nodes := #[.edge .dot 1, .done 0] } ==
{
  trans := Std.HashMap.ofList [((0, .dot), 1), ((1, .dot), 2), ((2, .dot), 2)],
  accepting := Std.HashMap.ofList [(1, 0)]
}

-- The `NFA` has no `char`/`dot` edges, so the alphabet is empty and no
-- transitions are produced at all. State 0 accepts because its ε-closure
-- reaches `done`.
#guard
NFA.toDFA { nodes := #[.edge .ε 1, .done 0] } ==
{ trans := {}, accepting := Std.HashMap.ofList [(0, 0)] }

-- "ab"
#guard
NFA.toDFA { nodes := #[.edge (.char 'a') 1, .edge (.char 'b') 2, .done 0] } ==
{
  trans := Std.HashMap.ofList [
    ((0, .char 'a'), 1), ((0, .char 'b'), 2),
    ((1, .char 'a'), 2), ((1, .char 'b'), 3),
    ((2, .char 'a'), 2), ((2, .char 'b'), 2),
    ((3, .char 'a'), 2), ((3, .char 'b'), 2)
  ],
  accepting := Std.HashMap.ofList [(3, 0)]
}

-- "a|b"
#guard
NFA.toDFA {
  nodes := #[
    .split 1 3,
    .edge (.char 'a') 2,
    .edge .ε 5,
    .edge (.char 'b') 4,
    .edge .ε 5,
    .done 0
  ]
} ==
{
  trans := Std.HashMap.ofList [
    ((0, .char 'a'), 1), ((0, .char 'b'), 2),
    ((1, .char 'a'), 3), ((1, .char 'b'), 3),
    ((2, .char 'a'), 3), ((2, .char 'b'), 3),
    ((3, .char 'a'), 3), ((3, .char 'b'), 3)
  ],
  accepting := Std.HashMap.ofList [(1, 0), (2, 0)]
}

-- "a*"
#guard
NFA.toDFA { nodes := #[.split 1 3, .edge (.char 'a') 2, .split 1 3, .done 0] } ==
{
  trans := Std.HashMap.ofList [((0, .char 'a'), 1), ((1, .char 'a'), 1)],
  accepting := Std.HashMap.ofList [(0, 0), (1, 0)]
}

-- The two cases below mix a literal with `.`, so reading `'a'` has to follow
-- the `dot` edge as well, not only the `char` one.

-- ".a"
#guard
NFA.toDFA { nodes := #[.edge .dot 1, .edge (.char 'a') 2, .done 0] } ==
{
  trans := Std.HashMap.ofList [
    ((0, .dot), 1), ((0, .char 'a'), 1),
    ((1, .dot), 2), ((1, .char 'a'), 3),
    ((2, .dot), 2), ((2, .char 'a'), 2),
    ((3, .dot), 2), ((3, .char 'a'), 2)
  ],
  accepting := Std.HashMap.ofList [(3, 0)]
}

-- "a."
#guard
NFA.toDFA { nodes := #[.edge (.char 'a') 1, .edge .dot 2, .done 0] } ==
{
  trans := Std.HashMap.ofList [
    ((0, .dot), 1), ((0, .char 'a'), 2),
    ((1, .dot), 1), ((1, .char 'a'), 1),
    ((2, .dot), 3), ((2, .char 'a'), 3),
    ((3, .dot), 1), ((3, .char 'a'), 1)
  ],
  accepting := Std.HashMap.ofList [(3, 0)]
}

-- Several rules.

-- "a", "."
#guard
NFA.toDFA { nodes := #[.split 1 3, .edge (.char 'a') 2, .done 0, .edge .dot 4, .done 1] } ==
{
  trans := Std.HashMap.ofList [
    ((0, .dot), 1), ((0, .char 'a'), 2),
    ((1, .dot), 3), ((1, .char 'a'), 3),
    ((2, .dot), 3), ((2, .char 'a'), 3),
    ((3, .dot), 3), ((3, .char 'a'), 3)
  ],
  accepting := Std.HashMap.ofList [(1, 1), (2, 0)]
}
