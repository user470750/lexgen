/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
import Lexgen.Internal.DFA.Construction

/-!
# Tests for the subset construction

Checks the `DFA` produced by `DFA.ofNFA` for basic regular expressions.
-/

-- The `NFA` inputs are the ones produced by Thompson's construction for the
-- regexes given in the comments.

-- Each row is total: one entry per alphabet symbol. State 0 is always the trap —
-- the empty set of `NFA` states, reached when no edge matches — looping back to
-- itself, and state 1 is the start state.

-- "a"
#guard
DFA.ofNFA { nodes := #[.edge (.char 'a') 1, .done 0] } ==
{
  trans := #[
    [(.char 'a', 0)],
    [(.char 'a', 2)],
    [(.char 'a', 0)]
  ],
  accepting := Std.HashMap.ofList [(2, 0)]
}

-- "."
#guard
DFA.ofNFA { nodes := #[.edge .dot 1, .done 0] } ==
{
  trans := #[[(.dot, 0)], [(.dot, 2)], [(.dot, 0)]],
  accepting := Std.HashMap.ofList [(2, 0)]
}

-- The `NFA` has no `char`/`dot` edges, so the alphabet is empty and every state
-- gets an empty row. State 1 accepts because its ε-closure reaches `done`.
#guard
DFA.ofNFA { nodes := #[.edge .ε 1, .done 0] } ==
{ trans := #[[], []], accepting := Std.HashMap.ofList [(1, 0)] }

-- "ab"
#guard
DFA.ofNFA { nodes := #[.edge (.char 'a') 1, .edge (.char 'b') 2, .done 0] } ==
{
  trans := #[
    [(.char 'b', 0), (.char 'a', 0)],
    [(.char 'b', 0), (.char 'a', 2)],
    [(.char 'b', 3), (.char 'a', 0)],
    [(.char 'b', 0), (.char 'a', 0)]
  ],
  accepting := Std.HashMap.ofList [(3, 0)]
}

-- "a|b"
#guard
DFA.ofNFA {
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
  trans := #[
    [(.char 'b', 0), (.char 'a', 0)],
    [(.char 'b', 3), (.char 'a', 2)],
    [(.char 'b', 0), (.char 'a', 0)],
    [(.char 'b', 0), (.char 'a', 0)]
  ],
  accepting := Std.HashMap.ofList [(2, 0), (3, 0)]
}

-- "a*"
#guard
DFA.ofNFA { nodes := #[.split 1 3, .edge (.char 'a') 2, .split 1 3, .done 0] } ==
{
  trans := #[[(.char 'a', 0)], [(.char 'a', 2)], [(.char 'a', 2)]],
  accepting := Std.HashMap.ofList [(1, 0), (2, 0)]
}

-- The two cases below mix a literal with `.`, so reading `'a'` has to follow
-- the `dot` edge as well, not only the `char` one.

-- ".a"
#guard
DFA.ofNFA { nodes := #[.edge .dot 1, .edge (.char 'a') 2, .done 0] } ==
{
  trans := #[
    [(.char 'a', 0), (.dot, 0)],
    [(.char 'a', 2), (.dot, 2)],
    [(.char 'a', 3), (.dot, 0)],
    [(.char 'a', 0), (.dot, 0)]
  ],
  accepting := Std.HashMap.ofList [(3, 0)]
}

-- "a."
#guard
DFA.ofNFA { nodes := #[.edge (.char 'a') 1, .edge .dot 2, .done 0] } ==
{
  trans := #[
    [(.char 'a', 0), (.dot, 0)],
    [(.char 'a', 2), (.dot, 0)],
    [(.char 'a', 3), (.dot, 3)],
    [(.char 'a', 0), (.dot, 0)]
  ],
  accepting := Std.HashMap.ofList [(3, 0)]
}

-- Several rules.

-- "a", "."
#guard
DFA.ofNFA { nodes := #[.split 1 3, .edge (.char 'a') 2, .done 0, .edge .dot 4, .done 1] } ==
{
  trans := #[
    [(.char 'a', 0), (.dot, 0)],
    [(.char 'a', 3), (.dot, 2)],
    [(.char 'a', 0), (.dot, 0)],
    [(.char 'a', 0), (.dot, 0)]
  ],
  accepting := Std.HashMap.ofList [(2, 1), (3, 0)]
}
