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

open Lexgen.Internal

-- The `NFA` inputs are the ones produced by Thompson's construction for the
-- regexes given in the comments.

-- Each row is total: one entry per alphabet interval. State 0 is always the trap —
-- the empty set of `NFA` states, reached when no edge matches — looping back to
-- itself, and state 1 is the start state.

/--
The characters above the surrogates: the last interval of every alphabet.
-/
private def aboveSurrogates : CharClass := .range '\uE000' (Char.ofNat 0x10FFFF)

-- "a"
#guard
DFA.ofNFA { nodes := #[.edge (.charClass false #[.single 'a']) 1, .done 0] } ==
{
  trans := #[
    #[(.range '\x00' '`', 0), (.single 'a', 0), (.range 'b' '\uD7FF', 0), (aboveSurrogates, 0)],
    #[(.range '\x00' '`', 0), (.single 'a', 2), (.range 'b' '\uD7FF', 0), (aboveSurrogates, 0)],
    #[(.range '\x00' '`', 0), (.single 'a', 0), (.range 'b' '\uD7FF', 0), (aboveSurrogates, 0)]
  ],
  accepting := Std.HashMap.ofList [(2, 0)]
}

-- "."
#guard
DFA.ofNFA { nodes := #[.edge (.charClass true #[.single '\n']) 1, .done 0] } ==
{
  trans := #[
    #[
      (.range '\x00' '\t', 0), (.single '\n', 0), (.range '\x0b' '\uD7FF', 0),
      (aboveSurrogates, 0)
    ],
    #[
      (.range '\x00' '\t', 2), (.single '\n', 0), (.range '\x0b' '\uD7FF', 2),
      (aboveSurrogates, 2)
    ],
    #[
      (.range '\x00' '\t', 0), (.single '\n', 0), (.range '\x0b' '\uD7FF', 0),
      (aboveSurrogates, 0)
    ]
  ],
  accepting := Std.HashMap.ofList [(2, 0)]
}

-- The `NFA` has no `charClass` edges, so the alphabet is just the characters below and
-- above the surrogates, and every state goes to the trap on them. State 1 accepts because
-- its ε-closure reaches `done`.
#guard
DFA.ofNFA { nodes := #[.edge .ε 1, .done 0] } ==
{
  trans := #[
    #[(.range '\x00' '\uD7FF', 0), (aboveSurrogates, 0)],
    #[(.range '\x00' '\uD7FF', 0), (aboveSurrogates, 0)]
  ],
  accepting := Std.HashMap.ofList [(1, 0)]
}

-- "ab"
#guard
DFA.ofNFA {
  nodes := #[
    .edge (.charClass false #[.single 'a']) 1,
    .edge (.charClass false #[.single 'b']) 2,
    .done 0
  ]
} ==
{
  trans := #[
    #[
      (.range '\x00' '`', 0), (.single 'a', 0), (.single 'b', 0), (.range 'c' '\uD7FF', 0),
      (aboveSurrogates, 0)
    ],
    #[
      (.range '\x00' '`', 0), (.single 'a', 2), (.single 'b', 0), (.range 'c' '\uD7FF', 0),
      (aboveSurrogates, 0)
    ],
    #[
      (.range '\x00' '`', 0), (.single 'a', 0), (.single 'b', 3), (.range 'c' '\uD7FF', 0),
      (aboveSurrogates, 0)
    ],
    #[
      (.range '\x00' '`', 0), (.single 'a', 0), (.single 'b', 0), (.range 'c' '\uD7FF', 0),
      (aboveSurrogates, 0)
    ]
  ],
  accepting := Std.HashMap.ofList [(3, 0)]
}

-- "a|b"
#guard
DFA.ofNFA {
  nodes := #[
    .split 1 3,
    .edge (.charClass false #[.single 'a']) 2,
    .edge .ε 5,
    .edge (.charClass false #[.single 'b']) 4,
    .edge .ε 5,
    .done 0
  ]
} ==
{
  trans := #[
    #[
      (.range '\x00' '`', 0), (.single 'a', 0), (.single 'b', 0), (.range 'c' '\uD7FF', 0),
      (aboveSurrogates, 0)
    ],
    #[
      (.range '\x00' '`', 0), (.single 'a', 2), (.single 'b', 3), (.range 'c' '\uD7FF', 0),
      (aboveSurrogates, 0)
    ],
    #[
      (.range '\x00' '`', 0), (.single 'a', 0), (.single 'b', 0), (.range 'c' '\uD7FF', 0),
      (aboveSurrogates, 0)
    ],
    #[
      (.range '\x00' '`', 0), (.single 'a', 0), (.single 'b', 0), (.range 'c' '\uD7FF', 0),
      (aboveSurrogates, 0)
    ]
  ],
  accepting := Std.HashMap.ofList [(2, 0), (3, 0)]
}

-- "a*"
#guard
DFA.ofNFA {
  nodes := #[.split 1 3, .edge (.charClass false #[.single 'a']) 2, .split 1 3, .done 0]
} ==
{
  trans := #[
    #[(.range '\x00' '`', 0), (.single 'a', 0), (.range 'b' '\uD7FF', 0), (aboveSurrogates, 0)],
    #[(.range '\x00' '`', 0), (.single 'a', 2), (.range 'b' '\uD7FF', 0), (aboveSurrogates, 0)],
    #[(.range '\x00' '`', 0), (.single 'a', 2), (.range 'b' '\uD7FF', 0), (aboveSurrogates, 0)]
  ],
  accepting := Std.HashMap.ofList [(1, 0), (2, 0)]
}

-- The two cases below mix a literal with `.`, so reading `'a'` has to follow
-- the edge of `.` as well, not only the edge of the literal.

-- ".a"
#guard
DFA.ofNFA {
  nodes := #[
    .edge (.charClass true #[.single '\n']) 1,
    .edge (.charClass false #[.single 'a']) 2,
    .done 0
  ]
} ==
{
  trans := #[
    #[
      (.range '\x00' '\t', 0), (.single '\n', 0), (.range '\x0b' '`', 0), (.single 'a', 0),
      (.range 'b' '\uD7FF', 0), (aboveSurrogates, 0)
    ],
    #[
      (.range '\x00' '\t', 2), (.single '\n', 0), (.range '\x0b' '`', 2), (.single 'a', 2),
      (.range 'b' '\uD7FF', 2), (aboveSurrogates, 2)
    ],
    #[
      (.range '\x00' '\t', 0), (.single '\n', 0), (.range '\x0b' '`', 0), (.single 'a', 3),
      (.range 'b' '\uD7FF', 0), (aboveSurrogates, 0)
    ],
    #[
      (.range '\x00' '\t', 0), (.single '\n', 0), (.range '\x0b' '`', 0), (.single 'a', 0),
      (.range 'b' '\uD7FF', 0), (aboveSurrogates, 0)
    ]
  ],
  accepting := Std.HashMap.ofList [(3, 0)]
}

-- "a."
#guard
DFA.ofNFA {
  nodes := #[
    .edge (.charClass false #[.single 'a']) 1,
    .edge (.charClass true #[.single '\n']) 2,
    .done 0
  ]
} ==
{
  trans := #[
    #[
      (.range '\x00' '\t', 0), (.single '\n', 0), (.range '\x0b' '`', 0), (.single 'a', 0),
      (.range 'b' '\uD7FF', 0), (aboveSurrogates, 0)
    ],
    #[
      (.range '\x00' '\t', 0), (.single '\n', 0), (.range '\x0b' '`', 0), (.single 'a', 2),
      (.range 'b' '\uD7FF', 0), (aboveSurrogates, 0)
    ],
    #[
      (.range '\x00' '\t', 3), (.single '\n', 0), (.range '\x0b' '`', 3), (.single 'a', 3),
      (.range 'b' '\uD7FF', 3), (aboveSurrogates, 3)
    ],
    #[
      (.range '\x00' '\t', 0), (.single '\n', 0), (.range '\x0b' '`', 0), (.single 'a', 0),
      (.range 'b' '\uD7FF', 0), (aboveSurrogates, 0)
    ]
  ],
  accepting := Std.HashMap.ofList [(3, 0)]
}

-- Several rules.

-- "a", "."
#guard
DFA.ofNFA {
  nodes := #[
    .split 1 3,
    .edge (.charClass false #[.single 'a']) 2,
    .done 0,
    .edge (.charClass true #[.single '\n']) 4,
    .done 1
  ]
} ==
{
  trans := #[
    #[
      (.range '\x00' '\t', 0), (.single '\n', 0), (.range '\x0b' '`', 0), (.single 'a', 0),
      (.range 'b' '\uD7FF', 0), (aboveSurrogates, 0)
    ],
    #[
      (.range '\x00' '\t', 2), (.single '\n', 0), (.range '\x0b' '`', 2), (.single 'a', 3),
      (.range 'b' '\uD7FF', 2), (aboveSurrogates, 2)
    ],
    #[
      (.range '\x00' '\t', 0), (.single '\n', 0), (.range '\x0b' '`', 0), (.single 'a', 0),
      (.range 'b' '\uD7FF', 0), (aboveSurrogates, 0)
    ],
    #[
      (.range '\x00' '\t', 0), (.single '\n', 0), (.range '\x0b' '`', 0), (.single 'a', 0),
      (.range 'b' '\uD7FF', 0), (aboveSurrogates, 0)
    ]
  ],
  accepting := Std.HashMap.ofList [(2, 1), (3, 0)]
}
