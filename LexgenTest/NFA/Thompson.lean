/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
import Lexgen.NFA.Thompson

/-!
# Tests for Thompson's construction

Checks the `NFA` produced by `NFA.ofRules` for basic regular expressions.
-/

-- "a"
#guard
NFA.ofRules (.symbol 'a') [] = { nodes := #[.edge (.char 'a') 1, .done 0] }

-- "."
#guard
NFA.ofRules .dot [] = { nodes := #[.edge .dot 1, .done 0] }

-- ""
#guard
NFA.ofRules .ε [] = { nodes := #[.edge .ε 1, .done 0] }

-- "ab"
#guard
NFA.ofRules (.concat (.symbol 'a') (.symbol 'b')) [] =
{ nodes := #[.edge (.char 'a') 1, .edge (.char 'b') 2, .done 0] }

-- "a|b"
#guard
NFA.ofRules (.alt (.symbol 'a') (.symbol 'b')) [] =
{
  nodes := #[
    .split 1 3,
    .edge (.char 'a') 2,
    .edge .ε 5,
    .edge (.char 'b') 4,
    .edge .ε 5,
    .done 0
  ]
}

-- "a*"
#guard
NFA.ofRules (.repeated (.symbol 'a')) [] =
{ nodes := #[.split 1 3, .edge (.char 'a') 2, .split 1 3, .done 0] }

-- Several rules.

-- "a", "b"
#guard
NFA.ofRules (.symbol 'a') [.symbol 'b'] =
{ nodes := #[.split 1 3, .edge (.char 'a') 2, .done 0, .edge (.char 'b') 4, .done 1] }

-- "a", "b", "c"
#guard
NFA.ofRules (.symbol 'a') [.symbol 'b', .symbol 'c'] =
{
  nodes := #[
    .split 1 3,
    .edge (.char 'a') 2,
    .done 0,
    .split 4 6,
    .edge (.char 'b') 5,
    .done 1,
    .edge (.char 'c') 7,
    .done 2
  ]
}
