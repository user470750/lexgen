/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
import Lexgen.NFA.Thompson

/-!
# Tests for Thompson's construction

Checks the `NFA` produced by `reToNFA` for basic regular expressions.
-/

-- "a"
#guard
reToNFA (.symbol 'a') 0 = { nodes := #[.edge (.char 'a') 1, .done 0] }

-- "."
#guard
reToNFA .dot 0 = { nodes := #[.edge .dot 1, .done 0] }

-- ""
#guard
reToNFA .ε 0 = { nodes := #[.edge .ε 1, .done 0] }

-- "ab"
#guard
reToNFA (.concat (.symbol 'a') (.symbol 'b')) 0 =
{ nodes := #[.edge (.char 'a') 1, .edge (.char 'b') 2, .done 0] }

-- "a|b"
#guard
reToNFA (.alt (.symbol 'a') (.symbol 'b')) 0 =
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
reToNFA (.repeated (.symbol 'a')) 0 =
{ nodes := #[.split 1 3, .edge (.char 'a') 2, .split 1 3, .done 0] }
