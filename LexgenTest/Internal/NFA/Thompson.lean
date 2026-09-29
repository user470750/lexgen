/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
import Lexgen.Internal.NFA.Thompson

/-!
# Tests for Thompson's construction

Checks the `NFA` produced by `NFA.ofRules` for basic regular expressions.
-/

open Lexgen.Internal

-- "a"
#guard
NFA.ofRules (.charClass false #[.single 'a']) [] =
{ nodes := #[.edge (.charClass false #[.single 'a']) 1, .done 0] }

-- "."
#guard
NFA.ofRules (.charClass true #[.single '\n']) [] =
{ nodes := #[.edge (.charClass true #[.single '\n']) 1, .done 0] }

-- ""
#guard
NFA.ofRules .ε [] = { nodes := #[.edge .ε 1, .done 0] }

-- "ab"
#guard
NFA.ofRules (.concat (.charClass false #[.single 'a']) (.charClass false #[.single 'b'])) [] =
{
  nodes := #[
    .edge (.charClass false #[.single 'a']) 1,
    .edge (.charClass false #[.single 'b']) 2,
    .done 0
  ]
}

-- "a|b"
#guard
NFA.ofRules (.alt (.charClass false #[.single 'a']) (.charClass false #[.single 'b'])) [] =
{
  nodes := #[
    .split 1 3,
    .edge (.charClass false #[.single 'a']) 2,
    .edge .ε 5,
    .edge (.charClass false #[.single 'b']) 4,
    .edge .ε 5,
    .done 0
  ]
}

-- "a*"
#guard
NFA.ofRules (.repeated (.charClass false #[.single 'a'])) [] =
{ nodes := #[.split 1 3, .edge (.charClass false #[.single 'a']) 2, .split 1 3, .done 0] }

-- Several rules.

-- "a", "b"
#guard
NFA.ofRules (.charClass false #[.single 'a']) [.charClass false #[.single 'b']] =
{
  nodes := #[
    .split 1 3,
    .edge (.charClass false #[.single 'a']) 2,
    .done 0,
    .edge (.charClass false #[.single 'b']) 4,
    .done 1
  ]
}

-- "a", "b", "c"
#guard
NFA.ofRules
  (.charClass false #[.single 'a'])
  [.charClass false #[.single 'b'], .charClass false #[.single 'c']] =
{
  nodes := #[
    .split 1 3,
    .edge (.charClass false #[.single 'a']) 2,
    .done 0,
    .split 4 6,
    .edge (.charClass false #[.single 'b']) 5,
    .done 1,
    .edge (.charClass false #[.single 'c']) 7,
    .done 2
  ]
}
