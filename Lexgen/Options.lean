/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public import Lean.Data.Options

public section

/-!
# Options

Declares the options that control the `lexer` command, set with `set_option`.
-/

/--
Enables Moore minimization of the DFA built by the `lexer` command.
-/
register_option lexgen.minimization : Bool := {
  defValue := true
  descr := "Moore DFA minimization"
}
