/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public import Lexgen.Internal.DFA.Automaton

public section

open Lean

/-!
# Lexer code generation

Defines the translation of a `DFA` into the code of the generated lexer.
-/
