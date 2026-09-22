/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

/-!
# Pipeline

Will define the entry point of the regex → NFA → DFA pipeline: a function
turning the rules of a `lexer` declaration into a single `DFA`.

It is the only declaration of the pipeline that `Codegen` needs, and once code
generation is implemented, `Codegen` will be the only module importing this one.
-/

-- TODO: Define `rulesToDFA`, translating the rules of a `lexer` declaration
-- into a single `DFA`.
