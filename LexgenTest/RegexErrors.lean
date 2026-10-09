/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
import Lexgen

/-!
# Tests on regex errors

Errors on invalid patterns and on patterns that match the empty string.
-/

/--
error: this rule matches the empty string, so the lexer would loop on it
---
error: this rule matches the empty string, so the lexer would loop on it
---
error: this rule matches the empty string, so the lexer would loop on it
---
error: this rule matches the empty string, so the lexer would loop on it
---
error: offset 2: bad escape (end of pattern or unknown escape)
---
error: offset 2: bad escape (end of pattern or unknown escape)
---
error: offset 3: bad escape (end of pattern or unknown escape)
---
error: offset 3: unescaped `[` in character class
---
error: offset 3: unescaped `^` in character class
---
error: offset 2: unescaped `-` in character class
---
error: offset 5: invalid range `a-`: a named class cannot bound a range
---
error: offset 3: missing upper bound of range `a-`
---
error: offset 4: invalid range `z-a`: upper bound less than lower bound
---
error: offset 3: missing `]`, unterminated character class
---
error: offset 2: empty character class
---
error: offset 3: empty character class
---
error: offset 5: invalid range `{3,1`: maximum less than minimum
---
error: offset 3: missing `}`, unterminated quantifier
---
error: offset 2: missing number after `{`
---
error: offset 2: missing number after `{`
---
error: offset 2: missing number after `{`
---
error: offset 1: nothing to repeat
---
error: offset 1: nothing to repeat
---
error: offset 1: nothing to repeat
---
error: offset 3: nothing to repeat
---
error: offset 3: nothing to repeat
---
error: offset 2: missing `)`, unterminated subpattern
---
error: offset 2: unmatched `)`
---
error: offset 2: unmatched `]`
---
error: offset 2: unmatched `}`
-/
#guard_msgs in
lexer Token where
  | rule₁  := r"a*"      -- matches the empty string
  | rule₂  := r"b"       -- no error
  | rule₃  := r"c?"      -- matches the empty string
  | rule₄  := r"(d|e)*"  -- matches the empty string
  | rule₅  := r"e{0,3}"  -- matches the empty string
  | rule₆  := r"a\q"     -- bad escape
  | rule₇  := r"a\"      -- bad escape at the end
  | rule₈  := r"[a\q]"   -- bad escape in a class
  | rule₉  := r"[a[]"    -- unescaped `[`
  | rule₁₀ := r"[a^]"    -- unescaped `^`
  | rule₁₁ := r"[-a]"    -- unescaped `-`
  | rule₁₂ := r"[a-\d]"  -- named class as a range bound
  | rule₁₃ := r"[a-]"    -- missing upper bound of a range
  | rule₁₄ := r"[z-a]"   -- reversed range
  | rule₁₅ := r"[ab"     -- missing `]`
  | rule₁₆ := r"[]"      -- empty class
  | rule₁₇ := r"[^]"     -- empty negated class
  | rule₁₈ := r"a{3,1}"  -- maximum less than minimum
  | rule₁₉ := r"a{3"     -- missing `}`
  | rule₂₀ := r"a{"      -- missing number after `{`
  | rule₂₁ := r"a{x}"    -- missing number after `{`
  | rule₂₂ := r"a{,3}"   -- missing number after `{`
  | rule₂₃ := r"*a"      -- nothing to repeat
  | rule₂₄ := r"+a"      -- nothing to repeat
  | rule₂₅ := r"?a"      -- nothing to repeat
  | rule₂₆ := r"{2}a"    -- nothing to repeat
  | rule₂₇ := r"a**"     -- nothing to repeat
  | rule₂₈ := r"(a"      -- missing `)`
  | rule₂₉ := r"a)"      -- unmatched `)`
  | rule₃₀ := r"a]"      -- unmatched `]`
  | rule₃₁ := r"a}"      -- unmatched `}`
