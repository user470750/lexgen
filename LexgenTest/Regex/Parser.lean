/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
import Lexgen.Regex.Parser

/-!
# Tests for the regular expression parser

Checks the `ReSyntax` produced by `parseRe`, and that invalid patterns are
rejected.
-/

-- "a"
#guard (parseRe.run r"a").toOption = some (.symbol 'a')

-- "."
#guard (parseRe.run r".").toOption = some .dot

-- ""
#guard (parseRe.run r"").toOption = some .ε

-- "ab"
#guard
(parseRe.run r"ab").toOption =
some (.concat (.symbol 'a') (.symbol 'b'))

-- "abc"
-- Concatenation is left-associative.
#guard
(parseRe.run r"abc").toOption =
some (.concat (.concat (.symbol 'a') (.symbol 'b')) (.symbol 'c'))

-- "a|b"
#guard (parseRe.run r"a|b").toOption = some (.alt (.symbol 'a') (.symbol 'b'))

-- "a|b|c"
-- Alternation is left-associative.
#guard
(parseRe.run r"a|b|c").toOption =
some (.alt (.alt (.symbol 'a') (.symbol 'b')) (.symbol 'c'))

-- "a|"
#guard (parseRe.run r"a|").toOption = some (.alt (.symbol 'a') .ε)

-- "|a"
#guard (parseRe.run r"|a").toOption = some (.alt .ε (.symbol 'a'))

-- "a*"
#guard
(parseRe.run r"a*").toOption =
some (.repeatRe { minimum := 0, maximum := none } (.symbol 'a'))

-- "a+"
#guard
(parseRe.run r"a+").toOption =
some (.repeatRe { minimum := 1, maximum := none } (.symbol 'a'))

-- "a?"
#guard
(parseRe.run r"a?").toOption =
some (.repeatRe { minimum := 0, maximum := some 1 } (.symbol 'a'))

-- "a{3}"
#guard
(parseRe.run r"a{3}").toOption =
some (.repeatRe { minimum := 3, maximum := some 3 } (.symbol 'a'))

-- "a{2,}"
#guard
(parseRe.run r"a{2,}").toOption =
some (.repeatRe { minimum := 2, maximum := none } (.symbol 'a'))

-- "a{2,4}"
#guard
(parseRe.run r"a{2,4}").toOption =
some (.repeatRe { minimum := 2, maximum := some 4 } (.symbol 'a'))

-- A quantifier binds tighter than concatenation.

-- "ab*"
#guard
(parseRe.run r"ab*").toOption =
some (.concat (.symbol 'a') (.repeatRe { minimum := 0, maximum := none } (.symbol 'b')))

-- "(ab)*"
#guard
(parseRe.run r"(ab)*").toOption =
some (.repeatRe { minimum := 0, maximum := none } (.concat (.symbol 'a') (.symbol 'b')))

-- "\."
-- Escaped metacharacter.
#guard (parseRe.run r"\.").toOption = some (.symbol '.')

-- "\n"
-- Escape sequence.
#guard (parseRe.run r"\n").toOption = some (.symbol '\n')

-- Invalid patterns.

-- "*"
/-- info: Except.error "offset 1: nothing to repeat" -/
#guard_msgs in
#eval parseRe.run r"*"

-- "(a"
/-- info: Except.error "offset 2: missing ), unterminated subpattern" -/
#guard_msgs in
#eval parseRe.run r"(a"

-- "a)"
/-- info: Except.error "offset 1: expected end of input" -/
#guard_msgs in
#eval parseRe.run r"a)"

-- "a{2"
/-- info: Except.error "offset 3: missing }, unterminated quantifier" -/
#guard_msgs in
#eval parseRe.run r"a{2"

-- "a{3,1}"
/-- info: Except.error "offset 5: invalid range {3,1}: maximum less than minimum" -/
#guard_msgs in
#eval parseRe.run r"a{3,1}"

-- "\q"
/-- info: Except.error "offset 1: bad escape (end of pattern or unknown escape)" -/
#guard_msgs in
#eval parseRe.run r"\q"

-- "a\"
/-- info: Except.error "offset 2: bad escape (end of pattern or unknown escape)" -/
#guard_msgs in
#eval parseRe.run r"a\"
