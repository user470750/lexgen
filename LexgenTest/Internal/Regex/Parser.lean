/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
import Lexgen.Internal.Regex.Parser

/-!
# Tests for the regex parser

Checks the `RegexSyntax` produced by `parseRegex`, and that invalid patterns are rejected.
-/

open Lexgen.Internal

#guard (parseRegex r"a").toOption = some (.symbol 'a')

#guard (parseRegex r".").toOption = some .dot

#guard (parseRegex r"").toOption = some .ε

#guard
(parseRegex r"ab").toOption =
some (.concat (.symbol 'a') (.symbol 'b'))

-- Concatenation is left-associative.
#guard
(parseRegex r"abc").toOption =
some (.concat (.concat (.symbol 'a') (.symbol 'b')) (.symbol 'c'))

#guard (parseRegex r"a|b").toOption = some (.alt (.symbol 'a') (.symbol 'b'))

-- Alternation is left-associative.
#guard
(parseRegex r"a|b|c").toOption =
some (.alt (.alt (.symbol 'a') (.symbol 'b')) (.symbol 'c'))

#guard (parseRegex r"a|").toOption = some (.alt (.symbol 'a') .ε)

#guard (parseRegex r"|a").toOption = some (.alt .ε (.symbol 'a'))

#guard
(parseRegex r"a*").toOption =
some (.repeated { minimum := 0, maximum := none } (.symbol 'a'))

#guard
(parseRegex r"a+").toOption =
some (.repeated { minimum := 1, maximum := none } (.symbol 'a'))

#guard
(parseRegex r"a?").toOption =
some (.repeated { minimum := 0, maximum := some 1 } (.symbol 'a'))

#guard
(parseRegex r"a{3}").toOption =
some (.repeated { minimum := 3, maximum := some 3 } (.symbol 'a'))

#guard
(parseRegex r"a{2,}").toOption =
some (.repeated { minimum := 2, maximum := none } (.symbol 'a'))

#guard
(parseRegex r"a{2,4}").toOption =
some (.repeated { minimum := 2, maximum := some 4 } (.symbol 'a'))

-- A quantifier binds tighter than concatenation.

#guard
(parseRegex r"ab*").toOption =
some (.concat (.symbol 'a') (.repeated { minimum := 0, maximum := none } (.symbol 'b')))

#guard
(parseRegex r"(ab)*").toOption =
some (.repeated { minimum := 0, maximum := none } (.concat (.symbol 'a') (.symbol 'b')))

-- Escaped metacharacter.
#guard (parseRegex r"\.").toOption = some (.symbol '.')

-- Escape sequence.
#guard (parseRegex r"\n").toOption = some (.symbol '\n')

-- Invalid patterns.

/-- info: Except.error { offset := 1, msg := "nothing to repeat" } -/
#guard_msgs in
#eval parseRegex r"*"

/-- info: Except.error { offset := 2, msg := "missing ), unterminated subpattern" } -/
#guard_msgs in
#eval parseRegex r"(a"

/-- info: Except.error { offset := 2, msg := "unmatched )" } -/
#guard_msgs in
#eval parseRegex r"a)"

/-- info: Except.error { offset := 3, msg := "missing }, unterminated quantifier" } -/
#guard_msgs in
#eval parseRegex r"a{2"

/-- info: Except.error { offset := 5, msg := "invalid range {3,1}: maximum less than minimum" } -/
#guard_msgs in
#eval parseRegex r"a{3,1}"

/-- info: Except.error { offset := 1, msg := "bad escape (end of pattern or unknown escape)" } -/
#guard_msgs in
#eval parseRegex r"\q"

/-- info: Except.error { offset := 2, msg := "bad escape (end of pattern or unknown escape)" } -/
#guard_msgs in
#eval parseRegex r"a\"
