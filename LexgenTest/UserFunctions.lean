/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
import Lexgen
import LexgenTest.Basic

/-!
# Tests on user functions

Token values computed by user functions: numbers in several bases and string literals with escapes.
-/

open Lexgen

/--
The value of the digits in base `base` after a two-character prefix such as `0x`.
-/
def prefixedValue (base : Nat) (s : String.Slice) : Nat :=
  (s.toString.toList.drop 2).foldl (fun n c => base * n + digit c) 0
where
  digit (c : Char) : Nat :=
    if c.isDigit then
      c.toNat - '0'.toNat
    else
      c.toLower.toNat - 'a'.toNat + 10

/--
The contents of a string literal: the quotes dropped, the escapes `\n`, `\t`, `\r`, `\"` and `\\`
resolved.
-/
def unquote (s : String.Slice) : String :=
  String.ofList (go (s.toString.toList.drop 1).dropLast)
where
  go : List Char → List Char
    | '\\' :: 'n' :: rest => '\n' :: go rest
    | '\\' :: 't' :: rest => '\t' :: go rest
    | '\\' :: 'r' :: rest => '\r' :: go rest
    | '\\' :: c :: rest   => c :: go rest
    | c :: rest           => c :: go rest
    | []                  => []

lexer Token where
  skip r"\s+"
  | dec : Nat    := r"[0-9]+"         => (·.toString.toNat!)
  | hex : Nat    := r"0x[0-9a-fA-F]+" => prefixedValue 16
  | oct : Nat    := r"0o[0-7]+"       => prefixedValue 8
  | bin : Nat    := r"0b[01]+"        => prefixedValue 2
  | str : String := r#""([^"\\\n]|\\["\\ntr])*""# => unquote
deriving BEq

#guard lex Token "42 007 0x1F 0xff 0o17 0b101" == some #[
  (.dec 42,  "42",    0,  2),
  (.dec 7,   "007",   3,  6),
  (.hex 31,  "0x1F",  7,  11),
  (.hex 255, "0xff",  12, 16),
  (.oct 15,  "0o17",  17, 21),
  (.bin 5,   "0b101", 22, 27)
]

#guard lex Token r#""hello" "" "a\nb" "a\tb\rc" "say \"hi\"" "\\" "привет""# == some #[
  (.str "hello",      r#""hello""#,      0,  7),
  (.str "",           r#""""#,           8,  10),
  (.str "a\nb",       r#""a\nb""#,       11, 17),
  (.str "a\tb\rc",    r#""a\tb\rc""#,    18, 27),
  (.str "say \"hi\"", r#""say \"hi\"""#, 28, 40),
  (.str "\\",         r#""\\""#,         41, 45),
  (.str "привет",     r#""привет""#,     46, 60)
]
