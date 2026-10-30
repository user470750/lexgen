/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public section

/-!
# Character class items

Defines `ClassItem`, an item of a character class in the CST, `NamedClass`, the named classes it
can refer to, and `ClassItem.normalize`, which turns the items of a class into non-overlapping
ranges in ascending order.
-/

namespace Lexgen.Internal

/--
A named character class. The classes are ASCII.
-/
inductive NamedClass where
  /--
  `\d`: a digit.
  -/
  | digit
  /--
  `\D`: any character but a digit.
  -/
  | nonDigit
  /--
  `\w`: a word character.
  -/
  | word
  /--
  `\W`: any character but a word character.
  -/
  | nonWord
  /--
  `\s`: a whitespace character.
  -/
  | space
  /--
  `\S`: any character but a whitespace character.
  -/
  | nonSpace
deriving Repr, DecidableEq

/--
A range of characters from `lower` to `upper`, inclusive.
-/
structure RangeItem where
  /--
  The lower bound of the range.
  -/
  lower : Char
  /--
  The upper bound of the range.
  -/
  upper : Char
  /--
  Proves that the bounds of the range are valid.
  -/
  wellFormed : lower ≤ upper
deriving Repr, DecidableEq

/--
An item of a character class in the CST.
-/
inductive ClassItem where
  /--
  Matches the literal character `character`.
  -/
  | single (character : Char)
  /--
  Matches a character from the range `r`.
  -/
  | range (r : RangeItem)
  /--
  Matches a character of the named class `kind`.
  -/
  | named (kind : NamedClass)
deriving Repr, DecidableEq

/--
Returns the ranges of the named character class `kind`.
-/
def NamedClass.ranges (kind : NamedClass) : Array RangeItem :=
  let max := Char.ofNat 0x10FFFF
  match kind with
  | .digit => #[⟨'0', '9', by decide⟩]
  | .nonDigit =>
    #[
      ⟨'\x00', '/', by decide⟩,
      ⟨':', max, by decide⟩
    ]
  | .word =>
    #[
      ⟨'0', '9', by decide⟩,
      ⟨'A', 'Z', by decide⟩,
      ⟨'_', '_', by decide⟩,
      ⟨'a', 'z', by decide⟩
    ]
  | .nonWord =>
    #[
      ⟨'\x00', '/', by decide⟩,
      ⟨':', '@', by decide⟩,
      ⟨'[', '^', by decide⟩,
      ⟨'`', '`', by decide⟩,
      ⟨'{', max, by decide⟩
    ]
  | .space =>
    #[
      ⟨'\t', '\r', by decide⟩,
      ⟨' ', ' ', by decide⟩
    ]
  | .nonSpace =>
    #[
      ⟨'\x00', '\x08', by decide⟩,
      ⟨'\x0e', '\x1f', by decide⟩,
      ⟨'!', max, by decide⟩
    ]

/--
Normalizes the items of a character class: turns `items` into ranges, sorts the ranges by their
lower bound and merges the ones that overlap or are adjacent. If `negate`, takes the complement of
the merged ranges in the set of all characters.

The result is non-overlapping ranges in ascending order.
-/
def ClassItem.normalize (negate : Bool) (items : Array ClassItem) : Array RangeItem :=
  let ranges := items.flatMap expand
    |>.mergeSort (·.lower ≤ ·.lower)
    |>.foldl (init := #[]) merge
  if negate then complement ranges else ranges
where
  /--
  Turns an item of a character class into ranges.
  -/
  expand : ClassItem → Array RangeItem
    | .single c         => #[⟨c, c, c.le_refl⟩]
    | .range r          => #[r]
    | .named namedClass => namedClass.ranges

  /--
  Adds the range `curr` to `merged`, which holds non-overlapping ranges in ascending order. If
  `curr` overlaps the last range or is adjacent to it, joins `curr` to that range; otherwise appends
  `curr`.
  -/
  merge (merged : Array RangeItem) (curr : RangeItem) : Array RangeItem :=
    match merged.back?.filter (fun last => curr.lower.toNat ≤ last.upper.toNat + 1) with
    | some last =>
      if h : last.upper < curr.upper then
        merged.pop.push
          ⟨last.lower, curr.upper, Char.le_trans last.wellFormed (Std.le_of_lt h)⟩
      else
        merged
    | none => merged.push curr

  /--
  Returns the characters not in `ranges`, among all characters.
  -/
  complement (ranges : Array RangeItem) : Array RangeItem :=
    let starts := #[some '\x00'] ++ ranges.map (next? ·.upper)
    let stops  := ranges.map (prev? ·.lower) ++ #[some (Char.ofNat 0x10FFFF)]
    (starts.zip stops).filterMap fun
      | (some s, some e) => if h : s ≤ e then some ⟨s, e, h⟩ else none
      | _                => none

  /--
  Returns the character after `c`, skipping the surrogates, or `none` if `c` is the last character.
  -/
  next? (c : Char) : Option Char :=
    match c.toNat with
    | 0x10FFFF => none
    | 0xD7FF   => some (Char.ofNat 0xE000)
    | n        => some (Char.ofNat (n + 1))

  /--
  Returns the character before `c`, skipping the surrogates, or `none` if `c` is the first
  character.
  -/
  prev? (c : Char) : Option Char :=
    match c.toNat with
    | 0      => none
    | 0xE000 => some (Char.ofNat 0xD7FF)
    | n + 1  => some (Char.ofNat n)

end Lexgen.Internal
