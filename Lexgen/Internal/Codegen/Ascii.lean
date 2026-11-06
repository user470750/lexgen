/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
module

public import Lexgen.Internal.DFA.Interval

import Lexgen.Internal.DFA.Automaton
import Std.Data.HashMap

public section

/-!
# ASCII transitions

Defines how the generated lexer tests the transitions of a state on ASCII bytes. `splitAscii` splits
the transitions of a state at `0x80`. For the ASCII part, `asciiTests` builds lookup tables for the
sets of bytes of the transitions that a single range does not cover, packs them eight to a byte
array, and maps every transition to its test: a range of bytes or a bit in a table.
-/

namespace Lexgen.Internal

/--
The test of a transition on ASCII bytes in the generated code.
-/
inductive AsciiTest where
  /--
  The test that the byte is in `interval`.
  -/
  | inRange (interval : DFA.Interval)
  /--
  The test that the entry of table `table` indexed by the byte has the bit `mask` set.
  -/
  | inTable (table : Nat) (mask : UInt8)

/--
The tests of the transitions on ASCII bytes of every state of a `DFA`, with the tables that they
use.
-/
structure AsciiTests where
  /--
  The byte arrays that the lookup tables, each covering all ASCII bytes, are packed into, eight to
  an array.
  -/
  tables : Array ByteArray
  /--
  For every state, the states that its ASCII bytes lead to but the trap, each with the test of its
  transition. Indexed by state.
  -/
  tests : Array (Array (Nat × AsciiTest))

/--
Splits the transitions of a state at `0x80`: returns its transitions on ASCII bytes and those on the
other bytes, with an interval across `0x80` cut in two.
-/
def splitAscii (row : Array (DFA.Interval × Nat)) :
    Array (DFA.Interval × Nat) × Array (DFA.Interval × Nat) :=
  row.foldl (init := (#[], #[])) fun (ascii, nonAscii) (interval, target) =>
    if interval.lower ≥ 0x80 then
      (ascii, nonAscii.push (interval, target))
    else if interval.upper < 0x80 then
      (ascii.push (interval, target), nonAscii)
    else
      (ascii.push (⟨interval.lower, 0x7F⟩, target), nonAscii.push (⟨0x80, interval.upper⟩, target))

/--
Returns the bit mask of the set numbered `number` in its array.
-/
private def mask (number : Nat) : UInt8 :=
  1 <<< (number % 8).toUInt8

/--
Packs the lookup tables of the sets numbered by `numbers` eight to a byte array: each set has its
own bit mask in its array.
-/
private def pack (numbers : Std.HashMap (Array Bool) Nat) : Array ByteArray :=
  numbers.fold
    (init := Array.replicate ((numbers.size + 7) / 8) (Array.replicate 0x80 0))
    (fun tables set number =>
      tables.modify (number / 8) fun table => table.zipWith (packBit number) set)
  |>.map ByteArray.mk
where
  /--
  Returns `entry` with the bit `mask number` set if the byte of the entry is in the set.
  -/
  packBit (number : Nat) (entry : UInt8) (inSet : Bool) : UInt8 :=
    if inSet then entry ||| mask number else entry

/--
Returns the tests of the transitions of every state of `trans` on ASCII bytes, with the tables that
they use.
-/
def asciiTests (trans : Array (Array (DFA.Interval × Nat))) : AsciiTests := Id.run do
  -- `numbers` gives every set of bytes in the tables its number.
  let mut numbers : Std.HashMap (Array Bool) Nat := {}
  let mut tests := #[]
  for row in trans do
    let mut stateTests : Array (Nat × AsciiTest) := #[]
    for (target, intervals) in groupIntervals (splitAscii row).1 do
      if let #[interval] := intervals then
        stateTests := stateTests.push (target, .inRange interval)
      else
        let set := (Array.range 0x80).map fun b => intervals.any (·.contains b.toUInt8)
        -- A new set gets the next number.
        let number := numbers.getD set numbers.size
        numbers := numbers.insertIfNew set number
        stateTests := stateTests.push (target, .inTable (number / 8) (mask number))
    tests := tests.push stateTests
  return { tables := pack numbers, tests }
where
  /--
  Returns the intervals of the transitions of `row` for every state that they lead to but the trap.
  -/
  groupIntervals (row : Array (DFA.Interval × Nat)) : Std.HashMap Nat (Array DFA.Interval) :=
    row.groupByKey (·.2)
    |>.erase DFA.trap
    |>.map fun _ transitions => transitions.map (·.1)

end Lexgen.Internal
