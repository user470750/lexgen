/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov

Parts of this file are ported from RE2, distributed under the following license:

Copyright (c) 2009 The RE2 Authors. All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

   * Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.
   * Redistributions in binary form must reproduce the above
copyright notice, this list of conditions and the following disclaimer
in the documentation and/or other materials provided with the
distribution.
   * Neither the name of Google Inc. nor the names of its
contributors may be used to endorse or promote products derived from
this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-/
module

public section

/-!
# UTF-8 sequences

Defines `UTF8.split`, which turns a range of code points into sequences of byte ranges that match
exactly the UTF-8 encodings of its characters.

Ported from `Compiler::AddRuneRangeUTF8` and `MaxRune` in RE2's [`re2/compile.cc`][re2], with the
split at the surrogates from `ScalarRange::split` in regex-syntax's [`src/utf8.rs`][regex-syntax]
(MIT or Apache-2.0).

[re2]: https://github.com/google/re2/blob/2da0056814cf/re2/compile.cc
[regex-syntax]: https://github.com/rust-lang/regex/blob/94ac45b2a6e7/regex-syntax/src/utf8.rs
-/

namespace Lexgen.Internal

/--
Returns the largest code point that a UTF-8 sequence of `len` bytes encodes, for `len < 4`.
-/
private def maxRune (len : Fin 4) : UInt32 :=
  -- The number of code point bits in a `len`-byte UTF-8 sequence.
  let bits : Nat := if len == 1 then 7 else 8 - (len + 1) + 6 * (len - 1)
  (1 <<< bits.toUInt32) - 1

/--
Returns the sequences of byte ranges that match the UTF-8 encodings of the characters with code
points from `lo` to `hi`, inclusive: a byte string encodes such a character if and only if it
matches exactly one of the sequences, each byte in the corresponding range.
-/
-- TODO: prove termination: every recursive call gets a smaller range.
partial def UTF8.split (lo hi : UInt32) : List (List (UInt8 × UInt8)) :=
  if lo > hi then
    []
  -- Split range at the surrogates, which have no UTF-8 encoding.
  else if lo < 0xE000 && hi > 0xD7FF then
    split lo 0xD7FF ++ split 0xE000 hi
  -- Split range into same-length sized ranges.
  else if let some max := [1, 2, 3].findSome? lengthSplit? then
    split lo max ++ split (max + 1) hi
  -- ASCII range is always a special case.
  else if hi < 0x80 then
    [[(lo.toUInt8, hi.toUInt8)]]
  -- Split range into sections that agree on leading bytes.
  else if let some sep := [1, 2, 3].findSome? maskSplit? then
    split lo sep ++ split (sep + 1) hi
  -- The byte ranges between the encodings of `lo` and `hi`.
  else
    let ulo := String.utf8EncodeChar (Char.ofNat lo.toNat)
    let uhi := String.utf8EncodeChar (Char.ofNat hi.toNat)
    [ulo.zip uhi]
where
  /--
  Returns the largest code point with an `i`-byte encoding if the range also has longer ones, as
  the end of the first part in a split into ranges whose encodings have the same length.
  -/
  lengthSplit? (i : Fin 4) : Option UInt32 :=
    Option.guard (fun max => lo ≤ max && max < hi) (maxRune i)

  /--
  Returns the end of the first part in a split into ranges that agree on the leading bytes before
  the last `i` ones, or `none` if the range needs no such split.
  -/
  maskSplit? (i : Fin 4) : Option UInt32 :=
    let mask : UInt32 := (1 <<< (6 * i.toNat).toUInt32) - 1
    if (lo &&& ~~~mask) == (hi &&& ~~~mask) then
      none
    else if (lo &&& mask) != 0 then
      some (lo ||| mask)
    else if (hi &&& mask) != mask then
      some ((hi &&& ~~~mask) - 1)
    else
      none

end Lexgen.Internal
