/-
Copyright (c) 2026 Oleg Shabanov. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Oleg Shabanov
-/
import Lexgen
import LexgenTest.Basic

/-!
# Tests on a lexer that needs raised heartbeats

A lexer for Zig, whose elaboration exceeds the default `maxHeartbeats`.

The lexical grammar of Zig after its own tokenizer, `lib/std/zig/tokenizer.zig` (Zig master,
2026-10): every token tag of `std.zig.Token` but `invalid` and `eof`, with the tag names in
camel case and the keywords prefixed with `kw`.
-/

open Lexgen

set_option maxHeartbeats 400000 in
lexer ZigToken where
  skip r"[ \t\n\r]+"
  -- A line comment that is not a doc comment: `//` followed by anything but `/` and `!`, or `////`.
  skip r"//(([^/!\x00-\x1f\x7f]|//)[^\x00-\x1f\x7f]*)?"
  | kwAddrspace                           := r"addrspace"
  | kwAlign                               := r"align"
  | kwAllowzero                           := r"allowzero"
  | kwAnd                                 := r"and"
  | kwAnyframe                            := r"anyframe"
  | kwAnytype                             := r"anytype"
  | kwAsm                                 := r"asm"
  | kwBreak                               := r"break"
  | kwCallconv                            := r"callconv"
  | kwCatch                               := r"catch"
  | kwComptime                            := r"comptime"
  | kwConst                               := r"const"
  | kwContinue                            := r"continue"
  | kwDefer                               := r"defer"
  | kwElse                                := r"else"
  | kwEnum                                := r"enum"
  | kwErrdefer                            := r"errdefer"
  | kwError                               := r"error"
  | kwExport                              := r"export"
  | kwExtern                              := r"extern"
  | kwFn                                  := r"fn"
  | kwFor                                 := r"for"
  | kwIf                                  := r"if"
  | kwInline                              := r"inline"
  | kwNoalias                             := r"noalias"
  | kwNoinline                            := r"noinline"
  | kwNosuspend                           := r"nosuspend"
  | kwOpaque                              := r"opaque"
  | kwOr                                  := r"or"
  | kwOrelse                              := r"orelse"
  | kwPacked                              := r"packed"
  | kwPub                                 := r"pub"
  | kwResume                              := r"resume"
  | kwReturn                              := r"return"
  | kwLinksection                         := r"linksection"
  | kwStruct                              := r"struct"
  | kwSuspend                             := r"suspend"
  | kwSwitch                              := r"switch"
  | kwTest                                := r"test"
  | kwThreadlocal                         := r"threadlocal"
  | kwTry                                 := r"try"
  | kwUnion                               := r"union"
  | kwUnreachable                         := r"unreachable"
  | kwVar                                 := r"var"
  | kwVolatile                            := r"volatile"
  | kwWhile                               := r"while"
  | identifier                            := r#"[a-zA-Z_][a-zA-Z0-9_]*|@"([^"\\\x00-\x1f\x7f]|\\[^\x00-\x1f\x7f])*""#
  | builtin                               := r"@[a-zA-Z_][a-zA-Z0-9_]*"
  | stringLiteral                         := r#""([^"\\\x00-\x1f\x7f]|\\[^\x00-\x1f\x7f])*""#
  | charLiteral                           := r"'([^'\\\x00-\x1f\x7f]|\\[^\x00-\x1f\x7f])*'"
  | multilineStringLiteralLine            := r"\\\\[^\x00-\x1f\x7f]*"
  | numberLiteral                         := r"[0-9][_a-zA-Z0-9]*(\.([_a-zA-Z0-9]|[eEpP][+\-])+|[eEpP][+\-]([_a-zA-Z0-9]|[eEpP][+\-])*)?"
  | docComment                            := r"///([^/\x00-\x1f\x7f][^\x00-\x1f\x7f]*)?"
  | containerDocComment                   := r"//![^\x00-\x1f\x7f]*"
  | bang                                  := r"!"
  | pipe                                  := r"\|"
  | pipePipe                              := r"\|\|"
  | pipeEqual                             := r"\|="
  | equal                                 := r"="
  | equalEqual                            := r"=="
  | equalAngleBracketRight                := r"=>"
  | bangEqual                             := r"!="
  | lParen                                := r"\("
  | rParen                                := r"\)"
  | semicolon                             := r";"
  | percent                               := r"%"
  | percentEqual                          := r"%="
  | lBrace                                := r"\{"
  | rBrace                                := r"\}"
  | lBracket                              := r"\["
  | rBracket                              := r"\]"
  | period                                := r"\."
  | periodAsterisk                        := r"\.\*"
  | ellipsis2                             := r"\.\."
  | ellipsis3                             := r"\.\.\."
  | caret                                 := r"^"
  | caretEqual                            := r"^="
  | plus                                  := r"\+"
  | plusPlus                              := r"\+\+"
  | plusEqual                             := r"\+="
  | plusPercent                           := r"\+%"
  | plusPercentEqual                      := r"\+%="
  | plusPipe                              := r"\+\|"
  | plusPipeEqual                         := r"\+\|="
  | minus                                 := r"-"
  | minusEqual                            := r"-="
  | minusPercent                          := r"-%"
  | minusPercentEqual                     := r"-%="
  | minusPipe                             := r"-\|"
  | minusPipeEqual                        := r"-\|="
  | asterisk                              := r"\*"
  | asteriskEqual                         := r"\*="
  | asteriskPercent                       := r"\*%"
  | asteriskPercentEqual                  := r"\*%="
  | asteriskPipe                          := r"\*\|"
  | asteriskPipeEqual                     := r"\*\|="
  | arrow                                 := r"->"
  | colon                                 := r":"
  | slash                                 := r"/"
  | slashEqual                            := r"/="
  | comma                                 := r","
  | ampersand                             := r"&"
  | ampersandEqual                        := r"&="
  | questionMark                          := r"\?"
  | angleBracketLeft                      := r"<"
  | angleBracketLeftEqual                 := r"<="
  | angleBracketAngleBracketLeft          := r"<<"
  | angleBracketAngleBracketLeftEqual     := r"<<="
  | angleBracketAngleBracketLeftPipe      := r"<<\|"
  | angleBracketAngleBracketLeftPipeEqual := r"<<\|="
  | angleBracketRight                     := r">"
  | angleBracketRightEqual                := r">="
  | angleBracketAngleBracketRight         := r">>"
  | angleBracketAngleBracketRightEqual    := r">>="
  | tilde                                 := r"~"
deriving Repr, BEq

#guard lex ZigToken "const x = 1;" == some #[
  (.kwConst,       "const", 0,  5),
  (.identifier,    "x",     6,  7),
  (.equal,         "=",     8,  9),
  (.numberLiteral, "1",     10, 11),
  (.semicolon,     ";",     11, 12)
]
