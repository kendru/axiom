(* Copyright 2026 Andrew Meredith
   SPDX-License-Identifier: Apache-2.0 *)

(** Lexer for the Axiom working form.
    See spec/axiom-overview-draft.md §8.2 for the full grammar. *)

type token =
  (* keywords *)
  | MODULE | REQUIRE | EFFECT | TYPE | FN | PUB
  | LET | LETREC | IN | MATCH | WITH | HANDLE | PERFORM
  | DO | IF | ELSE | RETURN | PURE
  (* punctuation *)
  | LBRACE | RBRACE | LPAREN | RPAREN | LANGLE | RANGLE
  | COMMA | COLON | SEMI | DOT | DOTDOT | BANG
  | ARROW      (** -> *)
  | FAT_ARROW  (** => *)
  | PIPE       (** |  *)
  | EQUALS
  (* literals *)
  | LIT_INT    of int
  | LIT_FLOAT  of float
  | LIT_STRING of string
  | LIT_BOOL   of bool
  | LIT_UNIT   (** () *)
  (* names: lowercase vs. uppercase initial character *)
  | IDENT of string
  | CTOR  of string
  (* end of input *)
  | EOF

(* TODO: implement tokenize : string -> (token * Ast.loc) list
   Lexer rules:
   - Line comments begin with '--' and extend to end of line
   - String escape sequences: \n \t \\ \" \uXXXX
   - Integer literals: decimal (e.g. 42) or 0x hex (e.g. 0xFF)
   - Float literals: must contain a decimal point (e.g. 3.14, 1.0e-5)
   - Keywords take priority over identifiers on exact match
   - IDENT: starts with lowercase letter or '_', continues with [a-zA-Z0-9_']
   - CTOR:  starts with uppercase letter, continues with [a-zA-Z0-9_'] *)
