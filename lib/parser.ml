(** Axiom working-form parser.

    Converts a token list (from {!Lexer}) into an {!Ast.expr}.
    The parser is a simple recursive-descent parser. It grows
    incrementally as new language constructs are added. *)

open Lexer
open Ast

(* ------------------------------------------------------------------ *)
(* Parser state                                                         *)
(* ------------------------------------------------------------------ *)

type state = {
  mutable tokens : token list;
}

let make_state tokens = { tokens }

let peek st = match st.tokens with
  | []     -> None
  | t :: _ -> Some t

let advance st = match st.tokens with
  | []      -> failwith "Parser.advance: unexpected end of input"
  | _ :: tl -> st.tokens <- tl

let consume st expected =
  match peek st with
  | Some t when t = expected -> advance st
  | Some t ->
    failwith (Format.asprintf "Parser: expected %a but got %a"
                pp_token expected pp_token t)
  | None ->
    failwith (Format.asprintf "Parser: expected %a but got end of input"
                pp_token expected)

(* ------------------------------------------------------------------ *)
(* Expression parser                                                    *)
(* ------------------------------------------------------------------ *)

(** Parse a single expression from the token stream. *)
let rec parse_expr_state (st : state) : expr =
  match peek st with
  | Some Let ->
    advance st;
    let name = match peek st with
      | Some (Ident s) -> advance st; s
      | Some t ->
        failwith (Format.asprintf "Parser: expected identifier after 'let', got %a"
                    pp_token t)
      | None -> failwith "Parser: expected identifier after 'let', got end of input"
    in
    consume st Equal;
    let value = parse_atom st in
    consume st In;
    let body = parse_expr_state st in
    Ast.Let { name; value; body }
  | _ ->
    parse_atom st

(** Parse an atomic expression: a literal or a variable. *)
and parse_atom (st : state) : expr =
  match peek st with
  | Some (IntLit n)    -> advance st; Ast.IntLit n
  | Some (FloatLit f)  -> advance st; Ast.FloatLit f
  | Some (StringLit s) -> advance st; Ast.StringLit s
  | Some True          -> advance st; Ast.BoolLit true
  | Some False         -> advance st; Ast.BoolLit false
  | Some UnitLit       -> advance st; Ast.UnitLit
  | Some (Ident s)     -> advance st; Ast.Var s
  | Some t ->
    failwith (Format.asprintf "Parser: unexpected token %a" pp_token t)
  | None ->
    failwith "Parser: unexpected end of input"

(* ------------------------------------------------------------------ *)
(* Public entry point                                                   *)
(* ------------------------------------------------------------------ *)

(** Parse a single expression from a token list. *)
let parse_expr (tokens : token list) : expr =
  let st = make_state tokens in
  let e = parse_expr_state st in
  (match peek st with
   | None   -> ()
   | Some t ->
     failwith (Format.asprintf "Parser: unexpected trailing token %a" pp_token t));
  e
