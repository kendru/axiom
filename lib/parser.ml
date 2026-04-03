(** Axiom working-form parser.

    Recursive-descent parser. Grows incrementally with the language. *)

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
(* Type expression parser                                               *)
(* ------------------------------------------------------------------ *)

(** Parse a type expression: IDENT or IDENT '<' type_expr (',' type_expr)* '>' *)
let rec parse_type_expr (st : state) : type_expr =
  let name = match peek st with
    | Some (Ident s) | Some (CtorIdent s) -> advance st; s
    | Some t ->
      failwith (Format.asprintf "Parser: expected type name, got %a" pp_token t)
    | None -> failwith "Parser: expected type name, got end of input"
  in
  match peek st with
  | Some LAngle ->
    advance st;
    let first = parse_type_expr st in
    let rest  = parse_type_args_rest st in
    consume st RAngle;
    TyApp (name, first :: rest)
  | _ -> TyName name

and parse_type_args_rest (st : state) : type_expr list =
  match peek st with
  | Some Comma -> advance st; let t = parse_type_expr st in t :: parse_type_args_rest st
  | _          -> []

(** Parse an effect set: 'pure' | '{' type_expr (',' type_expr)* '}' *)
let parse_effect_set (st : state) : effect_set =
  match peek st with
  | Some Pure ->
    advance st; Ast.Pure
  | Some LBrace ->
    advance st;
    (match peek st with
     | Some RBrace -> advance st; Ast.Effects []
     | _ ->
       let first = parse_type_expr st in
       let rest  = parse_type_args_rest st in  (* reuse comma-separated list *)
       consume st RBrace;
       Ast.Effects (first :: rest))
  | Some t ->
    failwith (Format.asprintf "Parser: expected effect set ('pure' or '{...}'), got %a"
                pp_token t)
  | None -> failwith "Parser: expected effect set, got end of input"

(** Parse a single parameter: IDENT ':' type_expr *)
let parse_param (st : state) : param =
  let name = match peek st with
    | Some (Ident s) -> advance st; s
    | Some t ->
      failwith (Format.asprintf "Parser: expected parameter name, got %a" pp_token t)
    | None -> failwith "Parser: expected parameter name, got end of input"
  in
  consume st Colon;
  let ty = parse_type_expr st in
  { param_name = name; param_type = ty }

(** Parse a comma-separated parameter list. The opening '(' has already been consumed. *)
let rec parse_params (st : state) : param list =
  match peek st with
  | Some RParen -> []
  | _ ->
    let first = parse_param st in
    let rest  = parse_params_rest st in
    first :: rest

and parse_params_rest (st : state) : param list =
  match peek st with
  | Some Comma -> advance st; parse_params st
  | _          -> []

(* ------------------------------------------------------------------ *)
(* Expression parser                                                    *)
(* ------------------------------------------------------------------ *)

(** Parse a comma-separated argument list (past the opening '('). *)
let rec parse_args (st : state) : expr list =
  match peek st with
  | Some RParen -> []
  | _ ->
    let first = parse_expr_state st in
    let rest  = parse_args_rest st in
    first :: rest

and parse_args_rest (st : state) : expr list =
  match peek st with
  | Some Comma -> advance st; parse_args st
  | _          -> []

and parse_expr_state (st : state) : expr =
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
    let value = parse_app st in
    consume st In;
    let body = parse_expr_state st in
    Ast.Let { name; value; body }

  | Some Fn ->
    advance st;
    consume st LParen;
    let params = parse_params st in
    consume st RParen;
    (* Optional return type and effect annotation *)
    let (return_type, effects) =
      match peek st with
      | Some Arrow ->
        advance st;
        let ret = parse_type_expr st in
        consume st Bang;
        let eff = parse_effect_set st in
        (Some ret, Some eff)
      | _ -> (None, None)
    in
    consume st LBrace;
    let body = parse_expr_state st in
    consume st RBrace;
    Ast.Fn { params; return_type; effects; fn_body = body }

  | _ ->
    parse_app st

(** Parse application: atom optionally followed by '(' args ')'. Left-associative. *)
and parse_app (st : state) : expr =
  let base = parse_atom st in
  parse_app_rest st base

and parse_app_rest (st : state) (f : expr) : expr =
  match peek st with
  | Some LParen ->
    advance st;
    let args = parse_args st in
    consume st RParen;
    parse_app_rest st (Ast.App (f, args))
  | _ -> f

(** Parse an atomic expression: literal, variable, unit, or fn expression.
    '(' immediately followed by ')' is the unit literal. *)
and parse_atom (st : state) : expr =
  match peek st with
  | Some (IntLit n)    -> advance st; Ast.IntLit n
  | Some (FloatLit f)  -> advance st; Ast.FloatLit f
  | Some (StringLit s) -> advance st; Ast.StringLit s
  | Some True          -> advance st; Ast.BoolLit true
  | Some False         -> advance st; Ast.BoolLit false
  | Some (Ident s)     -> advance st; Ast.Var s
  (* '()' — unit literal, distinguished from f() by not following a callee *)
  | Some LParen ->
    (match st.tokens with
     | _ :: RParen :: _ ->
       advance st; advance st; Ast.UnitLit
     | _ ->
       failwith "Parser: unexpected '(' (grouped expressions not yet supported)")
  | Some t ->
    failwith (Format.asprintf "Parser: unexpected token %a" pp_token t)
  | None ->
    failwith "Parser: unexpected end of input"

(* ------------------------------------------------------------------ *)
(* Public entry point                                                   *)
(* ------------------------------------------------------------------ *)

let parse_expr (tokens : token list) : expr =
  let st = make_state tokens in
  let e = parse_expr_state st in
  (match peek st with
   | None   -> ()
   | Some t ->
     failwith (Format.asprintf "Parser: unexpected trailing token %a" pp_token t));
  e
