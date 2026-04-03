(** Axiom working-form parser.

    Recursive-descent parser. All parsing functions are mutually
    recursive in a single [let rec ... and ...] block so that
    patterns and expressions can freely cross-reference each other. *)

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
(* Type expression parser (non-recursive with expressions)             *)
(* ------------------------------------------------------------------ *)

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

let parse_effect_set (st : state) : effect_set =
  match peek st with
  | Some Pure   -> advance st; Ast.Pure
  | Some LBrace ->
    advance st;
    (match peek st with
     | Some RBrace -> advance st; Ast.Effects []
     | _ ->
       let first = parse_type_expr st in
       let rest  = parse_type_args_rest st in
       consume st RBrace;
       Ast.Effects (first :: rest))
  | Some t ->
    failwith (Format.asprintf "Parser: expected effect set, got %a" pp_token t)
  | None -> failwith "Parser: expected effect set, got end of input"

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

let rec parse_params (st : state) : param list =
  match peek st with
  | Some RParen -> []
  | _ ->
    let first = parse_param st in
    first :: parse_params_rest st

and parse_params_rest (st : state) : param list =
  match peek st with
  | Some Comma -> advance st; parse_params st
  | _          -> []

(* ------------------------------------------------------------------ *)
(* Mutually recursive expression + pattern parsers                     *)
(* ------------------------------------------------------------------ *)

let rec parse_pattern (st : state) : pattern =
  match peek st with
  | Some (Ident "_")   -> advance st; Ast.PWild
  | Some (Ident s)     -> advance st; Ast.PVar s
  | Some (CtorIdent s) ->
    advance st;
    let sub_pats = match peek st with
      | Some LParen ->
        advance st;
        (match peek st with
         | Some RParen -> advance st; []
         | _ ->
           let first = parse_pattern st in
           let rest  = parse_pat_args_rest st in
           consume st RParen;
           first :: rest)
      | _ -> []
    in
    Ast.PCtor (s, sub_pats)
  | Some (IntLit n)    -> advance st; Ast.PLit (LInt n)
  | Some (FloatLit f)  -> advance st; Ast.PLit (LFloat f)
  | Some (StringLit s) -> advance st; Ast.PLit (LString s)
  | Some True          -> advance st; Ast.PLit (LBool true)
  | Some False         -> advance st; Ast.PLit (LBool false)
  | Some LParen ->
    (match st.tokens with
     | _ :: RParen :: _ -> advance st; advance st; Ast.PLit LUnit
     | _ -> failwith "Parser: unexpected '(' in pattern")
  | Some t ->
    failwith (Format.asprintf "Parser: unexpected token in pattern: %a" pp_token t)
  | None -> failwith "Parser: unexpected end of input in pattern"

and parse_pat_args_rest (st : state) : pattern list =
  match peek st with
  | Some Comma -> advance st; let p = parse_pattern st in p :: parse_pat_args_rest st
  | _          -> []

and parse_match_arms (st : state) : match_arm list =
  match peek st with
  | Some Pipe ->
    advance st;
    let pattern  = parse_pattern st in
    consume st FatArrow;
    let arm_body = parse_expr_state st in
    { pattern; arm_body } :: parse_match_arms st
  | _ -> []

and parse_effect_handlers (st : state) : effect_handler list =
  match peek st with
  | Some (CtorIdent effect_name) ->
    advance st;
    consume st LBrace;
    let op_handlers, return_handler = parse_op_handlers st in
    consume st RBrace;
    let h = { effect_handler = effect_name; op_handlers; return_handler } in
    h :: parse_effect_handlers st
  | _ -> []

and parse_op_handlers (st : state) : op_handler list * return_handler option =
  match peek st with
  | Some Return ->
    advance st;
    let return_var = match peek st with
      | Some (Ident s) -> advance st; s
      | Some t -> failwith (Format.asprintf "Parser: expected var after 'return', got %a" pp_token t)
      | None   -> failwith "Parser: expected var after 'return'"
    in
    consume st FatArrow;
    let return_body = parse_expr_state st in
    ([], Some { return_var; return_body })
  | Some (Ident op_name) ->
    advance st;
    consume st LParen;
    let params = parse_handler_params st in
    consume st RParen;
    consume st FatArrow;
    let op_handler_body = parse_expr_state st in
    let op = { op_handler_name = op_name; op_handler_params = params; op_handler_body } in
    let (rest_ops, ret) = parse_op_handlers st in
    (op :: rest_ops, ret)
  | _ -> ([], None)

and parse_handler_params (st : state) : string list =
  match peek st with
  | Some RParen -> []
  | Some (Ident s) ->
    advance st;
    let rest = match peek st with
      | Some Comma -> advance st; parse_handler_params st
      | _          -> []
    in
    s :: rest
  | Some t ->
    failwith (Format.asprintf "Parser: expected handler param, got %a" pp_token t)
  | None -> failwith "Parser: expected handler param"

and parse_do_stmts (st : state) : do_stmt list =
  (* Peek ahead: if next two tokens are 'let IDENT' it's a statement binding *)
  match st.tokens with
  | Let :: Ident name :: _ ->
    advance st; advance st;       (* consume 'let' and name *)
    consume st Equal;
    let value = parse_expr_state st in
    consume st Semi;
    Ast.StmtLet { name; value } :: parse_do_stmts st
  | _ ->
    let e = parse_expr_state st in
    (match peek st with
     | Some Semi ->
       advance st;
       Ast.StmtExpr e :: parse_do_stmts st
     | _ ->
       [Ast.StmtExpr e])

and parse_args (st : state) : expr list =
  match peek st with
  | Some RParen -> []
  | _ ->
    let first = parse_expr_state st in
    first :: parse_args_rest st

and parse_args_rest (st : state) : expr list =
  match peek st with
  | Some Comma -> advance st; parse_args st
  | _          -> []

and parse_letrec_binding (st : state) : Ast.letrec_binding =
  let letrec_name = match peek st with
    | Some (Ident s) -> advance st; s
    | Some t ->
      failwith (Format.asprintf "Parser: expected identifier in letrec binding, got %a" pp_token t)
    | None -> failwith "Parser: expected identifier in letrec binding"
  in
  consume st LParen;
  let letrec_params = parse_params st in
  consume st RParen;
  consume st Colon;
  let letrec_return_type = parse_type_expr st in
  consume st Equal;
  let letrec_body = parse_expr_state st in
  { Ast.letrec_name; letrec_params; letrec_return_type; letrec_body }

and parse_letrec_bindings_rest (st : state) : Ast.letrec_binding list =
  match peek st with
  | Some Comma -> advance st; let b = parse_letrec_binding st in b :: parse_letrec_bindings_rest st
  | _ -> []

and parse_expr_state (st : state) : expr =
  match peek st with
  | Some Letrec ->
    advance st;
    consume st LBrace;
    let first = parse_letrec_binding st in
    let rest  = parse_letrec_bindings_rest st in
    consume st RBrace;
    consume st In;
    let body = parse_expr_state st in
    Ast.Letrec (first :: rest, body)

  | Some Let ->
    advance st;
    let name = match peek st with
      | Some (Ident s) -> advance st; s
      | Some t ->
        failwith (Format.asprintf "Parser: expected identifier after 'let', got %a"
                    pp_token t)
      | None -> failwith "Parser: expected identifier after 'let'"
    in
    consume st Equal;
    let value = parse_expr_state st in
    consume st In;
    let body = parse_expr_state st in
    Ast.Let { name; value; body }

  | Some Handle ->
    advance st;
    let handled = parse_app st in
    consume st With;
    consume st LBrace;
    let handlers = parse_effect_handlers st in
    consume st RBrace;
    Ast.Handle { handled; handlers }

  | Some Perform ->
    advance st;
    let effect_name = match peek st with
      | Some (CtorIdent s) -> advance st; s
      | Some t -> failwith (Format.asprintf "Parser: expected effect name after 'perform', got %a" pp_token t)
      | None   -> failwith "Parser: expected effect name after 'perform'"
    in
    consume st Dot;
    let op_name = match peek st with
      | Some (Ident s) -> advance st; s
      | Some t -> failwith (Format.asprintf "Parser: expected operation name, got %a" pp_token t)
      | None   -> failwith "Parser: expected operation name"
    in
    consume st LParen;
    let args = parse_args st in
    consume st RParen;
    Ast.Perform { effect_name; op_name; args }

  | Some Do ->
    advance st;
    consume st LBrace;
    let stmts = parse_do_stmts st in
    consume st RBrace;
    Ast.Do stmts

  | Some If ->
    advance st;
    let cond = parse_app st in
    consume st LBrace;
    let then_ = parse_expr_state st in
    consume st RBrace;
    consume st Else;
    consume st LBrace;
    let else_ = parse_expr_state st in
    consume st RBrace;
    Ast.If { cond; then_; else_ }

  | Some Match ->
    advance st;
    let scrutinee = parse_app st in
    consume st With;
    consume st LBrace;
    let arms = parse_match_arms st in
    consume st RBrace;
    Ast.Match { scrutinee; arms }

  | Some Fn ->
    advance st;
    consume st LParen;
    let params = parse_params st in
    consume st RParen;
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
    let fn_body = parse_expr_state st in
    consume st RBrace;
    Ast.Fn { params; return_type; effects; fn_body }

  | _ ->
    parse_app st

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
  | Some Dot ->
    (match st.tokens with
     | _ :: Ident field :: _ ->
       advance st; advance st;
       parse_app_rest st (Ast.Project (f, field))
     | _ -> f)
  | _ -> f

and parse_record_fields_rest (st : state) : (string * expr) list =
  match peek st with
  | Some Comma -> advance st; parse_record_fields st
  | _          -> []

and parse_record_fields (st : state) : (string * expr) list =
  match peek st with
  | Some RBrace -> []
  | Some (Ident field) ->
    advance st;
    consume st Colon;
    let value = parse_expr_state st in
    (field, value) :: parse_record_fields_rest st
  | Some t ->
    failwith (Format.asprintf "Parser: expected field name in record, got %a" pp_token t)
  | None -> failwith "Parser: expected field name in record, got end of input"

and parse_atom (st : state) : expr =
  match peek st with
  | Some (IntLit n)    -> advance st; Ast.IntLit n
  | Some (FloatLit f)  -> advance st; Ast.FloatLit f
  | Some (StringLit s) -> advance st; Ast.StringLit s
  | Some True          -> advance st; Ast.BoolLit true
  | Some False         -> advance st; Ast.BoolLit false
  | Some (Ident s)     -> advance st; Ast.Var s
  | Some Resume        -> advance st; Ast.Var "resume"
  | Some LBrace ->
    (* Disambiguate: { ident : ... } is a record literal;
       { expr with ... } is a record update (expr must be parsed first).
       Peek ahead: if the sequence is LBrace Ident Colon, it's a record literal.
       If LBrace RBrace, it's an empty record. Otherwise try record update. *)
    (match st.tokens with
     | _ :: RBrace :: _ ->
       advance st; advance st; Ast.Record []
     | _ :: Ident _ :: Colon :: _ ->
       advance st;
       let fields = parse_record_fields st in
       consume st RBrace;
       Ast.Record fields
     | _ ->
       advance st;
       let base = parse_expr_state st in
       consume st With;
       let fields = parse_record_fields st in
       consume st RBrace;
       Ast.RecordUpdate (base, fields))
  | Some LParen ->
    (match st.tokens with
     | _ :: RParen :: _ -> advance st; advance st; Ast.UnitLit
     | _ -> failwith "Parser: unexpected '(' (grouped expressions not yet supported)")
  | Some t ->
    failwith (Format.asprintf "Parser: unexpected token %a" pp_token t)
  | None ->
    failwith "Parser: unexpected end of input"

(* ------------------------------------------------------------------ *)
(* Public entry point — expressions                                    *)
(* ------------------------------------------------------------------ *)

let parse_expr (tokens : token list) : expr =
  let st = make_state tokens in
  let e = parse_expr_state st in
  (match peek st with
   | None   -> ()
   | Some t ->
     failwith (Format.asprintf "Parser: unexpected trailing token %a" pp_token t));
  e

(* ------------------------------------------------------------------ *)
(* Top-level declaration parsers                                       *)
(* ------------------------------------------------------------------ *)

let parse_type_params (st : state) : string list =
  match peek st with
  | Some LAngle ->
    advance st;
    let rec loop () =
      match peek st with
      | Some (Ident s) ->
        advance st;
        let rest = match peek st with
          | Some Comma -> advance st; loop ()
          | _ -> []
        in
        s :: rest
      | _ -> []
    in
    let ps = loop () in
    consume st RAngle;
    ps
  | _ -> []

let parse_ctor_decl (st : state) : Ast.ctor_decl =
  let ctor_name = match peek st with
    | Some (CtorIdent s) -> advance st; s
    | Some t -> failwith (Format.asprintf "Parser: expected constructor name, got %a" pp_token t)
    | None   -> failwith "Parser: expected constructor name"
  in
  let ctor_params = match peek st with
    | Some LParen ->
      advance st;
      (match peek st with
       | Some RParen -> advance st; []
       | _ ->
         let first = parse_type_expr st in
         let rest  = parse_type_args_rest st in
         consume st RParen;
         first :: rest)
    | _ -> []
  in
  { Ast.ctor_name; ctor_params }

let rec parse_ctor_decls_rest (st : state) : Ast.ctor_decl list =
  match peek st with
  | Some Pipe -> advance st; let c = parse_ctor_decl st in c :: parse_ctor_decls_rest st
  | _         -> []

let parse_effect_op_decl (st : state) : Ast.effect_op =
  let effect_op_name = match peek st with
    | Some (Ident s) -> advance st; s
    | Some t -> failwith (Format.asprintf "Parser: expected op name, got %a" pp_token t)
    | None   -> failwith "Parser: expected op name"
  in
  consume st LParen;
  let effect_op_params = match peek st with
    | Some RParen -> advance st; []
    | _ ->
      let first = parse_type_expr st in
      let rest  = parse_type_args_rest st in
      consume st RParen;
      first :: rest
  in
  consume st Colon;
  let effect_op_return = parse_type_expr st in
  { Ast.effect_op_name; effect_op_params; effect_op_return }

let rec parse_effect_ops_rest (st : state) : Ast.effect_op list =
  match peek st with
  | Some Comma -> advance st; let op = parse_effect_op_decl st in op :: parse_effect_ops_rest st
  | _          -> []

let rec parse_decl (st : state) : Ast.decl =
  let pub = match peek st with
    | Some Pub -> advance st; true
    | _        -> false
  in
  match peek st with
  | Some Fn ->
    advance st;
    let fn_name = match peek st with
      | Some (Ident s) -> advance st; s
      | Some t -> failwith (Format.asprintf "Parser: expected fn name, got %a" pp_token t)
      | None   -> failwith "Parser: expected fn name"
    in
    let type_params = parse_type_params st in
    consume st LParen;
    let params = parse_params st in
    consume st RParen;
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
    let decl_body = parse_expr_state st in
    consume st RBrace;
    Ast.DeclFn { pub; fn_name; type_params; params; return_type; effects; decl_body }

  | Some Type ->
    advance st;
    let type_name = match peek st with
      | Some (CtorIdent s) -> advance st; s
      | Some t -> failwith (Format.asprintf "Parser: expected type name, got %a" pp_token t)
      | None   -> failwith "Parser: expected type name"
    in
    let type_params = parse_type_params st in
    consume st Equal;
    consume st Pipe;
    let first = parse_ctor_decl st in
    let rest  = parse_ctor_decls_rest st in
    Ast.DeclType { pub; type_name; type_params; ctors = first :: rest }

  | Some Effect ->
    advance st;
    let effect_name = match peek st with
      | Some (CtorIdent s) -> advance st; s
      | Some t -> failwith (Format.asprintf "Parser: expected effect name, got %a" pp_token t)
      | None   -> failwith "Parser: expected effect name"
    in
    let type_params = parse_type_params st in
    consume st LBrace;
    let ops = match peek st with
      | Some RBrace -> []
      | _ ->
        let first = parse_effect_op_decl st in
        let rest  = parse_effect_ops_rest st in
        first :: rest
    in
    consume st RBrace;
    Ast.DeclEffect { pub; effect_name; type_params; ops }

  | Some Module ->
    advance st;
    let module_name = match peek st with
      | Some (Ident s) -> advance st; s
      | Some t -> failwith (Format.asprintf "Parser: expected module name, got %a" pp_token t)
      | None   -> failwith "Parser: expected module name"
    in
    consume st LBrace;
    let body = parse_decls_until_rbrace st in
    consume st RBrace;
    Ast.DeclModule { pub; module_name; body }

  | Some Require ->
    advance st;
    let t = parse_type_expr st in
    Ast.DeclRequire t

  | Some t ->
    failwith (Format.asprintf "Parser: expected declaration, got %a" pp_token t)
  | None ->
    failwith "Parser: expected declaration, got end of input"

and parse_decls_until_rbrace (st : state) : Ast.decl list =
  match peek st with
  | Some RBrace | None -> []
  | _ ->
    let d = parse_decl st in
    d :: parse_decls_until_rbrace st

let parse_program (tokens : token list) : Ast.program =
  let st = make_state tokens in
  let rec loop () =
    match peek st with
    | None -> []
    | _    -> let d = parse_decl st in d :: loop ()
  in
  loop ()
