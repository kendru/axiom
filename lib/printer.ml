(** Axiom working-form printer: AST -> source text.

    Produces syntactically valid Axiom source from an AST such that
    [parse_program (Lexer.tokenize (print_program p)) = p]
    for any AST [p] produced by the parser. *)

open Ast

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let escape_string s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '"';
  String.iter (function
    | '"'  -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\t' -> Buffer.add_string buf "\\t"
    | c    -> Buffer.add_char buf c) s;
  Buffer.add_char buf '"';
  Buffer.contents buf

(* Use %.17g for full round-trip precision; add a trailing '.' when the
   output contains no decimal point or exponent so the lexer treats it
   as a float rather than an integer. *)
let print_float f =
  let s = Printf.sprintf "%.17g" f in
  if String.contains s '.' || String.contains s 'e' || String.contains s 'E'
  then s
  else s ^ "."

(* ------------------------------------------------------------------ *)
(* Type expressions                                                     *)
(* ------------------------------------------------------------------ *)

let rec print_type_expr = function
  | TyName s -> s
  | TyApp (s, args) ->
    s ^ "<" ^ String.concat ", " (List.map print_type_expr args) ^ ">"
  | TyTuple ts ->
    "(" ^ String.concat ", " (List.map print_type_expr ts) ^ ")"
  | TyFun (params, ret, eff) ->
    (* The parser always strips param names from TyFun; synthesise generic
       names so the named-param form (x: T) -> R ! E round-trips. *)
    let params_s =
      List.mapi (fun i ty ->
        "p" ^ string_of_int i ^ ": " ^ print_type_expr ty) params
    in
    let eff_s = match eff with
      | None   -> ""
      | Some e -> " ! " ^ print_effect_set e
    in
    "(" ^ String.concat ", " params_s ^ ") -> " ^ print_type_expr ret ^ eff_s

and print_effect_set = function
  | Pure       -> "pure"
  | Effects [] -> "{}"
  | Effects ts -> "{" ^ String.concat ", " (List.map print_type_expr ts) ^ "}"

let print_param { param_name; param_type } =
  param_name ^ ": " ^ print_type_expr param_type

(* ------------------------------------------------------------------ *)
(* Patterns                                                             *)
(* ------------------------------------------------------------------ *)

let rec print_pattern p =
  let s = print_pattern_desc p.pat_desc in
  match p.pat_comment with
  | None   -> s
  | Some c -> s ^ " @# " ^ c ^ " #@"

and print_pattern_desc = function
  | PWild        -> "_"
  | PVar s       -> s
  | PLitInt n    -> Int64.to_string n
  | PLitFloat f  -> print_float f
  | PLitString s -> escape_string s
  | PLitTrue     -> "true"
  | PLitFalse    -> "false"
  | PLitUnit     -> "()"
  | PCtor (s, []) -> s
  | PCtor (s, ps) ->
    s ^ "(" ^ String.concat ", " (List.map print_pattern ps) ^ ")"
  | PRecord (fields, open_) ->
    let field_parts =
      List.map (fun (name, p) -> name ^ " = " ^ print_pattern p) fields
    in
    let parts = field_parts @ (if open_ then [".."] else []) in
    if parts = [] then "{}"
    else "{ " ^ String.concat ", " parts ^ " }"
  | POr (p1, p2) ->
    (* The parser is right-associative; parenthesise a left-nested POr
       to preserve the original structure on round-trip. *)
    let left = match p1.pat_desc with
      | POr _ -> "(" ^ print_pattern_desc p1.pat_desc ^ ")"
      | _     -> print_pattern p1
    in
    left ^ " | " ^ print_pattern p2

(* ------------------------------------------------------------------ *)
(* Expressions                                                          *)
(* ------------------------------------------------------------------ *)

(* Returns true when the expression needs to be wrapped in parens to
   appear in function-call or field-projection position. *)
let needs_parens_as_base e = match e.desc with
  | Var _ | IntLit _ | FloatLit _ | StringLit _ | BoolLit _ | UnitLit
  | Record _ | RecordUpdate _ | App _ | Project _ -> false
  | Let _ | Fn _ | Match _ | If _ | Do _ | Letrec _ | Perform _ | Handle _ -> true

let rec print_expr e =
  let s = print_expr_desc e.desc in
  match e.comment with
  | None   -> s
  | Some c -> s ^ " @# " ^ c ^ " #@"

(* Print an expression that appears in function/base position; wrap
   non-atomic forms in parentheses so the parser can handle them. *)
and print_expr_core e =
  if needs_parens_as_base e then "(" ^ print_expr e ^ ")"
  else print_expr e

and print_expr_desc = function
  | Var s        -> s
  | IntLit n     -> Int64.to_string n
  | FloatLit f   -> print_float f
  | StringLit s  -> escape_string s
  | BoolLit true  -> "true"
  | BoolLit false -> "false"
  | UnitLit      -> "()"

  | Let { pat; value; body } ->
    "let " ^ print_pattern pat ^ " = " ^ print_expr value ^
    " in " ^ print_expr body

  | App (f, args) ->
    print_expr_core f ^ "(" ^
    String.concat ", " (List.map print_expr args) ^ ")"

  | Fn { params; return_type; effects; fn_body } ->
    let ann = match return_type, effects with
      | Some ret, Some eff ->
        " -> " ^ print_type_expr ret ^ " ! " ^ print_effect_set eff
      | _ -> ""
    in
    "fn (" ^ String.concat ", " (List.map print_param params) ^ ")" ^
    ann ^ " { " ^ print_expr fn_body ^ " }"

  | Match { scrutinee; arms } ->
    "match " ^ print_expr scrutinee ^ " with { " ^
    String.concat " " (List.map print_match_arm arms) ^ " }"

  | If { cond; then_; else_ } ->
    "if " ^ print_expr cond ^
    " { " ^ print_expr then_ ^
    " } else { " ^ print_expr else_ ^ " }"

  | Do stmts ->
    "do { " ^ print_do_stmts stmts ^ " }"

  | Letrec (bindings, body) ->
    "letrec { " ^
    String.concat ", " (List.map print_letrec_binding bindings) ^
    " } in " ^ print_expr body

  | Record [] -> "{}"
  | Record fields ->
    "{ " ^
    String.concat ", " (List.map (fun (name, e) ->
      name ^ ": " ^ print_expr e) fields) ^
    " }"

  | RecordUpdate (base, fields) ->
    "{ " ^ print_expr base ^ " with " ^
    String.concat ", " (List.map (fun (name, e) ->
      name ^ ": " ^ print_expr e) fields) ^
    " }"

  | Project (e, field) ->
    print_expr_core e ^ "." ^ field

  | Perform { effect_name; op_name; args } ->
    "perform " ^ effect_name ^ "." ^ op_name ^ "(" ^
    String.concat ", " (List.map print_expr args) ^ ")"

  | Handle { handled; handlers } ->
    "handle " ^ print_expr handled ^ " with { " ^
    String.concat " " (List.map print_effect_handler handlers) ^ " }"

and print_match_arm { pattern; arm_body } =
  "| " ^ print_pattern pattern ^ " => " ^ print_expr arm_body

and print_do_stmts = function
  | [] -> ""
  | [StmtExpr e] -> print_expr e
  | [StmtLet { pat; value }] ->
    (* A StmtLet as the last item shouldn't arise from the parser, but
       emit something parseable just in case. *)
    "let " ^ print_pattern pat ^ " = " ^ print_expr value ^ ";"
  | StmtLet { pat; value } :: rest ->
    "let " ^ print_pattern pat ^ " = " ^ print_expr value ^
    "; " ^ print_do_stmts rest
  | StmtExpr e :: rest ->
    print_expr e ^ "; " ^ print_do_stmts rest

and print_letrec_binding { letrec_name; letrec_params; letrec_return_type; letrec_body } =
  letrec_name ^ "(" ^
  String.concat ", " (List.map print_param letrec_params) ^
  "): " ^ print_type_expr letrec_return_type ^
  " = " ^ print_expr letrec_body

and print_effect_handler { effect_handler; op_handlers; return_handler } =
  let ops_s = String.concat " " (List.map print_op_handler op_handlers) in
  let sep   = if op_handlers <> [] then " " else "" in
  let ret_s = match return_handler with
    | None   -> ""
    | Some { return_var; return_body } ->
      sep ^ "return " ^ return_var ^ " => " ^ print_expr return_body
  in
  effect_handler ^ " { " ^ ops_s ^ ret_s ^ " }"

and print_op_handler { op_handler_name; op_handler_params; op_handler_body } =
  op_handler_name ^ "(" ^
  String.concat ", " op_handler_params ^
  ") => " ^ print_expr op_handler_body

(* ------------------------------------------------------------------ *)
(* Declarations                                                         *)
(* ------------------------------------------------------------------ *)

let print_ctor_decl { ctor_name; ctor_params } =
  if ctor_params = [] then ctor_name
  else
    ctor_name ^ "(" ^
    String.concat ", " (List.map print_type_expr ctor_params) ^ ")"

let print_effect_op { effect_op_name; effect_op_params; effect_op_return } =
  effect_op_name ^ ": (" ^
  String.concat ", " (List.map print_type_expr effect_op_params) ^
  ") -> " ^ print_type_expr effect_op_return

let rec print_decl d =
  let s = print_decl_desc d.decl_desc in
  match d.decl_comment with
  | None   -> s
  | Some c -> s ^ " @# " ^ c ^ " #@"

and print_decl_desc = function
  | DeclFn { pub; fn_name; type_params; params; return_type; effects; decl_body } ->
    let pub_s = if pub then "pub " else "" in
    let tp_s  =
      if type_params = [] then ""
      else "<" ^ String.concat ", " type_params ^ ">"
    in
    let ann = match return_type, effects with
      | Some ret, Some eff ->
        " -> " ^ print_type_expr ret ^ " ! " ^ print_effect_set eff
      | _ -> ""
    in
    pub_s ^ "fn " ^ fn_name ^ tp_s ^ "(" ^
    String.concat ", " (List.map print_param params) ^ ")" ^
    ann ^ " { " ^ print_expr decl_body ^ " }"

  | DeclType { pub; type_name; type_params; ctors } ->
    let pub_s = if pub then "pub " else "" in
    let tp_s  =
      if type_params = [] then ""
      else "<" ^ String.concat ", " type_params ^ ">"
    in
    pub_s ^ "type " ^ type_name ^ tp_s ^ " = | " ^
    String.concat " | " (List.map print_ctor_decl ctors)

  | DeclEffect { pub; effect_name; type_params; ops } ->
    let pub_s = if pub then "pub " else "" in
    let tp_s  =
      if type_params = [] then ""
      else "<" ^ String.concat ", " type_params ^ ">"
    in
    let ops_s =
      if ops = [] then ""
      else String.concat ", " (List.map print_effect_op ops)
    in
    pub_s ^ "effect " ^ effect_name ^ tp_s ^ " { " ^ ops_s ^ " }"

  | DeclModule { pub; module_name; body } ->
    let pub_s = if pub then "pub " else "" in
    pub_s ^ "module " ^ module_name ^ " { " ^
    String.concat " " (List.map print_decl body) ^
    " }"

  | DeclRequire t ->
    "require effect " ^ print_type_expr t

  | DeclImport { module_path; alias } ->
    "import " ^ module_path ^
    (match alias with None -> "" | Some a -> " as " ^ a)

(* ------------------------------------------------------------------ *)
(* Program                                                              *)
(* ------------------------------------------------------------------ *)

let print_program (ds : program) : string =
  String.concat "\n" (List.map print_decl ds)
