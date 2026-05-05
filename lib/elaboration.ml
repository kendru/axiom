(** Effect evidence-passing elaboration pass (§9.2.1).

    Transforms a type-checked [Ast.program] into an elaborated IR where:
    - Every function whose effect annotation admits effects gets explicit
      evidence parameters — one per named effect, in declaration order.
    - Every call to a function with known effects gets the matching evidence
      values threaded in as leading arguments.
    - [Perform] nodes are annotated with the name of the in-scope evidence
      variable for that effect, ready for later codegen.
    - [Handle] nodes are preserved; the handled expression sees evidence for
      each handled effect; handler bodies are elaborated under the outer
      ambient (handler side-effects flow to the enclosing context).
    - Pure functions are left structurally identical — no evidence params,
      no extra arguments.

    Letrec bindings carry no effect annotation in the surface AST; they are
    treated as pure for the purposes of evidence threading.  Full support
    for effectful letrec requires a future AST extension.
*)

open Ast

(* ------------------------------------------------------------------ *)
(* Evidence parameters                                                  *)
(* ------------------------------------------------------------------ *)

(** Canonical evidence-variable name for a named effect. *)
let ev_var_name eff_name = Printf.sprintf "__ev_%s" eff_name

(** An evidence parameter: a named binding for one effect's dictionary. *)
type evidence_param = {
  ev_name   : string;   (** e.g. "__ev_Console" *)
  ev_effect : string;   (** e.g. "Console" *)
}

let make_ev_param eff_name =
  { ev_name = ev_var_name eff_name; ev_effect = eff_name }

(* ------------------------------------------------------------------ *)
(* Elaborated IR                                                        *)
(* ------------------------------------------------------------------ *)

type eexpr = {
  edesc : eexpr_desc;
}

and eexpr_desc =
  | EVar          of string
  | EIntLit       of int64
  | EFloatLit     of float
  | EStringLit    of string
  | EBoolLit      of bool
  | EUnitLit
  | ELet          of elet_binding
  | EApp          of eexpr * eexpr list
  | EFn           of efn_data
  | EMatch        of ematch_data
  | EIf           of eif_data
  | EDo           of edo_stmt list
  | ELetrec       of eletrec_binding list * eexpr
  | ERecord       of (string * eexpr) list
  | ERecordUpdate of eexpr * (string * eexpr) list
  | EProject      of eexpr * string
  | EPerform      of eperform_data
  | EHandle       of ehandle_data

(** A function literal after elaboration.
    [ev_params] is empty for pure functions (identity transformation). *)
and efn_data = {
  ev_params   : evidence_param list;
  params      : param list;
  return_type : type_expr option;
  effects     : effect_set option;
  fn_body     : eexpr;
}

and elet_binding = {
  pat   : pattern;
  value : eexpr;
  body  : eexpr;
}

and ematch_data = {
  scrutinee : eexpr;
  arms      : ematch_arm list;
}

and ematch_arm = {
  pattern  : pattern;
  arm_body : eexpr;
}

and eif_data = {
  cond  : eexpr;
  then_ : eexpr;
  else_ : eexpr;
}

and edo_stmt =
  | EStmtLet  of { pat : pattern; value : eexpr }
  | EStmtExpr of eexpr

and eletrec_binding = {
  letrec_name        : string;
  letrec_ev_params   : evidence_param list;
  letrec_params      : param list;
  letrec_return_type : type_expr;
  letrec_body        : eexpr;
}

(** A [perform] node annotated with the evidence variable in scope.
    [ev_var] is the name of the evidence dictionary for [effect_name]. *)
and eperform_data = {
  effect_name : string;
  op_name     : string;
  args        : eexpr list;
  ev_var      : string;
}

(** A [handle] node with per-handler evidence bindings.
    [ev_param] is the evidence parameter created for each handled effect;
    [handled] is elaborated under an extended ambient that includes those
    evidence bindings; handler bodies are elaborated under the outer ambient. *)
and ehandle_data = {
  handled  : eexpr;
  handlers : eeffect_handler list;
}

and eeffect_handler = {
  effect_handler : string;
  ev_param       : evidence_param;
  op_handlers    : eop_handler list;
  return_handler : ereturn_handler option;
}

and eop_handler = {
  op_handler_name   : string;
  op_handler_params : string list;
  op_handler_body   : eexpr;
}

and ereturn_handler = {
  return_var  : string;
  return_body : eexpr;
}

(** Data for an elaborated top-level function declaration. *)
type edecl_fn = {
  pub         : bool;
  fn_name     : string;
  type_params : string list;
  ev_params   : evidence_param list;
  params      : param list;
  return_type : type_expr option;
  effects     : effect_set option;
  decl_body   : eexpr;
}

(** Top-level declaration after elaboration.
    Non-function declarations are passed through unchanged via [EDeclPassthrough]. *)
type edecl = {
  edecl_desc : edecl_desc;
}

and edecl_desc =
  | EDeclFn     of edecl_fn
  | EDeclModule of { pub : bool; module_name : string; body : edecl list }
  | EDeclPassthrough of decl_desc

type eprogram = edecl list

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let eexpr desc = { edesc = desc }

(** Extract the flat list of effect names from a surface effect annotation.
    [TyName "Console"] → ["Console"];  [TyApp("State",_)] → ["State"]. *)
let effects_of_set = function
  | None -> []
  | Some Pure -> []
  | Some (Effects (effs, _)) ->
    List.filter_map (function
      | TyName n     -> Some n
      | TyApp (n, _) -> Some n
      | _            -> None) effs

(* ------------------------------------------------------------------ *)
(* Elaboration context                                                  *)
(* ------------------------------------------------------------------ *)

(** Maps a function/variable name → the list of effects it requires. *)
type fn_env = (string * string list) list

(** Maps an effect name → the evidence-variable name currently in scope. *)
type ev_env = (string * string) list

(** If [pat] binds a single name to a [Fn] literal, record that name's
    effects so call sites can thread evidence. *)
let fn_env_extend_let fn_env pat value =
  match pat.pat_desc, value.desc with
  | PVar name, Fn { effects; _ } ->
    (name, effects_of_set effects) :: fn_env
  | _ -> fn_env

(** Best-effort: extract the effects the callee expression requires.
    Handles direct named references, module-qualified references, and immediate
    lambda applications; returns [[]] for arbitrary higher-order expressions. *)
let effects_of_callee fn_env (e : Ast.expr) =
  match e.desc with
  | Var name ->
    (match List.assoc_opt name fn_env with Some es -> es | None -> [])
  | Project ({ desc = Var mod_name; _ }, fn_name) ->
    let qualified = mod_name ^ "." ^ fn_name in
    (match List.assoc_opt qualified fn_env with Some es -> es | None -> [])
  | Fn { effects; _ } -> effects_of_set effects
  | _ -> []

(* ------------------------------------------------------------------ *)
(* Main elaboration                                                     *)
(* ------------------------------------------------------------------ *)

let rec elaborate_expr (fn_env : fn_env) (ev_env : ev_env) (e : Ast.expr) : eexpr =
  match e.desc with
  | Var x       -> eexpr (EVar x)
  | IntLit n    -> eexpr (EIntLit n)
  | FloatLit f  -> eexpr (EFloatLit f)
  | StringLit s -> eexpr (EStringLit s)
  | BoolLit b   -> eexpr (EBoolLit b)
  | UnitLit     -> eexpr EUnitLit

  | Let { pat; value; body } ->
    let fn_env' = fn_env_extend_let fn_env pat value in
    eexpr (ELet {
      pat;
      value = elaborate_expr fn_env  ev_env value;
      body  = elaborate_expr fn_env' ev_env body;
    })

  | App (callee, args) ->
    let ecallee = elaborate_expr fn_env ev_env callee in
    let eargs   = List.map (elaborate_expr fn_env ev_env) args in
    let callee_effs = effects_of_callee fn_env callee in
    let ev_args = List.filter_map (fun eff ->
        match List.assoc_opt eff ev_env with
        | Some v -> Some (eexpr (EVar v))
        | None   -> None
      ) callee_effs in
    eexpr (EApp (ecallee, ev_args @ eargs))

  | Fn { params; return_type; effects; fn_body } ->
    let eff_names = effects_of_set effects in
    let ev_params = List.map make_ev_param eff_names in
    let ev_env' = List.fold_left
        (fun acc ep -> (ep.ev_effect, ep.ev_name) :: acc)
        ev_env ev_params in
    eexpr (EFn {
      ev_params;
      params;
      return_type;
      effects;
      fn_body = elaborate_expr fn_env ev_env' fn_body;
    })

  | Perform { effect_name; op_name; args } ->
    let eargs = List.map (elaborate_expr fn_env ev_env) args in
    let ev_var =
      match List.assoc_opt effect_name ev_env with
      | Some v -> v
      | None   ->
        failwith (Printf.sprintf
                    "Elaboration: no evidence in scope for effect '%s' \
                     at 'perform %s.%s'"
                    effect_name effect_name op_name)
    in
    eexpr (EPerform { effect_name; op_name; args = eargs; ev_var })

  | Handle { handled; handlers } ->
    (* Op/return-handler bodies run in the outer ambient (ev_env unchanged). *)
    let ehandlers = List.map (fun (h : Ast.effect_handler) ->
        let ev_param = make_ev_param h.effect_handler in
        let eop_handlers = List.map (fun (oh : Ast.op_handler) ->
            { op_handler_name   = oh.op_handler_name;
              op_handler_params = oh.op_handler_params;
              op_handler_body   = elaborate_expr fn_env ev_env oh.op_handler_body;
            }
          ) h.op_handlers in
        let ereturn = Option.map (fun (rh : Ast.return_handler) ->
            { return_var  = rh.return_var;
              return_body = elaborate_expr fn_env ev_env rh.return_body;
            }
          ) h.return_handler in
        { effect_handler = h.effect_handler;
          ev_param;
          op_handlers    = eop_handlers;
          return_handler = ereturn;
        }
      ) handlers in
    (* The handled expression sees evidence for each handled effect. *)
    let ev_env' = List.fold_left (fun acc (h : Ast.effect_handler) ->
        (h.effect_handler, ev_var_name h.effect_handler) :: acc
      ) ev_env handlers in
    eexpr (EHandle {
      handled  = elaborate_expr fn_env ev_env' handled;
      handlers = ehandlers;
    })

  | Match { scrutinee; arms } ->
    eexpr (EMatch {
      scrutinee = elaborate_expr fn_env ev_env scrutinee;
      arms = List.map (fun (a : Ast.match_arm) ->
          { pattern  = a.pattern;
            arm_body = elaborate_expr fn_env ev_env a.arm_body;
          }) arms;
    })

  | If { cond; then_; else_ } ->
    eexpr (EIf {
      cond  = elaborate_expr fn_env ev_env cond;
      then_ = elaborate_expr fn_env ev_env then_;
      else_ = elaborate_expr fn_env ev_env else_;
    })

  | Do stmts ->
    let rec go fn_env stmts =
      match stmts with
      | [] -> []
      | StmtLet { pat; value } :: rest ->
        let fn_env' = fn_env_extend_let fn_env pat value in
        EStmtLet { pat; value = elaborate_expr fn_env ev_env value }
        :: go fn_env' rest
      | StmtExpr e :: rest ->
        EStmtExpr (elaborate_expr fn_env ev_env e) :: go fn_env rest
    in
    eexpr (EDo (go fn_env stmts))

  | Letrec (bindings, body) ->
    (* Letrec bindings have no effect annotation in the surface AST; they
       are treated as pure.  A future AST extension could carry effects. *)
    let fn_env' = List.fold_left
        (fun acc (b : Ast.letrec_binding) -> (b.letrec_name, []) :: acc)
        fn_env bindings in
    let ebindings = List.map (fun (b : Ast.letrec_binding) ->
        { letrec_name        = b.letrec_name;
          letrec_ev_params   = [];
          letrec_params      = b.letrec_params;
          letrec_return_type = b.letrec_return_type;
          letrec_body        = elaborate_expr fn_env' ev_env b.letrec_body;
        }
      ) bindings in
    eexpr (ELetrec (ebindings, elaborate_expr fn_env' ev_env body))

  | Record fields ->
    eexpr (ERecord (List.map (fun (n, v) ->
        (n, elaborate_expr fn_env ev_env v)) fields))

  | RecordUpdate (e, fields) ->
    eexpr (ERecordUpdate (
      elaborate_expr fn_env ev_env e,
      List.map (fun (n, v) -> (n, elaborate_expr fn_env ev_env v)) fields
    ))

  | Project (e, field) ->
    eexpr (EProject (elaborate_expr fn_env ev_env e, field))

(** Build a fn_env from a list of top-level (or module-level) declarations.
    Functions inside [DeclModule] are included with their qualified name
    ([module_name.fn_name]) so that module-qualified calls can thread evidence. *)
let fn_env_of_decls decls =
  let rec collect prefix (d : Ast.decl) =
    match d.decl_desc with
    | DeclFn { fn_name; effects; _ } ->
      let name = match prefix with
        | None   -> fn_name
        | Some p -> p ^ "." ^ fn_name
      in
      [(name, effects_of_set effects)]
    | DeclModule { module_name; body; _ } ->
      List.concat_map (collect (Some module_name)) body
    | _ -> []
  in
  List.concat_map (collect None) decls

let rec elaborate_decl (fn_env : fn_env) (d : Ast.decl) : edecl =
  match d.decl_desc with
  | DeclFn { pub; fn_name; type_params; params; return_type; effects; decl_body } ->
    let eff_names = effects_of_set effects in
    let ev_params = List.map make_ev_param eff_names in
    let ev_env    = List.map (fun ep -> (ep.ev_effect, ep.ev_name)) ev_params in
    { edecl_desc = EDeclFn {
        pub; fn_name; type_params; ev_params; params; return_type; effects;
        decl_body = elaborate_expr fn_env ev_env decl_body;
      }
    }
  | DeclModule { pub; module_name; body } ->
    let fn_env' = fn_env_of_decls body in
    { edecl_desc = EDeclModule {
        pub; module_name;
        body = List.map (elaborate_decl fn_env') body;
      }
    }
  | other ->
    { edecl_desc = EDeclPassthrough other }

let elaborate_program (prog : Ast.program) : eprogram =
  let fn_env = fn_env_of_decls prog in
  List.map (elaborate_decl fn_env) prog

(* ------------------------------------------------------------------ *)
(* Pretty-printer (for dump / round-trip tests)                        *)
(* ------------------------------------------------------------------ *)

let pp_ev_params fmt = function
  | [] -> ()
  | evps ->
    Format.pp_print_string fmt "[";
    Format.pp_print_list
      ~pp_sep:(fun f () -> Format.pp_print_string f ", ")
      (fun f ep -> Format.pp_print_string f ep.ev_name)
      fmt evps;
    Format.pp_print_string fmt "]"

let rec pp_eexpr fmt e =
  match e.edesc with
  | EVar x       -> Format.pp_print_string fmt x
  | EIntLit n    -> Format.fprintf fmt "%Ld" n
  | EFloatLit f  -> Format.fprintf fmt "%g" f
  | EStringLit s -> Format.fprintf fmt "%S" s
  | EBoolLit b   -> Format.pp_print_bool fmt b
  | EUnitLit     -> Format.pp_print_string fmt "()"

  | ELet { pat; value; body } ->
    Format.fprintf fmt "let %a = %a in %a"
      pp_pattern pat pp_eexpr value pp_eexpr body

  | EApp (f, args) ->
    Format.fprintf fmt "%a(%a)"
      pp_eexpr f
      (Format.pp_print_list
         ~pp_sep:(fun f () -> Format.pp_print_string f ", ")
         pp_eexpr) args

  | EFn { ev_params; params; fn_body; _ } ->
    Format.fprintf fmt "fn %a(%a) { %a }"
      pp_ev_params ev_params
      (Format.pp_print_list
         ~pp_sep:(fun f () -> Format.pp_print_string f ", ")
         (fun f p -> Format.pp_print_string f p.param_name))
      params
      pp_eexpr fn_body

  | EPerform { effect_name; op_name; args; ev_var } ->
    Format.fprintf fmt "perform[%s] %s.%s(%a)"
      ev_var effect_name op_name
      (Format.pp_print_list
         ~pp_sep:(fun f () -> Format.pp_print_string f ", ")
         pp_eexpr) args

  | EHandle { handled; handlers } ->
    Format.fprintf fmt "handle %a with { %a }"
      pp_eexpr handled
      (Format.pp_print_list
         ~pp_sep:(fun f () -> Format.pp_print_string f "; ")
         pp_eeffect_handler) handlers

  | EMatch { scrutinee; arms } ->
    Format.fprintf fmt "match %a with { %a }"
      pp_eexpr scrutinee
      (Format.pp_print_list
         ~pp_sep:(fun f () -> Format.pp_print_string f " | ")
         pp_ematch_arm) arms

  | EIf { cond; then_; else_ } ->
    Format.fprintf fmt "if %a { %a } else { %a }"
      pp_eexpr cond pp_eexpr then_ pp_eexpr else_

  | EDo stmts ->
    Format.fprintf fmt "do { %a }"
      (Format.pp_print_list
         ~pp_sep:(fun f () -> Format.pp_print_string f "; ")
         pp_edo_stmt) stmts

  | ELetrec (bindings, body) ->
    Format.fprintf fmt "letrec %a in %a"
      (Format.pp_print_list
         ~pp_sep:(fun f () -> Format.pp_print_string f " and ")
         pp_eletrec_binding) bindings
      pp_eexpr body

  | ERecord fields ->
    Format.fprintf fmt "{ %a }"
      (Format.pp_print_list
         ~pp_sep:(fun f () -> Format.pp_print_string f ", ")
         (fun f (n, v) -> Format.fprintf f "%s = %a" n pp_eexpr v)) fields

  | ERecordUpdate (e, fields) ->
    Format.fprintf fmt "{ %a with %a }"
      pp_eexpr e
      (Format.pp_print_list
         ~pp_sep:(fun f () -> Format.pp_print_string f ", ")
         (fun f (n, v) -> Format.fprintf f "%s = %a" n pp_eexpr v)) fields

  | EProject (e, field) ->
    Format.fprintf fmt "%a.%s" pp_eexpr e field

and pp_eeffect_handler fmt h =
  Format.fprintf fmt "%s[ev:%s] { %a%a }"
    h.effect_handler h.ev_param.ev_name
    (Format.pp_print_list
       ~pp_sep:(fun f () -> Format.pp_print_string f " ")
       pp_eop_handler) h.op_handlers
    (fun f -> function
       | None    -> ()
       | Some rh ->
         Format.fprintf f " return %s => %a" rh.return_var pp_eexpr rh.return_body)
    h.return_handler

and pp_eop_handler fmt oh =
  Format.fprintf fmt "%s(%a) => %a"
    oh.op_handler_name
    (Format.pp_print_list
       ~pp_sep:(fun f () -> Format.pp_print_string f ", ")
       Format.pp_print_string) oh.op_handler_params
    pp_eexpr oh.op_handler_body

and pp_ematch_arm fmt a =
  Format.fprintf fmt "%a => %a" pp_pattern a.pattern pp_eexpr a.arm_body

and pp_edo_stmt fmt = function
  | EStmtLet { pat; value } ->
    Format.fprintf fmt "let %a = %a" pp_pattern pat pp_eexpr value
  | EStmtExpr e -> pp_eexpr fmt e

and pp_eletrec_binding fmt b =
  Format.fprintf fmt "%s%a(%a) = %a"
    b.letrec_name
    pp_ev_params b.letrec_ev_params
    (Format.pp_print_list
       ~pp_sep:(fun f () -> Format.pp_print_string f ", ")
       (fun f p -> Format.pp_print_string f p.param_name))
    b.letrec_params
    pp_eexpr b.letrec_body

let rec pp_edecl fmt d =
  match d.edecl_desc with
  | EDeclFn { fn_name; ev_params; params; decl_body; _ } ->
    Format.fprintf fmt "fn %s%a(%a) { %a }"
      fn_name
      pp_ev_params ev_params
      (Format.pp_print_list
         ~pp_sep:(fun f () -> Format.pp_print_string f ", ")
         (fun f p -> Format.pp_print_string f p.param_name))
      params
      pp_eexpr decl_body
  | EDeclModule { module_name; body; _ } ->
    Format.fprintf fmt "module %s { %a }"
      module_name
      (Format.pp_print_list
         ~pp_sep:(fun f () -> Format.pp_print_string f "; ")
         pp_edecl) body
  | EDeclPassthrough (DeclEffect { effect_name; _ }) ->
    Format.fprintf fmt "effect %s" effect_name
  | EDeclPassthrough (DeclType { type_name; _ }) ->
    Format.fprintf fmt "type %s" type_name
  | EDeclPassthrough (DeclImport { module_path; _ }) ->
    Format.fprintf fmt "import %s" module_path
  | EDeclPassthrough (DeclRequire _) ->
    Format.pp_print_string fmt "require ..."
  | EDeclPassthrough _ ->
    Format.pp_print_string fmt "..."

let dump_program (prog : eprogram) : string =
  let buf = Buffer.create 512 in
  let fmt = Format.formatter_of_buffer buf in
  List.iter (fun d ->
      pp_edecl fmt d;
      Format.pp_print_char fmt '\n') prog;
  Format.pp_print_flush fmt ();
  Buffer.contents buf
