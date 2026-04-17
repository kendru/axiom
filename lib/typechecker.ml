(** Axiom type checker — Hindley-Milner with let-generalization.

    Algorithm W extended with:
    - Annotated function parameters (the annotation is trusted, not checked
      against an external source of truth yet — that comes with the full
      declaration checker).
    - Let-generalization: the type of a let-bound expression is generalized
      over all free type variables not in the environment.
    - Program-level checking via [check_program]: a two-pass walk collects
      top-level effect and function declarations, then checks each function
      body against its declared signature.
    - Effect operations are looked up in the program's effect environment
      and their types are instantiated at each [perform] site. Effect-row
      tracking on function types is a future layer — this pass only checks
      that every performed operation resolves to a declared one and that
      argument/return types agree. *)

open Ast

(* ------------------------------------------------------------------ *)
(* Types                                                                *)
(* ------------------------------------------------------------------ *)

type ty =
  | TyCon    of string          (** Int, Bool, String, Unit, Float64 *)
  | TyVar    of string          (** type variable (before unification) *)
  | TyFun    of ty * ty         (** A -> B  (curried) *)
  | TyForall of string * ty     (** forall a. T  (generalized) *)
  | TyMeta   of meta_var        (** unification variable *)

and meta_var = {
  id       : int;
  mutable inst : ty option;   (** None = uninstantiated *)
}

(* ------------------------------------------------------------------ *)
(* Pretty-printer and equality (for tests)                             *)
(* ------------------------------------------------------------------ *)

let rec pp_ty fmt = function
  | TyCon s        -> Format.pp_print_string fmt s
  | TyVar s        -> Format.pp_print_string fmt s
  | TyFun (a, b)   -> Format.fprintf fmt "(%a -> %a)" pp_ty a pp_ty b
  | TyForall (v,t) -> Format.fprintf fmt "(forall %s. %a)" v pp_ty t
  | TyMeta mv      ->
    match mv.inst with
    | Some t -> pp_ty fmt t
    | None   -> Format.fprintf fmt "?%d" mv.id

let rec equal_ty a b = match a, b with
  | TyCon x,      TyCon y      -> x = y
  | TyVar x,      TyVar y      -> x = y
  | TyFun (a1,b1), TyFun (a2,b2) -> equal_ty a1 a2 && equal_ty b1 b2
  | TyForall (v1,t1), TyForall (v2,t2) ->
    v1 = v2 && equal_ty t1 t2
  | TyMeta m1,    TyMeta m2    -> m1.id = m2.id
  (* Dereference metas *)
  | TyMeta { inst = Some t; _ }, other
  | other, TyMeta { inst = Some t; _ } -> equal_ty t other
  | _, _ -> false

(* ------------------------------------------------------------------ *)
(* Type environment                                                     *)
(* ------------------------------------------------------------------ *)

(** A type scheme in the environment: a list of bound variables + the type. *)
type scheme = {
  bound : string list;
  body  : ty;
}

type env = (string * scheme) list

let empty_env : env = []

let env_lookup name env =
  match List.assoc_opt name env with
  | Some s -> s
  | None   -> failwith (Printf.sprintf "Typechecker: unbound variable '%s'" name)

let env_extend name scheme env = (name, scheme) :: env

(** A monomorphic scheme — no bound variables. *)
let mono ty = { bound = []; body = ty }

(* ------------------------------------------------------------------ *)
(* Unification variables                                               *)
(* ------------------------------------------------------------------ *)

let next_id = ref 0
let fresh_meta () =
  let id = !next_id in
  incr next_id;
  TyMeta { id; inst = None }

(** Dereference a chain of instantiated metas. *)
let rec deref = function
  | TyMeta { inst = Some t; _ } -> deref t
  | t -> t

(** Collect all free meta-variables in a type. *)
let rec free_metas acc = function
  | TyCon _    | TyVar _   -> acc
  | TyForall (_, t)        -> free_metas acc t
  | TyFun (a, b)           -> free_metas (free_metas acc a) b
  | TyMeta mv ->
    (match mv.inst with
     | None   -> mv :: acc
     | Some t -> free_metas acc t)

(** Free metas in the entire environment. *)
let env_free_metas env =
  List.concat_map (fun (_, { body; _ }) -> free_metas [] body) env

(* ------------------------------------------------------------------ *)
(* Effect environment                                                   *)
(* ------------------------------------------------------------------ *)

(** A single operation in an effect declaration, after conversion to [ty].
    Effect-level type parameters appear as [TyVar] and are instantiated
    afresh at each [perform] site. *)
type effect_op_scheme = {
  op_name   : string;
  op_params : ty list;
  op_return : ty;
}

(** A declared effect: its quantified type parameters and its operations. *)
type effect_scheme = {
  eff_type_params : string list;
  eff_ops         : effect_op_scheme list;
}

type effect_env = (string * effect_scheme) list

let empty_effect_env : effect_env = []

(* ------------------------------------------------------------------ *)
(* Unification                                                          *)
(* ------------------------------------------------------------------ *)

let occurs_check mv ty =
  List.exists (fun m -> m.id = mv.id) (free_metas [] ty)

let rec unify a b =
  match deref a, deref b with
  | TyCon x, TyCon y when x = y -> ()
  | TyVar x, TyVar y when x = y -> ()
  | TyFun (a1, b1), TyFun (a2, b2) ->
    unify a1 a2; unify b1 b2
  (* Same uninstantiated meta on both sides: nothing to do. Without this
     case, the occurs check below would spuriously fail, since a meta
     trivially "occurs" in itself. *)
  | TyMeta m1, TyMeta m2 when m1.id = m2.id -> ()
  | TyMeta mv, t | t, TyMeta mv ->
    if occurs_check mv t
    then failwith "Typechecker: occurs check failed (infinite type)"
    else mv.inst <- Some t
  | a, b ->
    failwith (Format.asprintf "Typechecker: cannot unify %a with %a" pp_ty a pp_ty b)

(* ------------------------------------------------------------------ *)
(* Instantiation and generalization                                     *)
(* ------------------------------------------------------------------ *)

(** Replace all bound type variables in a scheme with fresh metas.
    The scheme body uses [TyVar] for quantified variables; we replace
    each with a fresh [TyMeta] so unification can proceed. *)
let instantiate { bound; body } =
  if bound = [] then body
  else begin
    let subs = List.map (fun v -> (v, fresh_meta ())) bound in
    let rec go = function
      | TyVar v         -> (match List.assoc_opt v subs with Some t -> t | None -> TyVar v)
      | TyCon _ as t    -> t
      | TyFun (a, b)    -> TyFun (go a, go b)
      | TyForall (v, t) ->
        (* Stop substituting under a forall that shadows one of our vars *)
        let subs' = List.filter (fun (x, _) -> x <> v) subs in
        let rec go' = function
          | TyVar x         -> (match List.assoc_opt x subs' with Some u -> u | None -> TyVar x)
          | TyCon _ as c    -> c
          | TyFun (a, b)    -> TyFun (go' a, go' b)
          | TyForall (w, u) -> TyForall (w, go' u)
          | TyMeta _ as m   -> m
        in
        TyForall (v, go' t)
      | TyMeta _ as m -> m
    in
    go body
  end

(** Generalize a type over free metas not in the environment.
    Returns a scheme whose [body] is the type with metas replaced by [TyVar]s,
    and [bound] lists the variable names for instantiation.
    The body does NOT contain wrapping [TyForall] nodes — [bound] carries
    that information separately. *)
let generalize env ty =
  let env_metas = env_free_metas env in
  let env_ids   = List.map (fun m -> m.id) env_metas in
  let ty_metas  = free_metas [] ty in
  (* Deduplicate while preserving order *)
  let seen = Hashtbl.create 8 in
  let unique = List.filter (fun m ->
      if Hashtbl.mem seen m.id then false
      else (Hashtbl.add seen m.id (); true)
    ) ty_metas in
  let to_gen = List.filter (fun m -> not (List.mem m.id env_ids)) unique in
  if to_gen = [] then mono ty
  else begin
    let subs = List.mapi (fun i m ->
        let name = String.make 1 (Char.chr (Char.code 'a' + i)) in
        (m.id, name)
      ) to_gen in
    let rec go t = match deref t with
      | TyMeta mv ->
        (match List.assoc_opt mv.id subs with
         | Some name -> TyVar name
         | None      -> TyMeta mv)
      | TyCon _ as c    -> c
      | TyVar _ as v    -> v
      | TyFun (a, b)    -> TyFun (go a, go b)
      | TyForall (v, u) -> TyForall (v, go u)
    in
    let body  = go ty in
    let bound = List.map snd subs in
    { bound; body }
  end

(* ------------------------------------------------------------------ *)
(* Type-expression AST -> ty                                            *)
(* ------------------------------------------------------------------ *)

(** Convert an AST type_expr to a checker ty.
    Type variables (lowercase names not in scope as type constructors)
    are treated as rigid type variables. *)
let rec ty_of_type_expr = function
  | TyName s when String.length s > 0 && s.[0] >= 'a' && s.[0] <= 'z' ->
    TyVar s
  | TyName s  -> TyCon s
  | TyApp (s, _args) -> TyCon s
  | TyTuple _ -> fresh_meta ()    (* tuple types deferred *)
  | TyFun (param_tys, ret, _eff) ->
    let ret_ty = ty_of_type_expr ret in
    List.fold_right (fun pt acc -> TyFun (ty_of_type_expr pt, acc)) param_tys ret_ty

(* ------------------------------------------------------------------ *)
(* Inference                                                            *)
(* ------------------------------------------------------------------ *)

(** Substitute the TyVars listed in [subs] with the paired types.
    Used to instantiate an effect operation's quantified type parameters
    with fresh metas. *)
let subst_tyvars subs =
  let rec go = function
    | TyVar v as t ->
      (match List.assoc_opt v subs with Some u -> u | None -> t)
    | TyCon _ as c -> c
    | TyFun (a, b) -> TyFun (go a, go b)
    | TyForall (v, t) ->
      let subs' = List.filter (fun (x, _) -> x <> v) subs in
      let rec go' = function
        | TyVar x as t ->
          (match List.assoc_opt x subs' with Some u -> u | None -> t)
        | TyCon _ as c -> c
        | TyFun (a, b) -> TyFun (go' a, go' b)
        | TyForall (w, u) -> TyForall (w, go' u)
        | TyMeta _ as m -> m
      in
      TyForall (v, go' t)
    | TyMeta _ as m -> m
  in
  go

(** [infer_expr_in eenv env e] infers the type of [e] under the value
    environment [env] and effect environment [eenv]. Raises [Failure] on
    type errors. *)
let rec infer_expr_in (eenv : effect_env) (env : env) (e : expr) : ty =
  match e.desc with

  | IntLit _    -> TyCon "Int"
  | FloatLit _  -> TyCon "Float64"
  | StringLit _ -> TyCon "String"
  | BoolLit _   -> TyCon "Bool"
  | UnitLit     -> TyCon "Unit"

  | Var name ->
    let scheme = env_lookup name env in
    instantiate scheme

  | Let { pat; value; body } ->
    let ty_val = infer_expr_in eenv env value in
    let scheme = generalize env ty_val in
    let env'   = match pat.pat_desc with
      | PVar name -> env_extend name scheme env
      | _         -> env_from_pattern env pat ty_val
    in
    infer_expr_in eenv env' body

  | Letrec (bindings, body) ->
    (* Compute annotated types for each binding: param types + return type -> fun_ty *)
    let binding_info = List.map (fun b ->
        let param_tys = List.map (fun p -> ty_of_type_expr p.param_type) b.letrec_params in
        let ret_ty    = ty_of_type_expr b.letrec_return_type in
        let fun_ty    = List.fold_right (fun pt acc -> TyFun (pt, acc)) param_tys ret_ty in
        (b, param_tys, ret_ty, fun_ty)
      ) bindings in
    (* Extend env with all bindings at their monomorphic function types for recursion *)
    let env_rec = List.fold_left (fun acc (b, _, _, fun_ty) ->
        env_extend b.letrec_name (mono fun_ty) acc
      ) env binding_info in
    (* Infer each body in env extended with its own params, unify with return type *)
    List.iter (fun (b, param_tys, ret_ty, _) ->
        let env_params = List.fold_left2
            (fun acc p pt -> env_extend p.param_name (mono pt) acc)
            env_rec b.letrec_params param_tys
        in
        let body_ty = infer_expr_in eenv env_params b.letrec_body in
        unify body_ty ret_ty
      ) binding_info;
    (* Generalize and build the env for the continuation *)
    let env' = List.fold_left (fun acc (b, _, _, fun_ty) ->
        env_extend b.letrec_name (generalize env fun_ty) acc
      ) env binding_info in
    infer_expr_in eenv env' body

  | Fn { params; return_type; fn_body; _ } ->
    (* Replace type variable names with fresh metas so that let-generalization
       can quantify over them.  A single shared table ensures that the same name
       (e.g. 'a') maps to the same meta across all param annotations. *)
    let tv_table : (string * ty) list ref = ref [] in
    let freshen_type_expr ty_expr =
      let go = function
        | TyName s when String.length s > 0 && s.[0] >= 'a' && s.[0] <= 'z' ->
          (match List.assoc_opt s !tv_table with
           | Some m -> m
           | None   ->
             let m = fresh_meta () in
             tv_table := (s, m) :: !tv_table; m)
        | TyName s            -> TyCon s
        | TyApp (s, _)        -> TyCon s
        | TyTuple _ | TyFun _ -> fresh_meta ()
      in
      go ty_expr
    in
    let param_tys = List.map (fun p -> freshen_type_expr p.param_type) params in
    (* Extend env with monomorphic param bindings *)
    let env' = List.fold_left2
        (fun acc p ty -> env_extend p.param_name (mono ty) acc)
        env params param_tys
    in
    let body_ty = infer_expr_in eenv env' fn_body in
    (* Unify body type with return annotation if present *)
    (match return_type with
     | Some ann_ty -> unify body_ty (freshen_type_expr ann_ty)
     | None -> ());
    (* Build curried function type: p1 -> p2 -> ... -> body_ty *)
    List.fold_right (fun pt acc -> TyFun (pt, acc)) param_tys body_ty

  | App (f, args) ->
    let f_ty   = infer_expr_in eenv env f in
    let ret_ty = fresh_meta () in
    (* Build the expected function type from the arguments *)
    let arg_tys = List.map (infer_expr_in eenv env) args in
    let expected = List.fold_right (fun at acc -> TyFun (at, acc)) arg_tys ret_ty in
    unify f_ty expected;
    deref ret_ty

  | Match { scrutinee; arms } ->
    let _scrut_ty = infer_expr_in eenv env scrutinee in
    let result_ty = fresh_meta () in
    List.iter (fun arm ->
        (* Extend env with pattern bindings — conservative: only PVar binds *)
        let env' = env_from_pattern env arm.pattern _scrut_ty in
        let arm_ty = infer_expr_in eenv env' arm.arm_body in
        unify result_ty arm_ty
      ) arms;
    deref result_ty

  | Record fields ->
    (* Record types are nominal/structural — deferred until kind system.
       Infer field types for side-effects (catches unbound vars), return a fresh meta. *)
    List.iter (fun (_, e) -> ignore (infer_expr_in eenv env e)) fields;
    fresh_meta ()

  | RecordUpdate (base, fields) ->
    let _base_ty = infer_expr_in eenv env base in
    List.iter (fun (_, e) -> ignore (infer_expr_in eenv env e)) fields;
    fresh_meta ()

  | Project (e, _field) ->
    ignore (infer_expr_in eenv env e);
    fresh_meta ()

  | Perform { effect_name; op_name; args } ->
    let scheme =
      match List.assoc_opt effect_name eenv with
      | Some s -> s
      | None ->
        failwith (Printf.sprintf
                    "Typechecker: unknown effect '%s' at 'perform %s.%s'"
                    effect_name effect_name op_name)
    in
    let op =
      try List.find (fun o -> o.op_name = op_name) scheme.eff_ops
      with Not_found ->
        failwith (Printf.sprintf
                    "Typechecker: effect '%s' has no operation '%s'"
                    effect_name op_name)
    in
    (* Each perform instantiates the effect's type parameters afresh. *)
    let subs = List.map (fun v -> (v, fresh_meta ())) scheme.eff_type_params in
    let param_tys = List.map (subst_tyvars subs) op.op_params in
    let ret_ty    = subst_tyvars subs op.op_return in
    let n_params = List.length param_tys in
    let n_args   = List.length args in
    if n_params <> n_args then
      failwith (Printf.sprintf
                  "Typechecker: effect operation '%s.%s' expects %d arg(s), got %d"
                  effect_name op_name n_params n_args);
    List.iter2 (fun a pt ->
        let at = infer_expr_in eenv env a in
        unify at pt)
      args param_tys;
    deref ret_ty

  | Handle { handled; handlers } ->
    (* Type-check a linear (single-shot) handler.

       Model: each handler clause consumes one effect from the handled
       computation and produces a value of the overall `handle` result
       type [result_ty].

       - The handled expression has some value type [handled_ty]. Without a
         return clause this is also the final result; a return clause
         transforms [handled_ty] into [result_ty].
       - Each op handler receives the operation's arguments at their declared
         types (after fresh instantiation of the effect's type parameters)
         and must produce a value of [result_ty].
       - [resume] is bound in the op handler body with type
         [op_return_ty -> result_ty]. Calling it continues the handled
         computation; linear handlers resume at most once.

       Effect-row tracking on function types — and the constraint that the
       handled expression actually performs the effect being handled — is a
       follow-on in the next layer. *)
    let handled_ty = infer_expr_in eenv env handled in
    let result_ty  = fresh_meta () in
    List.iter (fun (h : effect_handler) ->
        let scheme =
          match List.assoc_opt h.effect_handler eenv with
          | Some s -> s
          | None ->
            failwith (Printf.sprintf
                        "Typechecker: unknown effect '%s' in handler"
                        h.effect_handler)
        in
        (* Each handler clause instantiates the effect's type parameters
           with fresh metas. These are shared across the op clauses of
           this handler so that, e.g. for State<s>, both get: () -> s and
           put: (s) -> Unit refer to the same s. *)
        let subs = List.map (fun v -> (v, fresh_meta ())) scheme.eff_type_params in
        List.iter (fun (oh : op_handler) ->
            let op =
              try List.find (fun o -> o.op_name = oh.op_handler_name) scheme.eff_ops
              with Not_found ->
                failwith (Printf.sprintf
                            "Typechecker: effect '%s' has no operation '%s' \
                             (in handler clause)"
                            h.effect_handler oh.op_handler_name)
            in
            let op_param_tys = List.map (subst_tyvars subs) op.op_params in
            let op_ret_ty    = subst_tyvars subs op.op_return in
            let n_params = List.length op_param_tys in
            let n_names  = List.length oh.op_handler_params in
            if n_params <> n_names then
              failwith (Printf.sprintf
                          "Typechecker: handler for '%s.%s' binds %d param(s), \
                           but the operation declares %d"
                          h.effect_handler oh.op_handler_name n_names n_params);
            (* Bind each param name to the declared op param type. *)
            let env_params =
              List.fold_left2 (fun acc name t -> env_extend name (mono t) acc)
                env oh.op_handler_params op_param_tys
            in
            (* Bind `resume` : op_return_ty -> result_ty. Linear handlers
               resume at most once, so we type it as a function from the
               op's return value to the overall handle result. *)
            let resume_ty  = TyFun (op_ret_ty, result_ty) in
            let env_resume = env_extend "resume" (mono resume_ty) env_params in
            let body_ty    = infer_expr_in eenv env_resume oh.op_handler_body in
            unify body_ty result_ty)
          h.op_handlers;
        (* Return clause (if present) transforms the handled value into the
           final handle result. Without one, the two types coincide. *)
        (match h.return_handler with
         | None ->
           unify handled_ty result_ty
         | Some { return_var; return_body } ->
           let env_ret = env_extend return_var (mono handled_ty) env in
           let body_ty = infer_expr_in eenv env_ret return_body in
           unify body_ty result_ty))
      handlers;
    deref result_ty

  | Do stmts ->
    (* Each StmtExpr is inferred and discarded; StmtLet extends the env.
       The final stmt must be a StmtExpr whose type is the block's type. *)
    let rec go env = function
      | []                         -> TyCon "Unit"
      | [StmtExpr e]               -> infer_expr_in eenv env e
      | StmtExpr e :: rest         -> ignore (infer_expr_in eenv env e); go env rest
      | StmtLet { pat; value } :: rest ->
        let ty   = infer_expr_in eenv env value in
        let env' = match pat.pat_desc with
          | PVar name -> env_extend name (generalize env ty) env
          | _         -> env_from_pattern env pat ty
        in
        go env' rest
    in
    go env stmts

  | If { cond; then_; else_ } ->
    unify (infer_expr_in eenv env cond) (TyCon "Bool");
    let t = infer_expr_in eenv env then_ in
    let e = infer_expr_in eenv env else_ in
    unify t e;
    deref t

(** Extend environment with bindings introduced by a pattern.
    Currently handles PWild and PVar; constructor patterns don't bind new scrutinee types. *)
and env_from_pattern env (p : pattern) scrut_ty =
  match p.pat_desc with
  | PWild         -> env
  | PVar x        -> env_extend x (mono scrut_ty) env
  | PLitInt _ | PLitFloat _ | PLitString _
  | PLitTrue | PLitFalse | PLitUnit -> env
  | PCtor (_, sub_pats) ->
    List.fold_left (fun e p -> env_from_pattern e p (fresh_meta ())) env sub_pats
  | PRecord (fields, _) ->
    List.fold_left (fun e (_, p) -> env_from_pattern e p (fresh_meta ())) env fields
  | POr (p1, _p2) ->
    (* Both branches bind the same variables; use p1 *)
    env_from_pattern env p1 scrut_ty

(** [infer_expr env e] infers the type of [e] with no effects declared.
    Program-level inference — with declared effects in scope — uses
    [check_program] or [infer_expr_in] directly. *)
let infer_expr (env : env) (e : expr) : ty =
  infer_expr_in empty_effect_env env e

(* ------------------------------------------------------------------ *)
(* Program-level checking                                              *)
(* ------------------------------------------------------------------ *)

(** Build an effect scheme from an AST effect declaration. *)
let effect_scheme_of_decl (type_params : string list) (ops : Ast.effect_op list)
  : effect_scheme =
  let eff_ops = List.map (fun (o : Ast.effect_op) ->
      { op_name   = o.effect_op_name
      ; op_params = List.map ty_of_type_expr o.effect_op_params
      ; op_return = ty_of_type_expr o.effect_op_return
      }) ops
  in
  { eff_type_params = type_params; eff_ops }

(** Build a top-level function scheme from its declared parameter types
    and return type. Type parameters named on the declaration become the
    scheme's bound variables so callers instantiate them with fresh metas. *)
let fn_scheme_of_decl
    (type_params : string list)
    (params      : param list)
    (return_type : type_expr option)
  : scheme =
  let param_tys = List.map (fun p -> ty_of_type_expr p.param_type) params in
  let ret_ty = match return_type with
    | Some t -> ty_of_type_expr t
    | None   -> fresh_meta ()
  in
  let fun_ty = List.fold_right (fun pt acc -> TyFun (pt, acc)) param_tys ret_ty in
  { bound = type_params; body = fun_ty }

(** [check_program prog] type-checks a whole program in two passes:

    Pass 1 walks every declaration and builds:
    - the value environment, seeded with each [DeclFn]'s declared signature
      (as a scheme over its type parameters), so that bodies can reference
      one another regardless of declaration order;
    - the effect environment, one entry per [DeclEffect], carrying the
      operation signatures to be instantiated at each [perform] site.

    Pass 2 walks [DeclFn]s again and type-checks each body under the seeded
    environment extended with the function's own parameter bindings; the
    body's inferred type is then unified with the declared return type.

    [DeclType], [DeclModule], [DeclRequire]: not yet handled by this pass.
    Constructors remain unbound in the value env and module bodies are
    not recursed into — those are follow-on work items.

    Returns the populated [(env, effect_env)] for reuse in tests and
    downstream tooling. Raises [Failure] on type errors. *)
let check_program (prog : program) : env * effect_env =
  let collect (env, eenv) d =
    match d.decl_desc with
    | DeclEffect { effect_name; type_params; ops; _ } ->
      let scheme = effect_scheme_of_decl type_params ops in
      (env, (effect_name, scheme) :: eenv)
    | DeclFn { fn_name; type_params; params; return_type; _ } ->
      let scheme = fn_scheme_of_decl type_params params return_type in
      ((fn_name, scheme) :: env, eenv)
    | DeclType _ | DeclModule _ | DeclRequire _ ->
      (env, eenv)
  in
  let env0, eenv0 =
    List.fold_left collect (empty_env, empty_effect_env) prog
  in
  List.iter (fun d ->
      match d.decl_desc with
      | DeclFn { params; return_type; decl_body; _ } ->
        let param_tys = List.map (fun p -> ty_of_type_expr p.param_type) params in
        let body_env =
          List.fold_left2 (fun acc p t -> env_extend p.param_name (mono t) acc)
            env0 params param_tys
        in
        let body_ty = infer_expr_in eenv0 body_env decl_body in
        (match return_type with
         | Some t -> unify body_ty (ty_of_type_expr t)
         | None   -> ())
      | _ -> ())
    prog;
  (env0, eenv0)
