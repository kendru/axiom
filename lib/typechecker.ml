(** Axiom type checker — Hindley-Milner with let-generalization.

    Algorithm W extended with:
    - Annotated function parameters (the annotation is trusted, not checked
      against an external source of truth yet — that comes with the full
      declaration checker).
    - Let-generalization: the type of a let-bound expression is generalized
      over all free type variables not in the environment.
    - Effects are ignored in this pass; only value types are inferred.
      Effect checking is a future layer. *)

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
let ty_of_type_expr = function
  | TyName s when String.length s > 0 && s.[0] >= 'a' && s.[0] <= 'z' ->
    TyVar s   (* lowercase: type variable *)
  | TyName s  -> TyCon s
  | TyApp (s, _args) -> TyCon s  (* ignore params for now — type ctors are opaque *)

(* ------------------------------------------------------------------ *)
(* Inference                                                            *)
(* ------------------------------------------------------------------ *)

(** [infer_expr env e] infers the type of expression [e] under [env].
    Returns the inferred type. Raises [Failure] on type errors. *)
let rec infer_expr (env : env) (e : expr) : ty =
  match e with

  | IntLit _    -> TyCon "Int"
  | FloatLit _  -> TyCon "Float64"
  | StringLit _ -> TyCon "String"
  | BoolLit _   -> TyCon "Bool"
  | UnitLit     -> TyCon "Unit"

  | Var name ->
    let scheme = env_lookup name env in
    instantiate scheme

  | Let { name; value; body } ->
    let ty_val  = infer_expr env value in
    let scheme  = generalize env ty_val in
    let env'    = env_extend name scheme env in
    infer_expr env' body

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
        let body_ty = infer_expr env_params b.letrec_body in
        unify body_ty ret_ty
      ) binding_info;
    (* Generalize and build the env for the continuation *)
    let env' = List.fold_left (fun acc (b, _, _, fun_ty) ->
        env_extend b.letrec_name (generalize env fun_ty) acc
      ) env binding_info in
    infer_expr env' body

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
        | TyName s     -> TyCon s
        | TyApp (s, _) -> TyCon s   (* type args ignored until kind system exists *)
      in
      go ty_expr
    in
    let param_tys = List.map (fun p -> freshen_type_expr p.param_type) params in
    (* Extend env with monomorphic param bindings *)
    let env' = List.fold_left2
        (fun acc p ty -> env_extend p.param_name (mono ty) acc)
        env params param_tys
    in
    let body_ty = infer_expr env' fn_body in
    (* Unify body type with return annotation if present *)
    (match return_type with
     | Some ann_ty -> unify body_ty (freshen_type_expr ann_ty)
     | None -> ());
    (* Build curried function type: p1 -> p2 -> ... -> body_ty *)
    List.fold_right (fun pt acc -> TyFun (pt, acc)) param_tys body_ty

  | App (f, args) ->
    let f_ty   = infer_expr env f in
    let ret_ty = fresh_meta () in
    (* Build the expected function type from the arguments *)
    let arg_tys = List.map (infer_expr env) args in
    let expected = List.fold_right (fun at acc -> TyFun (at, acc)) arg_tys ret_ty in
    unify f_ty expected;
    deref ret_ty

  | Match { scrutinee; arms } ->
    let _scrut_ty = infer_expr env scrutinee in
    let result_ty = fresh_meta () in
    List.iter (fun arm ->
        (* Extend env with pattern bindings — conservative: only PVar binds *)
        let env' = env_from_pattern env arm.pattern _scrut_ty in
        let arm_ty = infer_expr env' arm.arm_body in
        unify result_ty arm_ty
      ) arms;
    deref result_ty

  | Record fields ->
    (* Record types are nominal/structural — deferred until kind system.
       Infer field types for side-effects (catches unbound vars), return a fresh meta. *)
    List.iter (fun (_, e) -> ignore (infer_expr env e)) fields;
    fresh_meta ()

  | RecordUpdate (base, fields) ->
    let _base_ty = infer_expr env base in
    List.iter (fun (_, e) -> ignore (infer_expr env e)) fields;
    fresh_meta ()

  | Project (e, _field) ->
    ignore (infer_expr env e);
    fresh_meta ()

  | Perform _ ->
    (* Effect typing deferred — return a fresh meta for now *)
    fresh_meta ()

  | Handle { handled; handlers = _ } ->
    (* Effect typing deferred — infer the handled expression's type for now *)
    infer_expr env handled

  | Do stmts ->
    (* Each StmtExpr is inferred and discarded; StmtLet extends the env.
       The final stmt must be a StmtExpr whose type is the block's type. *)
    let rec go env = function
      | []                         -> TyCon "Unit"
      | [StmtExpr e]               -> infer_expr env e
      | StmtExpr e :: rest         -> ignore (infer_expr env e); go env rest
      | StmtLet { name; value } :: rest ->
        let ty  = infer_expr env value in
        let env' = env_extend name (generalize env ty) env in
        go env' rest
    in
    go env stmts

  | If { cond; then_; else_ } ->
    unify (infer_expr env cond) (TyCon "Bool");
    let t = infer_expr env then_ in
    let e = infer_expr env else_ in
    unify t e;
    deref t

(** Extend environment with bindings introduced by a pattern.
    Currently handles PWild and PVar; constructor patterns don't bind new scrutinee types. *)
and env_from_pattern env pat scrut_ty =
  match pat with
  | PWild  -> env
  | PVar x -> env_extend x (mono scrut_ty) env
  | PLit _ -> env
  | PCtor (_, sub_pats) ->
    (* Give each sub-pattern a fresh meta for now *)
    List.fold_left (fun e p -> env_from_pattern e p (fresh_meta ())) env sub_pats
