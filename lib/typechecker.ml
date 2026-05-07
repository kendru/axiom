(** Axiom type checker — Hindley-Milner with let-generalization and
    row-typed effects.

    Algorithm W extended with:
    - Annotated function parameters (the annotation is trusted, not checked
      against an external source of truth yet — that comes with the full
      declaration checker).
    - Let-generalization: the type of a let-bound expression is generalized
      over all free type variables not in the environment.
    - Program-level checking via [check_program]: a two-pass walk collects
      top-level effect and function declarations, then checks each function
      body against its declared signature.
    - Row-typed effects. Every [TyFun] carries an effect row. Every
      expression is type-checked against an ambient row. [perform E.op]
      requires [E] to be in the ambient; [handle e with \{ E { ... } \}]
      adds [E] to the handled expression's ambient while leaving the
      enclosing ambient unchanged. Row unification is Rémy-style: rows
      may be reordered, and an open row (one with a row-meta tail) can
      absorb additional effects from the other side.
    - Implicit effect polymorphism via row re-opening at instantiation.
      Each use of a function scheme freshly re-opens every [TyFun] row in
      its body: a function declared with effect row [\{Log\}] is visible at
      each call site as [\{Log | ρ\}] for some fresh [ρ], so it may be
      called under any ambient row that contains [\{Log\}]. *)

open Ast

(* ------------------------------------------------------------------ *)
(* Types and effect rows                                                *)
(* ------------------------------------------------------------------ *)

type ty =
  | TyCon    of string          (** Int, Bool, String, Unit, Float64 *)
  | TyVar    of string          (** type variable (before unification) *)
  | TyFun    of ty * ty * row   (** A -> B ! ε (curried) *)
  | TyForall of string * ty     (** forall a. T  (generalized) *)
  | TyMeta   of meta_var        (** unification variable *)
  | TyRecord of (string * ty) list * rec_meta option
                               (** structural record: {f1:T1,...} or open {f1:T1,...|ρ} *)
  | TyApp    of string * ty list (** parameterized type: Option<Int>, List<a>, Map<K,V> *)

and meta_var = {
  id       : int;
  mutable inst : ty option;   (** None = uninstantiated *)
}

(** An effect row — the set of effects a computation may perform.
    [RPure] is the empty, closed row. [RCons (E, r)] prepends an effect
    to a row. [RMeta m] is a row unification variable; it may be
    instantiated either to [RPure] (absorbing no more effects) or to
    [RCons (E, fresh)] when row unification needs to extend the row. *)
and row =
  | RPure
  | RCons of effect_inst * row
  | RMeta of row_meta

and effect_inst = {
  eff_name : string;
  eff_args : ty list;   (** Instantiation of the effect's type parameters. *)
}

and row_meta = {
  rid        : int;
  mutable rinst : row option;
}

(** A record row variable.  [rrec_inst = None] = open/unknown tail.
    When instantiated, always points to a [TyRecord] carrying the extra fields
    that were absorbed during unification. *)
and rec_meta = {
  rrec_id   : int;
  mutable rrec_inst : ty option;
}

(* ------------------------------------------------------------------ *)
(* Record-tail helper (used throughout; defined early for pp_ty/equal_ty) *)
(* ------------------------------------------------------------------ *)

(** Flatten a record by chasing instantiated [rec_meta] tails.
    Returns [(all_fields, tail)] where [tail = None] means the record is now
    fully closed and [tail = Some rm] means [rm] is still open (uninstantiated). *)
let rec flatten_record fields tail =
  match tail with
  | None -> (fields, None)
  | Some rm ->
    match rm.rrec_inst with
    | None -> (fields, Some rm)
    | Some (TyRecord (extra, t)) -> flatten_record (fields @ extra) t
    | Some _ -> (fields, None)   (* defensive; shouldn't occur *)

(* ------------------------------------------------------------------ *)
(* Pretty-printer and equality (for tests)                             *)
(* ------------------------------------------------------------------ *)

let rec pp_ty fmt = function
  | TyCon s        -> Format.pp_print_string fmt s
  | TyVar s        -> Format.pp_print_string fmt s
  | TyFun (a, b, r) ->
    Format.fprintf fmt "(%a -> %a ! %a)" pp_ty a pp_ty b pp_row r
  | TyForall (v,t) -> Format.fprintf fmt "(forall %s. %a)" v pp_ty t
  | TyMeta mv      ->
    (match mv.inst with
     | Some t -> pp_ty fmt t
     | None   -> Format.fprintf fmt "?%d" mv.id)
  | TyRecord (fields, tail) ->
    let all_fields, final_tail = flatten_record fields tail in
    let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) all_fields in
    Format.fprintf fmt "{%a%s}"
      (Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f ", ")
         (fun fmt (name, ty) -> Format.fprintf fmt "%s: %a" name pp_ty ty))
      sorted
      (match final_tail with
       | None    -> ""
       | Some rm -> Format.sprintf " | ?r%d" rm.rrec_id)
  | TyApp (name, args) ->
    Format.fprintf fmt "%s<%a>" name
      (Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f ", ")
         pp_ty)
      args

and pp_row fmt = function
  | RPure -> Format.pp_print_string fmt "pure"
  | RMeta rm ->
    (match rm.rinst with
     | Some r -> pp_row fmt r
     | None   -> Format.fprintf fmt "?ρ%d" rm.rid)
  | r ->
    let rec collect acc = function
      | RPure     -> (List.rev acc, None)
      | RCons (e, rest) -> collect (e :: acc) rest
      | RMeta rm ->
        (match rm.rinst with
         | Some r -> collect acc r
         | None   -> (List.rev acc, Some rm))
    in
    let effs, tail = collect [] r in
    Format.fprintf fmt "{";
    Format.pp_print_list
      ~pp_sep:(fun f () -> Format.pp_print_string f ", ")
      pp_effect_inst fmt effs;
    (match tail with
     | None -> Format.fprintf fmt "}"
     | Some rm -> Format.fprintf fmt " | ?ρ%d}" rm.rid)

and pp_effect_inst fmt { eff_name; eff_args } =
  if eff_args = [] then Format.pp_print_string fmt eff_name
  else
    Format.fprintf fmt "%s<%a>" eff_name
      (Format.pp_print_list
         ~pp_sep:(fun f () -> Format.pp_print_string f ", ")
         pp_ty)
      eff_args

let rec equal_ty a b = match a, b with
  | TyCon x,      TyCon y      -> x = y
  | TyVar x,      TyVar y      -> x = y
  | TyFun (a1,b1,r1), TyFun (a2,b2,r2) ->
    equal_ty a1 a2 && equal_ty b1 b2 && equal_row r1 r2
  | TyForall (v1,t1), TyForall (v2,t2) ->
    v1 = v2 && equal_ty t1 t2
  | TyMeta m1,    TyMeta m2    -> m1.id = m2.id
  (* Dereference metas *)
  | TyMeta { inst = Some t; _ }, other
  | other, TyMeta { inst = Some t; _ } -> equal_ty t other
  | TyRecord (fa, ta), TyRecord (fb, tb) ->
    let fa, ta = flatten_record fa ta and fb, tb = flatten_record fb tb in
    let sort f = List.sort (fun (a, _) (b, _) -> String.compare a b) f in
    let fa' = sort fa and fb' = sort fb in
    List.length fa' = List.length fb'
    && List.for_all2 (fun (na, ta) (nb, tb) -> na = nb && equal_ty ta tb) fa' fb'
    && (match ta, tb with
        | None,    None    -> true
        | Some ra, Some rb -> ra.rrec_id = rb.rrec_id
        | _,       _       -> false)
  | TyApp (n1, a1), TyApp (n2, a2) ->
    n1 = n2 && List.length a1 = List.length a2
    && List.for_all2 equal_ty a1 a2
  | _, _ -> false

and equal_row a b = match a, b with
  | RPure, RPure -> true
  | RCons (e1, r1), RCons (e2, r2) -> equal_effect_inst e1 e2 && equal_row r1 r2
  | RMeta m1, RMeta m2 -> m1.rid = m2.rid
  | RMeta { rinst = Some r; _ }, other
  | other, RMeta { rinst = Some r; _ } -> equal_row r other
  | _, _ -> false

and equal_effect_inst a b =
  a.eff_name = b.eff_name
  && List.length a.eff_args = List.length b.eff_args
  && List.for_all2 equal_ty a.eff_args b.eff_args

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

let next_rid = ref 0
let fresh_row_meta () =
  let rid = !next_rid in
  incr next_rid;
  { rid; rinst = None }

let fresh_row () = RMeta (fresh_row_meta ())

let next_rrec_id = ref 0
let fresh_rec_meta () =
  let id = !next_rrec_id in
  incr next_rrec_id;
  { rrec_id = id; rrec_inst = None }

(** Dereference a chain of instantiated type metas. *)
let rec deref = function
  | TyMeta { inst = Some t; _ } -> deref t
  | t -> t

(** Dereference a chain of instantiated row metas. *)
let rec row_deref = function
  | RMeta { rinst = Some r; _ } -> row_deref r
  | r -> r

(** Collect all free type metas in a type (including those reached
    through effect rows). *)
let rec free_metas acc = function
  | TyCon _    | TyVar _   -> acc
  | TyForall (_, t)        -> free_metas acc t
  | TyFun (a, b, r)        ->
    let acc = free_metas (free_metas acc a) b in
    free_row_ty_metas acc r
  | TyMeta mv ->
    (match mv.inst with
     | None   -> mv :: acc
     | Some t -> free_metas acc t)
  | TyRecord (fields, tail) ->
    let all_fields, _ = flatten_record fields tail in
    List.fold_left (fun acc (_, t) -> free_metas acc t) acc all_fields
  | TyApp (_, args) ->
    List.fold_left free_metas acc args

and free_row_ty_metas acc = function
  | RPure -> acc
  | RCons (e, rest) ->
    let acc = List.fold_left free_metas acc e.eff_args in
    free_row_ty_metas acc rest
  | RMeta rm ->
    (match rm.rinst with
     | None   -> acc
     | Some r -> free_row_ty_metas acc r)

(** Collect all free row metas reachable from a type. *)
let rec free_row_metas acc = function
  | TyCon _ | TyVar _ -> acc
  | TyForall (_, t) -> free_row_metas acc t
  | TyFun (a, b, r) ->
    let acc = free_row_metas (free_row_metas acc a) b in
    free_row_metas_row acc r
  | TyMeta mv ->
    (match mv.inst with
     | None   -> acc
     | Some t -> free_row_metas acc t)
  | TyRecord (fields, tail) ->
    let all_fields, _ = flatten_record fields tail in
    List.fold_left (fun acc (_, t) -> free_row_metas acc t) acc all_fields
  | TyApp (_, args) ->
    List.fold_left free_row_metas acc args

and free_row_metas_row acc = function
  | RPure -> acc
  | RCons (e, rest) ->
    let acc = List.fold_left free_row_metas acc e.eff_args in
    free_row_metas_row acc rest
  | RMeta rm ->
    (match rm.rinst with
     | None   -> rm :: acc
     | Some r -> free_row_metas_row acc r)

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

(** Maps each declared type name to its constructor names.
    Populated by [check_program] before inference begins; used by
    [check_match_exhaustive] to verify pattern coverage for ADTs. *)
let type_ctor_env : (string * string list) list ref = ref []

(* ------------------------------------------------------------------ *)
(* Unification                                                          *)
(* ------------------------------------------------------------------ *)

let occurs_check mv ty =
  List.exists (fun m -> m.id = mv.id) (free_metas [] ty)

let row_occurs_check rm row =
  List.exists (fun m -> m.rid = rm.rid) (free_row_metas_row [] row)

let rec unify a b =
  match deref a, deref b with
  | TyCon x, TyCon y when x = y -> ()
  | TyVar x, TyVar y when x = y -> ()
  | TyFun (a1, b1, r1), TyFun (a2, b2, r2) ->
    unify a1 a2; unify b1 b2; unify_row r1 r2
  (* Same uninstantiated meta on both sides: nothing to do. Without this
     case, the occurs check below would spuriously fail, since a meta
     trivially "occurs" in itself. *)
  | TyMeta m1, TyMeta m2 when m1.id = m2.id -> ()
  | TyMeta mv, t | t, TyMeta mv ->
    if occurs_check mv t
    then failwith "Typechecker: occurs check failed (infinite type)"
    else mv.inst <- Some t
  | TyRecord (fa, tail_a), TyRecord (fb, tail_b) ->
    let fa, tail_a = flatten_record fa tail_a in
    let fb, tail_b = flatten_record fb tail_b in
    unify_records fa tail_a fb tail_b
  | TyApp (n1, a1), TyApp (n2, a2) ->
    if n1 <> n2 then
      failwith (Format.asprintf
                  "Typechecker: cannot unify %a with %a" pp_ty (TyApp (n1, a1)) pp_ty (TyApp (n2, a2)));
    if List.length a1 <> List.length a2 then
      failwith (Format.asprintf
                  "Typechecker: type constructor %s applied to wrong number of arguments" n1);
    List.iter2 unify a1 a2
  | TyCon "Nothing", _ | _, TyCon "Nothing" -> ()
  | a, b ->
    failwith (Format.asprintf "Typechecker: cannot unify %a with %a" pp_ty a pp_ty b)

(** Row unification — Rémy's algorithm. Two rows unify when they contain
    the same multiset of effects, with open tails absorbing any
    remainder. The order of effects does not matter. *)
and unify_row a b =
  match row_deref a, row_deref b with
  | RPure, RPure -> ()
  | RMeta m1, RMeta m2 when m1.rid = m2.rid -> ()
  | RMeta rm, r | r, RMeta rm ->
    if row_occurs_check rm r
    then failwith "Typechecker: row occurs check failed (cyclic row)"
    else rm.rinst <- Some r
  | RCons (e, rest1), r2 ->
    let rest2 = rewrite_row r2 e in
    unify_row rest1 rest2
  | RPure, RCons (e, _) ->
    failwith (Format.asprintf
                "Typechecker: cannot unify pure row with row containing %a"
                pp_effect_inst e)

(** [rewrite_row r e] unifies the first head of [r] equal to [e] out of
    [r] and returns the remainder. If [r] ends in an open row meta, that
    meta is extended with [e] to create a fresh tail.
    The arguments of matching effects are unified in place. *)
and rewrite_row r e =
  match row_deref r with
  | RPure ->
    failwith (Format.asprintf
                "Typechecker: effect %a is not present in the expected row"
                pp_effect_inst e)
  | RCons (e', rest) ->
    if e.eff_name = e'.eff_name
       && List.length e.eff_args = List.length e'.eff_args
    then begin
      List.iter2 unify e.eff_args e'.eff_args;
      rest
    end else
      RCons (e', rewrite_row rest e)
  | RMeta rm ->
    let fresh = fresh_row_meta () in
    rm.rinst <- Some (RCons (e, RMeta fresh));
    RMeta fresh

(** Unify two already-flattened record types.
    - Both closed ([None]): field sets must be identical.
    - One open, one closed: the open tail absorbs the extra fields from the
      closed side; all fields declared by the open side must exist in the
      closed side.
    - Both open: unify common fields; each tail absorbs the unique fields of
      the other side, sharing a fresh tail for any further unknowns. *)
and unify_records fa tail_a fb tail_b =
  match tail_a, tail_b with
  | None, None ->
    let sort f = List.sort (fun (a, _) (b, _) -> String.compare a b) f in
    let fa' = sort fa and fb' = sort fb in
    if List.length fa' <> List.length fb' then
      failwith (Format.asprintf
                  "Typechecker: cannot unify record types with different fields: %a vs %a"
                  pp_ty (TyRecord (fa, None)) pp_ty (TyRecord (fb, None)))
    else
      List.iter2 (fun (na, ta) (nb, tb) ->
          if na <> nb then
            failwith (Printf.sprintf
                        "Typechecker: record field name mismatch: '%s' vs '%s'" na nb)
          else unify ta tb)
        fa' fb'
  | Some rm_a, None ->
    List.iter (fun (na, ta) ->
        match List.assoc_opt na fb with
        | None ->
          failwith (Printf.sprintf
                      "Typechecker: open record requires field '%s' \
                       not present in closed record" na)
        | Some tb -> unify ta tb
      ) fa;
    let extra = List.filter (fun (n, _) -> not (List.mem_assoc n fa)) fb in
    rm_a.rrec_inst <- Some (TyRecord (extra, None))
  | None, Some rm_b ->
    List.iter (fun (nb, tb) ->
        match List.assoc_opt nb fa with
        | None ->
          failwith (Printf.sprintf
                      "Typechecker: open record requires field '%s' \
                       not present in closed record" nb)
        | Some ta -> unify ta tb
      ) fb;
    let extra = List.filter (fun (n, _) -> not (List.mem_assoc n fb)) fa in
    rm_b.rrec_inst <- Some (TyRecord (extra, None))
  | Some rm_a, Some rm_b when rm_a.rrec_id = rm_b.rrec_id ->
    List.iter (fun (na, ta) ->
        match List.assoc_opt na fb with
        | Some tb -> unify ta tb
        | None    -> ()
      ) fa
  | Some rm_a, Some rm_b ->
    let only_a = List.filter (fun (n, _) -> not (List.mem_assoc n fb)) fa in
    let only_b = List.filter (fun (n, _) -> not (List.mem_assoc n fa)) fb in
    List.iter (fun (na, ta) ->
        match List.assoc_opt na fb with
        | Some tb -> unify ta tb
        | None    -> ()
      ) fa;
    let shared = fresh_rec_meta () in
    rm_a.rrec_inst <- Some (TyRecord (only_b, Some shared));
    rm_b.rrec_inst <- Some (TyRecord (only_a, Some shared))

(* ------------------------------------------------------------------ *)
(* Instantiation and generalization                                     *)
(* ------------------------------------------------------------------ *)

(** Walk a row, possibly substituting TyVars inside effect args, and
    always re-open a closed tail with a fresh row meta. The re-opening
    is what gives every function use its own polymorphic effect tail:
    a declared closed row [\{E1, E2\}] is visible at the call site as
    [\{E1, E2 | ?ρ\}], so ?ρ can absorb the rest of the ambient row. *)
let rec reopen_row subs = function
  | RPure -> fresh_row ()
  | RCons (e, rest) ->
    let e' = { e with eff_args = List.map (subst_ty subs) e.eff_args } in
    RCons (e', reopen_row subs rest)
  | RMeta rm as r ->
    (match rm.rinst with
     | Some r' -> reopen_row subs r'
     | None    -> r)

(** Substitute TyVars listed in [subs], and re-open every TyFun row. *)
and subst_ty subs = function
  | TyVar v as t ->
    (match List.assoc_opt v subs with Some u -> u | None -> t)
  | TyCon _ as c -> c
  | TyFun (a, b, r) -> TyFun (subst_ty subs a, subst_ty subs b, reopen_row subs r)
  | TyForall (v, t) ->
    let subs' = List.filter (fun (x, _) -> x <> v) subs in
    TyForall (v, subst_ty subs' t)
  | TyMeta _ as m -> m
  | TyRecord (fields, tail) ->
    TyRecord (List.map (fun (n, t) -> (n, subst_ty subs t)) fields, tail)
  | TyApp (name, args) -> TyApp (name, List.map (subst_ty subs) args)

(** Replace all bound type variables in a scheme with fresh metas, and
    re-open every TyFun row along the way with a fresh row meta tail.
    Each use of a scheme gets fresh metas — including row metas — so
    function values are effectively row-polymorphic. *)
let instantiate { bound; body } =
  let subs = List.map (fun v -> (v, fresh_meta ())) bound in
  subst_ty subs body

(** Generalize a type over free type metas not in the environment.
    Returns a scheme whose [body] is the type with metas replaced by [TyVar]s,
    and [bound] lists the variable names for instantiation.
    The body does NOT contain wrapping [TyForall] nodes — [bound] carries
    that information separately.

    Row metas are not generalized here; they are re-opened afresh on each
    instantiation (see [instantiate]/[subst_ty]), which provides implicit
    row polymorphism without an explicit quantifier. *)
let generalize env ty =
  let env_metas = env_free_metas env in
  let env_ids   = List.map (fun m -> m.id) env_metas in
  let ty_metas  = free_metas [] ty in
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
      | TyFun (a, b, r) -> TyFun (go a, go b, go_row r)
      | TyForall (v, u) -> TyForall (v, go u)
      | TyRecord (fields, tail) ->
        TyRecord (List.map (fun (n, t) -> (n, go t)) fields, tail)
      | TyApp (name, args) -> TyApp (name, List.map go args)
    and go_row = function
      | RPure -> RPure
      | RCons (e, rest) ->
        RCons ({ e with eff_args = List.map go e.eff_args }, go_row rest)
      | RMeta rm as r ->
        (match rm.rinst with
         | Some r' -> go_row r'
         | None    -> r)
    in
    let body  = go ty in
    let bound = List.map snd subs in
    { bound; body }
  end

(* ------------------------------------------------------------------ *)
(* Type-expression AST -> ty                                            *)
(* ------------------------------------------------------------------ *)

(** Maps named row-tail variables (e.g. the [e] in [{ Log | e }]) to a
    shared [row_meta] within a single function signature so that two
    occurrences of the same name produce the same unification variable. *)
type row_var_env = (string, row_meta) Hashtbl.t

let new_row_var_env () : row_var_env = Hashtbl.create 4

let find_or_create_row_meta (env : row_var_env) (name : string) : row_meta =
  match Hashtbl.find_opt env name with
  | Some rm -> rm
  | None ->
    let rm = fresh_row_meta () in
    Hashtbl.add env name rm;
    rm

(** Convert an AST type_expr to a checker ty, sharing row-variable
    metas within [env] so that the same variable name at two positions
    maps to the same [RMeta]. *)
let rec ty_of_type_expr_env (env : row_var_env) = function
  | TyName s when String.length s > 0 && s.[0] >= 'a' && s.[0] <= 'z' ->
    TyVar s
  | TyName s  -> TyCon s
  | TyApp (s, args) -> TyApp (s, List.map (ty_of_type_expr_env env) args)
  | TyTuple _ -> fresh_meta ()    (* tuple types deferred *)
  | TyFun (param_tys, ret, eff) ->
    let ret_ty = ty_of_type_expr_env env ret in
    let row = row_of_effect_set_opt_env env eff in
    (* Curried: the declared effect row lives on the innermost arrow (the
       last one to be applied), so we build from the right and attach [row]
       there. Intermediate arrows get fresh open rows. *)
    (match List.rev param_tys with
     | [] -> ret_ty   (* nullary function types have no arrow at all *)
     | last :: rest_rev ->
       let innermost = TyFun (ty_of_type_expr_env env last, ret_ty, row) in
       List.fold_left
         (fun acc pt -> TyFun (ty_of_type_expr_env env pt, acc, fresh_row ()))
         innermost rest_rev)

(** Convert an AST [effect_set] to a checker row using [env] to share
    row-variable metas. [Pure] → [RPure]; closed [Effects] → [RCons]
    chain ending in [RPure]; open [Effects] with tail variable → chain
    ending in the [RMeta] for that variable. *)
and row_of_effect_set_env (env : row_var_env) = function
  | Pure -> RPure
  | Effects (ts, None) ->
    List.fold_right
      (fun t acc -> RCons (effect_inst_of_type_expr_env env t, acc))
      ts RPure
  | Effects (ts, Some v) ->
    let rm = find_or_create_row_meta env v in
    List.fold_right
      (fun t acc -> RCons (effect_inst_of_type_expr_env env t, acc))
      ts (RMeta rm)

and row_of_effect_set_opt_env (env : row_var_env) = function
  | Some e -> row_of_effect_set_env env e
  | None   -> fresh_row ()   (* unannotated: inference will discover it *)

and effect_inst_of_type_expr_env (env : row_var_env) = function
  | TyName s -> { eff_name = s; eff_args = [] }
  | TyApp (s, args) ->
    { eff_name = s; eff_args = List.map (ty_of_type_expr_env env) args }
  | t ->
    failwith (Format.asprintf
                "Typechecker: not a legal effect: %a"
                Ast.pp_type_expr t)

(** Shims with the original names — use a fresh empty env so existing
    call sites that don't need row-variable sharing continue to work. *)
let ty_of_type_expr t       = ty_of_type_expr_env (new_row_var_env ()) t
let row_of_effect_set e     = row_of_effect_set_env (new_row_var_env ()) e
let row_of_effect_set_opt e = row_of_effect_set_opt_env (new_row_var_env ()) e

(* ------------------------------------------------------------------ *)
(* Exhaustiveness checking                                              *)
(* ------------------------------------------------------------------ *)

let rec flatten_or_pat p =
  match p.pat_desc with
  | POr (p1, p2) -> flatten_or_pat p1 @ flatten_or_pat p2
  | _ -> [p]

(** [missing_cases scrut_ty pats] returns [None] if [pats] cover all
    possible values of [scrut_ty], or [Some descriptions] listing the
    missing cases. Uses [type_ctor_env] to look up ADT constructors.
    For record types, recursively checks that each field's sub-patterns
    are themselves exhaustive for the field's type. *)
let rec missing_cases (scrut_ty : ty) (pats : pattern list) : string list option =
  let flat = List.concat_map flatten_or_pat pats in
  if List.exists (fun p ->
      match p.pat_desc with PWild | PVar _ -> true | _ -> false) flat
  then None
  else
    match deref scrut_ty with
    | TyCon "Bool" ->
      let has_true  = List.exists (fun p -> p.pat_desc = PLitTrue)  flat in
      let has_false = List.exists (fun p -> p.pat_desc = PLitFalse) flat in
      let missing =
        (if has_true  then [] else ["true"]) @
        (if has_false then [] else ["false"])
      in
      if missing = [] then None else Some missing
    | TyCon "Unit" ->
      if List.exists (fun p -> p.pat_desc = PLitUnit) flat
      then None else Some ["()"]
    | TyCon "Int" | TyCon "String" | TyCon "Float64" ->
      Some ["_ (wildcard required for infinite-domain types)"]
    | TyCon type_name | TyApp (type_name, _) ->
      (match List.assoc_opt type_name !type_ctor_env with
       | None | Some [] -> None
       | Some ctor_names ->
         let missing = List.filter (fun ctor_name ->
             not (List.exists (fun p ->
                 match p.pat_desc with
                 | PCtor (n, _) -> n = ctor_name
                 | _ -> false) flat)
           ) ctor_names in
         if missing = [] then None else Some missing)
    | TyRecord (record_fields, tail) ->
      let all_fields, _ = flatten_record record_fields tail in
      let record_pats = List.filter_map (fun p ->
          match p.pat_desc with
          | PRecord (fps, is_open) -> Some (fps, is_open)
          | _ -> None
        ) flat in
      if record_pats = [] then Some ["{...}"]
      else
        (* For each field, collect the sub-patterns from all record arms and
           check that they recursively cover the field's type.  An open
           pattern that omits a field implicitly covers it with a wildcard. *)
        let missing_fields = List.filter_map (fun (field_name, field_ty) ->
            let field_sub_pats = List.filter_map (fun (fps, is_open) ->
                match List.assoc_opt field_name fps with
                | Some sub_pat -> Some sub_pat
                | None ->
                  if is_open
                  then Some { pat_desc = PWild; pat_comment = None }
                  else None
              ) record_pats in
            match missing_cases field_ty field_sub_pats with
            | None -> None
            | Some _ -> Some field_name
          ) all_fields in
        if missing_fields = [] then None
        else Some (List.map (fun f -> "field '" ^ f ^ "'") missing_fields)
    | _ -> None

let check_match_exhaustive (scrut_ty : ty) (arms : match_arm list) =
  let pats = List.map (fun arm -> arm.pattern) arms in
  match missing_cases scrut_ty pats with
  | None -> ()
  | Some missing ->
    let ty_str = match deref scrut_ty with
      | TyCon n | TyApp (n, _) -> n
      | t -> Format.asprintf "%a" pp_ty t
    in
    failwith (Printf.sprintf
                "Typechecker: non-exhaustive match on type '%s'; \
                 missing cases: %s"
                ty_str (String.concat ", " missing))

(* ------------------------------------------------------------------ *)
(* Inference                                                            *)
(* ------------------------------------------------------------------ *)

(** Substitute TyVars listed in [subs]. Used to instantiate an effect
    operation's quantified type parameters with fresh metas. Rows inside
    [TyFun] slots are preserved as-is (rather than re-opened), since we
    are not instantiating a scheme here — merely substituting variables. *)
let rec subst_tyvars subs t =
  let rec go = function
    | TyVar v as t ->
      (match List.assoc_opt v subs with Some u -> u | None -> t)
    | TyCon _ as c -> c
    | TyFun (a, b, r) -> TyFun (go a, go b, go_row r)
    | TyForall (v, t) ->
      let subs' = List.filter (fun (x, _) -> x <> v) subs in
      TyForall (v, subst_tyvars subs' t)
    | TyMeta _ as m -> m
    | TyRecord (fields, tail) ->
      TyRecord (List.map (fun (n, t) -> (n, go t)) fields, tail)
    | TyApp (name, args) -> TyApp (name, List.map go args)
  and go_row = function
    | RPure -> RPure
    | RCons (e, rest) ->
      RCons ({ e with eff_args = List.map go e.eff_args }, go_row rest)
    | RMeta _ as r -> r
  in
  go t

(** [infer_expr_in eenv env ambient e] infers the type of [e] under the
    value environment [env] and effect environment [eenv]. The [ambient]
    row is the effect row of the enclosing computation — any effect
    performed by [e] (directly via [perform], or indirectly via calling a
    function whose row isn't pure) must unify with [ambient]. Raises
    [Failure] on type errors. *)
let rec infer_expr_in (eenv : effect_env) (env : env) (ambient : row) (e : expr) : ty =
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
    let ty_val = infer_expr_in eenv env ambient value in
    let scheme = generalize env ty_val in
    let env'   = match pat.pat_desc with
      | PVar name -> env_extend name scheme env
      | _         -> env_from_pattern env pat ty_val
    in
    infer_expr_in eenv env' ambient body

  | Letrec (bindings, body) ->
    (* Compute annotated types for each binding: param types + return type -> fun_ty.
       The innermost arrow carries a fresh open row that will be unified with the
       body's own ambient during checking. *)
    let binding_info = List.map (fun b ->
        let param_tys = List.map (fun p -> ty_of_type_expr p.param_type) b.letrec_params in
        let ret_ty    = ty_of_type_expr b.letrec_return_type in
        let body_row  = fresh_row () in
        let fun_ty    = match List.rev param_tys with
          | [] -> ret_ty
          | last :: rest_rev ->
            let innermost = TyFun (last, ret_ty, body_row) in
            List.fold_left
              (fun acc pt -> TyFun (pt, acc, fresh_row ()))
              innermost rest_rev
        in
        (b, param_tys, ret_ty, body_row, fun_ty)
      ) bindings in
    (* Extend env with all bindings at their monomorphic function types for recursion *)
    let env_rec = List.fold_left (fun acc (b, _, _, _, fun_ty) ->
        env_extend b.letrec_name (mono fun_ty) acc
      ) env binding_info in
    (* Infer each body in env extended with its own params, unify with return type *)
    List.iter (fun (b, param_tys, ret_ty, body_row, _) ->
        let env_params = List.fold_left2
            (fun acc p pt -> env_extend p.param_name (mono pt) acc)
            env_rec b.letrec_params param_tys
        in
        let body_ty = infer_expr_in eenv env_params body_row b.letrec_body in
        unify body_ty ret_ty
      ) binding_info;
    (* Generalize and build the env for the continuation *)
    let env' = List.fold_left (fun acc (b, _, _, _, fun_ty) ->
        env_extend b.letrec_name (generalize env fun_ty) acc
      ) env binding_info in
    infer_expr_in eenv env' ambient body

  | Fn { params; return_type; effects; fn_body } ->
    (* Replace type variable names with fresh metas so that let-generalization
       can quantify over them.  A single shared table ensures that the same name
       (e.g. 'a') maps to the same meta across all param annotations. *)
    let tv_table : (string * ty) list ref = ref [] in
    let freshen_type_expr ty_expr =
      let rec go = function
        | TyName s when String.length s > 0 && s.[0] >= 'a' && s.[0] <= 'z' ->
          (match List.assoc_opt s !tv_table with
           | Some m -> m
           | None   ->
             let m = fresh_meta () in
             tv_table := (s, m) :: !tv_table; m)
        | TyName s            -> TyCon s
        | TyApp (s, args)     -> TyApp (s, List.map go args)
        | TyTuple _ | TyFun _ -> fresh_meta ()
      in
      go ty_expr
    in
    let param_tys = List.map (fun p -> freshen_type_expr p.param_type) params in
    (* Body's ambient row: declared effect annotation (if any) or fresh open row. *)
    let body_row = row_of_effect_set_opt effects in
    (* Extend env with monomorphic param bindings *)
    let env' = List.fold_left2
        (fun acc p ty -> env_extend p.param_name (mono ty) acc)
        env params param_tys
    in
    let body_ty = infer_expr_in eenv env' body_row fn_body in
    (* Unify body type with return annotation if present *)
    (match return_type with
     | Some ann_ty -> unify body_ty (freshen_type_expr ann_ty)
     | None -> ());
    (* Build curried function type: p1 -> p2 -> ... -> body_ty.
       The declared/inferred body row lives on the innermost arrow;
       intermediate arrows get fresh open rows (partial applications are pure). *)
    (match List.rev param_tys with
     | [] -> body_ty
     | last :: rest_rev ->
       let innermost = TyFun (last, body_ty, body_row) in
       List.fold_left
         (fun acc pt -> TyFun (pt, acc, fresh_row ()))
         innermost rest_rev)

  | App (f, args) ->
    let f_ty   = infer_expr_in eenv env ambient f in
    let ret_ty = fresh_meta () in
    (* Build the expected function type from the arguments. A full application
       performs the callee's innermost effect row, which must unify with the
       ambient. Intermediate curried arrows get fresh open rows since partial
       application itself performs no effects. *)
    let arg_tys = List.map (infer_expr_in eenv env ambient) args in
    let expected = match List.rev arg_tys with
      | [] -> ret_ty
      | last :: rest_rev ->
        let innermost = TyFun (last, ret_ty, ambient) in
        List.fold_left
          (fun acc at -> TyFun (at, acc, fresh_row ()))
          innermost rest_rev
    in
    unify f_ty expected;
    deref ret_ty

  | Match { scrutinee; arms } ->
    let scrut_ty = infer_expr_in eenv env ambient scrutinee in
    let result_ty = fresh_meta () in
    List.iter (fun arm ->
        let env' = env_from_pattern env arm.pattern scrut_ty in
        let arm_ty = infer_expr_in eenv env' ambient arm.arm_body in
        unify result_ty arm_ty
      ) arms;
    check_match_exhaustive scrut_ty arms;
    deref result_ty

  | Record fields ->
    let field_tys = List.map (fun (name, e) ->
        (name, infer_expr_in eenv env ambient e)) fields in
    TyRecord (field_tys, None)

  | RecordUpdate (base, updates) ->
    let base_ty = infer_expr_in eenv env ambient base in
    (match deref base_ty with
     | TyRecord (base_fields, tail) ->
       let all_fields, _ = flatten_record base_fields tail in
       List.iter (fun (name, _) ->
           if not (List.mem_assoc name all_fields) then
             failwith (Printf.sprintf
                         "Typechecker: record update targets unknown field '%s'" name)
         ) updates;
       List.iter (fun (name, e) ->
           let update_ty = infer_expr_in eenv env ambient e in
           unify update_ty (List.assoc name all_fields)
         ) updates;
       base_ty
     | TyMeta _ ->
       (* Base type not yet determined; infer updates for side-effects *)
       List.iter (fun (_, e) -> ignore (infer_expr_in eenv env ambient e)) updates;
       base_ty
     | t ->
       failwith (Format.asprintf
                   "Typechecker: record update on non-record type %a" pp_ty t))

  | Project (e, field) ->
    (* Check for module-qualified access: if e is a bare Var whose name
       matches a known module, look up "module.field" directly in the env
       rather than trying to type-check the Var as a standalone value. *)
    let module_qualified_ty =
      match e.desc with
      | Var possible_module ->
        let qualified = possible_module ^ "." ^ field in
        (match List.assoc_opt qualified env with
         | Some scheme -> Some (instantiate scheme)
         | None        -> None)
      | _ -> None
    in
    (match module_qualified_ty with
     | Some ty -> ty
     | None ->
       let e_ty = infer_expr_in eenv env ambient e in
       (match deref e_ty with
        | TyRecord (fields, tail) ->
          let all_fields, final_tail = flatten_record fields tail in
          (match List.assoc_opt field all_fields with
           | Some field_ty -> deref field_ty
           | None ->
             (match final_tail with
              | None ->
                failwith (Printf.sprintf
                            "Typechecker: record has no field '%s'" field)
              | Some rm ->
                let field_ty = fresh_meta () in
                let new_tail = fresh_rec_meta () in
                rm.rrec_inst <- Some (TyRecord ([(field, field_ty)], Some new_tail));
                field_ty))
        | TyMeta _ ->
          let field_ty = fresh_meta () in
          let tail_meta = fresh_rec_meta () in
          unify e_ty (TyRecord ([(field, field_ty)], Some tail_meta));
          deref field_ty
        | t ->
          failwith (Format.asprintf
                      "Typechecker: field projection applied to non-record type %a" pp_ty t)))

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
    (* Each perform instantiates the effect's type parameters afresh. The
       instantiated arg types become the effect's row entry, so that two
       performs on the same effect within a single ambient row agree
       (e.g. two State.get calls share the same state type). *)
    let subs = List.map (fun v -> (v, fresh_meta ())) scheme.eff_type_params in
    let eff_args = List.map (fun v -> List.assoc v subs) scheme.eff_type_params in
    let param_tys = List.map (subst_tyvars subs) op.op_params in
    let ret_ty    = subst_tyvars subs op.op_return in
    let n_params = List.length param_tys in
    let n_args   = List.length args in
    if n_params <> n_args then
      failwith (Printf.sprintf
                  "Typechecker: effect operation '%s.%s' expects %d arg(s), got %d"
                  effect_name op_name n_params n_args);
    List.iter2 (fun a pt ->
        let at = infer_expr_in eenv env ambient a in
        unify at pt)
      args param_tys;
    (* This perform requires [effect_name] to be in the ambient row. *)
    let this_eff = { eff_name = effect_name; eff_args } in
    unify_row ambient (RCons (this_eff, RMeta (fresh_row_meta ())));
    deref ret_ty

  | Handle { handled; handlers } ->
    (* Type-check a linear (single-shot) handler.

       Effect-row model: the [handled] expression is checked under an
       ambient row that extends the outer ambient with the effects the
       handlers consume. Handler clauses run in the outer ambient (the
       handler's side effects are whatever the surrounding computation
       allows). A return clause (if present) likewise runs in the outer
       ambient and transforms [handled_ty] into [result_ty].

       - Each op handler receives the operation's arguments at their declared
         types (after fresh instantiation of the effect's type parameters)
         and must produce a value of [result_ty].
       - [resume] is bound in the op handler body with type
         [op_return_ty -> result_ty]. Calling it continues the handled
         computation; linear handlers resume at most once. *)
    (* Build the extended ambient for the handled expression: prepend each
       handled effect (with its instantiated type arguments) onto the outer
       ambient. Keep the per-effect substitution so op handlers can reuse
       the same type args. *)
    let handler_subs = List.map (fun (h : effect_handler) ->
        let scheme =
          match List.assoc_opt h.effect_handler eenv with
          | Some s -> s
          | None ->
            failwith (Printf.sprintf
                        "Typechecker: unknown effect '%s' in handler"
                        h.effect_handler)
        in
        let subs = List.map (fun v -> (v, fresh_meta ())) scheme.eff_type_params in
        let eff_args = List.map (fun v -> List.assoc v subs) scheme.eff_type_params in
        (h, scheme, subs, { eff_name = h.effect_handler; eff_args })
      ) handlers in
    let handled_row =
      List.fold_right (fun (_, _, _, einst) acc -> RCons (einst, acc))
        handler_subs ambient
    in
    let handled_ty = infer_expr_in eenv env handled_row handled in
    let result_ty  = fresh_meta () in
    List.iter (fun ((h : effect_handler), scheme, subs, _einst) ->
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
               op's return value to the overall handle result. Resume runs
               in the outer ambient. *)
            let resume_ty  = TyFun (op_ret_ty, result_ty, ambient) in
            let env_resume = env_extend "resume" (mono resume_ty) env_params in
            let body_ty    = infer_expr_in eenv env_resume ambient oh.op_handler_body in
            unify body_ty result_ty)
          h.op_handlers;
        (* Return clause (if present) transforms the handled value into the
           final handle result. Without one, the two types coincide. *)
        (match h.return_handler with
         | None ->
           unify handled_ty result_ty
         | Some { return_var; return_body } ->
           let env_ret = env_extend return_var (mono handled_ty) env in
           let body_ty = infer_expr_in eenv env_ret ambient return_body in
           unify body_ty result_ty))
      handler_subs;
    deref result_ty

  | Do stmts ->
    (* Each StmtExpr is inferred and discarded; StmtLet extends the env.
       The final stmt must be a StmtExpr whose type is the block's type. *)
    let rec go env = function
      | []                         -> TyCon "Unit"
      | [StmtExpr e]               -> infer_expr_in eenv env ambient e
      | StmtExpr e :: rest         -> ignore (infer_expr_in eenv env ambient e); go env rest
      | StmtLet { pat; value } :: rest ->
        let ty   = infer_expr_in eenv env ambient value in
        let env' = match pat.pat_desc with
          | PVar name -> env_extend name (generalize env ty) env
          | _         -> env_from_pattern env pat ty
        in
        go env' rest
    in
    go env stmts

  | If { cond; then_; else_ } ->
    unify (infer_expr_in eenv env ambient cond) (TyCon "Bool");
    let t = infer_expr_in eenv env ambient then_ in
    let e = infer_expr_in eenv env ambient else_ in
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
  | PCtor (name, sub_pats) ->
    (match List.assoc_opt name env with
     | None ->
       List.fold_left (fun e p -> env_from_pattern e p (fresh_meta ())) env sub_pats
     | Some scheme ->
       let ctor_ty = instantiate scheme in
       let rec extract_args t = match deref t with
         | TyFun (arg, ret, _) ->
           let args, result = extract_args ret in
           (arg :: args, result)
         | t -> ([], t)
       in
       let arg_tys, ret_ty = extract_args ctor_ty in
       unify scrut_ty ret_ty;
       let n_args = List.length arg_tys in
       let n_pats = List.length sub_pats in
       if n_args <> n_pats then
         failwith (Printf.sprintf
                     "Typechecker: constructor '%s' expects %d argument(s), \
                      pattern has %d"
                     name n_args n_pats);
       List.fold_left2 (fun e p t -> env_from_pattern e p t)
         env sub_pats arg_tys)
  | PRecord (fields, is_open) ->
    let field_tys =
      match deref scrut_ty with
      | TyRecord (record_fields, tail) ->
        let all_fields, _ = flatten_record record_fields tail in
        List.map (fun (name, _) ->
            match List.assoc_opt name all_fields with
            | Some ty -> (name, ty)
            | None ->
              failwith (Printf.sprintf
                          "Typechecker: record pattern mentions unknown field '%s'" name)
          ) fields
      | TyMeta _ ->
        let fresh_fields = List.map (fun (name, _) -> (name, fresh_meta ())) fields in
        let tail = if is_open then Some (fresh_rec_meta ()) else None in
        unify scrut_ty (TyRecord (fresh_fields, tail));
        fresh_fields
      | t ->
        failwith (Format.asprintf
                    "Typechecker: record pattern applied to non-record type %a" pp_ty t)
    in
    List.fold_left2 (fun e (_, sub_pat) (_, field_ty) ->
        env_from_pattern e sub_pat field_ty)
      env fields field_tys
  | POr (p1, _p2) ->
    (* Both branches bind the same variables; use p1 *)
    env_from_pattern env p1 scrut_ty

(** [infer_expr env e] infers the type of [e] with no effects declared,
    using a fresh open row for the ambient effect row so that [e] is free
    to perform any effects its subterms introduce. Program-level inference —
    with declared effects in scope — uses [check_program] or
    [infer_expr_in] directly. *)
let infer_expr (env : env) (e : expr) : ty =
  infer_expr_in empty_effect_env env (fresh_row ()) e

(* ------------------------------------------------------------------ *)
(* Standard library — built-in environments                             *)
(* ------------------------------------------------------------------ *)

(** Helper: curried function scheme with all-[RPure] arrows.
    [RPure] rows are re-opened to fresh metas on every [instantiate] call
    via [reopen_row], giving each use site its own effect tail. *)
let stdlib_fun_scheme bound params ret =
  let body = match List.rev params with
    | [] -> ret
    | last :: rest_rev ->
      let innermost = TyFun (last, ret, RPure) in
      List.fold_left (fun acc pt -> TyFun (pt, acc, RPure)) innermost rest_rev
  in
  { bound; body }

(** Type-constructor sets for the three built-in ADTs.
    Merged with program-specific entries in [check_program] so that
    exhaustiveness checking works without a user-level type declaration. *)
let stdlib_type_ctor_env : (string * string list) list =
  [ ("List",   ["Nil"; "Cons"])
  ; ("Option", ["None"; "Some"])
  ; ("Result", ["Ok"; "Err"])
  ]

(** Pre-declared value environment.  Every entry here is available in any
    program without an explicit declaration or import. *)
let stdlib_env : env =
  let int  = TyCon "Int" in
  let bool = TyCon "Bool" in
  let str  = TyCon "String" in
  let list_a    = TyApp ("List",   [TyVar "a"]) in
  let option_a  = TyApp ("Option", [TyVar "a"]) in
  let result_ae = TyApp ("Result", [TyVar "a"; TyVar "e"]) in
  let arith2    = stdlib_fun_scheme []       [int; int] int in
  let arith1    = stdlib_fun_scheme []       [int]      int in
  let div_body  =
    TyFun (int, TyFun (int, int,
      RCons ({ eff_name = "DivByZero"; eff_args = [] }, RPure)), RPure) in
  let cmp       = stdlib_fun_scheme ["a"]    [TyVar "a"; TyVar "a"] bool in
  let nil_s     = { bound = ["a"]; body = list_a } in
  let cons_s    = stdlib_fun_scheme ["a"]    [TyVar "a"; list_a] list_a in
  let none_s    = { bound = ["a"]; body = option_a } in
  let some_s    = stdlib_fun_scheme ["a"]    [TyVar "a"] option_a in
  let ok_s      = stdlib_fun_scheme ["a";"e"] [TyVar "a"] result_ae in
  let err_s     = stdlib_fun_scheme ["a";"e"] [TyVar "e"] result_ae in
  let max_s     = stdlib_fun_scheme ["a"]    [TyVar "a"; TyVar "a"] (TyVar "a") in
  [ (* Arithmetic *)
    ("add", arith2); ("sub", arith2); ("mul", arith2)
  ; ("div", { bound = []; body = div_body }); ("neg", arith1)
  (* Comparison *)
  ; ("eq", cmp); ("neq", cmp)
  ; ("lt", cmp); ("gt", cmp); ("le", cmp); ("ge", cmp); ("lte", cmp); ("gte", cmp)
  (* String *)
  ; ("concat",        stdlib_fun_scheme [] [str; str] str)
  ; ("string_length", stdlib_fun_scheme [] [str] int)
  ; ("string_eq",     stdlib_fun_scheme [] [str; str] bool)
  ; ("string_head",   stdlib_fun_scheme [] [str] int)
  (* List — uppercase constructors and lowercase helper aliases *)
  ; ("Nil",  nil_s);  ("nil",  nil_s)
  ; ("Cons", cons_s); ("cons", cons_s)
  (* Option — uppercase constructors and lowercase helper aliases *)
  ; ("None", none_s); ("none", none_s)
  ; ("Some", some_s); ("some", some_s)
  (* Result — uppercase constructors and lowercase helper aliases *)
  ; ("Ok",  ok_s);  ("ok",  ok_s)
  ; ("Err", err_s); ("err", err_s)
  (* Ordered max *)
  ; ("max", max_s)
  ]

(** Pre-declared effect environment.  [DivByZero] is carried by [div]. *)
let stdlib_effect_env : effect_env =
  [ ("DivByZero",
     { eff_type_params = []
     ; eff_ops =
         [ { op_name   = "throw"
           ; op_params = []
           ; op_return = TyCon "Nothing"
           } ]
     })
  ]

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
    scheme's bound variables so callers instantiate them with fresh metas.
    A shared [row_var_env] ensures that the same row-variable name at
    different positions in the signature maps to the same [RMeta]. *)
let fn_scheme_of_decl
    (type_params : string list)
    (params      : param list)
    (return_type : type_expr option)
    (effects     : effect_set option)
  : scheme =
  let renv = new_row_var_env () in
  let param_tys = List.map (fun p -> ty_of_type_expr_env renv p.param_type) params in
  let ret_ty = match return_type with
    | Some t -> ty_of_type_expr_env renv t
    | None   -> fresh_meta ()
  in
  let body_row = row_of_effect_set_opt_env renv effects in
  let fun_ty = match List.rev param_tys with
    | [] -> ret_ty
    | last :: rest_rev ->
      let innermost = TyFun (last, ret_ty, body_row) in
      List.fold_left
        (fun acc pt -> TyFun (pt, acc, fresh_row ()))
        innermost rest_rev
  in
  { bound = type_params; body = fun_ty }

(** Build a scheme for a single constructor of a type declaration.
    The return type is [TyCon type_name]; each constructor parameter is
    converted via [ty_of_type_expr], so type-parameter names (lowercase)
    become [TyVar]s that are generalized under [type_params]. *)
let ctor_scheme_of_type
    (type_name   : string)
    (type_params : string list)
    (ctor        : Ast.ctor_decl)
  : scheme =
  let ret_ty =
    if type_params = [] then TyCon type_name
    else TyApp (type_name, List.map (fun v -> TyVar v) type_params)
  in
  let param_tys = List.map ty_of_type_expr ctor.ctor_params in
  let body = match List.rev param_tys with
    | [] -> ret_ty
    | last :: rest_rev ->
      let innermost = TyFun (last, ret_ty, RPure) in
      List.fold_left
        (fun acc pt -> TyFun (pt, acc, RPure))
        innermost rest_rev
  in
  { bound = type_params; body }

(** A module registry maps module names to the bare (env, effect_env) of
    their direct declarations, keyed by unqualified name. Used by
    [check_program] to resolve [DeclImport] and to register qualified names
    for [DeclModule]. *)
type module_registry = (string * (env * effect_env)) list

(** Build the module registry by scanning [prog] for [DeclModule] nodes.
    Only top-level modules are registered; nested modules are not. *)
let build_module_registry (prog : program) : module_registry =
  List.filter_map (fun d ->
    match d.decl_desc with
    | DeclModule { module_name; body; _ } ->
      let collect_body (env, eenv) b =
        match b.decl_desc with
        | DeclEffect { effect_name; type_params; ops; _ } ->
          let scheme = effect_scheme_of_decl type_params ops in
          (env, (effect_name, scheme) :: eenv)
        | DeclFn { fn_name; type_params; params; return_type; effects; _ } ->
          let scheme = fn_scheme_of_decl type_params params return_type effects in
          ((fn_name, scheme) :: env, eenv)
        | DeclType { type_name; type_params; ctors; _ } ->
          let env' = List.fold_left (fun acc ctor ->
              env_extend ctor.Ast.ctor_name
                (ctor_scheme_of_type type_name type_params ctor) acc
            ) env ctors in
          (env', eenv)
        | _ -> (env, eenv)
      in
      let mod_env, mod_eenv = List.fold_left collect_body ([], []) body in
      Some (module_name, (mod_env, mod_eenv))
    | _ -> None
  ) prog

(** Build a map from each declared type name to the names of its constructors.
    Used to populate [type_ctor_env] before exhaustiveness checking begins. *)
let build_type_ctor_env (prog : program) : (string * string list) list =
  let rec collect_decl acc d =
    match d.decl_desc with
    | DeclType { type_name; ctors; _ } ->
      (type_name, List.map (fun c -> c.Ast.ctor_name) ctors) :: acc
    | DeclModule { body; _ } ->
      List.fold_left collect_decl acc body
    | _ -> acc
  in
  List.fold_left collect_decl [] prog

(** [check_program prog] type-checks a whole program in two passes:

    Pass 1 walks every declaration and builds:
    - the value environment, seeded with each [DeclFn]'s declared signature
      (as a scheme over its type parameters), so that bodies can reference
      one another regardless of declaration order;
    - the effect environment, one entry per [DeclEffect], carrying the
      operation signatures to be instantiated at each [perform] site.

    Pass 2 walks [DeclFn]s (and [DeclFn]s nested inside [DeclModule]s)
    and type-checks each body under the seeded environment extended with
    the function's own parameter bindings; the body's inferred type is
    then unified with the declared return type.

    [DeclType]: each constructor is added to the value environment as a
    scheme generalized over the type's parameters, so constructors may be
    called like functions and matched in patterns.

    [DeclModule]: declarations in module bodies are collected into the
    flat value/effect environments (no namespace support yet) and module
    function bodies are type-checked in pass 2.

    Returns the populated [(env, effect_env)] for reuse in tests and
    downstream tooling. Raises [Failure] on type errors. *)
let check_program (prog : program) : env * effect_env =
  type_ctor_env := stdlib_type_ctor_env @ build_type_ctor_env prog;
  let module_registry = build_module_registry prog in
  (* [add_qualified prefix mod_env mod_eenv env eenv] folds [mod_env] and
     [mod_eenv] into [env] and [eenv], prepending [prefix ^ "."] to each key. *)
  let add_qualified prefix mod_env mod_eenv env eenv =
    let qualify name = prefix ^ "." ^ name in
    let env' = List.fold_left
        (fun acc (name, scheme) -> env_extend (qualify name) scheme acc)
        env mod_env in
    let eenv' = List.fold_left
        (fun acc (name, scheme) -> (qualify name, scheme) :: acc)
        eenv mod_eenv in
    (env', eenv')
  in
  let rec collect (env, eenv) d =
    match d.decl_desc with
    | DeclEffect { effect_name; type_params; ops; _ } ->
      let scheme = effect_scheme_of_decl type_params ops in
      (env, (effect_name, scheme) :: eenv)
    | DeclFn { fn_name; type_params; params; return_type; effects; _ } ->
      let scheme = fn_scheme_of_decl type_params params return_type effects in
      ((fn_name, scheme) :: env, eenv)
    | DeclType { type_name; type_params; ctors; _ } ->
      let env' = List.fold_left (fun acc ctor ->
          env_extend ctor.Ast.ctor_name
            (ctor_scheme_of_type type_name type_params ctor)
            acc
        ) env ctors in
      (env', eenv)
    | DeclModule { module_name; body; _ } ->
      (* Fold body into the flat env with bare names (existing behaviour). *)
      let env', eenv' = List.fold_left collect (env, eenv) body in
      (* Also register each declaration under module_name.name so callers
         can access them via qualified syntax (module_name.fn). *)
      (match List.assoc_opt module_name module_registry with
       | None -> (env', eenv')
       | Some (mod_env, mod_eenv) ->
         add_qualified module_name mod_env mod_eenv env' eenv')
    | DeclImport { module_path; alias } ->
      let prefix = match alias with Some a -> a | None -> module_path in
      (match List.assoc_opt module_path module_registry with
       | None ->
         failwith (Printf.sprintf
                     "Typechecker: unknown module '%s' in import" module_path)
       | Some (mod_env, mod_eenv) ->
         add_qualified prefix mod_env mod_eenv env eenv)
    | DeclRequire _ ->
      (env, eenv)
  in
  let env0, eenv0 =
    List.fold_left collect (stdlib_env, stdlib_effect_env) prog
  in
  let rec check_decl d =
    match d.decl_desc with
    | DeclFn { params; return_type; effects; decl_body; _ } ->
      (* Use a shared row-var env so that the same row-variable name in the
         param types, effect annotation, and return type produces the same
         RMeta, giving the body check consistent ambient rows. *)
      let renv = new_row_var_env () in
      let param_tys = List.map (fun p -> ty_of_type_expr_env renv p.param_type) params in
      let body_env =
        List.fold_left2 (fun acc p t -> env_extend p.param_name (mono t) acc)
          env0 params param_tys
      in
      (* The body runs in the ambient row declared on the signature. If
         the signature omits the effect annotation, use a fresh open row
         so inference can discover the effects performed. *)
      let ambient = row_of_effect_set_opt_env renv effects in
      let body_ty = infer_expr_in eenv0 body_env ambient decl_body in
      (match return_type with
       | Some t -> unify body_ty (ty_of_type_expr_env renv t)
       | None   -> ())
    | DeclModule { body; _ } ->
      List.iter check_decl body
    | DeclImport _ | DeclRequire _ | DeclEffect _ | DeclType _ -> ()
  in
  List.iter check_decl prog;
  (env0, eenv0)

(* ------------------------------------------------------------------ *)
(* Exhaustiveness warning collection (non-raising)                     *)
(* ------------------------------------------------------------------ *)

type match_warning = {
  mw_line    : int;
  mw_col     : int;
  mw_missing : string list;
  mw_fn_name : string option;  (* containing function, for hash lookup *)
}

(** Walk [prog] and return one [match_warning] per non-exhaustive match
    expression.  Uses [type_ctor_env] (rebuilt from [prog]) for ADT
    constructor lookup.  Because the AST carries no source positions,
    [mw_line] and [mw_col] are always 0. *)
let collect_match_warnings (prog : program) : match_warning list =
  type_ctor_env := stdlib_type_ctor_env @ build_type_ctor_env prog;
  let warnings = ref [] in
  let add_warning fn_name missing =
    warnings := { mw_line = 0; mw_col = 0; mw_missing = missing;
                  mw_fn_name = fn_name } :: !warnings
  in
  let check_match_pats fn_name arms =
    let flat =
      List.concat_map flatten_or_pat (List.map (fun a -> a.pattern) arms)
    in
    if List.exists
        (fun p -> match p.pat_desc with PWild | PVar _ -> true | _ -> false)
        flat
    then ()
    else begin
      let ctor_names = List.filter_map (fun p ->
          match p.pat_desc with PCtor (n, _) -> Some n | _ -> None) flat in
      let has_true  = List.exists (fun p -> p.pat_desc = PLitTrue)  flat in
      let has_false = List.exists (fun p -> p.pat_desc = PLitFalse) flat in
      if ctor_names <> [] then begin
        match List.find_opt (fun (_, ctors) ->
            List.exists (fun c -> List.mem c ctor_names) ctors)
            !type_ctor_env
        with
        | None -> ()
        | Some (_, all_ctors) ->
          let missing =
            List.filter (fun c -> not (List.mem c ctor_names)) all_ctors
          in
          if missing <> [] then add_warning fn_name missing
      end else if has_true || has_false then begin
        let missing =
          (if has_true  then [] else ["true"]) @
          (if has_false then [] else ["false"])
        in
        if missing <> [] then add_warning fn_name missing
      end
    end
  in
  let rec walk_expr fn_name e =
    match e.desc with
    | Match { scrutinee; arms } ->
      walk_expr fn_name scrutinee;
      List.iter (fun arm -> walk_expr fn_name arm.arm_body) arms;
      check_match_pats fn_name arms
    | Let { value; body; _ } ->
      walk_expr fn_name value; walk_expr fn_name body
    | App (f, args) ->
      walk_expr fn_name f; List.iter (walk_expr fn_name) args
    | Fn { fn_body; _ } ->
      walk_expr fn_name fn_body
    | If { cond; then_; else_ } ->
      walk_expr fn_name cond; walk_expr fn_name then_; walk_expr fn_name else_
    | Do stmts ->
      List.iter (function
        | StmtLet { value; _ } -> walk_expr fn_name value
        | StmtExpr e -> walk_expr fn_name e) stmts
    | Letrec (bindings, body) ->
      List.iter (fun b -> walk_expr fn_name b.letrec_body) bindings;
      walk_expr fn_name body
    | Record fields ->
      List.iter (fun (_, e) -> walk_expr fn_name e) fields
    | RecordUpdate (base, fields) ->
      walk_expr fn_name base; List.iter (fun (_, e) -> walk_expr fn_name e) fields
    | Project (e, _) ->
      walk_expr fn_name e
    | Perform { args; _ } ->
      List.iter (walk_expr fn_name) args
    | Handle { handled; handlers } ->
      walk_expr fn_name handled;
      List.iter (fun h ->
          List.iter (fun op -> walk_expr fn_name op.op_handler_body) h.op_handlers;
          Option.iter (fun r -> walk_expr fn_name r.return_body) h.return_handler)
        handlers
    | Var _ | IntLit _ | FloatLit _ | StringLit _
    | BoolLit _ | UnitLit -> ()
  in
  let rec walk_decl d =
    match d.decl_desc with
    | DeclFn { fn_name; decl_body; _ } -> walk_expr (Some fn_name) decl_body
    | DeclModule { body; _ } -> List.iter walk_decl body
    | DeclType _ | DeclEffect _ | DeclRequire _ | DeclImport _ -> ()
  in
  List.iter walk_decl prog;
  List.rev !warnings

(* ------------------------------------------------------------------ *)
(* Unhandled effect collection                                          *)
(* ------------------------------------------------------------------ *)

type effect_site = {
  es_effect   : string;
  es_function : string;
  es_line     : int;
  es_col      : int;
}

(** [collect_unhandled_effects prog entry_point] walks the call graph
    reachable from [entry_point] and reports [Perform] nodes whose effect
    is not covered by an enclosing [Handle] on any path from [entry_point].
    Raises [Failure] if [entry_point] is not declared in [prog]. *)
let collect_unhandled_effects (prog : program) (entry_point : string)
    : effect_site list =
  let fn_map : (string, expr) Hashtbl.t = Hashtbl.create 16 in
  let rec index_decls ds =
    List.iter (fun d ->
        match d.decl_desc with
        | DeclFn { fn_name; decl_body; _ } ->
          Hashtbl.replace fn_map fn_name decl_body
        | DeclModule { body; _ } -> index_decls body
        | _ -> ()
      ) ds
  in
  index_decls prog;
  if not (Hashtbl.mem fn_map entry_point) then
    failwith (Printf.sprintf "Entry point '%s' not found in module" entry_point);
  let seen     : (string * string, unit) Hashtbl.t = Hashtbl.create 8 in
  let unhandled = ref [] in
  let in_flight = Hashtbl.create 8 in
  let rec walk_fn fn_name handled =
    if not (Hashtbl.mem in_flight fn_name) then
      match Hashtbl.find_opt fn_map fn_name with
      | None -> ()
      | Some body ->
        Hashtbl.replace in_flight fn_name ();
        walk_expr fn_name handled body;
        Hashtbl.remove in_flight fn_name
  and walk_expr fn_name handled e =
    match e.desc with
    | Perform { effect_name; args; _ } ->
      List.iter (walk_expr fn_name handled) args;
      if not (List.mem effect_name handled) then begin
        let key = (effect_name, fn_name) in
        if not (Hashtbl.mem seen key) then begin
          Hashtbl.replace seen key ();
          unhandled :=
            { es_effect = effect_name; es_function = fn_name;
              es_line = 0; es_col = 0 } :: !unhandled
        end
      end
    | Handle { handled = inner; handlers } ->
      let new_effs     = List.map (fun h -> h.effect_handler) handlers in
      let inner_handled = new_effs @ handled in
      walk_expr fn_name inner_handled inner;
      List.iter (fun h ->
          List.iter (fun op ->
              walk_expr fn_name handled op.op_handler_body) h.op_handlers;
          Option.iter (fun r ->
              walk_expr fn_name handled r.return_body) h.return_handler
        ) handlers
    | App ({ desc = Var callee; _ }, args) ->
      List.iter (walk_expr fn_name handled) args;
      walk_fn callee handled
    | App (f, args) ->
      walk_expr fn_name handled f;
      List.iter (walk_expr fn_name handled) args
    | Fn { fn_body; _ } ->
      walk_expr fn_name handled fn_body
    | Let { value; body; _ } ->
      walk_expr fn_name handled value;
      walk_expr fn_name handled body
    | If { cond; then_; else_ } ->
      walk_expr fn_name handled cond;
      walk_expr fn_name handled then_;
      walk_expr fn_name handled else_
    | Do stmts ->
      List.iter (function
          | StmtLet { value; _ } -> walk_expr fn_name handled value
          | StmtExpr e           -> walk_expr fn_name handled e
        ) stmts
    | Letrec (bindings, body) ->
      List.iter (fun b -> walk_expr fn_name handled b.letrec_body) bindings;
      walk_expr fn_name handled body
    | Record fields ->
      List.iter (fun (_, e) -> walk_expr fn_name handled e) fields
    | RecordUpdate (base, fields) ->
      walk_expr fn_name handled base;
      List.iter (fun (_, e) -> walk_expr fn_name handled e) fields
    | Project (e, _) ->
      walk_expr fn_name handled e
    | Match { scrutinee; arms } ->
      walk_expr fn_name handled scrutinee;
      List.iter (fun arm -> walk_expr fn_name handled arm.arm_body) arms
    | Var _ | IntLit _ | FloatLit _ | StringLit _
    | BoolLit _ | UnitLit -> ()
  in
  walk_fn entry_point [];
  List.rev !unhandled

(* ------------------------------------------------------------------ *)
(* Unused declaration collection                                        *)
(* ------------------------------------------------------------------ *)

type unused_item = {
  ui_name : string;
  ui_kind : string;  (** "function", "type", or "import" *)
  ui_line : int;
  ui_col  : int;
}

(** [collect_unused prog] returns one [unused_item] per top-level declaration
    that is defined but never referenced.  [pub] declarations and [main] are
    treated as roots and are never flagged.  Mutual recursion is handled via a
    BFS: a declaration is considered used iff it is reachable from any root
    through the reference graph.  Because the AST carries no source positions,
    [ui_line] and [ui_col] are always 0. *)
let collect_unused (prog : program) : unused_item list =
  (* Collect string references from an expression into acc. *)
  let rec expr_refs acc e =
    match e.desc with
    | Var s -> acc := s :: !acc
    | App (f, args) ->
      expr_refs acc f; List.iter (expr_refs acc) args
    | Fn { fn_body; _ } -> expr_refs acc fn_body
    | Let { value; body; _ } ->
      expr_refs acc value; expr_refs acc body
    | If { cond; then_; else_ } ->
      expr_refs acc cond; expr_refs acc then_; expr_refs acc else_
    | Do stmts ->
      List.iter (function
        | StmtLet { value; _ } -> expr_refs acc value
        | StmtExpr e -> expr_refs acc e) stmts
    | Match { scrutinee; arms } ->
      expr_refs acc scrutinee;
      List.iter (fun arm -> expr_refs acc arm.arm_body) arms
    | Letrec (bindings, body) ->
      List.iter (fun b -> expr_refs acc b.letrec_body) bindings;
      expr_refs acc body
    | Record fields -> List.iter (fun (_, e) -> expr_refs acc e) fields
    | RecordUpdate (base, fields) ->
      expr_refs acc base;
      List.iter (fun (_, e) -> expr_refs acc e) fields
    | Project (e, _) -> expr_refs acc e
    | Perform { effect_name; args; _ } ->
      acc := effect_name :: !acc;
      List.iter (expr_refs acc) args
    | Handle { handled; handlers } ->
      expr_refs acc handled;
      List.iter (fun h ->
        acc := h.effect_handler :: !acc;
        List.iter (fun op -> expr_refs acc op.op_handler_body) h.op_handlers;
        Option.iter (fun r -> expr_refs acc r.return_body) h.return_handler
      ) handlers
    | IntLit _ | FloatLit _ | StringLit _ | BoolLit _ | UnitLit -> ()
  in
  (* Collect string references from a type expression into acc. *)
  let rec type_refs acc t =
    match t with
    | TyName s -> acc := s :: !acc
    | TyApp (s, args) ->
      acc := s :: !acc; List.iter (type_refs acc) args
    | TyTuple ts -> List.iter (type_refs acc) ts
    | TyFun (params, ret, _) ->
      List.iter (type_refs acc) params; type_refs acc ret
  in
  (* Phase 1: index definitions and build the reference graph. *)
  (* defs: (name, kind, is_pub) in source order *)
  let defs : (string * string * bool) list ref = ref [] in
  let refs_map : (string, string list) Hashtbl.t = Hashtbl.create 16 in
  let import_canonical module_path alias =
    match alias with
    | Some a -> a
    | None ->
      let parts = String.split_on_char '/' module_path in
      (match List.rev parts with s :: _ -> s | [] -> module_path)
  in
  let rec index ds =
    List.iter (fun d ->
      match d.decl_desc with
      | DeclFn { fn_name; pub; params; return_type; decl_body; _ } ->
        defs := (fn_name, "function", pub) :: !defs;
        let acc = ref [] in
        List.iter (fun p -> type_refs acc p.param_type) params;
        Option.iter (type_refs acc) return_type;
        expr_refs acc decl_body;
        Hashtbl.replace refs_map fn_name !acc
      | DeclType { type_name; pub; ctors; _ } ->
        defs := (type_name, "type", pub) :: !defs;
        let acc = ref [] in
        List.iter (fun c -> List.iter (type_refs acc) c.ctor_params) ctors;
        Hashtbl.replace refs_map type_name !acc
      | DeclEffect { effect_name; pub; ops; _ } ->
        defs := (effect_name, "type", pub) :: !defs;
        let acc = ref [] in
        List.iter (fun op ->
          List.iter (type_refs acc) op.effect_op_params;
          type_refs acc op.effect_op_return) ops;
        Hashtbl.replace refs_map effect_name !acc
      | DeclImport { module_path; alias } ->
        let name = import_canonical module_path alias in
        defs := (name, "import", false) :: !defs;
        Hashtbl.replace refs_map name []
      | DeclModule { body; _ } -> index body
      | DeclRequire _ -> ()
    ) ds
  in
  index prog;
  (* Phase 2: BFS from roots (pub declarations and "main"). *)
  let defined : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  List.iter (fun (n, _, _) -> Hashtbl.replace defined n ()) !defs;
  let used : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  let queue = Queue.create () in
  let mark name =
    if not (Hashtbl.mem used name) then begin
      Hashtbl.replace used name ();
      Queue.push name queue
    end
  in
  List.iter (fun (name, _, pub) ->
    if pub || name = "main" then mark name
  ) !defs;
  while not (Queue.is_empty queue) do
    let name = Queue.pop queue in
    List.iter (fun ref_name ->
      if Hashtbl.mem defined ref_name then mark ref_name
    ) (match Hashtbl.find_opt refs_map name with Some r -> r | None -> [])
  done;
  (* Phase 3: report declarations not reached by BFS. *)
  List.filter_map (fun (name, kind, pub) ->
    if pub || name = "main" || Hashtbl.mem used name then None
    else Some { ui_name = name; ui_kind = kind; ui_line = 0; ui_col = 0 }
  ) (List.rev !defs)

(* ------------------------------------------------------------------ *)
(* Caller collection                                                    *)
(* ------------------------------------------------------------------ *)

type caller_site = {
  cs_caller    : string;
  cs_line      : int;
  cs_col       : int;
  cs_call_expr : Ast.expr;  (* the App expression at the call site *)
}

(** [collect_callers prog function_name] returns one [caller_site] for each
    direct call to [function_name] found in [prog].  Returns an error string
    if [function_name] is not defined in [prog].  Because the AST carries no
    source positions, [cs_line] and [cs_col] are always 0. *)
let collect_callers (prog : program) (function_name : string)
    : (caller_site list, string) result =
  let defined = List.exists (fun decl ->
    match decl.decl_desc with
    | DeclFn { fn_name; _ } -> fn_name = function_name
    | _ -> false) prog
  in
  if not defined then
    Error (Printf.sprintf "Function '%s' is not defined in the module" function_name)
  else begin
    let sites = ref [] in
    let rec walk_expr current_fn e =
      match e.desc with
      | App ({ desc = Var callee; _ }, args) ->
        if callee = function_name then
          sites := { cs_caller = current_fn; cs_line = 0; cs_col = 0;
                     cs_call_expr = e } :: !sites;
        List.iter (walk_expr current_fn) args
      | App (f, args) ->
        walk_expr current_fn f;
        List.iter (walk_expr current_fn) args
      | Fn { fn_body; _ } ->
        walk_expr current_fn fn_body
      | Let { value; body; _ } ->
        walk_expr current_fn value;
        walk_expr current_fn body
      | If { cond; then_; else_ } ->
        walk_expr current_fn cond;
        walk_expr current_fn then_;
        walk_expr current_fn else_
      | Do stmts ->
        List.iter (function
          | StmtLet { value; _ } -> walk_expr current_fn value
          | StmtExpr e           -> walk_expr current_fn e
        ) stmts
      | Letrec (bindings, body) ->
        List.iter (fun b -> walk_expr current_fn b.letrec_body) bindings;
        walk_expr current_fn body
      | Record fields ->
        List.iter (fun (_, e) -> walk_expr current_fn e) fields
      | RecordUpdate (base, fields) ->
        walk_expr current_fn base;
        List.iter (fun (_, e) -> walk_expr current_fn e) fields
      | Project (e, _) ->
        walk_expr current_fn e
      | Match { scrutinee; arms } ->
        walk_expr current_fn scrutinee;
        List.iter (fun arm -> walk_expr current_fn arm.arm_body) arms
      | Handle { handled; handlers } ->
        walk_expr current_fn handled;
        List.iter (fun h ->
          List.iter (fun op -> walk_expr current_fn op.op_handler_body) h.op_handlers;
          Option.iter (fun r -> walk_expr current_fn r.return_body) h.return_handler
        ) handlers
      | Perform { args; _ } ->
        List.iter (walk_expr current_fn) args
      | Var _ | IntLit _ | FloatLit _ | StringLit _ | BoolLit _ | UnitLit -> ()
    in
    List.iter (fun decl ->
      match decl.decl_desc with
      | DeclFn { fn_name; decl_body; _ } -> walk_expr fn_name decl_body
      | _ -> ()
    ) prog;
    Ok (List.rev !sites)
  end

(** [collect_callers_in prog function_name] returns one [caller_site] for each
    direct call to [function_name] found in [prog], without requiring the
    function to be defined in [prog].  Used for cross-module caller search. *)
let collect_callers_in (prog : program) (function_name : string) : caller_site list =
  let sites = ref [] in
  let rec walk_expr current_fn e =
    match e.desc with
    | App ({ desc = Var callee; _ }, args) ->
      if callee = function_name then
        sites := { cs_caller = current_fn; cs_line = 0; cs_col = 0;
                   cs_call_expr = e } :: !sites;
      List.iter (walk_expr current_fn) args
    | App (f, args) ->
      walk_expr current_fn f;
      List.iter (walk_expr current_fn) args
    | Fn { fn_body; _ } ->
      walk_expr current_fn fn_body
    | Let { value; body; _ } ->
      walk_expr current_fn value;
      walk_expr current_fn body
    | If { cond; then_; else_ } ->
      walk_expr current_fn cond;
      walk_expr current_fn then_;
      walk_expr current_fn else_
    | Do stmts ->
      List.iter (function
        | StmtLet { value; _ } -> walk_expr current_fn value
        | StmtExpr e           -> walk_expr current_fn e
      ) stmts
    | Letrec (bindings, body) ->
      List.iter (fun b -> walk_expr current_fn b.letrec_body) bindings;
      walk_expr current_fn body
    | Record fields ->
      List.iter (fun (_, e) -> walk_expr current_fn e) fields
    | RecordUpdate (base, fields) ->
      walk_expr current_fn base;
      List.iter (fun (_, e) -> walk_expr current_fn e) fields
    | Project (e, _) ->
      walk_expr current_fn e
    | Match { scrutinee; arms } ->
      walk_expr current_fn scrutinee;
      List.iter (fun arm -> walk_expr current_fn arm.arm_body) arms
    | Handle { handled; handlers } ->
      walk_expr current_fn handled;
      List.iter (fun h ->
        List.iter (fun op -> walk_expr current_fn op.op_handler_body) h.op_handlers;
        Option.iter (fun r -> walk_expr current_fn r.return_body) h.return_handler
      ) handlers
    | Perform { args; _ } ->
      List.iter (walk_expr current_fn) args
    | Var _ | IntLit _ | FloatLit _ | StringLit _ | BoolLit _ | UnitLit -> ()
  in
  List.iter (fun decl ->
    match decl.decl_desc with
    | DeclFn { fn_name; decl_body; _ } -> walk_expr fn_name decl_body
    | _ -> ()
  ) prog;
  List.rev !sites

(* ------------------------------------------------------------------ *)
(* Tail-call violation collection                                       *)
(* ------------------------------------------------------------------ *)

type tail_call_violation = {
  tv_fn_name : string;
  tv_line    : int;
  tv_col     : int;
}

(** [collect_tail_call_violations prog fn_name] walks the body of [fn_name]
    in [prog] and returns one [tail_call_violation] per recursive self-call
    that is NOT in tail position.  Returns an empty list if [fn_name] is not
    defined or has no recursive calls at all. *)
let collect_tail_call_violations (prog : program) (fn_name : string)
    : tail_call_violation list =
  let fn_body_opt =
    List.find_map (fun d ->
      match d.decl_desc with
      | DeclFn { fn_name = n; decl_body; _ } when n = fn_name -> Some decl_body
      | _ -> None) prog
  in
  match fn_body_opt with
  | None -> []
  | Some body ->
    let violations = ref [] in
    let rec walk is_tail e =
      match e.desc with
      | App (f, args) ->
        (match f.desc with
         | Var callee when callee = fn_name ->
           if not is_tail then
             violations := { tv_fn_name = fn_name; tv_line = 0; tv_col = 0 }
                           :: !violations
         | _ -> walk false f);
        List.iter (walk false) args
      | Let { value; body; _ } ->
        walk false value;
        walk is_tail body
      | If { cond; then_; else_ } ->
        walk false cond;
        walk is_tail then_;
        walk is_tail else_
      | Match { scrutinee; arms } ->
        walk false scrutinee;
        List.iter (fun arm -> walk is_tail arm.arm_body) arms
      | Do stmts ->
        let n = List.length stmts in
        List.iteri (fun i stmt ->
          match stmt with
          | StmtLet { value; _ } -> walk false value
          | StmtExpr e           -> walk (is_tail && i = n - 1) e
        ) stmts
      | Fn { fn_body; _ } ->
        walk true fn_body
      | Letrec (bindings, body) ->
        List.iter (fun b -> walk false b.letrec_body) bindings;
        walk is_tail body
      | Handle { handled; handlers } ->
        walk false handled;
        List.iter (fun h ->
          List.iter (fun op -> walk is_tail op.op_handler_body) h.op_handlers;
          Option.iter (fun r -> walk is_tail r.return_body) h.return_handler
        ) handlers
      | Record fields -> List.iter (fun (_, e) -> walk false e) fields
      | RecordUpdate (base, fields) ->
        walk false base;
        List.iter (fun (_, e) -> walk false e) fields
      | Project (e, _) -> walk false e
      | Perform { args; _ } -> List.iter (walk false) args
      | Var _ | IntLit _ | FloatLit _ | StringLit _ | BoolLit _ | UnitLit -> ()
    in
    walk true body;
    List.rev !violations
