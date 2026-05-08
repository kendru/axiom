(** Code generator: translates type-checked Axiom programs to WASM binary.

    Runs the elaboration pass first, then compiles [Elaboration.eprogram]
    to WASM.  Key additions over Milestone 1:
    - Evidence parameters (leading i32 locals in every effectful function)
    - [EPerform]: lowered to call_indirect through the evidence struct
    - [EHandle]: op-handler bodies compiled as module functions, evidence
      struct built in linear memory at the handle site
    - Tail-call optimisation (issue #43):
      · Default mode emits [return_call] / [return_call_indirect] in tail
        position (WASM tail-call proposal, opcodes 0x12/0x13).
      · [--no-tail-calls] mode wraps each function body in a [loop] and
        rewrites direct self-tail-calls into local-set + [br 0], giving
        guaranteed stack-safety for self-recursive functions. *)

open Wasm_encode
open Elaboration

(* ------------------------------------------------------------------ *)
(* Type conversion                                                      *)
(* ------------------------------------------------------------------ *)

let val_type_of_type_expr = function
  | Ast.TyName _ | Ast.TyApp _ -> I32   (* all scalars and ADT pointers lower to i32 *)
  | _ -> failwith "Codegen: unsupported type"

let is_supported_type = function
  | Ast.TyName _ | Ast.TyApp _ -> true
  | _ -> false

(* ------------------------------------------------------------------ *)
(* Primitive binary operations                                          *)
(* ------------------------------------------------------------------ *)

let primitives = ["add"; "sub"; "mul"; "eq"; "neq"; "lt"; "gt"; "lte"; "le"; "gte"; "ge"]

let instr_of_prim = function
  | "add"        -> I32Add
  | "sub"        -> I32Sub
  | "mul"        -> I32Mul
  | "eq"         -> I32Eq
  | "neq"        -> I32Ne
  | "lt"         -> I32LtS
  | "gt"         -> I32GtS
  | "lte" | "le" -> I32LeS
  | "gte" | "ge" -> I32GeS
  | _            -> assert false

(* ------------------------------------------------------------------ *)
(* TCO types                                                            *)
(* ------------------------------------------------------------------ *)

type tco_mode =
  | TailCallInstr  (** emit return_call / return_call_indirect *)
  | TrampolineLoop (** loop + br rewrite for direct self-tail-calls *)

type trampoline_ctx = {
  self_idx   : int;      (** WASM func index of the function being compiled *)
  param_locs : int list; (** local indices for each param, in declaration order *)
  br_depth   : int;      (** br label distance to the enclosing trampoline loop *)
}

(* ------------------------------------------------------------------ *)
(* Per-function compilation state                                       *)
(* ------------------------------------------------------------------ *)

type ctx = {
  env          : (string * int) list;         (** variable name -> local index *)
  next_local   : int ref;                     (** next free local slot *)
  extra_locals : local_decl list ref;         (** additional locals beyond params *)
  func_map     : (string * int) list;         (** function name -> wasm func index *)
  ctor_map     : (string * (int * int)) list; (** ctor name -> (tag, arity) *)
  alloc_idx    : int;                         (** func index of __alloc *)
  module_      : module_builder;
  pending      : (int * local_decl list * instr list) list ref;
  effect_map   : (string * Ast.effect_op list) list; (** effect name -> ops *)
  table_funcs  : int list ref;               (** accumulated func indices for table *)
  tco          : tco_mode;
  trampoline   : trampoline_ctx option;
  string_pool  : (string * int) list;        (** literal -> static memory offset *)
}

let make_ctx tco params func_map ctor_map alloc_idx module_ pending effect_map table_funcs
    string_pool =
  { env          = List.mapi (fun i (name, _) -> (name, i)) params
  ; next_local   = ref (List.length params)
  ; extra_locals = ref []
  ; func_map
  ; ctor_map
  ; alloc_idx
  ; module_
  ; pending
  ; effect_map
  ; table_funcs
  ; tco
  ; trampoline   = None
  ; string_pool
  }

let alloc_local ctx =
  let idx = !(ctx.next_local) in
  incr ctx.next_local;
  ctx.extra_locals := !(ctx.extra_locals) @ [{ count = 1; ty = I32 }];
  idx

let add_to_table ctx func_idx =
  let slot = List.length !(ctx.table_funcs) in
  ctx.table_funcs := !(ctx.table_funcs) @ [func_idx];
  slot

(* ------------------------------------------------------------------ *)
(* TCO helpers                                                          *)
(* ------------------------------------------------------------------ *)

(** Increment br_depth when entering a structured WASM block that wraps
    the tail position (i.e. an [If] block).  Other constructs like [let]
    and [do] don't introduce new WASM blocks so the depth is unchanged. *)
let bump_nest ctx =
  match ctx.trampoline with
  | None   -> ctx
  | Some t -> { ctx with trampoline = Some { t with br_depth = t.br_depth + 1 } }

(** Install a trampoline context for a function with [n_params] params
    starting at local 0.  No-op in TailCallInstr mode. *)
let setup_trampoline tco func_idx n_params ctx =
  match tco with
  | TailCallInstr  -> ctx
  | TrampolineLoop ->
    let param_locs = List.init n_params (fun i -> i) in
    { ctx with trampoline = Some { self_idx = func_idx; param_locs; br_depth = 0 } }

(** Wrap compiled body instructions in a trampoline [loop] when needed.
    The loop has result type i32 (all current Axiom values are i32); paths
    that produce a result fall through normally, while self-tail-calls do
    [br 0] to restart without consuming stack space. *)
let wrap_trampoline tco body =
  match tco with
  | TailCallInstr  -> body
  | TrampolineLoop -> [Loop (ValType I32, body)]

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let list_find_index f lst =
  let rec go i = function
    | [] -> failwith "Codegen: list_find_index: element not found"
    | x :: rest -> if f x then i else go (i + 1) rest
  in
  go 0 lst

(* ------------------------------------------------------------------ *)
(* Free-variable analysis for handler closure capture                   *)
(* ------------------------------------------------------------------ *)

(** Extract variable names bound by a pattern. *)
let rec bound_vars_of_epat (pat : Ast.pattern) : string list =
  match pat.Ast.pat_desc with
  | Ast.PVar x            -> [x]
  | Ast.PCtor (_, subs)   -> List.concat_map bound_vars_of_epat subs
  | Ast.POr (p1, p2)      -> bound_vars_of_epat p1 @ bound_vars_of_epat p2
  | _                     -> []

(** [free_vars_in_eexpr env bound e acc] collects variables referenced in [e]
    that are present in [env] (outer local variables) and not already in [bound]
    or [acc].  Top-level functions and constructors (absent from [env]) are
    excluded automatically. *)
let rec free_vars_in_eexpr
    (env   : (string * int) list)
    (bound : string list)
    (e     : eexpr)
    (acc   : string list)
  : string list =
  match e.edesc with
  | EVar x ->
    if List.mem x bound || not (List.mem_assoc x env) || List.mem x acc
    then acc
    else x :: acc
  | EIntLit _ | EFloatLit _ | EBoolLit _ | EUnitLit | EStringLit _ -> acc
  | ELet { pat; value; body } ->
    let acc'   = free_vars_in_eexpr env bound value acc in
    let bound' = bound_vars_of_epat pat @ bound in
    free_vars_in_eexpr env bound' body acc'
  | EApp (f, args) ->
    List.fold_left (fun ac a -> free_vars_in_eexpr env bound a ac)
      (free_vars_in_eexpr env bound f acc) args
  | EFn { ev_params; params; fn_body; _ } ->
    let bound' =
      List.map (fun ep -> ep.ev_name) ev_params
      @ List.map (fun p -> p.Ast.param_name) params
      @ bound
    in
    free_vars_in_eexpr env bound' fn_body acc
  | EIf { cond; then_; else_ } ->
    free_vars_in_eexpr env bound else_
      (free_vars_in_eexpr env bound then_
         (free_vars_in_eexpr env bound cond acc))
  | EDo stmts ->
    let rec go bnd ac = function
      | [] -> ac
      | EStmtLet { pat; value } :: rest ->
        let ac'  = free_vars_in_eexpr env bnd value ac in
        let bnd' = bound_vars_of_epat pat @ bnd in
        go bnd' ac' rest
      | EStmtExpr e2 :: rest ->
        go bnd (free_vars_in_eexpr env bnd e2 ac) rest
    in
    go bound acc stmts
  | EMatch { scrutinee; arms } ->
    List.fold_left (fun ac arm ->
      let bnd' = bound_vars_of_epat arm.pattern @ bound in
      free_vars_in_eexpr env bnd' arm.arm_body ac
    ) (free_vars_in_eexpr env bound scrutinee acc) arms
  | ELetrec (bindings, body) ->
    let bnd' = List.map (fun b -> b.letrec_name) bindings @ bound in
    List.fold_left (fun ac b ->
      let bp = List.map (fun p -> p.Ast.param_name) b.letrec_params @ bnd' in
      free_vars_in_eexpr env bp b.letrec_body ac
    ) (free_vars_in_eexpr env bnd' body acc) bindings
  | ERecord fields ->
    List.fold_left (fun ac (_, v) -> free_vars_in_eexpr env bound v ac) acc fields
  | ERecordUpdate (base, fields, _) ->
    List.fold_left (fun ac (_, v) -> free_vars_in_eexpr env bound v ac)
      (free_vars_in_eexpr env bound base acc) fields
  | EProject (e2, _, _) -> free_vars_in_eexpr env bound e2 acc
  | EPerform { args; ev_var; _ } ->
    (* ev_var is itself an outer variable that may need capture *)
    let acc' =
      if List.mem ev_var bound || not (List.mem_assoc ev_var env) || List.mem ev_var acc
      then acc
      else ev_var :: acc
    in
    List.fold_left (fun ac a -> free_vars_in_eexpr env bound a ac) acc' args
  | EHandle { handled; handlers } ->
    List.fold_left (fun ac h ->
      let ret_acc = match h.return_handler with
        | None -> ac
        | Some rh ->
          let bnd' = [rh.return_var] @ bound in
          free_vars_in_eexpr env bnd' rh.return_body ac
      in
      List.fold_left (fun a oh ->
        let bnd' = oh.op_handler_params @ bound in
        free_vars_in_eexpr env bnd' oh.op_handler_body a
      ) ret_acc h.op_handlers
    ) (free_vars_in_eexpr env bound handled acc) handlers

(* ------------------------------------------------------------------ *)
(* Expression compiler (elaborated IR)                                  *)
(* ------------------------------------------------------------------ *)

(* Allocate an ADT value: tag at offset 0, fields at offsets 4, 8, ...
   Returns pointer via the last LocalGet. *)
let rec compile_ctor ctx tag args =
  let arity = List.length args in
  let ptr_local = alloc_local ctx in
  [I32Const ((1 + arity) * 4); Call ctx.alloc_idx; LocalTee ptr_local;
   I32Const tag; I32Store (2, 0)]
  @ List.concat (List.mapi (fun i arg ->
      [LocalGet ptr_local] @ compile_eexpr ctx arg @ [I32Store (2, (i + 1) * 4)]
    ) args)
  @ [LocalGet ptr_local]

(* Compile a single pattern against scrut_local. *)
and compile_pat ctx scrut_local pat on_match on_fail =
  match pat.Ast.pat_desc with
  | Ast.PWild ->
    on_match ctx

  | Ast.PVar x ->
    on_match { ctx with env = (x, scrut_local) :: ctx.env }

  | Ast.PLitInt n ->
    (* Each If wrapper adds one level; bump depth so Br inside on_match is correct. *)
    let inner = bump_nest ctx in
    [LocalGet scrut_local; I32Const (Int64.to_int n); I32Eq;
     If (ValType I32, on_match inner, Some on_fail)]

  | Ast.PLitTrue ->
    let inner = bump_nest ctx in
    [LocalGet scrut_local; I32Const 1; I32Eq;
     If (ValType I32, on_match inner, Some on_fail)]

  | Ast.PLitFalse ->
    let inner = bump_nest ctx in
    [LocalGet scrut_local; I32Const 0; I32Eq;
     If (ValType I32, on_match inner, Some on_fail)]

  | Ast.PCtor (ctor_name, sub_pats) ->
    let (tag, _) = List.assoc ctor_name ctx.ctor_map in
    let field_locals = List.mapi (fun i _ -> (i, alloc_local ctx)) sub_pats in
    let load_fields = List.concat_map (fun (i, li) ->
      [LocalGet scrut_local; I32Load (2, (i + 1) * 4); LocalSet li]
    ) field_locals in
    let pairs = List.map2 (fun (_, li) p -> (li, p)) field_locals sub_pats in
    let inner = bump_nest ctx in
    let inner_match = chain_pats inner pairs on_match on_fail in
    [LocalGet scrut_local; I32Load (2, 0); I32Const tag; I32Eq;
     If (ValType I32, load_fields @ inner_match, Some on_fail)]

  | Ast.POr (p1, p2) ->
    let try_p2 = compile_pat ctx scrut_local p2 on_match on_fail in
    compile_pat ctx scrut_local p1 on_match try_p2

  | other ->
    failwith (Format.asprintf "Codegen: unsupported pattern: %a"
                Ast.pp_pattern_desc other)

and chain_pats ctx pairs on_match on_fail =
  match pairs with
  | [] -> on_match ctx
  | (li, pat) :: rest ->
    compile_pat ctx li pat
      (fun ctx' -> chain_pats ctx' rest on_match on_fail)
      on_fail

(* In trampoline mode tail position is not safe to propagate through match
   because on_fail is pre-compiled at an outer depth; using return_call in
   match arms is fine since it ignores br depth entirely. *)
and compile_match ?(tail=false) ctx scrut_local arms =
  match arms with
  | [] ->
    (* Exhaustiveness is guaranteed by the type-checker; this path is
       a defensive trap that fires only on a bug in an earlier pass. *)
    [Unreachable]
  | arm :: rest ->
    let arm_tail = tail && (match ctx.tco with TailCallInstr -> true | TrampolineLoop -> false) in
    let on_fail = compile_match ~tail:arm_tail ctx scrut_local rest in
    compile_pat ctx scrut_local arm.pattern
      (fun ctx' -> compile_eexpr ~tail:arm_tail ctx' arm.arm_body)
      on_fail

(** [compile_eexpr ~tail ctx e] compiles expression [e].
    [tail] signals that [e] is in tail position; the compiler may then emit
    [return_call] (TailCallInstr mode) or a [br] to the trampoline loop
    (TrampolineLoop mode) instead of a plain [call]. *)
and compile_eexpr ?(tail=false) ctx e =
  match e.edesc with
  | EIntLit n ->
    [I32Const (Int64.to_int n)]

  | EFloatLit _ ->
    failwith "Codegen: floating-point not yet supported"

  | EUnitLit ->
    [I32Const 0]

  | EBoolLit b ->
    [I32Const (if b then 1 else 0)]

  (* Nullary constructor used as a value *)
  | EVar name when List.mem_assoc name ctx.ctor_map ->
    let (tag, arity) = List.assoc name ctx.ctor_map in
    if arity <> 0 then
      failwith ("Codegen: constructor " ^ name ^ " requires arguments");
    compile_ctor ctx tag []

  | EVar x ->
    (match List.assoc_opt x ctx.env with
     | Some idx -> [LocalGet idx]
     | None     -> failwith ("Codegen: unbound variable: " ^ x))

  (* resume(v) in a handler body: for linear effects, just return v *)
  | EApp ({ edesc = EVar "resume"; _ }, [e]) ->
    compile_eexpr ctx e

  (* Unary negation: neg(x) = 0 - x *)
  | EApp ({ edesc = EVar "neg"; _ }, [a]) ->
    [I32Const 0] @ compile_eexpr ctx a @ [I32Sub]

  (* Known binary primitives *)
  | EApp ({ edesc = EVar op; _ }, [a; b])
    when List.mem op primitives ->
    compile_eexpr ctx a @ compile_eexpr ctx b @ [instr_of_prim op]

  (* Constructor application *)
  | EApp ({ edesc = EVar name; _ }, args)
    when List.mem_assoc name ctx.ctor_map ->
    let (tag, _) = List.assoc name ctx.ctor_map in
    compile_ctor ctx tag args

  (* Module-qualified function call: mod.fn(args) — index is unused here *)
  | EApp ({ edesc = EProject ({ edesc = EVar mod_name; _ }, fn_name, _); _ }, args) ->
    let qualified = mod_name ^ "." ^ fn_name in
    (match List.assoc_opt qualified ctx.func_map with
     | Some idx ->
       let arg_code = List.concat_map (compile_eexpr ctx) args in
       arg_code @ [Call idx]
     | None ->
       failwith ("Codegen: unknown module function: " ^ qualified))

  (* General function call — may be a tail call *)
  | EApp ({ edesc = EVar name; _ }, args) ->
    (match List.assoc_opt name ctx.func_map with
     | Some idx ->
       let arg_code = List.concat_map (compile_eexpr ctx) args in
       if tail then
         (match ctx.tco with
          | TailCallInstr ->
            arg_code @ [ReturnCall idx]
          | TrampolineLoop ->
            (match ctx.trampoline with
             | Some t when t.self_idx = idx
                        && List.length t.param_locs = List.length args ->
               (* Self-tail-call: args are already on the stack in order.
                  Pop them into the param locals (reverse order = LIFO),
                  then branch back to the loop start. *)
               arg_code
               @ List.rev_map (fun li -> LocalSet li) t.param_locs
               @ [Br t.br_depth]
             | _ ->
               (* Non-self or arity mismatch: regular call (may stack overflow
                  for mutual recursion, acceptable trampoline limitation). *)
               arg_code @ [Call idx]))
       else
         arg_code @ [Call idx]
     | None ->
       failwith ("Codegen: unknown function: " ^ name))

  | ELet { pat = { Ast.pat_desc = Ast.PVar x; _ }; value; body } ->
    let idx        = alloc_local ctx in
    let value_code = compile_eexpr ctx value in
    let body_code  = compile_eexpr ~tail { ctx with env = (x, idx) :: ctx.env } body in
    value_code @ [LocalSet idx] @ body_code

  | ELet { pat = { Ast.pat_desc = Ast.PWild; _ }; value; body } ->
    let value_code = compile_eexpr ctx value in
    let body_code  = compile_eexpr ~tail ctx body in
    value_code @ [Drop] @ body_code

  | EIf { cond; then_; else_ } ->
    (* The If instruction creates a structured block; bump depth so that Br
       inside a branch correctly targets the outer trampoline loop. *)
    let inner = bump_nest ctx in
    compile_eexpr ctx cond @
    [If (ValType I32,
         compile_eexpr ~tail inner then_,
         Some (compile_eexpr ~tail inner else_))]

  | EMatch { scrutinee; arms } ->
    let scrut_local = alloc_local ctx in
    compile_eexpr ctx scrutinee
    @ [LocalSet scrut_local]
    @ compile_match ~tail ctx scrut_local arms

  | EDo stmts ->
    compile_do ~tail ctx stmts

  | ELetrec (bindings, body) ->
    compile_eletrec ~tail ctx bindings body

  | EPerform { effect_name; op_name; args; ev_var } ->
    let ops = List.assoc effect_name ctx.effect_map in
    let op_idx = list_find_index
        (fun op -> op.Ast.effect_op_name = op_name) ops in
    let op = List.nth ops op_idx in
    let ev_local =
      match List.assoc_opt ev_var ctx.env with
      | Some i -> i
      | None   -> failwith ("Codegen: unbound evidence variable: " ^ ev_var)
    in
    (* Evidence struct layout (8 bytes per op):
       op_idx*8+0 = fn table slot; op_idx*8+4 = closure env pointer *)
    let env_ptr_code = [LocalGet ev_local; I32Load (2, op_idx * 8 + 4)] in
    let load_fn      = [LocalGet ev_local; I32Load (2, op_idx * 8    )] in
    (* Handler type: (env_ptr: i32, op_params...) -> op_ret *)
    let param_tys = I32 :: List.map val_type_of_type_expr op.Ast.effect_op_params in
    let ret_ty    = val_type_of_type_expr op.Ast.effect_op_return in
    let type_idx  = add_type ctx.module_ { params = param_tys; results = [ret_ty] } in
    let arg_code  = List.concat_map (compile_eexpr ctx) args in
    (* Stack: env_ptr  arg0 … argN  fn_slot  → call_indirect *)
    if tail && ctx.tco = TailCallInstr then
      env_ptr_code @ arg_code @ load_fn @ [ReturnCallIndirect (type_idx, 0)]
    else
      env_ptr_code @ arg_code @ load_fn @ [CallIndirect (type_idx, 0)]

  | EHandle { handled; handlers } ->
    compile_ehandle ctx handled handlers

  | EStringLit s ->
    (match List.assoc_opt s ctx.string_pool with
     | Some off -> [I32Const off]
     | None -> failwith ("Codegen: string literal not in pool: " ^ String.escaped s))

  (* ── Record construction ─────────────────────────────────────────────
     Layout: heap-allocated block of n × 4 bytes.  Field i (in canonical
     alphabetical order) lives at byte offset i × 4.  The elaboration pass
     guarantees that [fields] are already sorted alphabetically. *)
  | ERecord fields ->
    let n = List.length fields in
    if n = 0 then
      [I32Const 0]   (* empty record is represented as a null pointer *)
    else
      let ptr_local = alloc_local ctx in
      [I32Const (n * 4); Call ctx.alloc_idx; LocalSet ptr_local]
      @ List.concat (List.mapi (fun i (_, v) ->
          [LocalGet ptr_local]
          @ compile_eexpr ctx v
          @ [I32Store (2, i * 4)]
        ) fields)
      @ [LocalGet ptr_local]

  (* ── Record field projection ─────────────────────────────────────────
     Load the i32 value at offset [idx × 4] from the record pointer. *)
  | EProject (base, _field, idx) ->
    compile_eexpr ctx base @ [I32Load (2, idx * 4)]

  (* ── Functional record update ────────────────────────────────────────
     Allocate a fresh record of the same size.  For each field (in sorted
     order): if it is listed in [updates], emit the new value; otherwise
     copy the original value from [base_ptr + offset]. *)
  | ERecordUpdate (base, updates, all_field_names) ->
    let n = List.length all_field_names in
    let base_local = alloc_local ctx in
    let new_ptr_local = alloc_local ctx in
    compile_eexpr ctx base
    @ [LocalSet base_local]
    @ [I32Const (n * 4); Call ctx.alloc_idx; LocalSet new_ptr_local]
    @ List.concat (List.mapi (fun i field_name ->
        let value_instrs =
          match List.assoc_opt field_name updates with
          | Some update_expr -> compile_eexpr ctx update_expr
          | None             -> [LocalGet base_local; I32Load (2, i * 4)]
        in
        [LocalGet new_ptr_local] @ value_instrs @ [I32Store (2, i * 4)]
      ) all_field_names)
    @ [LocalGet new_ptr_local]

  | other ->
    failwith (Format.asprintf "Codegen: unsupported eexpr: %a" pp_eexpr { edesc = other })

(* Sequential do-block: all but the last statement drop their value. *)
and compile_do ?(tail=false) ctx stmts =
  match stmts with
  | [] -> failwith "Codegen: empty do block"
  | [EStmtExpr e] ->
    compile_eexpr ~tail ctx e
  | EStmtLet { pat = { Ast.pat_desc = Ast.PVar x; _ }; value } :: rest ->
    let idx = alloc_local ctx in
    compile_eexpr ctx value
    @ [LocalSet idx]
    @ compile_do ~tail { ctx with env = (x, idx) :: ctx.env } rest
  | EStmtExpr e :: rest ->
    compile_eexpr ctx e @ [Drop] @ compile_do ~tail ctx rest
  | _ ->
    failwith "Codegen: unsupported do statement"

(* Letrec: pre-register all bindings as module functions so mutual and
   self references work, then compile each body and the continuation. *)
and compile_eletrec ?(tail=false) ctx bindings body =
  let entries = List.map (fun (b : eletrec_binding) ->
    let param_tys = List.map (fun p ->
      (p.Ast.param_name, val_type_of_type_expr p.Ast.param_type)
    ) b.letrec_params in
    let ret_ty = val_type_of_type_expr b.letrec_return_type in
    let idx = reserve_function ctx.module_ (List.map snd param_tys) [ret_ty] in
    (b, idx, param_tys)
  ) bindings in
  let new_func_map =
    List.map (fun (b, idx, _) -> (b.letrec_name, idx)) entries
    @ ctx.func_map
  in
  List.iter (fun (b, idx, param_tys) ->
    let fn_ctx =
      make_ctx ctx.tco param_tys new_func_map ctx.ctor_map ctx.alloc_idx
        ctx.module_ ctx.pending ctx.effect_map ctx.table_funcs ctx.string_pool
    in
    let fn_ctx = setup_trampoline ctx.tco idx (List.length param_tys) fn_ctx in
    let instrs = compile_eexpr ~tail:true fn_ctx b.letrec_body in
    let instrs = wrap_trampoline ctx.tco instrs in
    ctx.pending := !(ctx.pending) @ [(idx, !(fn_ctx.extra_locals), instrs)]
  ) entries;
  compile_eexpr ~tail { ctx with func_map = new_func_map } body

(* Handle: compile each op-handler as a module function with a closure
   environment, build the evidence struct in linear memory, then compile
   the handled expression in a context where each effect's evidence
   variable is bound.

   Evidence struct layout (8 bytes per op):
     op_idx*8+0 : i32  table slot of the handler function
     op_idx*8+4 : i32  pointer to the closure environment (0 = no closure)

   Closure environment layout (variable size):
     For abort-style ops (return type Nothing) that have a return handler:
       slot 0     : throw_ctrl_ptr  (pointer to [abort_flag (i32), abort_val (i32)])
       slots 1..n : captured outer variables
     For all other ops:
       slots 0..n-1 : captured outer variables

   Handler function signature: (env_ptr: i32, op_params...) -> op_ret

   For abort-style handlers, after compiling the body:
     - store the result in throw_ctrl[4] (abort_val)
     - store 1 in throw_ctrl[0] (abort_flag)
     - return 0 (dummy; the actual result is retrieved at the handle site)

   After the handled expression, if a throw_ctrl was allocated for a handler
   with a return clause, the abort flag is checked:
     - flag set:   return throw_ctrl[4] (bypass the return handler)
     - flag clear: apply the return handler to the computation result *)
and compile_ehandle ctx handled handlers =
  let is_nothing ty = ty = Ast.TyName "Nothing" in

  let handler_ops eff =
    match List.assoc_opt eff ctx.effect_map with
    | Some ops -> ops
    | None     -> failwith ("Codegen: unknown effect: " ^ eff)
  in

  (* For each handler group, determine if a throw-control block is needed.
     It is needed when there is a return handler AND at least one op whose
     declared return type is Nothing (i.e. the handler never calls resume). *)
  let handler_needs_throw_ctrl (h : eeffect_handler) =
    h.return_handler <> None &&
    let ops = handler_ops h.effect_handler in
    List.exists (fun (oh : eop_handler) ->
      match List.find_opt
              (fun o -> o.Ast.effect_op_name = oh.op_handler_name) ops with
      | Some op -> is_nothing op.Ast.effect_op_return
      | None    -> false
    ) h.op_handlers
  in

  (* Compile all handlers, collecting evidence setup code and ev-var bindings. *)
  let handler_infos = List.map (fun (h : eeffect_handler) ->
    let ops  = handler_ops h.effect_handler in
    let n_ops = List.length ops in
    let needs_tc = handler_needs_throw_ctrl h in

    (* Optionally allocate a throw-control block: [abort_flag=0, abort_val=0] *)
    let tc_local, tc_setup =
      if needs_tc then
        let l = alloc_local ctx in
        let s = [I32Const 8; Call ctx.alloc_idx; LocalTee l;
                 I32Const 0; I32Store (2, 0)] in   (* abort_flag = 0 *)
        (Some l, s)
      else
        (None, [])
    in

    let op_compiled = List.map (fun (oh : eop_handler) ->
      let op_idx = list_find_index
          (fun op -> op.Ast.effect_op_name = oh.op_handler_name) ops in
      let op = List.nth ops op_idx in
      let is_abort = is_nothing op.Ast.effect_op_return && tc_local <> None in

      (* Compute outer variables captured by this handler body.
         "resume" is a special form, not a variable; constructors and
         top-level functions live in func_map / ctor_map, not env. *)
      let own_params = oh.op_handler_params in
      let free_vars  = free_vars_in_eexpr ctx.env own_params
                         oh.op_handler_body [] in

      (* Closure env layout:
           slot 0           : throw_ctrl_ptr  (only for abort ops)
           slots 1..n       : captured vars   (abort case)
           OR
           slots 0..n-1     : captured vars   (non-abort case) *)
      let tc_slot         = if is_abort then Some 0 else None in
      let free_base       = if is_abort then 1 else 0 in
      let free_var_slots  = List.mapi (fun i v -> (free_base + i, v)) free_vars in
      let n_env = List.length free_vars + (if is_abort then 1 else 0) in

      (* Handler WASM function params: (__env: i32, op_params…) *)
      let param_pairs =
        ("__env", I32) ::
        List.map2 (fun name ty -> (name, val_type_of_type_expr ty))
          oh.op_handler_params op.Ast.effect_op_params
      in
      let ret_ty  = val_type_of_type_expr op.Ast.effect_op_return in
      let fn_idx  = reserve_function ctx.module_
                      (List.map snd param_pairs) [ret_ty] in
      let tbl_slot = add_to_table ctx fn_idx in

      (* Build closure env at the call site (inside ctx) *)
      let env_outer_local = alloc_local ctx in
      let env_build =
        if n_env = 0 then
          [I32Const 0; LocalSet env_outer_local]
        else
          [I32Const (n_env * 4); Call ctx.alloc_idx; LocalSet env_outer_local]
          @ (match tc_slot, tc_local with
             | Some sl, Some tl ->
               [LocalGet env_outer_local; LocalGet tl;
                I32Store (2, sl * 4)]
             | _ -> [])
          @ List.concat_map (fun (sl, vname) ->
              let vl = List.assoc vname ctx.env in
              [LocalGet env_outer_local; LocalGet vl;
               I32Store (2, sl * 4)]
            ) free_var_slots
      in

      (* Build the handler function context *)
      let handler_ctx =
        make_ctx ctx.tco param_pairs ctx.func_map ctx.ctor_map
          ctx.alloc_idx ctx.module_ ctx.pending
          ctx.effect_map ctx.table_funcs ctx.string_pool
      in
      (* env_ptr is local 0 in handler_ctx (first param) *)

      (* Load captured vars from the closure env into handler locals *)
      let handler_ctx, prologue =
        (* Optionally load throw_ctrl_ptr *)
        let hctx, prol =
          match tc_slot with
          | Some sl ->
            let tcl = alloc_local handler_ctx in
            let ld  = [LocalGet 0; I32Load (2, sl * 4); LocalSet tcl] in
            ({ handler_ctx with env = ("__throw_ctrl", tcl) :: handler_ctx.env }, ld)
          | None -> (handler_ctx, [])
        in
        (* Load each free variable *)
        List.fold_left (fun (hc, prol) (sl, vname) ->
          let vl  = alloc_local hc in
          let ld  = [LocalGet 0; I32Load (2, sl * 4); LocalSet vl] in
          ({ hc with env = (vname, vl) :: hc.env }, prol @ ld)
        ) (hctx, prol) free_var_slots
      in

      let handler_ctx =
        setup_trampoline ctx.tco fn_idx (List.length param_pairs) handler_ctx
      in

      (* Compile the handler body *)
      let body = compile_eexpr ~tail:(ctx.tco = TailCallInstr)
                   handler_ctx oh.op_handler_body in

      (* For abort-style handlers, wrap body to set the throw-ctrl block *)
      let full_body =
        if is_abort then begin
          let tcl        = List.assoc "__throw_ctrl" handler_ctx.env in
          let av_local   = alloc_local handler_ctx in
          prologue @ body
          @ [LocalSet av_local;
             LocalGet tcl; I32Const 1; I32Store (2, 0);   (* abort_flag = 1 *)
             LocalGet tcl; LocalGet av_local; I32Store (2, 4); (* abort_val *)
             I32Const 0]                                   (* dummy return *)
        end else
          prologue @ body
      in

      let wrapped = wrap_trampoline ctx.tco full_body in
      ctx.pending :=
        !(ctx.pending) @ [(fn_idx, !(handler_ctx.extra_locals), wrapped)];

      (op_idx, tbl_slot, env_outer_local, env_build)
    ) h.op_handlers in

    (* Build this effect's evidence struct (n_ops × 8 bytes) *)
    let ev_local = alloc_local ctx in
    let ev_setup =
      tc_setup
      (* First emit all closure-env build code (allocates & populates env ptrs) *)
      @ List.concat_map (fun (_, _, _, env_build) -> env_build) op_compiled
      @ [I32Const (n_ops * 8); Call ctx.alloc_idx; LocalSet ev_local]
      @ List.concat_map (fun (op_idx, tbl_slot, env_local, _) ->
          [LocalGet ev_local; I32Const tbl_slot;
           I32Store (2, op_idx * 8)]
          @ [LocalGet ev_local; LocalGet env_local;
             I32Store (2, op_idx * 8 + 4)]
        ) op_compiled
    in
    (h, tc_local, ev_setup, (h.ev_param.ev_name, ev_local))
  ) handlers in

  let ev_setups = List.map (fun (_, _, s, _) -> s) handler_infos in
  let ev_locals = List.map (fun (_, _, _, e) -> e) handler_infos in

  let ctx' = { ctx with env = ev_locals @ ctx.env } in
  let handled_code = List.concat ev_setups @ compile_eexpr ctx' handled in

  (* Apply return handlers, conditionally checking abort flag when needed *)
  List.fold_left (fun instrs (h, tc_local, _, _) ->
    match h.return_handler with
    | None -> instrs
    | Some rh ->
      let result_local = alloc_local ctx in
      let base = instrs @ [LocalSet result_local] in
      let ret_body ctx_rh =
        compile_eexpr
          { ctx_rh with env = (rh.return_var, result_local) :: ctx_rh.env }
          rh.return_body
      in
      (match tc_local with
       | Some tcl ->
         base
         @ [LocalGet tcl; I32Load (2, 0);
            If (ValType I32,
                [LocalGet tcl; I32Load (2, 4)],   (* abort: return abort_val *)
                Some (ret_body ctx))]               (* normal: apply return handler *)
       | None ->
         base @ ret_body ctx)
  ) handled_code handler_infos

(* ------------------------------------------------------------------ *)
(* String literal pool                                                  *)
(* ------------------------------------------------------------------ *)

(** Recursively collect unique string literals from an elaborated expression. *)
let rec scan_eexpr_strings acc e =
  match e.edesc with
  | EStringLit s -> if List.mem s acc then acc else s :: acc
  | EIntLit _ | EBoolLit _ | EUnitLit | EVar _ | EFloatLit _ -> acc
  | ELet { value; body; _ } ->
    scan_eexpr_strings (scan_eexpr_strings acc value) body
  | EApp (f, args) ->
    List.fold_left scan_eexpr_strings (scan_eexpr_strings acc f) args
  | EFn { fn_body; _ } -> scan_eexpr_strings acc fn_body
  | EMatch { scrutinee; arms } ->
    List.fold_left (fun a arm -> scan_eexpr_strings a arm.arm_body)
      (scan_eexpr_strings acc scrutinee) arms
  | EIf { cond; then_; else_ } ->
    scan_eexpr_strings
      (scan_eexpr_strings (scan_eexpr_strings acc cond) then_) else_
  | EDo stmts ->
    List.fold_left (fun a stmt ->
      match stmt with
      | EStmtExpr e2 -> scan_eexpr_strings a e2
      | EStmtLet { value; _ } -> scan_eexpr_strings a value
    ) acc stmts
  | ELetrec (bindings, body) ->
    List.fold_left (fun a b -> scan_eexpr_strings a b.letrec_body)
      (scan_eexpr_strings acc body) bindings
  | ERecord fields ->
    List.fold_left (fun a (_, e2) -> scan_eexpr_strings a e2) acc fields
  | ERecordUpdate (e2, fields, _) ->
    List.fold_left (fun a (_, fe) -> scan_eexpr_strings a fe)
      (scan_eexpr_strings acc e2) fields
  | EProject (e2, _, _) -> scan_eexpr_strings acc e2
  | EPerform { args; _ } -> List.fold_left scan_eexpr_strings acc args
  | EHandle { handled; handlers } ->
    let acc' = scan_eexpr_strings acc handled in
    List.fold_left (fun a handler ->
      let a' = match handler.return_handler with
        | None -> a
        | Some rh -> scan_eexpr_strings a rh.return_body
      in
      List.fold_left (fun a2 oh -> scan_eexpr_strings a2 oh.op_handler_body)
        a' handler.op_handlers
    ) acc' handlers

let rec scan_edecl_strings acc (d : edecl) =
  match d.edecl_desc with
  | EDeclFn f -> scan_eexpr_strings acc f.decl_body
  | EDeclModule { body; _ } -> List.fold_left scan_edecl_strings acc body
  | EDeclPassthrough _ -> acc

(** Walk [eprog], collect unique string literals, assign each a static memory
    offset starting at [base].  Layout per string:
    [offset+0 .. offset+3] = i32 byte-length (little-endian)
    [offset+4 .. offset+4+n-1] = UTF-8 content
    Returns [(literal, offset) list * next_free_offset]. *)
let build_string_pool eprog base =
  let strings = List.fold_left scan_edecl_strings [] eprog in
  List.fold_left (fun (pool, off) s ->
    let n = String.length s in
    let next = (off + 4 + n + 3) / 4 * 4 in   (* 4-byte aligned *)
    (pool @ [(s, off)], next)
  ) ([], base) strings

(** Encode one string as a length-prefixed segment and add it to the module. *)
let emit_string_data m s off =
  let n = String.length s in
  let seg = Bytes.create (4 + n) in
  Bytes.set_int32_le seg 0 (Int32.of_int n);
  Bytes.blit_string s 0 seg 4 n;
  add_data m off seg

(* ------------------------------------------------------------------ *)
(* Built-in string helper functions                                     *)
(* ------------------------------------------------------------------ *)

(** __str_length(ptr) -> len: load the i32 length prefix. *)
let add_str_length_fn m =
  add_function m [I32] [I32] []
    [ LocalGet 0; I32Load (2, 0) ]

(** __str_eq(a, b) -> Bool: byte-by-byte equality. *)
let add_str_eq_fn m =
  add_function m [I32; I32] [I32]
    [{ count = 2; ty = I32 }]   (* locals 2=len_a, 3=i *)
    [ LocalGet 0; I32Load (2, 0); LocalTee 2;
      LocalGet 1; I32Load (2, 0);
      I32Ne; If (Void, [I32Const 0; Return], None);
      I32Const 0; LocalSet 3;
      Block (Void, [
        Loop (Void, [
          LocalGet 3; LocalGet 2; I32GeS; BrIf 1;
          LocalGet 0; I32Const 4; I32Add; LocalGet 3; I32Add; I32Load8U (0, 0);
          LocalGet 1; I32Const 4; I32Add; LocalGet 3; I32Add; I32Load8U (0, 0);
          I32Ne; If (Void, [I32Const 0; Return], None);
          LocalGet 3; I32Const 1; I32Add; LocalSet 3;
          Br 0
        ])
      ]);
      I32Const 1
    ]

(** __str_concat(a, b) -> ptr: allocate and fill a new length-prefixed string.
    params: 0=a, 1=b; locals: 2=len_a, 3=len_b, 4=new_ptr, 5=i, 6=dst *)
let add_str_concat_fn m alloc_idx =
  add_function m [I32; I32] [I32]
    [{ count = 5; ty = I32 }]
    [ LocalGet 0; I32Load (2, 0); LocalSet 2;
      LocalGet 1; I32Load (2, 0); LocalSet 3;
      I32Const 4; LocalGet 2; I32Add; LocalGet 3; I32Add;
      Call alloc_idx; LocalSet 4;
      LocalGet 4; LocalGet 2; LocalGet 3; I32Add; I32Store (2, 0);
      (* copy a bytes: dst = new_ptr+4, src base = a+4 *)
      LocalGet 4; I32Const 4; I32Add; LocalSet 6;
      I32Const 0; LocalSet 5;
      Block (Void, [
        Loop (Void, [
          LocalGet 5; LocalGet 2; I32GeS; BrIf 1;
          LocalGet 6; LocalGet 5; I32Add;
          LocalGet 0; I32Const 4; I32Add; LocalGet 5; I32Add; I32Load8U (0, 0);
          I32Store8 (0, 0);
          LocalGet 5; I32Const 1; I32Add; LocalSet 5;
          Br 0
        ])
      ]);
      (* copy b bytes: dst = new_ptr+4+len_a, src base = b+4 *)
      LocalGet 4; I32Const 4; I32Add; LocalGet 2; I32Add; LocalSet 6;
      I32Const 0; LocalSet 5;
      Block (Void, [
        Loop (Void, [
          LocalGet 5; LocalGet 3; I32GeS; BrIf 1;
          LocalGet 6; LocalGet 5; I32Add;
          LocalGet 1; I32Const 4; I32Add; LocalGet 5; I32Add; I32Load8U (0, 0);
          I32Store8 (0, 0);
          LocalGet 5; I32Const 1; I32Add; LocalSet 5;
          Br 0
        ])
      ]);
      LocalGet 4
    ]

(** __str_head(s) -> Int: first byte as char code, or 0 if empty. *)
let add_str_head_fn m =
  add_function m [I32] [I32] []
    [ LocalGet 0; I32Load (2, 0); I32Const 0; I32Eq;
      If (ValType I32,
        [I32Const 0],
        Some [LocalGet 0; I32Const 4; I32Add; I32Load8U (0, 0)])
    ]

(** max(a, b) -> Int: return the larger of two i32 values. *)
let add_max_fn m =
  add_function m [I32; I32] [I32] []
    [ LocalGet 0; LocalGet 1; I32GtS;
      If (ValType I32, [LocalGet 0], Some [LocalGet 1]) ]

(* ------------------------------------------------------------------ *)
(* Module emitter                                                       *)
(* ------------------------------------------------------------------ *)

let emit ?(use_tail_calls=true) ?(extra_env=[]) ?(extra_eenv=[])
         ?(prelude_prog=[]) (prog : Ast.program) : bytes =
  let tco    = if use_tail_calls then TailCallInstr else TrampolineLoop in
  let eprog  = elaborate_program ~extra_env ~extra_eenv prog in
  (* Pre-scan for string literals; static data starts at offset 64 to leave
     the first 64 bytes as a reserved zero-page area. *)
  let static_base = 64 in
  let (string_pool, _static_end) = build_string_pool eprog static_base in
  let m      = create () in
  (* Add one data segment per unique string literal. *)
  List.iter (fun (s, off) -> emit_string_data m s off) string_pool;
  (* Import memory and core runtime primitives from the "axm" runtime module.
     The runtime owns linear memory (2 pages minimum) and the bump allocator.
     Static string data is placed by the data segments above into the shared
     memory; the runtime's heap starts at page 1 (byte offset 65536) so there
     is no overlap as long as string pool fits within the first 64 KiB.
     The imported memory is re-exported so downstream tooling can inspect it. *)
  add_import_mem m "axm" "memory" { min = 2; max = None };
  add_export m "memory" (ExportMem 0);
  let alloc_idx    = add_import_func m "axm" "axm_alloc"    [I32]       [I32] in
  let axm_print_idx = add_import_func m "axm" "axm_print"   [I32; I32]  [I32] in
  let axm_read_line_idx = add_import_func m "axm" "axm_read_line" []    [I32] in
  (* console_print(str_ptr: i32) -> i32: wrapper that unpacks the
     length-prefixed string layout and calls axm_print(data_ptr, len). *)
  let console_print_idx = add_function m [I32] [I32] []
    [ LocalGet 0; I32Const 4; I32Add   (* data_ptr = str_ptr + 4 *)
    ; LocalGet 0; I32Load (2, 0)       (* len      = load(str_ptr) *)
    ; Call axm_print_idx
    ]
  in
  (* Built-in string primitives — always emitted; func_map entries make them
     callable from Axiom source under their standard names. *)
  let str_length_idx = add_str_length_fn m in
  let str_eq_idx     = add_str_eq_fn m in
  let str_concat_idx = add_str_concat_fn m alloc_idx in
  let str_head_idx   = add_str_head_fn m in
  let max_idx        = add_max_fn m in
  let string_builtins =
    [ ("string_length",   str_length_idx)
    ; ("string_eq",       str_eq_idx)
    ; ("concat",          str_concat_idx)
    ; ("string_head",     str_head_idx)
    ; ("max",             max_idx)
    ; ("console_print",   console_print_idx)
    ; ("axm_print",       axm_print_idx)
    ; ("axm_read_line",   axm_read_line_idx)
    ]
  in
  let pending     = ref [] in
  let table_funcs = ref [] in
  let build_effect_map decls =
    List.filter_map (fun (d : Ast.decl) ->
      match d.Ast.decl_desc with
      | Ast.DeclEffect { effect_name; ops; _ } -> Some (effect_name, ops)
      | _ -> None
    ) decls
  in
  let build_ctor_map decls =
    List.concat_map (fun (d : Ast.decl) ->
      match d.Ast.decl_desc with
      | Ast.DeclType { ctors; _ } ->
        List.concat_map (fun (tag, c) ->
          let arity = List.length c.Ast.ctor_params in
          let entry = (c.Ast.ctor_name, (tag, arity)) in
          let lower = String.lowercase_ascii c.Ast.ctor_name in
          (* Add a lowercase alias so that stdlib-style calls like none(), cons(h,t)
             resolve to the same constructor as the uppercase form. *)
          if lower = c.Ast.ctor_name then [entry]
          else [entry; (lower, (tag, arity))]
        ) (List.mapi (fun tag c -> (tag, c)) ctors)
      | _ -> []
    ) decls
  in
  (* Build maps from prog first, then fill in missing entries from prelude_prog
     so that user-defined effects/types take precedence over the prelude. *)
  let user_effect_map = build_effect_map prog in
  let user_ctor_map   = build_ctor_map prog in
  let prelude_effect_entries =
    List.filter (fun (name, _) -> not (List.mem_assoc name user_effect_map))
      (build_effect_map prelude_prog)
  in
  let prelude_ctor_entries =
    List.filter (fun (name, _) -> not (List.mem_assoc name user_ctor_map))
      (build_ctor_map prelude_prog)
  in
  let effect_map = user_effect_map @ prelude_effect_entries in
  let ctor_map   = user_ctor_map   @ prelude_ctor_entries in
  let fn_decls =
    let check_fn f =
      let params_ok =
        List.for_all (fun p -> is_supported_type p.Ast.param_type) f.params
      in
      let ret_ok =
        match f.return_type with
        | Some t -> is_supported_type t
        | None   -> false
      in
      params_ok && ret_ok
    in
    let rec collect prefix (d : edecl) =
      match d.edecl_desc with
      | EDeclFn f ->
        let f' = match prefix with
          | None   -> f
          | Some p -> { f with fn_name = p ^ "." ^ f.fn_name }
        in
        if check_fn f' then [f'] else []
      | EDeclModule { module_name; body; _ } ->
        List.concat_map (collect (Some module_name)) body
      | _ -> []
    in
    List.concat_map (collect None) eprog
  in
  let fn_entries =
    List.map (fun (f : edecl_fn) ->
      let ev_param_tys =
        List.map (fun ep -> (ep.ev_name, I32)) f.ev_params
      in
      let param_tys =
        List.map (fun p ->
          (p.Ast.param_name, val_type_of_type_expr p.Ast.param_type)
        ) f.params
      in
      let all_param_tys = ev_param_tys @ param_tys in
      let ret_ty = val_type_of_type_expr (Option.get f.return_type) in
      let idx = reserve_function m (List.map snd all_param_tys) [ret_ty] in
      if f.pub || f.fn_name = "main" then
        add_export m f.fn_name (ExportFunc idx);
      (f.fn_name, idx, all_param_tys, f.decl_body)
    ) fn_decls
  in
  let func_map =
    let base =
      string_builtins
      @ List.map (fun (fn_name, idx, _, _) -> (fn_name, idx)) fn_entries
    in
    (* For each "import M as A" declaration, add aliased entries so that
       calls written as A.fn resolve to the same WASM function as M.fn. *)
    let aliases =
      List.filter_map (fun (d : Ast.decl) ->
        match d.Ast.decl_desc with
        | Ast.DeclImport { module_path; alias = Some a } -> Some (module_path, a)
        | _ -> None
      ) prog
    in
    List.fold_left (fun fm (module_path, alias) ->
      let mpfx  = module_path ^ "." in
      let apfx  = alias ^ "." in
      let added = List.filter_map (fun (name, idx) ->
        if String.length name > String.length mpfx
           && String.sub name 0 (String.length mpfx) = mpfx
        then
          let fn_name = String.sub name (String.length mpfx)
                          (String.length name - String.length mpfx) in
          Some (apfx ^ fn_name, idx)
        else None
      ) fm in
      fm @ added
    ) base aliases
  in
  List.iter (fun (_, idx, param_tys, decl_body) ->
    let ctx =
      make_ctx tco param_tys func_map ctor_map alloc_idx m pending
        effect_map table_funcs string_pool
    in
    let ctx = setup_trampoline tco idx (List.length param_tys) ctx in
    (match (try Ok (compile_eexpr ~tail:true ctx decl_body) with Failure msg -> Error msg) with
     | Ok instrs ->
       let instrs = wrap_trampoline tco instrs in
       pending := !pending @ [(idx, !(ctx.extra_locals), instrs)]
     | Error _ ->
       (* Body uses unsupported constructs (e.g. String ops, unimplemented
          primitives); emit a stub so the WASM module stays valid. *)
       pending := !pending @ [(idx, [], [I32Const 0])])
  ) fn_entries;
  if not (List.exists (fun (name, _, _, _) -> name = "main") fn_entries) then begin
    let idx = reserve_function m [] [I32] in
    add_export m "main" (ExportFunc idx);
    pending := !pending @ [(idx, [], [I32Const 0])]
  end;
  if !table_funcs <> [] then begin
    let n = List.length !table_funcs in
    add_table m { min = n; max = Some n };
    add_element m 0 !table_funcs
  end;
  let sorted =
    List.sort (fun (i, _, _) (j, _, _) -> compare i j) !pending
  in
  List.iter (fun (_, locals, instrs) -> add_body m locals instrs) sorted;
  encode m

let emit_stub = emit
