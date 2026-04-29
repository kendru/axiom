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
  | Ast.TyName _ -> I32   (* Int, Bool, and all ADT types lower to i32 *)
  | _ -> failwith "Codegen: unsupported type"

let is_supported_type = function
  | Ast.TyName _ -> true
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
}

let make_ctx tco params func_map ctor_map alloc_idx module_ pending effect_map table_funcs =
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
    [I32Const 0]
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
    (* let introduces no new WASM block, so trampoline depth is unchanged *)
    let body_code  = compile_eexpr ~tail { ctx with env = (x, idx) :: ctx.env } body in
    value_code @ [LocalSet idx] @ body_code

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
    let param_tys = List.map val_type_of_type_expr op.Ast.effect_op_params in
    let ret_ty    = val_type_of_type_expr op.Ast.effect_op_return in
    let type_idx  = add_type ctx.module_ { params = param_tys; results = [ret_ty] } in
    let arg_code  = List.concat_map (compile_eexpr ctx) args in
    let load_fn   = [LocalGet ev_local; I32Load (2, op_idx * 4)] in
    if tail && ctx.tco = TailCallInstr then
      arg_code @ load_fn @ [ReturnCallIndirect (type_idx, 0)]
    else
      arg_code @ load_fn @ [CallIndirect (type_idx, 0)]

  | EHandle { handled; handlers } ->
    compile_ehandle ctx handled handlers

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
        ctx.module_ ctx.pending ctx.effect_map ctx.table_funcs
    in
    let fn_ctx = setup_trampoline ctx.tco idx (List.length param_tys) fn_ctx in
    let instrs = compile_eexpr ~tail:true fn_ctx b.letrec_body in
    let instrs = wrap_trampoline ctx.tco instrs in
    ctx.pending := !(ctx.pending) @ [(idx, !(fn_ctx.extra_locals), instrs)]
  ) entries;
  compile_eexpr ~tail { ctx with func_map = new_func_map } body

(* Handle: compile each op-handler as a module function, build the
   evidence struct in linear memory, then compile the handled expression
   in a context where each effect's evidence variable is bound. *)
and compile_ehandle ctx handled handlers =
  let (ev_setups, ev_locals) =
    List.split (List.map (fun (h : eeffect_handler) ->
      let ops =
        match List.assoc_opt h.effect_handler ctx.effect_map with
        | Some ops -> ops
        | None ->
          failwith ("Codegen: unknown effect: " ^ h.effect_handler)
      in
      let n_ops = List.length ops in
      let op_slots = List.map (fun (oh : eop_handler) ->
        let op_idx = list_find_index
            (fun op -> op.Ast.effect_op_name = oh.op_handler_name) ops in
        let op = List.nth ops op_idx in
        let param_pairs =
          List.map2 (fun name ty -> (name, val_type_of_type_expr ty))
            oh.op_handler_params op.Ast.effect_op_params
        in
        let ret_ty = val_type_of_type_expr op.Ast.effect_op_return in
        let fn_idx = reserve_function ctx.module_
            (List.map snd param_pairs) [ret_ty] in
        let table_slot = add_to_table ctx fn_idx in
        let handler_ctx =
          make_ctx ctx.tco param_pairs ctx.func_map ctx.ctor_map
            ctx.alloc_idx ctx.module_ ctx.pending
            ctx.effect_map ctx.table_funcs
        in
        let handler_ctx =
          setup_trampoline ctx.tco fn_idx (List.length param_pairs) handler_ctx
        in
        let body = compile_eexpr ~tail:(ctx.tco = TailCallInstr) handler_ctx oh.op_handler_body in
        let body = wrap_trampoline ctx.tco body in
        ctx.pending :=
          !(ctx.pending) @ [(fn_idx, !(handler_ctx.extra_locals), body)];
        (op_idx, table_slot)
      ) h.op_handlers in
      let ev_local = alloc_local ctx in
      let setup =
        [I32Const (n_ops * 4); Call ctx.alloc_idx; LocalSet ev_local]
        @ List.concat_map (fun (op_idx, slot) ->
            [LocalGet ev_local; I32Const slot; I32Store (2, op_idx * 4)]
          ) op_slots
      in
      (setup, (h.ev_param.ev_name, ev_local))
    ) handlers)
  in
  let ctx' = { ctx with env = ev_locals @ ctx.env } in
  let handled_code = List.concat ev_setups @ compile_eexpr ctx' handled in
  List.fold_left (fun instrs (h : eeffect_handler) ->
    match h.return_handler with
    | None -> instrs
    | Some rh ->
      let result_local = alloc_local ctx in
      instrs
      @ [LocalSet result_local]
      @ compile_eexpr
          { ctx with env = (rh.return_var, result_local) :: ctx.env }
          rh.return_body
  ) handled_code handlers

(* ------------------------------------------------------------------ *)
(* Module emitter                                                       *)
(* ------------------------------------------------------------------ *)

let emit ?(use_tail_calls=true) (prog : Ast.program) : bytes =
  let tco    = if use_tail_calls then TailCallInstr else TrampolineLoop in
  let eprog  = elaborate_program prog in
  let m      = create () in
  add_memory m { min = 1; max = None };
  add_export m "memory" (ExportMem 0);
  let heap_ptr_idx = add_global m I32 true (GlobI32 1024) in
  let alloc_idx = add_function m ~export:"__alloc"
    [I32] [I32]
    [{ count = 1; ty = I32 }]
    [ GlobalGet heap_ptr_idx
    ; LocalTee 1
    ; LocalGet 0
    ; I32Add
    ; GlobalSet heap_ptr_idx
    ; LocalGet 1
    ]
  in
  let pending     = ref [] in
  let table_funcs = ref [] in
  let effect_map =
    List.filter_map (fun (d : Ast.decl) ->
      match d.Ast.decl_desc with
      | Ast.DeclEffect { effect_name; ops; _ } -> Some (effect_name, ops)
      | _ -> None
    ) prog
  in
  let ctor_map =
    List.concat_map (fun (d : Ast.decl) ->
      match d.Ast.decl_desc with
      | Ast.DeclType { ctors; _ } ->
        List.mapi (fun tag c ->
          (c.Ast.ctor_name, (tag, List.length c.Ast.ctor_params))
        ) ctors
      | _ -> []
    ) prog
  in
  let fn_decls =
    List.filter_map (fun (d : edecl) ->
      match d.edecl_desc with
      | EDeclFn f ->
        let params_ok =
          List.for_all (fun p -> is_supported_type p.Ast.param_type) f.params
        in
        let ret_ok =
          match f.return_type with
          | Some t -> is_supported_type t
          | None   -> false
        in
        if params_ok && ret_ok then Some f else None
      | _ -> None
    ) eprog
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
    List.map (fun (fn_name, idx, _, _) -> (fn_name, idx)) fn_entries
  in
  List.iter (fun (_, idx, param_tys, decl_body) ->
    let ctx =
      make_ctx tco param_tys func_map ctor_map alloc_idx m pending
        effect_map table_funcs
    in
    let ctx = setup_trampoline tco idx (List.length param_tys) ctx in
    let instrs = compile_eexpr ~tail:true ctx decl_body in
    let instrs = wrap_trampoline tco instrs in
    pending := !pending @ [(idx, !(ctx.extra_locals), instrs)]
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
