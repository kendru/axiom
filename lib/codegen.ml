(** Code generator: translates type-checked Axiom programs to WASM binary.

    Runs the elaboration pass first, then compiles [Elaboration.eprogram]
    to WASM.  Key additions over Milestone 1:
    - Evidence parameters (leading i32 locals in every effectful function)
    - [EPerform]: lowered to call_indirect through the evidence struct
    - [EHandle]: op-handler bodies compiled as module functions, evidence
      struct built in linear memory at the handle site *)

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
}

let make_ctx params func_map ctor_map alloc_idx module_ pending effect_map table_funcs =
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
    [LocalGet scrut_local; I32Const (Int64.to_int n); I32Eq;
     If (ValType I32, on_match ctx, Some on_fail)]

  | Ast.PLitTrue ->
    [LocalGet scrut_local; I32Const 1; I32Eq;
     If (ValType I32, on_match ctx, Some on_fail)]

  | Ast.PLitFalse ->
    [LocalGet scrut_local; I32Const 0; I32Eq;
     If (ValType I32, on_match ctx, Some on_fail)]

  | Ast.PCtor (ctor_name, sub_pats) ->
    let (tag, _) = List.assoc ctor_name ctx.ctor_map in
    let field_locals = List.mapi (fun i _ -> (i, alloc_local ctx)) sub_pats in
    let load_fields = List.concat_map (fun (i, li) ->
      [LocalGet scrut_local; I32Load (2, (i + 1) * 4); LocalSet li]
    ) field_locals in
    let pairs = List.map2 (fun (_, li) p -> (li, p)) field_locals sub_pats in
    let inner = chain_pats ctx pairs on_match on_fail in
    [LocalGet scrut_local; I32Load (2, 0); I32Const tag; I32Eq;
     If (ValType I32, load_fields @ inner, Some on_fail)]

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

and compile_match ctx scrut_local arms =
  match arms with
  | [] ->
    [I32Const 0]
  | arm :: rest ->
    let on_fail = compile_match ctx scrut_local rest in
    compile_pat ctx scrut_local arm.pattern
      (fun ctx' -> compile_eexpr ctx' arm.arm_body)
      on_fail

and compile_eexpr ctx e =
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

  (* General function call via Call instruction *)
  | EApp ({ edesc = EVar name; _ }, args) ->
    (match List.assoc_opt name ctx.func_map with
     | Some idx ->
       List.concat_map (compile_eexpr ctx) args @ [Call idx]
     | None ->
       failwith ("Codegen: unknown function: " ^ name))

  | ELet { pat = { Ast.pat_desc = Ast.PVar x; _ }; value; body } ->
    let idx        = alloc_local ctx in
    let value_code = compile_eexpr ctx value in
    let body_code  = compile_eexpr { ctx with env = (x, idx) :: ctx.env } body in
    value_code @ [LocalSet idx] @ body_code

  | EIf { cond; then_; else_ } ->
    compile_eexpr ctx cond @
    [If (ValType I32,
         compile_eexpr ctx then_,
         Some (compile_eexpr ctx else_))]

  | EMatch { scrutinee; arms } ->
    let scrut_local = alloc_local ctx in
    compile_eexpr ctx scrutinee
    @ [LocalSet scrut_local]
    @ compile_match ctx scrut_local arms

  | EDo stmts ->
    compile_do ctx stmts

  | ELetrec (bindings, body) ->
    compile_eletrec ctx bindings body

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
    (* Push args, then load fn-table index (call_indirect pops index last) *)
    List.concat_map (compile_eexpr ctx) args
    @ [LocalGet ev_local; I32Load (2, op_idx * 4)]
    @ [CallIndirect (type_idx, 0)]

  | EHandle { handled; handlers } ->
    compile_ehandle ctx handled handlers

  | other ->
    failwith (Format.asprintf "Codegen: unsupported eexpr: %a" pp_eexpr { edesc = other })

(* Sequential do-block: all but the last statement drop their value. *)
and compile_do ctx stmts =
  match stmts with
  | [] -> failwith "Codegen: empty do block"
  | [EStmtExpr e] ->
    compile_eexpr ctx e
  | EStmtLet { pat = { Ast.pat_desc = Ast.PVar x; _ }; value } :: rest ->
    let idx = alloc_local ctx in
    compile_eexpr ctx value
    @ [LocalSet idx]
    @ compile_do { ctx with env = (x, idx) :: ctx.env } rest
  | EStmtExpr e :: rest ->
    compile_eexpr ctx e @ [Drop] @ compile_do ctx rest
  | _ ->
    failwith "Codegen: unsupported do statement"

(* Letrec: pre-register all bindings as module functions so mutual and
   self references work, then compile each body and the continuation. *)
and compile_eletrec ctx bindings body =
  let entries = List.map (fun (b : eletrec_binding) ->
    (* ev_params are always [] for letrec bindings (surface AST limitation) *)
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
      make_ctx param_tys new_func_map ctx.ctor_map ctx.alloc_idx
        ctx.module_ ctx.pending ctx.effect_map ctx.table_funcs
    in
    let instrs = compile_eexpr fn_ctx b.letrec_body in
    ctx.pending := !(ctx.pending) @ [(idx, !(fn_ctx.extra_locals), instrs)]
  ) entries;
  compile_eexpr { ctx with func_map = new_func_map } body

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
      (* Compile each op handler as a module-level function. *)
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
          make_ctx param_pairs ctx.func_map ctx.ctor_map
            ctx.alloc_idx ctx.module_ ctx.pending
            ctx.effect_map ctx.table_funcs
        in
        let body = compile_eexpr handler_ctx oh.op_handler_body in
        ctx.pending :=
          !(ctx.pending) @ [(fn_idx, !(handler_ctx.extra_locals), body)];
        (op_idx, table_slot)
      ) h.op_handlers in
      (* At runtime: alloc n_ops*4 bytes and write each table slot index. *)
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
  (* Apply each handler's return clause (if present), left-to-right.
     Each clause transforms the result of the handled expression. *)
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

let emit (prog : Ast.program) : bytes =
  let eprog = elaborate_program prog in
  let m       = create () in
  (* Linear memory: one 64 KiB page; no upper bound. *)
  add_memory m { min = 1; max = None };
  add_export m "memory" (ExportMem 0);
  (* Mutable heap pointer; bump-allocator starts above a 1 KiB reserved zone. *)
  let heap_ptr_idx = add_global m I32 true (GlobI32 1024) in
  (* __alloc(size: i32) -> i32 *)
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
  (* Collect effect declarations for op-index and type-signature lookup. *)
  let effect_map =
    List.filter_map (fun (d : Ast.decl) ->
      match d.Ast.decl_desc with
      | Ast.DeclEffect { effect_name; ops; _ } -> Some (effect_name, ops)
      | _ -> None
    ) prog
  in
  (* Build constructor map: ctor_name -> (tag, arity). *)
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
  (* Collect top-level elaborated function declarations with supported types. *)
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
  (* Pass 1: pre-register all functions; builds the func_map.
     Evidence params become leading i32 parameters in the WASM type. *)
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
  (* Pass 2: compile each function body. *)
  List.iter (fun (_, idx, param_tys, decl_body) ->
    let ctx =
      make_ctx param_tys func_map ctor_map alloc_idx m pending
        effect_map table_funcs
    in
    let instrs = compile_eexpr ctx decl_body in
    pending := !pending @ [(idx, !(ctx.extra_locals), instrs)]
  ) fn_entries;
  (* Emit a stub main returning 0 when none was declared. *)
  if not (List.exists (fun (name, _, _, _) -> name = "main") fn_entries) then begin
    let idx = reserve_function m [] [I32] in
    add_export m "main" (ExportFunc idx);
    pending := !pending @ [(idx, [], [I32Const 0])]
  end;
  (* Add a funcref table and element segment if any handler functions
     were compiled during this module. *)
  if !table_funcs <> [] then begin
    let n = List.length !table_funcs in
    add_table m { min = n; max = Some n };
    add_element m 0 !table_funcs
  end;
  (* Flush bodies in function-index order so func and code sections align. *)
  let sorted =
    List.sort (fun (i, _, _) (j, _, _) -> compare i j) !pending
  in
  List.iter (fun (_, locals, instrs) -> add_body m locals instrs) sorted;
  encode m

let emit_stub = emit
