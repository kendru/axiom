(** Code generator: translates type-checked Axiom programs to WASM binary.

    Issue #35 scope: top-level function definitions with direct and mutually
    recursive calls.  A two-pass approach pre-registers all function
    signatures before compiling any body so recursive and forward references
    resolve correctly.  Letrec expressions are lowered to additional module
    functions using the same mechanism.

    Milestone 1 type support: Int and Bool map to i32; all others are
    skipped at the top level so that earlier tests remain unaffected. *)

open Wasm_encode

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
  (** Accumulator for (func_idx, locals, instrs); sorted and flushed in emit. *)
  pending      : (int * local_decl list * instr list) list ref;
}

let make_ctx params func_map ctor_map alloc_idx module_ pending =
  { env          = List.mapi (fun i (name, _) -> (name, i)) params
  ; next_local   = ref (List.length params)
  ; extra_locals = ref []
  ; func_map     = func_map
  ; ctor_map     = ctor_map
  ; alloc_idx    = alloc_idx
  ; module_      = module_
  ; pending      = pending
  }

let alloc_local ctx =
  let idx = !(ctx.next_local) in
  incr ctx.next_local;
  ctx.extra_locals := !(ctx.extra_locals) @ [{ count = 1; ty = I32 }];
  idx

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
(* Expression compiler                                                  *)
(* ------------------------------------------------------------------ *)

(* Allocate an ADT value: tag at offset 0, fields at offsets 4, 8, ...
   Returns pointer via the last LocalGet. *)
let rec compile_ctor ctx tag args =
  let arity = List.length args in
  let ptr_local = alloc_local ctx in
  [I32Const ((1 + arity) * 4); Call ctx.alloc_idx; LocalTee ptr_local;
   I32Const tag; I32Store (2, 0)]
  @ List.concat (List.mapi (fun i arg ->
      [LocalGet ptr_local] @ compile_expr ctx arg @ [I32Store (2, (i + 1) * 4)]
    ) args)
  @ [LocalGet ptr_local]

(* Compile a single pattern against scrut_local.
   on_match ctx' = instructions to emit when the pattern matches (ctx' has new bindings).
   on_fail       = instruction list to emit when the pattern does not match. *)
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
    (* Allocate a local for each constructor field up front. *)
    let field_locals = List.mapi (fun i _ -> (i, alloc_local ctx)) sub_pats in
    (* Load each field from heap slot i+1 into its local. *)
    let load_fields = List.concat_map (fun (i, li) ->
      [LocalGet scrut_local; I32Load (2, (i + 1) * 4); LocalSet li]
    ) field_locals in
    (* Chain sub-pattern checks for each field. *)
    let pairs = List.map2 (fun (_, li) p -> (li, p)) field_locals sub_pats in
    let inner = chain_pats ctx pairs on_match on_fail in
    [LocalGet scrut_local; I32Load (2, 0); I32Const tag; I32Eq;
     If (ValType I32, load_fields @ inner, Some on_fail)]

  | Ast.POr (p1, p2) ->
    (* Try p1; on failure try p2. *)
    let try_p2 = compile_pat ctx scrut_local p2 on_match on_fail in
    compile_pat ctx scrut_local p1 on_match try_p2

  | other ->
    failwith (Format.asprintf "Codegen: unsupported pattern: %a"
                Ast.pp_pattern_desc other)

(* Chain a list of (field_local, sub_pattern) pairs for constructor field checks. *)
and chain_pats ctx pairs on_match on_fail =
  match pairs with
  | [] -> on_match ctx
  | (li, pat) :: rest ->
    compile_pat ctx li pat
      (fun ctx' -> chain_pats ctx' rest on_match on_fail)
      on_fail

(* Decision-tree dispatch for match arms. scrut_local holds the scrutinee value. *)
and compile_match ctx scrut_local arms =
  match arms with
  | [] ->
    [I32Const 0]  (* exhaustive matches should not reach here *)
  | arm :: rest ->
    let on_fail = compile_match ctx scrut_local rest in
    compile_pat ctx scrut_local arm.Ast.pattern
      (fun ctx' -> compile_expr ctx' arm.Ast.arm_body)
      on_fail

and compile_expr ctx expr =
  match expr.Ast.desc with
  | Ast.IntLit n ->
    [I32Const (Int64.to_int n)]

  | Ast.BoolLit b ->
    [I32Const (if b then 1 else 0)]

  (* Nullary constructor used as a value *)
  | Ast.Var name when List.mem_assoc name ctx.ctor_map ->
    let (tag, arity) = List.assoc name ctx.ctor_map in
    if arity <> 0 then
      failwith ("Codegen: constructor " ^ name ^ " requires arguments");
    compile_ctor ctx tag []

  | Ast.Var x ->
    (match List.assoc_opt x ctx.env with
     | Some idx -> [LocalGet idx]
     | None     -> failwith ("Codegen: unbound variable: " ^ x))

  (* Unary negation: neg(x) = 0 - x *)
  | Ast.App ({ Ast.desc = Ast.Var "neg"; _ }, [a]) ->
    [I32Const 0] @ compile_expr ctx a @ [I32Sub]

  (* Known binary primitives — matched before the general App case *)
  | Ast.App ({ Ast.desc = Ast.Var op; _ }, [a; b])
    when List.mem op primitives ->
    compile_expr ctx a @ compile_expr ctx b @ [instr_of_prim op]

  (* Constructor application *)
  | Ast.App ({ Ast.desc = Ast.Var name; _ }, args)
    when List.mem_assoc name ctx.ctor_map ->
    let (tag, _) = List.assoc name ctx.ctor_map in
    compile_ctor ctx tag args

  (* General function call via Call instruction *)
  | Ast.App ({ Ast.desc = Ast.Var name; _ }, args) ->
    (match List.assoc_opt name ctx.func_map with
     | Some idx ->
       List.concat_map (compile_expr ctx) args @ [Call idx]
     | None ->
       failwith ("Codegen: unknown function: " ^ name))

  | Ast.Let { pat = { Ast.pat_desc = Ast.PVar x; _ }; value; body } ->
    let idx        = alloc_local ctx in
    let value_code = compile_expr ctx value in
    let body_code  = compile_expr { ctx with env = (x, idx) :: ctx.env } body in
    value_code @ [LocalSet idx] @ body_code

  | Ast.If { cond; then_; else_ } ->
    compile_expr ctx cond @
    [If (ValType I32,
         compile_expr ctx then_,
         Some (compile_expr ctx else_))]

  | Ast.Match { scrutinee; arms } ->
    let scrut_local = alloc_local ctx in
    compile_expr ctx scrutinee
    @ [LocalSet scrut_local]
    @ compile_match ctx scrut_local arms

  | Ast.Do stmts ->
    compile_do ctx stmts

  | Ast.Letrec (bindings, body) ->
    compile_letrec ctx bindings body

  | other ->
    failwith (Format.asprintf "Codegen: unsupported expression: %a"
                Ast.pp_expr_desc other)

(* Sequential do-block: all but the last statement drop their value. *)
and compile_do ctx stmts =
  match stmts with
  | [] -> failwith "Codegen: empty do block"
  | [Ast.StmtExpr e] ->
    compile_expr ctx e
  | Ast.StmtLet { pat = { Ast.pat_desc = Ast.PVar x; _ }; value } :: rest ->
    let idx = alloc_local ctx in
    compile_expr ctx value
    @ [LocalSet idx]
    @ compile_do { ctx with env = (x, idx) :: ctx.env } rest
  | Ast.StmtExpr e :: rest ->
    compile_expr ctx e @ [Drop] @ compile_do ctx rest
  | _ ->
    failwith "Codegen: unsupported do statement"

(* Letrec: pre-register all bindings as module functions so mutual and
   self references work, then compile each body and the continuation. *)
and compile_letrec ctx bindings body =
  let entries = List.map (fun b ->
    let param_tys = List.map (fun p ->
      (p.Ast.param_name, val_type_of_type_expr p.Ast.param_type)
    ) b.Ast.letrec_params in
    let ret_ty = val_type_of_type_expr b.Ast.letrec_return_type in
    let idx = reserve_function ctx.module_ (List.map snd param_tys) [ret_ty] in
    (b, idx, param_tys)
  ) bindings in
  let new_func_map =
    List.map (fun (b, idx, _) -> (b.Ast.letrec_name, idx)) entries
    @ ctx.func_map
  in
  List.iter (fun (b, idx, param_tys) ->
    let fn_ctx =
      make_ctx param_tys new_func_map ctx.ctor_map ctx.alloc_idx
        ctx.module_ ctx.pending
    in
    let instrs  = compile_expr fn_ctx b.Ast.letrec_body in
    ctx.pending := !(ctx.pending) @ [(idx, !(fn_ctx.extra_locals), instrs)]
  ) entries;
  compile_expr { ctx with func_map = new_func_map } body

(* ------------------------------------------------------------------ *)
(* Module emitter                                                       *)
(* ------------------------------------------------------------------ *)

let emit (prog : Ast.program) : bytes =
  let m       = create () in
  (* Linear memory: one 64 KiB page; no upper bound. *)
  add_memory m { min = 1; max = None };
  add_export m "memory" (ExportMem 0);
  (* Mutable heap pointer; bump-allocator starts above a 1 KiB reserved zone. *)
  let heap_ptr_idx = add_global m I32 true (GlobI32 1024) in
  (* __alloc(size: i32) -> i32 — bumps __heap_ptr by size, returns old value.
     local[0]=size (param), local[1]=saved old ptr (extra). *)
  let alloc_idx = add_function m ~export:"__alloc"
    [I32] [I32]
    [{ count = 1; ty = I32 }]
    [ GlobalGet heap_ptr_idx   (* push old __heap_ptr *)
    ; LocalTee 1               (* save it; still on stack *)
    ; LocalGet 0               (* push size *)
    ; I32Add                   (* old + size *)
    ; GlobalSet heap_ptr_idx   (* __heap_ptr = old + size *)
    ; LocalGet 1               (* return old __heap_ptr *)
    ]
  in
  let pending = ref [] in
  (* Build constructor map: ctor_name -> (tag, arity).
     Each DeclType assigns consecutive tags starting at 0 to its constructors. *)
  let ctor_map =
    List.concat_map (fun d ->
      match d.Ast.decl_desc with
      | Ast.DeclType { ctors; _ } ->
        List.mapi (fun tag c ->
          (c.Ast.ctor_name, (tag, List.length c.Ast.ctor_params))
        ) ctors
      | _ -> []
    ) prog
  in
  (* Collect top-level function declarations whose types we can handle,
     extracting the fields we need as a plain tuple to avoid inline-record
     binding restrictions. *)
  let fn_decls =
    List.filter_map (fun d ->
      match d.Ast.decl_desc with
      | Ast.DeclFn { fn_name; pub; params; return_type = Some ret; decl_body; _ }
        when List.for_all (fun p -> is_supported_type p.Ast.param_type) params
          && is_supported_type ret ->
        Some (fn_name, pub, params, ret, decl_body)
      | _ -> None
    ) prog
  in
  (* Pass 1: pre-register all functions; builds the func_map. *)
  let fn_entries =
    List.map (fun (fn_name, pub, params, ret, decl_body) ->
      let param_tys = List.map (fun p ->
        (p.Ast.param_name, val_type_of_type_expr p.Ast.param_type)
      ) params in
      let ret_ty = val_type_of_type_expr ret in
      let idx = reserve_function m (List.map snd param_tys) [ret_ty] in
      if pub || fn_name = "main" then
        add_export m fn_name (ExportFunc idx);
      (fn_name, idx, param_tys, decl_body)
    ) fn_decls
  in
  let func_map = List.map (fun (fn_name, idx, _, _) -> (fn_name, idx)) fn_entries in
  (* Pass 2: compile each function body, accumulating into pending. *)
  List.iter (fun (_, idx, param_tys, decl_body) ->
    let ctx    = make_ctx param_tys func_map ctor_map alloc_idx m pending in
    let instrs = compile_expr ctx decl_body in
    pending := !pending @ [(idx, !(ctx.extra_locals), instrs)]
  ) fn_entries;
  (* Emit a stub main returning 0 when none was declared. *)
  if not (List.exists (fun (name, _, _, _) -> name = "main") fn_entries) then begin
    let idx = reserve_function m [] [I32] in
    add_export m "main" (ExportFunc idx);
    pending := !pending @ [(idx, [], [I32Const 0])]
  end;
  (* Flush bodies in function-index order so func and code sections align. *)
  let sorted = List.sort (fun (i, _, _) (j, _, _) -> compare i j) !pending in
  List.iter (fun (_, locals, instrs) -> add_body m locals instrs) sorted;
  encode m

let emit_stub = emit
