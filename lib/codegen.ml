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
  | Ast.TyName "Int"  -> I32
  | Ast.TyName "Bool" -> I32
  | _ -> failwith "Codegen: unsupported type"

let is_supported_type = function
  | Ast.TyName "Int" | Ast.TyName "Bool" -> true
  | _ -> false

(* ------------------------------------------------------------------ *)
(* Per-function compilation state                                       *)
(* ------------------------------------------------------------------ *)

type ctx = {
  env          : (string * int) list;   (** variable name -> local index *)
  next_local   : int ref;               (** next free local slot *)
  extra_locals : local_decl list ref;   (** additional locals beyond params *)
  func_map     : (string * int) list;   (** function name -> wasm func index *)
  module_      : module_builder;
  (** Accumulator for (func_idx, locals, instrs); sorted and flushed in emit. *)
  pending      : (int * local_decl list * instr list) list ref;
}

let make_ctx params func_map module_ pending =
  { env          = List.mapi (fun i (name, _) -> (name, i)) params
  ; next_local   = ref (List.length params)
  ; extra_locals = ref []
  ; func_map     = func_map
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

let primitives = ["add"; "sub"; "mul"; "eq"; "lt"; "gt"]

let instr_of_prim = function
  | "add" -> I32Add
  | "sub" -> I32Sub
  | "mul" -> I32Mul
  | "eq"  -> I32Eq
  | "lt"  -> I32LtS
  | "gt"  -> I32GtS
  | _     -> assert false

(* ------------------------------------------------------------------ *)
(* Expression compiler                                                  *)
(* ------------------------------------------------------------------ *)

let rec compile_expr ctx expr =
  match expr.Ast.desc with
  | Ast.IntLit n ->
    [I32Const (Int64.to_int n)]

  | Ast.Var x ->
    (match List.assoc_opt x ctx.env with
     | Some idx -> [LocalGet idx]
     | None     -> failwith ("Codegen: unbound variable: " ^ x))

  (* Known binary primitives — matched before the general App case *)
  | Ast.App ({ Ast.desc = Ast.Var op; _ }, [a; b])
    when List.mem op primitives ->
    compile_expr ctx a @ compile_expr ctx b @ [instr_of_prim op]

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
    let fn_ctx = make_ctx param_tys new_func_map ctx.module_ ctx.pending in
    let instrs  = compile_expr fn_ctx b.Ast.letrec_body in
    ctx.pending := !(ctx.pending) @ [(idx, !(fn_ctx.extra_locals), instrs)]
  ) entries;
  compile_expr { ctx with func_map = new_func_map } body

(* ------------------------------------------------------------------ *)
(* Module emitter                                                       *)
(* ------------------------------------------------------------------ *)

let emit (prog : Ast.program) : bytes =
  let m       = create () in
  let pending = ref [] in
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
    let ctx    = make_ctx param_tys func_map m pending in
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
