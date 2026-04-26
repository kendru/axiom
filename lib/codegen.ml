(** Code generator: translates type-checked Axiom programs to WASM binary.

    Milestone 1 scope: Int literals (i32.const), arithmetic primitives
    add/sub/mul (i32.add/sub/mul), and let-bindings (fresh locals). The
    special function [main : () -> Int ! pure] is exported as the module
    entry point.  All other top-level declarations are ignored for now. *)

open Wasm_encode

(* ------------------------------------------------------------------ *)
(* Per-function compilation state                                       *)
(* ------------------------------------------------------------------ *)

type ctx = {
  env          : (string * int) list;   (** variable name -> local index *)
  next_local   : int ref;               (** next free local slot *)
  extra_locals : local_decl list ref;   (** additional locals beyond params *)
}

let make_ctx params =
  { env          = List.mapi (fun i (name, _) -> (name, i)) params
  ; next_local   = ref (List.length params)
  ; extra_locals = ref []
  }

(** Allocate a fresh i32 local; returns its index. *)
let alloc_local ctx =
  let idx = !(ctx.next_local) in
  incr ctx.next_local;
  ctx.extra_locals := !(ctx.extra_locals) @ [{ count = 1; ty = I32 }];
  idx

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

  | Ast.App ({ Ast.desc = Ast.Var op; _ }, [a; b]) ->
    let binop = match op with
      | "add" -> I32Add
      | "sub" -> I32Sub
      | "mul" -> I32Mul
      | other -> failwith ("Codegen: unknown primitive: " ^ other)
    in
    compile_expr ctx a @ compile_expr ctx b @ [binop]

  | Ast.Let { pat = { Ast.pat_desc = Ast.PVar x; _ }; value; body } ->
    let idx         = alloc_local ctx in
    let value_code  = compile_expr ctx value in
    let body_code   = compile_expr { ctx with env = (x, idx) :: ctx.env } body in
    value_code @ [LocalSet idx] @ body_code

  | other ->
    failwith (Format.asprintf "Codegen: unsupported expression: %a"
                Ast.pp_expr_desc other)

(* ------------------------------------------------------------------ *)
(* Function compiler                                                    *)
(* ------------------------------------------------------------------ *)

let compile_fn (m : module_builder) ~export
    ~(params : (string * val_type) list) (body : Ast.expr) =
  let ctx = make_ctx params in
  let instrs = compile_expr ctx body in
  ignore (add_function m ~export (List.map snd params) [I32]
            !(ctx.extra_locals) instrs)

(* ------------------------------------------------------------------ *)
(* Module emitter                                                       *)
(* ------------------------------------------------------------------ *)

let emit (prog : Ast.program) : bytes =
  let m = create () in
  let main_decl =
    List.find_opt (fun d -> match d.Ast.decl_desc with
      | Ast.DeclFn { fn_name = "main"; _ } -> true
      | _ -> false) prog
  in
  (match main_decl with
   | Some { Ast.decl_desc = Ast.DeclFn { decl_body; _ }; _ } ->
     compile_fn m ~export:"main" ~params:[] decl_body
   | _ ->
     ignore (add_function m ~export:"main" [] [I32] [] [I32Const 0]));
  encode m

let emit_stub = emit
