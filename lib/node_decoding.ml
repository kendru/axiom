(** Binary IR node decoder.

    Decodes node payloads back into AST values. This is the inverse of
    {!Node_encoding}. The decoder takes a hash-to-payload lookup function
    and reconstructs the AST by recursively resolving child hashes.

    All multi-byte integers are little-endian. *)

open Ast
open Node_tag

(* ================================================================== *)
(* Cursor: a mutable read position over a bytes buffer                 *)
(* ================================================================== *)

type cursor = {
  data : bytes;
  mutable pos : int;
}

let make_cursor data = { data; pos = 0 }

let ensure cur n =
  if cur.pos + n > Bytes.length cur.data then
    failwith (Printf.sprintf "Node_decoding: unexpected end of data at offset %d (need %d bytes, have %d)"
      cur.pos n (Bytes.length cur.data))

(* ================================================================== *)
(* Primitive readers — little-endian                                    *)
(* ================================================================== *)

let get_u8 cur =
  ensure cur 1;
  let v = Char.code (Bytes.get cur.data cur.pos) in
  cur.pos <- cur.pos + 1;
  v

let get_u16 cur =
  ensure cur 2;
  let lo = Char.code (Bytes.get cur.data cur.pos) in
  let hi = Char.code (Bytes.get cur.data (cur.pos + 1)) in
  cur.pos <- cur.pos + 2;
  lo lor (hi lsl 8)

let get_u32 cur =
  ensure cur 4;
  let b0 = Char.code (Bytes.get cur.data cur.pos) in
  let b1 = Char.code (Bytes.get cur.data (cur.pos + 1)) in
  let b2 = Char.code (Bytes.get cur.data (cur.pos + 2)) in
  let b3 = Char.code (Bytes.get cur.data (cur.pos + 3)) in
  cur.pos <- cur.pos + 4;
  b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24)

let get_i64 cur =
  ensure cur 8;
  let v = ref 0L in
  for i = 0 to 7 do
    let byte = Char.code (Bytes.get cur.data (cur.pos + i)) in
    v := Int64.logor !v (Int64.shift_left (Int64.of_int byte) (i * 8))
  done;
  cur.pos <- cur.pos + 8;
  !v

let get_f64 cur =
  let bits = get_i64 cur in
  Int64.float_of_bits bits

let get_bool cur =
  let v = get_u8 cur in
  v <> 0

let get_str cur =
  let len = get_u16 cur in
  ensure cur len;
  let s = Bytes.sub_string cur.data cur.pos len in
  cur.pos <- cur.pos + len;
  s

let get_lstr cur =
  let len = get_u32 cur in
  ensure cur len;
  let s = Bytes.sub_string cur.data cur.pos len in
  cur.pos <- cur.pos + len;
  s

let get_opt cur f =
  let present = get_bool cur in
  if present then Some (f cur) else None

let get_list cur f =
  let n = get_u16 cur in
  List.init n (fun _ -> f cur)

let get_hash cur =
  let n = Node_hash.hash_size in
  ensure cur n;
  let h = Bytes.sub cur.data cur.pos n in
  cur.pos <- cur.pos + n;
  h

(* ================================================================== *)
(* Inline sub-structure decoders                                       *)
(* ================================================================== *)

let rec get_type_expr cur =
  let tag = get_u8 cur in
  match tag with
  | t when t = ttag_name ->
    TyName (get_str cur)
  | t when t = ttag_app ->
    let name = get_str cur in
    let args = get_list cur get_type_expr in
    TyApp (name, args)
  | t when t = ttag_tuple ->
    let elems = get_list cur get_type_expr in
    TyTuple elems
  | t when t = ttag_fun ->
    let params = get_list cur get_type_expr in
    let ret = get_type_expr cur in
    let eff = get_opt cur get_effect_set in
    TyFun (params, ret, eff)
  | _ -> failwith (Printf.sprintf "Node_decoding: unknown type_expr tag 0x%02x" tag)

and get_effect_set cur =
  let tag = get_u8 cur in
  match tag with
  | t when t = etag_pure -> Pure
  | t when t = etag_effects ->
    let tys = get_list cur get_type_expr in
    Effects tys
  | _ -> failwith (Printf.sprintf "Node_decoding: unknown effect_set tag 0x%02x" tag)

let get_param cur =
  let name = get_str cur in
  let ty = get_type_expr cur in
  { param_name = name; param_type = ty }

let get_comment cur =
  get_opt cur get_lstr

let rec get_pattern cur =
  let tag = get_u8 cur in
  let pat_desc = match tag with
    | t when t = ptag_wild       -> PWild
    | t when t = ptag_var        -> PVar (get_str cur)
    | t when t = ptag_lit_int    -> PLitInt (get_i64 cur)
    | t when t = ptag_lit_float  -> PLitFloat (get_f64 cur)
    | t when t = ptag_lit_string -> PLitString (get_lstr cur)
    | t when t = ptag_lit_true   -> PLitTrue
    | t when t = ptag_lit_false  -> PLitFalse
    | t when t = ptag_lit_unit   -> PLitUnit
    | t when t = ptag_ctor ->
      let name = get_str cur in
      let pats = get_list cur get_pattern in
      PCtor (name, pats)
    | t when t = ptag_record ->
      let is_open = get_bool cur in
      let fields = get_list cur (fun c ->
        let name = get_str c in
        let p = get_pattern c in
        (name, p)) in
      PRecord (fields, is_open)
    | t when t = ptag_or ->
      let left = get_pattern cur in
      let right = get_pattern cur in
      POr (left, right)
    | _ -> failwith (Printf.sprintf "Node_decoding: unknown pattern tag 0x%02x" tag)
  in
  let pat_comment = get_comment cur in
  { pat_desc; pat_comment }

(* ================================================================== *)
(* Payload lookup type                                                 *)
(* ================================================================== *)

(** A lookup function resolves a hash to its stored payload bytes.
    Raises [Not_found] if the hash is not in the store. *)
type lookup = bytes -> bytes

(** Build a lookup from a mem_store hash table. *)
let lookup_of_hashtbl tbl : lookup = fun hash ->
  Hashtbl.find tbl hash

(* ================================================================== *)
(* Node payload decoder                                                *)
(* ================================================================== *)

(** Parse the header of a payload, returning (tag, children hashes, inline cursor). *)
let parse_header payload =
  let cur = make_cursor payload in
  let tag = get_u8 cur in
  let n_children = get_u16 cur in
  let _len_inline = get_u32 cur in
  let children = List.init n_children (fun _ -> get_hash cur) in
  (tag, children, cur)

(* ================================================================== *)
(* Expression decoder                                                  *)
(* ================================================================== *)

let rec decode_expr lookup hash : expr =
  let payload = lookup hash in
  let (tag, children, cur) = parse_header payload in
  let child i = List.nth children i in
  let desc, comment = match tag with
    | t when t = tag_var ->
      let name = get_str cur in
      let comment = get_comment cur in
      (Var name, comment)

    | t when t = tag_int_lit ->
      let n = get_i64 cur in
      let comment = get_comment cur in
      (IntLit n, comment)

    | t when t = tag_float_lit ->
      let f = get_f64 cur in
      let comment = get_comment cur in
      (FloatLit f, comment)

    | t when t = tag_string_lit ->
      let s = get_lstr cur in
      let comment = get_comment cur in
      (StringLit s, comment)

    | t when t = tag_bool_true ->
      let comment = get_comment cur in
      (BoolLit true, comment)

    | t when t = tag_bool_false ->
      let comment = get_comment cur in
      (BoolLit false, comment)

    | t when t = tag_unit_lit ->
      let comment = get_comment cur in
      (UnitLit, comment)

    | t when t = tag_let ->
      let pat = get_pattern cur in
      let comment = get_comment cur in
      let value = decode_expr lookup (child 0) in
      let body  = decode_expr lookup (child 1) in
      (Let { pat; value; body }, comment)

    | t when t = tag_app ->
      let comment = get_comment cur in
      let fn_expr = decode_expr lookup (child 0) in
      let arg_exprs = List.init (List.length children - 1) (fun i ->
        decode_expr lookup (child (i + 1))) in
      (App (fn_expr, arg_exprs), comment)

    | t when t = tag_fn ->
      let params = get_list cur get_param in
      let return_type = get_opt cur get_type_expr in
      let effects = get_opt cur get_effect_set in
      let comment = get_comment cur in
      let fn_body = decode_expr lookup (child 0) in
      (Fn { params; return_type; effects; fn_body }, comment)

    | t when t = tag_match ->
      let arm_pats = get_list cur get_pattern in
      let comment = get_comment cur in
      let scrutinee = decode_expr lookup (child 0) in
      let arms = List.mapi (fun i pat ->
        let arm_body = decode_expr lookup (child (i + 1)) in
        { pattern = pat; arm_body }
      ) arm_pats in
      (Match { scrutinee; arms }, comment)

    | t when t = tag_if ->
      let comment = get_comment cur in
      let cond  = decode_expr lookup (child 0) in
      let then_ = decode_expr lookup (child 1) in
      let else_ = decode_expr lookup (child 2) in
      (If { cond; then_; else_ }, comment)

    | t when t = tag_do ->
      let n_stmts = get_u16 cur in
      let stmt_infos = List.init n_stmts (fun _ ->
        let stag = get_u8 cur in
        if stag = stmt_tag_let then
          `Let (get_pattern cur)
        else
          `Expr
      ) in
      let comment = get_comment cur in
      let stmts = List.mapi (fun i info ->
        let child_expr = decode_expr lookup (child i) in
        match info with
        | `Expr  -> StmtExpr child_expr
        | `Let p -> StmtLet { pat = p; value = child_expr }
      ) stmt_infos in
      (Do stmts, comment)

    | t when t = tag_letrec ->
      let n_bindings = get_u16 cur in
      let binding_infos = List.init n_bindings (fun _ ->
        let name = get_str cur in
        let params = get_list cur get_param in
        let ret_type = get_type_expr cur in
        (name, params, ret_type)
      ) in
      let comment = get_comment cur in
      let outer_body = decode_expr lookup (child 0) in
      let bindings = List.mapi (fun i (name, params, ret_type) ->
        let body = decode_expr lookup (child (i + 1)) in
        { letrec_name = name
        ; letrec_params = params
        ; letrec_return_type = ret_type
        ; letrec_body = body }
      ) binding_infos in
      (Letrec (bindings, outer_body), comment)

    | t when t = tag_record ->
      let field_names = get_list cur get_str in
      let comment = get_comment cur in
      let fields = List.mapi (fun i name ->
        (name, decode_expr lookup (child i))
      ) field_names in
      (Record fields, comment)

    | t when t = tag_record_update ->
      let field_names = get_list cur get_str in
      let comment = get_comment cur in
      let base = decode_expr lookup (child 0) in
      let fields = List.mapi (fun i name ->
        (name, decode_expr lookup (child (i + 1)))
      ) field_names in
      (RecordUpdate (base, fields), comment)

    | t when t = tag_project ->
      let field = get_str cur in
      let comment = get_comment cur in
      let record_expr = decode_expr lookup (child 0) in
      (Project (record_expr, field), comment)

    | t when t = tag_perform ->
      let effect_name = get_str cur in
      let op_name = get_str cur in
      let comment = get_comment cur in
      let args = List.init (List.length children) (fun i ->
        decode_expr lookup (child i)) in
      (Perform { effect_name; op_name; args }, comment)

    | t when t = tag_handle ->
      let n_handlers = get_u16 cur in
      (* Parse inline handler metadata and count children consumed *)
      let handler_infos = List.init n_handlers (fun _ ->
        let eff_name = get_str cur in
        let n_ops = get_u16 cur in
        let ops = List.init n_ops (fun _ ->
          let op_name = get_str cur in
          let param_names = get_list cur get_str in
          (op_name, param_names)
        ) in
        let has_return = get_bool cur in
        let return_var = if has_return then Some (get_str cur) else None in
        (eff_name, ops, return_var)
      ) in
      let comment = get_comment cur in
      let handled = decode_expr lookup (child 0) in
      (* Reconstruct handlers by consuming children in order *)
      let child_idx = ref 1 in
      let handlers = List.map (fun (eff_name, ops, return_var) ->
        let op_handlers = List.map (fun (op_name, param_names) ->
          let body = decode_expr lookup (child !child_idx) in
          incr child_idx;
          { op_handler_name = op_name
          ; op_handler_params = param_names
          ; op_handler_body = body }
        ) ops in
        let return_handler = match return_var with
          | None -> None
          | Some rv ->
            let body = decode_expr lookup (child !child_idx) in
            incr child_idx;
            Some { return_var = rv; return_body = body }
        in
        { effect_handler = eff_name
        ; op_handlers
        ; return_handler }
      ) handler_infos in
      (Handle { handled; handlers }, comment)

    | _ -> failwith (Printf.sprintf "Node_decoding: unknown expression tag 0x%02x" tag)
  in
  { desc; comment }

(* ================================================================== *)
(* Declaration decoder                                                 *)
(* ================================================================== *)

let rec decode_decl lookup hash : decl =
  let payload = lookup hash in
  let (tag, children, cur) = parse_header payload in
  let child i = List.nth children i in
  let decl_desc, decl_comment = match tag with
    | t when t = tag_decl_fn ->
      let pub = get_bool cur in
      let fn_name = get_str cur in
      let type_params = get_list cur get_str in
      let params = get_list cur get_param in
      let return_type = get_opt cur get_type_expr in
      let effects = get_opt cur get_effect_set in
      let decl_comment = get_comment cur in
      let decl_body = decode_expr lookup (child 0) in
      (DeclFn { pub; fn_name; type_params; params; return_type; effects; decl_body },
       decl_comment)

    | t when t = tag_decl_type ->
      let pub = get_bool cur in
      let type_name = get_str cur in
      let type_params = get_list cur get_str in
      let n_ctors = get_u16 cur in
      let ctors = List.init n_ctors (fun _ ->
        let ctor_name = get_str cur in
        let ctor_params = get_list cur get_type_expr in
        { ctor_name; ctor_params }
      ) in
      let decl_comment = get_comment cur in
      (DeclType { pub; type_name; type_params; ctors }, decl_comment)

    | t when t = tag_decl_effect ->
      let pub = get_bool cur in
      let effect_name = get_str cur in
      let type_params = get_list cur get_str in
      let n_ops = get_u16 cur in
      let ops = List.init n_ops (fun _ ->
        let effect_op_name = get_str cur in
        let effect_op_params = get_list cur get_type_expr in
        let effect_op_return = get_type_expr cur in
        { effect_op_name; effect_op_params; effect_op_return }
      ) in
      let decl_comment = get_comment cur in
      (DeclEffect { pub; effect_name; type_params; ops }, decl_comment)

    | t when t = tag_decl_module ->
      let pub = get_bool cur in
      let module_name = get_str cur in
      let decl_comment = get_comment cur in
      let body = List.map (decode_decl lookup) children in
      (DeclModule { pub; module_name; body }, decl_comment)

    | t when t = tag_decl_require ->
      let ty = get_type_expr cur in
      let decl_comment = get_comment cur in
      (DeclRequire ty, decl_comment)

    | _ -> failwith (Printf.sprintf "Node_decoding: unknown declaration tag 0x%02x" tag)
  in
  { decl_desc; decl_comment }

(* ================================================================== *)
(* Program decoder                                                     *)
(* ================================================================== *)

let decode_program lookup hash : program =
  let payload = lookup hash in
  let (_tag, children, _cur) = parse_header payload in
  List.map (decode_decl lookup) children
