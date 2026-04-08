(** Binary IR node encoding.

    Encodes AST nodes into the binary payload format specified in
    docs/implementation/node-encoding.md. Each node is encoded as:

      [tag:u8][n_children:u16][len_inline:u32][children:32B each][inline]

    The hash is Blake3(payload) (currently a placeholder; see {!Node_hash}).

    All multi-byte integers are little-endian. *)

open Ast
open Node_tag

(* ================================================================== *)
(* Buffer writer — little-endian primitives                            *)
(* ================================================================== *)

let put_u8 buf v =
  Buffer.add_char buf (Char.chr (v land 0xFF))

let put_u16 buf v =
  Buffer.add_char buf (Char.chr (v land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 8) land 0xFF))

let put_u32 buf v =
  Buffer.add_char buf (Char.chr (v land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 8) land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 16) land 0xFF));
  Buffer.add_char buf (Char.chr ((v lsr 24) land 0xFF))

let put_i64 buf v =
  for i = 0 to 7 do
    let byte = Int64.to_int (Int64.logand (Int64.shift_right_logical v (i * 8)) 0xFFL) in
    Buffer.add_char buf (Char.chr byte)
  done

let put_f64 buf f =
  let bits = Int64.bits_of_float f in
  (* Canonicalize NaN *)
  let bits =
    if Float.is_nan f then 0x7FF8000000000000L
    else bits
  in
  put_i64 buf bits

let put_bool buf b =
  put_u8 buf (if b then 0x01 else 0x00)

let put_str buf s =
  let len = String.length s in
  if len > 0xFFFF then failwith "Node_encoding: str too long (max 65535 bytes)";
  put_u16 buf len;
  Buffer.add_string buf s

let put_lstr buf s =
  let len = String.length s in
  put_u32 buf len;
  Buffer.add_string buf s

let put_opt buf f = function
  | None   -> put_bool buf false
  | Some x -> put_bool buf true; f buf x

let put_list buf f xs =
  let n = List.length xs in
  if n > 0xFFFF then failwith "Node_encoding: list too long (max 65535 items)";
  put_u16 buf n;
  List.iter (f buf) xs

(* ================================================================== *)
(* Inline sub-structure encoders                                       *)
(* ================================================================== *)

let rec put_type_expr buf = function
  | TyName s ->
    put_u8 buf ttag_name;
    put_str buf s
  | TyApp (s, args) ->
    put_u8 buf ttag_app;
    put_str buf s;
    put_list buf put_type_expr args
  | TyTuple elems ->
    put_u8 buf ttag_tuple;
    put_list buf put_type_expr elems
  | TyFun (params, ret, eff) ->
    put_u8 buf ttag_fun;
    put_list buf put_type_expr params;
    put_type_expr buf ret;
    put_opt buf put_effect_set eff

and put_effect_set buf = function
  | Pure ->
    put_u8 buf etag_pure
  | Effects tys ->
    put_u8 buf etag_effects;
    put_list buf put_type_expr tys

let put_param buf (p : param) =
  put_str buf p.param_name;
  put_type_expr buf p.param_type

let put_comment buf (c : string option) =
  put_opt buf put_lstr c

let rec put_pattern buf (p : pattern) =
  (match p.pat_desc with
   | PWild ->
     put_u8 buf ptag_wild
   | PVar s ->
     put_u8 buf ptag_var;
     put_str buf s
   | PLitInt n ->
     put_u8 buf ptag_lit_int;
     put_i64 buf n
   | PLitFloat f ->
     put_u8 buf ptag_lit_float;
     put_f64 buf f
   | PLitString s ->
     put_u8 buf ptag_lit_string;
     put_lstr buf s
   | PLitTrue ->
     put_u8 buf ptag_lit_true
   | PLitFalse ->
     put_u8 buf ptag_lit_false
   | PLitUnit ->
     put_u8 buf ptag_lit_unit
   | PCtor (name, pats) ->
     put_u8 buf ptag_ctor;
     put_str buf name;
     put_list buf put_pattern pats
   | PRecord (fields, is_open) ->
     put_u8 buf ptag_record;
     put_bool buf is_open;
     put_list buf (fun buf (name, p) -> put_str buf name; put_pattern buf p) fields
   | POr (left, right) ->
     put_u8 buf ptag_or;
     put_pattern buf left;
     put_pattern buf right);
  put_comment buf p.pat_comment

(* ================================================================== *)
(* Node store interface                                                *)
(* ================================================================== *)

(** A store records encoded nodes. The encoder calls [store] with the
    payload; the store hashes it, persists it, and returns the 32-byte
    hash. If the node already exists (by hash), it returns the existing
    hash without re-storing. *)
type store = {
  store : bytes -> bytes;
}

let make_mem_store () : store * (bytes, bytes) Hashtbl.t =
  let tbl = Hashtbl.create 256 in
  let store_fn payload =
    let hash = Node_hash.digest payload in
    if not (Hashtbl.mem tbl hash) then
      Hashtbl.add tbl hash (Bytes.copy payload);
    hash
  in
  ({ store = store_fn }, tbl)

(* ================================================================== *)
(* Node payload builder                                                *)
(* ================================================================== *)

(** Build a complete node payload and store it. Returns the 32-byte hash. *)
let build_node store ~tag ~(children : bytes list) ~(write_inline : Buffer.t -> unit) : bytes =
  let n_children = List.length children in
  (* Build inline data first to know its length *)
  let inline_buf = Buffer.create 64 in
  write_inline inline_buf;
  let len_inline = Buffer.length inline_buf in
  (* Build full payload *)
  let payload_buf = Buffer.create (7 + 32 * n_children + len_inline) in
  put_u8 payload_buf tag;
  put_u16 payload_buf n_children;
  put_u32 payload_buf len_inline;
  List.iter (fun h -> Buffer.add_bytes payload_buf h) children;
  Buffer.add_buffer payload_buf inline_buf;
  store.store (Buffer.to_bytes payload_buf)

(* ================================================================== *)
(* Expression encoder                                                  *)
(* ================================================================== *)

let rec encode_expr store (e : expr) : bytes =
  match e.desc with
  | Var name ->
    build_node store ~tag:tag_var ~children:[] ~write_inline:(fun buf ->
      put_str buf name;
      put_comment buf e.comment)

  | IntLit n ->
    build_node store ~tag:tag_int_lit ~children:[] ~write_inline:(fun buf ->
      put_i64 buf n;
      put_comment buf e.comment)

  | FloatLit f ->
    build_node store ~tag:tag_float_lit ~children:[] ~write_inline:(fun buf ->
      put_f64 buf f;
      put_comment buf e.comment)

  | StringLit s ->
    build_node store ~tag:tag_string_lit ~children:[] ~write_inline:(fun buf ->
      put_lstr buf s;
      put_comment buf e.comment)

  | BoolLit true ->
    build_node store ~tag:tag_bool_true ~children:[] ~write_inline:(fun buf ->
      put_comment buf e.comment)

  | BoolLit false ->
    build_node store ~tag:tag_bool_false ~children:[] ~write_inline:(fun buf ->
      put_comment buf e.comment)

  | UnitLit ->
    build_node store ~tag:tag_unit_lit ~children:[] ~write_inline:(fun buf ->
      put_comment buf e.comment)

  | Let { pat; value; body } ->
    let h_value = encode_expr store value in
    let h_body  = encode_expr store body in
    build_node store ~tag:tag_let ~children:[h_value; h_body]
      ~write_inline:(fun buf ->
        put_pattern buf pat;
        put_comment buf e.comment)

  | App (fn_expr, arg_exprs) ->
    let h_fn   = encode_expr store fn_expr in
    let h_args = List.map (encode_expr store) arg_exprs in
    build_node store ~tag:tag_app ~children:(h_fn :: h_args)
      ~write_inline:(fun buf ->
        put_comment buf e.comment)

  | Fn { params; return_type; effects; fn_body } ->
    let h_body = encode_expr store fn_body in
    build_node store ~tag:tag_fn ~children:[h_body]
      ~write_inline:(fun buf ->
        put_list buf put_param params;
        put_opt buf put_type_expr return_type;
        put_opt buf put_effect_set effects;
        put_comment buf e.comment)

  | Match { scrutinee; arms } ->
    let h_scrutinee = encode_expr store scrutinee in
    let h_arm_bodies = List.map (fun (a : match_arm) -> encode_expr store a.arm_body) arms in
    build_node store ~tag:tag_match ~children:(h_scrutinee :: h_arm_bodies)
      ~write_inline:(fun buf ->
        put_list buf (fun buf (a : match_arm) -> put_pattern buf a.pattern) arms;
        put_comment buf e.comment)

  | If { cond; then_; else_ } ->
    let h_cond   = encode_expr store cond in
    let h_then   = encode_expr store then_ in
    let h_else   = encode_expr store else_ in
    build_node store ~tag:tag_if ~children:[h_cond; h_then; h_else]
      ~write_inline:(fun buf ->
        put_comment buf e.comment)

  | Do stmts ->
    let h_exprs = List.map (fun s -> match s with
      | StmtLet { value; _ } -> encode_expr store value
      | StmtExpr e           -> encode_expr store e
    ) stmts in
    build_node store ~tag:tag_do ~children:h_exprs
      ~write_inline:(fun buf ->
        put_u16 buf (List.length stmts);
        List.iter (fun s -> match s with
          | StmtExpr _ ->
            put_u8 buf stmt_tag_expr
          | StmtLet { pat; _ } ->
            put_u8 buf stmt_tag_let;
            put_pattern buf pat
        ) stmts;
        put_comment buf e.comment)

  | Letrec (bindings, outer_body) ->
    let h_outer = encode_expr store outer_body in
    let h_bindings = List.map (fun (b : letrec_binding) ->
      encode_expr store b.letrec_body
    ) bindings in
    build_node store ~tag:tag_letrec ~children:(h_outer :: h_bindings)
      ~write_inline:(fun buf ->
        put_u16 buf (List.length bindings);
        List.iter (fun (b : letrec_binding) ->
          put_str buf b.letrec_name;
          put_list buf put_param b.letrec_params;
          put_type_expr buf b.letrec_return_type
        ) bindings;
        put_comment buf e.comment)

  | Record fields ->
    let h_values = List.map (fun (_, v) -> encode_expr store v) fields in
    build_node store ~tag:tag_record ~children:h_values
      ~write_inline:(fun buf ->
        put_list buf (fun buf (name, _) -> put_str buf name) fields;
        put_comment buf e.comment)

  | RecordUpdate (base, fields) ->
    let h_base   = encode_expr store base in
    let h_values = List.map (fun (_, v) -> encode_expr store v) fields in
    build_node store ~tag:tag_record_update ~children:(h_base :: h_values)
      ~write_inline:(fun buf ->
        put_list buf (fun buf (name, _) -> put_str buf name) fields;
        put_comment buf e.comment)

  | Project (record_expr, field) ->
    let h_record = encode_expr store record_expr in
    build_node store ~tag:tag_project ~children:[h_record]
      ~write_inline:(fun buf ->
        put_str buf field;
        put_comment buf e.comment)

  | Perform { effect_name; op_name; args } ->
    let h_args = List.map (encode_expr store) args in
    build_node store ~tag:tag_perform ~children:h_args
      ~write_inline:(fun buf ->
        put_str buf effect_name;
        put_str buf op_name;
        put_comment buf e.comment)

  | Handle { handled; handlers } ->
    (* Collect all child hashes: handled, then for each handler:
       op bodies in order, then return body if present *)
    let h_handled = encode_expr store handled in
    let handler_children = List.concat_map (fun (h : effect_handler) ->
      let op_hashes = List.map (fun (op : op_handler) ->
        encode_expr store op.op_handler_body
      ) h.op_handlers in
      let ret_hash = match h.return_handler with
        | None   -> []
        | Some r -> [encode_expr store r.return_body]
      in
      op_hashes @ ret_hash
    ) handlers in
    build_node store ~tag:tag_handle ~children:(h_handled :: handler_children)
      ~write_inline:(fun buf ->
        put_u16 buf (List.length handlers);
        List.iter (fun (h : effect_handler) ->
          put_str buf h.effect_handler;
          put_u16 buf (List.length h.op_handlers);
          List.iter (fun (op : op_handler) ->
            put_str buf op.op_handler_name;
            put_list buf put_str op.op_handler_params
          ) h.op_handlers;
          let has_return = h.return_handler <> None in
          put_bool buf has_return;
          (match h.return_handler with
           | None   -> ()
           | Some r -> put_str buf r.return_var)
        ) handlers;
        put_comment buf e.comment)

(* ================================================================== *)
(* Declaration encoder                                                 *)
(* ================================================================== *)

let rec encode_decl store (d : decl) : bytes =
  match d.decl_desc with
  | DeclFn { pub; fn_name; type_params; params; return_type; effects; decl_body } ->
    let h_body = encode_expr store decl_body in
    build_node store ~tag:tag_decl_fn ~children:[h_body]
      ~write_inline:(fun buf ->
        put_bool buf pub;
        put_str buf fn_name;
        put_list buf put_str type_params;
        put_list buf put_param params;
        put_opt buf put_type_expr return_type;
        put_opt buf put_effect_set effects;
        put_comment buf d.decl_comment)

  | DeclType { pub; type_name; type_params; ctors } ->
    build_node store ~tag:tag_decl_type ~children:[]
      ~write_inline:(fun buf ->
        put_bool buf pub;
        put_str buf type_name;
        put_list buf put_str type_params;
        put_u16 buf (List.length ctors);
        List.iter (fun (c : ctor_decl) ->
          put_str buf c.ctor_name;
          put_list buf put_type_expr c.ctor_params
        ) ctors;
        put_comment buf d.decl_comment)

  | DeclEffect { pub; effect_name; type_params; ops } ->
    build_node store ~tag:tag_decl_effect ~children:[]
      ~write_inline:(fun buf ->
        put_bool buf pub;
        put_str buf effect_name;
        put_list buf put_str type_params;
        put_u16 buf (List.length ops);
        List.iter (fun (op : effect_op) ->
          put_str buf op.effect_op_name;
          put_list buf put_type_expr op.effect_op_params;
          put_type_expr buf op.effect_op_return
        ) ops;
        put_comment buf d.decl_comment)

  | DeclModule { pub; module_name; body } ->
    let h_decls = List.map (encode_decl store) body in
    build_node store ~tag:tag_decl_module ~children:h_decls
      ~write_inline:(fun buf ->
        put_bool buf pub;
        put_str buf module_name;
        put_comment buf d.decl_comment)

  | DeclRequire ty ->
    build_node store ~tag:tag_decl_require ~children:[]
      ~write_inline:(fun buf ->
        put_type_expr buf ty;
        put_comment buf d.decl_comment)

(* ================================================================== *)
(* Program encoder                                                     *)
(* ================================================================== *)

let encode_program store (prog : program) : bytes =
  let h_decls = List.map (encode_decl store) prog in
  build_node store ~tag:tag_program ~children:h_decls
    ~write_inline:(fun buf ->
      put_comment buf None)
