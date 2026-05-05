open Axiom_lib
open Mcp_lib

(* ─── Minimal JSON type ──────────────────────────────────────────────────── *)
type json =
  | Null
  | Bool   of bool
  | Int    of int
  | String of string
  | Array  of json list
  | Object of (string * json) list

(* ─── JSON serializer ────────────────────────────────────────────────────── *)
let rec json_to_string = function
  | Null   -> "null"
  | Bool b -> if b then "true" else "false"
  | Int  n -> string_of_int n
  | String s ->
    let buf = Buffer.create (String.length s + 2) in
    Buffer.add_char buf '"';
    String.iter (function
      | '"'  -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c    -> Buffer.add_char buf c) s;
    Buffer.add_char buf '"';
    Buffer.contents buf
  | Array items ->
    "[" ^ String.concat "," (List.map json_to_string items) ^ "]"
  | Object fields ->
    "{" ^ String.concat "," (List.map (fun (k, v) ->
      json_to_string (String k) ^ ":" ^ json_to_string v) fields) ^ "}"

(* ─── JSON parser ────────────────────────────────────────────────────────── *)
exception Json_error of string

let parse_json s =
  let len = String.length s in
  let pos = ref 0 in
  let peek () = if !pos < len then s.[!pos] else '\000' in
  let next () = let c = peek () in incr pos; c in
  let skip_ws () =
    while !pos < len &&
      (let c = peek () in c = ' ' || c = '\t' || c = '\n' || c = '\r')
    do incr pos done
  in
  let expect c =
    let got = next () in
    if got <> c then raise (Json_error (Printf.sprintf "expected '%c', got '%c'" c got))
  in
  let parse_string () =
    expect '"';
    let buf = Buffer.create 16 in
    let rec loop () =
      match next () with
      | '"'  -> Buffer.contents buf
      | '\\' ->
        (match next () with
         | '"'  -> Buffer.add_char buf '"'
         | '\\' -> Buffer.add_char buf '\\'
         | '/'  -> Buffer.add_char buf '/'
         | 'n'  -> Buffer.add_char buf '\n'
         | 'r'  -> Buffer.add_char buf '\r'
         | 't'  -> Buffer.add_char buf '\t'
         | 'b'  -> Buffer.add_char buf '\b'
         | 'f'  -> Buffer.add_char buf '\012'
         | 'u'  ->
           for _ = 1 to 4 do ignore (next ()) done;
           Buffer.add_char buf '?'
         | c    -> raise (Json_error (Printf.sprintf "bad escape \\%c" c)));
        loop ()
      | '\000' -> raise (Json_error "unterminated string")
      | c -> Buffer.add_char buf c; loop ()
    in
    loop ()
  in
  let rec parse_value () =
    skip_ws ();
    match peek () with
    | '"' -> String (parse_string ())
    | '[' -> parse_array ()
    | '{' -> parse_object ()
    | 't' -> incr pos; expect 'r'; expect 'u'; expect 'e'; Bool true
    | 'f' -> incr pos; expect 'a'; expect 'l'; expect 's'; expect 'e'; Bool false
    | 'n' -> incr pos; expect 'u'; expect 'l'; expect 'l'; Null
    | c when c = '-' || (c >= '0' && c <= '9') -> parse_number ()
    | c   -> raise (Json_error (Printf.sprintf "unexpected char '%c'" c))
  and parse_number () =
    let start = !pos in
    if peek () = '-' then incr pos;
    while !pos < len && peek () >= '0' && peek () <= '9' do incr pos done;
    let is_float =
      !pos < len && (peek () = '.' || peek () = 'e' || peek () = 'E')
    in
    if is_float then begin
      while !pos < len && (let c = peek () in
        c >= '0' && c <= '9' || c = '.' || c = 'e' || c = 'E' || c = '+' || c = '-')
      do incr pos done;
      ignore (String.sub s start (!pos - start));
      Int 0
    end else
      Int (int_of_string (String.sub s start (!pos - start)))
  and parse_array () =
    expect '['; skip_ws ();
    if peek () = ']' then (incr pos; Array [])
    else begin
      let items = ref [] in
      let continue_ = ref true in
      while !continue_ do
        items := parse_value () :: !items;
        skip_ws ();
        match peek () with
        | ',' -> incr pos
        | ']' -> incr pos; continue_ := false
        | _   -> raise (Json_error "expected ',' or ']'")
      done;
      Array (List.rev !items)
    end
  and parse_object () =
    expect '{'; skip_ws ();
    if peek () = '}' then (incr pos; Object [])
    else begin
      let fields = ref [] in
      let continue_ = ref true in
      while !continue_ do
        skip_ws ();
        let k = parse_string () in
        skip_ws (); expect ':';
        let v = parse_value () in
        fields := (k, v) :: !fields;
        skip_ws ();
        match peek () with
        | ',' -> incr pos
        | '}' -> incr pos; continue_ := false
        | _   -> raise (Json_error "expected ',' or '}'")
      done;
      Object (List.rev !fields)
    end
  in
  parse_value ()

let obj_get key = function
  | Object fields -> (match List.assoc_opt key fields with Some v -> v | None -> Null)
  | _ -> Null

(* ─── JSON-RPC helpers ───────────────────────────────────────────────────── *)
let send json =
  print_string (json_to_string json);
  print_char '\n';
  flush stdout

let response id result =
  Object [("jsonrpc", String "2.0"); ("id", id); ("result", result)]

let error_response id code message =
  Object [
    ("jsonrpc", String "2.0");
    ("id", id);
    ("error", Object [("code", Int code); ("message", String message)]);
  ]

let bytes_to_hex b =
  let n = Bytes.length b in
  let buf = Buffer.create (n * 2) in
  for i = 0 to n - 1 do
    Buffer.add_string buf (Printf.sprintf "%02x" (Char.code (Bytes.get b i)))
  done;
  Buffer.contents buf

(* ─── Filesystem helpers ─────────────────────────────────────────────────── *)

(** Create [path] and any missing parent directories, like [mkdir -p]. *)
let mkdir_p path =
  let parts = String.split_on_char '/' path in
  ignore (List.fold_left (fun acc part ->
    let p =
      if acc = "" then part
      else if part = "" then acc
      else acc ^ "/" ^ part
    in
    if p <> "" && not (Sys.file_exists p) then
      (try Unix.mkdir p 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    p
  ) "" parts)

(* ─── Server state ───────────────────────────────────────────────────────── *)

(* Per-module state stored after a successful write/submit_module command. *)
type module_state = {
  typed_ast   : Ast.program;
  type_env    : Typechecker.env;
  effect_env  : Typechecker.effect_env;
  root_hash   : string;  (* hex-encoded BLAKE3 hash of the program root node *)
  decl_hashes : (string, string) Hashtbl.t;  (* decl name → hex hash *)
}

type state = {
  mutable initialized : bool;
  modules             : (string, module_state) Hashtbl.t;
  (* source cache: module_name → source (for persistence across restarts) *)
  sources             : (string, string) Hashtbl.t;
  node_store          : Node_store.t;
  store_dir           : string;
}

(* ─── Module registry (persistence) ─────────────────────────────────────── *)

let modules_registry_path dir = Filename.concat dir "modules.json"

(** Atomically write the module source registry to disk. *)
let save_module_registry dir sources =
  let fields = Hashtbl.fold (fun name src acc ->
    (name, String src) :: acc) sources [] in
  let json = Object [("modules", Object fields)] in
  let path = modules_registry_path dir in
  let tmp  = path ^ ".tmp" in
  let oc   = open_out tmp in
  (try output_string oc (json_to_string json); close_out oc
   with e -> close_out_noerr oc; (try Sys.remove tmp with _ -> ()); raise e);
  Sys.rename tmp path

(** Load module sources from the registry file.  Returns [(name, source)] pairs. *)
let load_module_registry dir =
  let path = modules_registry_path dir in
  if not (Sys.file_exists path) then []
  else begin
    let ic = open_in path in
    let n  = in_channel_length ic in
    let s  = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    match parse_json (Bytes.to_string s) with
    | Object fields ->
      (match List.assoc_opt "modules" fields with
       | Some (Object mods) ->
         List.filter_map (fun (name, v) ->
           match v with String src -> Some (name, src) | _ -> None) mods
       | _ -> [])
    | _ -> []
  end

(* ─── Diagnostic → JSON ──────────────────────────────────────────────────── *)

let json_of_severity = function
  | Mcp_types.Sev_error   -> String "error"
  | Mcp_types.Sev_warning -> String "warning"
  | Mcp_types.Sev_info    -> String "info"

let json_of_location (loc : Mcp_types.location) =
  let fields = [("line", Int loc.loc_line); ("col", Int loc.loc_col)] in
  let fields = match loc.loc_module with
    | None   -> fields
    | Some m -> ("module", String m) :: fields
  in
  Object fields

let json_of_related (r : Mcp_types.related_ref) =
  let fields = [("message", String r.ref_message)] in
  let fields = match r.ref_node with
    | None   -> fields
    | Some h -> ("node", String h) :: fields
  in
  Object fields

let json_of_diagnostic (d : Mcp_types.diagnostic) =
  Object [
    ("severity", json_of_severity d.diag_severity);
    ("code",     String d.diag_code);
    ("message",  String d.diag_message);
    ("node",     (match d.diag_node with None -> Null | Some h -> String h));
    ("location", (match d.diag_location with None -> Null | Some l -> json_of_location l));
    ("related",  Array (List.map json_of_related d.diag_related));
  ]

(* ─── Batch response ─────────────────────────────────────────────────────── *)

(* All four tool handlers share this single response shape, enforced by type. *)
type batch_response = {
  br_results     : json list;
  br_stopped_at  : int option;
  br_diagnostics : Mcp_types.diagnostic list;
}

let json_of_batch_response br =
  Object [
    ("results",     Array br.br_results);
    ("stopped_at",  (match br.br_stopped_at with None -> Null | Some i -> Int i));
    ("diagnostics", Array (List.map json_of_diagnostic br.br_diagnostics));
  ]

(* ─── Command outcome ────────────────────────────────────────────────────── *)

(* Unrecoverable failures halt the batch at the current index.
   Recoverable diagnostics (e.g. type/effect warnings) accumulate and allow
   subsequent commands to continue. *)
type cmd_outcome =
  | Cmd_success of json * Mcp_types.diagnostic list
  | Cmd_halt    of Mcp_types.diagnostic

(* ─── Batch executor ─────────────────────────────────────────────────────── *)

let batch_execute (dispatch : json -> cmd_outcome) (cmds : json list) : batch_response =
  let results    = ref [] in
  let diags      = ref [] in
  let stopped_at = ref None in
  let i          = ref 0 in
  let go         = ref true in
  let arr        = Array.of_list cmds in
  let n          = Array.length arr in
  while !go && !i < n do
    (match dispatch arr.(!i) with
     | Cmd_success (result, ds) ->
       results := !results @ [result];
       diags   := !diags @ ds;
       incr i
     | Cmd_halt d ->
       diags      := !diags @ [d];
       stopped_at := Some !i;
       go         := false)
  done;
  { br_results = !results; br_stopped_at = !stopped_at; br_diagnostics = !diags }

let get_commands args =
  match obj_get "commands" args with
  | Array cmds -> cmds
  | _          -> []

(* ─── Type pretty-printing helpers ──────────────────────────────────────── *)

let rec ty_deref = function
  | Typechecker.TyMeta { Typechecker.inst = Some t; _ } -> ty_deref t
  | t -> t

let rec row_deref = function
  | Typechecker.RMeta { Typechecker.rinst = Some r; _ } -> row_deref r
  | r -> r

let collect_row_effects row =
  let rec go acc r =
    match row_deref r with
    | Typechecker.RPure         -> List.rev acc
    | Typechecker.RCons (e, tl) -> go (e.Typechecker.eff_name :: acc) tl
    | Typechecker.RMeta _       -> List.rev acc
  in
  go [] row

let rec fn_effect_row ty =
  match ty_deref ty with
  | Typechecker.TyForall (_, t) -> fn_effect_row t
  | Typechecker.TyFun (_, ret, row) ->
    (match ty_deref ret with
     | Typechecker.TyFun _ as inner -> fn_effect_row inner
     | _                            -> Some row)
  | _ -> None

let effect_names_of_decl_annotation = function
  | None | Some Ast.Pure -> []
  | Some (Ast.Effects (ts, _)) ->
    List.filter_map (function
      | Ast.TyName n      -> Some n
      | Ast.TyApp (n, _)  -> Some n
      | _                 -> None) ts

(* ─── Public-interface rendering ─────────────────────────────────────────── *)

let render_interface_decl decl =
  match decl.Ast.decl_desc with
  | Ast.DeclFn { pub = true; fn_name; type_params; params; return_type; effects; _ } ->
    let tp_s =
      if type_params = [] then ""
      else "<" ^ String.concat ", " type_params ^ ">"
    in
    let ann = match return_type, effects with
      | Some ret, Some eff ->
        " -> " ^ Printer.print_type_expr ret ^ " ! " ^ Printer.print_effect_set eff
      | Some ret, None -> " -> " ^ Printer.print_type_expr ret
      | _              -> ""
    in
    Some ("pub fn " ^ fn_name ^ tp_s ^ "(" ^
          String.concat ", " (List.map Printer.print_param params) ^ ")" ^ ann)
  | Ast.DeclType   { pub = true; _ } -> Some (Printer.print_decl decl)
  | Ast.DeclEffect { pub = true; _ } -> Some (Printer.print_decl decl)
  | _ -> None

(* ─── Location helpers ───────────────────────────────────────────────────── *)

let make_loc ?module_name line col =
  Mcp_types.{ loc_module = module_name; loc_line = line; loc_col = col }

(* ─── Anchor resolver ────────────────────────────────────────────────────── *)

(* Accepts either {"hash":"<hex>"} (direct) or {"module":"...","name":"..."}
   (symbolic) and resolves to (module_name, decl_name, module_state).
   When module is omitted but name is given, searches all submitted modules:
   a unique match resolves directly; multiple matches return an error listing them. *)
let resolve_anchor state args =
  match obj_get "hash" args with
  | String h when String.length h > 0 ->
    let found = Hashtbl.fold (fun mod_name ms acc ->
        match acc with
        | Some _ -> acc
        | None ->
          match Hashtbl.fold (fun name hash a ->
              if a = None && hash = h then Some name else a
            ) ms.decl_hashes None
          with
          | Some name -> Some (mod_name, name, ms)
          | None -> None
      ) state.modules None in
    (match found with
     | Some r -> Ok r
     | None ->
       Error (Mcp_types.make_error "anchor/not-found"
         (Printf.sprintf "No declaration found with hash '%s'" h)))
  | _ ->
    let mod_name = match obj_get "module" args with String s -> s | _ -> "" in
    let fn_name  = match obj_get "name"   args with String s -> s | _ -> "" in
    if mod_name = "" then begin
      (* Search all submitted modules for the name. *)
      let matches = Hashtbl.fold (fun mname ms acc ->
        if List.assoc_opt fn_name ms.type_env <> None
        then (mname, fn_name, ms) :: acc
        else acc
      ) state.modules [] in
      match matches with
      | [(mn, fn, ms)] -> Ok (mn, fn, ms)
      | [] ->
        Error (Mcp_types.make_error "anchor/not-found"
          (Printf.sprintf "Declaration '%s' not found in any submitted module" fn_name))
      | many ->
        let mods = String.concat ", " (List.map (fun (mn, _, _) -> mn) many) in
        Error (Mcp_types.make_error "anchor/ambiguous"
          (Printf.sprintf "Multiple modules define '%s': %s — add a 'module' field to disambiguate" fn_name mods))
    end else
      match Hashtbl.find_opt state.modules mod_name with
      | None ->
        Error (Mcp_types.make_error "anchor/not-found"
          (Printf.sprintf "Module '%s' not found" mod_name))
      | Some ms -> Ok (mod_name, fn_name, ms)

(* ─── Decl-name extraction (for hash look-up) ────────────────────────────── *)

let decl_name decl =
  match decl.Ast.decl_desc with
  | Ast.DeclFn     { fn_name;     _ } -> Some fn_name
  | Ast.DeclType   { type_name;   _ } -> Some type_name
  | Ast.DeclEffect { effect_name; _ } -> Some effect_name
  | Ast.DeclModule { module_name; _ } -> Some module_name
  | _                                  -> None

(* ─── Combined project program (all submitted modules concatenated) ───────── *)

(** Build a combined AST from every submitted module, for cross-module analysis. *)
let combined_program state =
  Hashtbl.fold (fun _ ms acc -> acc @ ms.typed_ast) state.modules []

(** Find which module (if any) defines a given function name. *)
let module_of_fn state fn_name =
  Hashtbl.fold (fun mod_name ms acc ->
    match acc with
    | Some _ -> acc
    | None ->
      let found = List.exists (fun d ->
        match d.Ast.decl_desc with
        | Ast.DeclFn { fn_name = n; _ } -> n = fn_name
        | _ -> false) ms.typed_ast in
      if found then Some mod_name else None
  ) state.modules None

(* ─── Graph traversal helpers ────────────────────────────────────────────── *)

(* Collect all direct call sites (App (Var callee, args)) within an expression,
   returning (callee, full_app_expr, args) triples. *)
let rec collect_calls_in acc expr =
  match expr.Ast.desc with
  | Ast.App ({ Ast.desc = Ast.Var callee; _ }, args) ->
    acc := (callee, expr, args) :: !acc;
    List.iter (collect_calls_in acc) args
  | Ast.App (f, args) ->
    collect_calls_in acc f;
    List.iter (collect_calls_in acc) args
  | Ast.Let { Ast.value; Ast.body; _ } ->
    collect_calls_in acc value;
    collect_calls_in acc body
  | Ast.Fn { Ast.fn_body; _ } -> collect_calls_in acc fn_body
  | Ast.If { Ast.cond; Ast.then_; Ast.else_ } ->
    collect_calls_in acc cond;
    collect_calls_in acc then_;
    collect_calls_in acc else_
  | Ast.Do stmts ->
    List.iter (function
      | Ast.StmtLet { value; _ } -> collect_calls_in acc value
      | Ast.StmtExpr e -> collect_calls_in acc e) stmts
  | Ast.Match { Ast.scrutinee; Ast.arms } ->
    collect_calls_in acc scrutinee;
    List.iter (fun arm -> collect_calls_in acc arm.Ast.arm_body) arms
  | Ast.Letrec (bindings, body) ->
    List.iter (fun b -> collect_calls_in acc b.Ast.letrec_body) bindings;
    collect_calls_in acc body
  | Ast.Record fields ->
    List.iter (fun (_, e) -> collect_calls_in acc e) fields
  | Ast.RecordUpdate (base, fields) ->
    collect_calls_in acc base;
    List.iter (fun (_, e) -> collect_calls_in acc e) fields
  | Ast.Project (e, _) -> collect_calls_in acc e
  | Ast.Perform { Ast.args; _ } ->
    List.iter (collect_calls_in acc) args
  | Ast.Handle { Ast.handled; Ast.handlers } ->
    collect_calls_in acc handled;
    List.iter (fun h ->
      List.iter (fun op -> collect_calls_in acc op.Ast.op_handler_body)
        h.Ast.op_handlers;
      Option.iter (fun r -> collect_calls_in acc r.Ast.return_body)
        h.Ast.return_handler) handlers
  | Ast.Var _ | Ast.IntLit _ | Ast.FloatLit _ | Ast.StringLit _
  | Ast.BoolLit _ | Ast.UnitLit -> ()

(* Collect let-bindings visible in an expression into acc: (var_name, value). *)
let rec collect_let_bindings acc expr =
  match expr.Ast.desc with
  | Ast.Let { Ast.pat = { Ast.pat_desc = Ast.PVar x; _ }; Ast.value; Ast.body } ->
    acc := (x, value) :: !acc;
    collect_let_bindings acc value;
    collect_let_bindings acc body
  | Ast.Let { Ast.value; Ast.body; _ } ->
    collect_let_bindings acc value;
    collect_let_bindings acc body
  | Ast.Letrec (bindings, body) ->
    List.iter (fun b -> collect_let_bindings acc b.Ast.letrec_body) bindings;
    collect_let_bindings acc body
  | Ast.Fn { Ast.fn_body; _ } -> collect_let_bindings acc fn_body
  | Ast.If { Ast.cond; Ast.then_; Ast.else_ } ->
    collect_let_bindings acc cond;
    collect_let_bindings acc then_;
    collect_let_bindings acc else_
  | Ast.Do stmts ->
    List.iter (function
      | Ast.StmtLet { pat = { pat_desc = Ast.PVar x; _ }; value } ->
        acc := (x, value) :: !acc;
        collect_let_bindings acc value
      | Ast.StmtLet { value; _ } -> collect_let_bindings acc value
      | Ast.StmtExpr e -> collect_let_bindings acc e) stmts
  | Ast.Match { Ast.scrutinee; Ast.arms } ->
    collect_let_bindings acc scrutinee;
    List.iter (fun arm -> collect_let_bindings acc arm.Ast.arm_body) arms
  | Ast.App (f, args) ->
    collect_let_bindings acc f;
    List.iter (collect_let_bindings acc) args
  | Ast.Record fields ->
    List.iter (fun (_, e) -> collect_let_bindings acc e) fields
  | Ast.RecordUpdate (base, fields) ->
    collect_let_bindings acc base;
    List.iter (fun (_, e) -> collect_let_bindings acc e) fields
  | Ast.Project (e, _) -> collect_let_bindings acc e
  | Ast.Perform { Ast.args; _ } ->
    List.iter (collect_let_bindings acc) args
  | Ast.Handle { Ast.handled; Ast.handlers } ->
    collect_let_bindings acc handled;
    List.iter (fun h ->
      List.iter (fun op -> collect_let_bindings acc op.Ast.op_handler_body)
        h.Ast.op_handlers;
      Option.iter (fun r -> collect_let_bindings acc r.Ast.return_body)
        h.Ast.return_handler) handlers
  | Ast.Var _ | Ast.IntLit _ | Ast.FloatLit _ | Ast.StringLit _
  | Ast.BoolLit _ | Ast.UnitLit -> ()

(* Walk expr collecting all perform sites: (effect_name, fn_name). *)
let collect_all_performs_in_body prog fn_name =
  let fn_map : (string, Ast.expr) Hashtbl.t = Hashtbl.create 16 in
  List.iter (fun d ->
    match d.Ast.decl_desc with
    | Ast.DeclFn { fn_name = n; decl_body; _ } ->
      Hashtbl.replace fn_map n decl_body
    | _ -> ()) prog;
  let all    = ref [] in
  let seen   = Hashtbl.create 8 in
  let in_flt = Hashtbl.create 4 in
  let rec walk_fn fn =
    if not (Hashtbl.mem in_flt fn) then begin
      Hashtbl.replace in_flt fn ();
      (match Hashtbl.find_opt fn_map fn with
       | None -> ()
       | Some body -> walk_expr fn body);
      Hashtbl.remove in_flt fn
    end
  and walk_expr fn e =
    match e.Ast.desc with
    | Ast.Perform { Ast.effect_name; _ } ->
      let key = (fn, effect_name) in
      if not (Hashtbl.mem seen key) then begin
        Hashtbl.replace seen key ();
        all := (effect_name, fn) :: !all
      end
    | Ast.App ({ Ast.desc = Ast.Var callee; _ }, args) ->
      List.iter (walk_expr fn) args; walk_fn callee
    | Ast.App (f, args) ->
      walk_expr fn f; List.iter (walk_expr fn) args
    | Ast.Let { Ast.value; Ast.body; _ } ->
      walk_expr fn value; walk_expr fn body
    | Ast.Fn { Ast.fn_body; _ } -> walk_expr fn fn_body
    | Ast.If { Ast.cond; Ast.then_; Ast.else_ } ->
      walk_expr fn cond; walk_expr fn then_; walk_expr fn else_
    | Ast.Do stmts ->
      List.iter (function
        | Ast.StmtLet { value; _ } -> walk_expr fn value
        | Ast.StmtExpr e -> walk_expr fn e) stmts
    | Ast.Match { Ast.scrutinee; Ast.arms } ->
      walk_expr fn scrutinee;
      List.iter (fun arm -> walk_expr fn arm.Ast.arm_body) arms
    | Ast.Letrec (bindings, body) ->
      List.iter (fun b -> walk_expr fn b.Ast.letrec_body) bindings;
      walk_expr fn body
    | Ast.Record fields -> List.iter (fun (_, e) -> walk_expr fn e) fields
    | Ast.RecordUpdate (base, fields) ->
      walk_expr fn base;
      List.iter (fun (_, e) -> walk_expr fn e) fields
    | Ast.Project (e, _) -> walk_expr fn e
    | Ast.Handle { Ast.handled; Ast.handlers } ->
      walk_expr fn handled;
      List.iter (fun h ->
        List.iter (fun op -> walk_expr fn op.Ast.op_handler_body) h.Ast.op_handlers;
        Option.iter (fun r -> walk_expr fn r.Ast.return_body) h.Ast.return_handler)
        handlers
    | Ast.Var _ | Ast.IntLit _ | Ast.FloatLit _ | Ast.StringLit _
    | Ast.BoolLit _ | Ast.UnitLit -> ()
  in
  walk_fn fn_name;
  List.rev !all

(* Walk prog looking for references to a type or effect name in each decl. *)
(* Returns (mod_name, decl_name, kind) triples. kind ∈ "defines"|"uses"|"performs"|"handles" *)
let find_dependents_of state name =
  let results = ref [] in
  Hashtbl.iter (fun mod_name ms ->
    List.iter (fun d ->
      match d.Ast.decl_desc with
      | Ast.DeclType { type_name; _ } when type_name = name ->
        results := (mod_name, type_name, "defines") :: !results
      | Ast.DeclEffect { effect_name; _ } when effect_name = name ->
        results := (mod_name, effect_name, "defines") :: !results
      | Ast.DeclFn { fn_name; params; return_type; effects;
                     decl_body; _ } ->
        (* Check if name appears in type annotations *)
        let rec ty_refs t =
          match t with
          | Ast.TyName s -> s = name
          | Ast.TyApp (s, args) -> s = name || List.exists ty_refs args
          | Ast.TyTuple ts -> List.exists ty_refs ts
          | Ast.TyFun (ps, r, _) -> List.exists ty_refs ps || ty_refs r
        in
        let eff_refs = function
          | None | Some Ast.Pure -> false
          | Some (Ast.Effects (ts, _)) -> List.exists ty_refs ts
        in
        let used_in_sig =
          List.exists (fun p -> ty_refs p.Ast.param_type) params ||
          (match return_type with Some t -> ty_refs t | None -> false) ||
          eff_refs effects
        in
        (* Check body for Perform and Handle *)
        let rec body_refs_perform e =
          match e.Ast.desc with
          | Ast.Perform { Ast.effect_name = en; _ } -> en = name
          | Ast.App (f, args) ->
            body_refs_perform f || List.exists body_refs_perform args
          | Ast.Let { Ast.value; Ast.body; _ } ->
            body_refs_perform value || body_refs_perform body
          | Ast.Fn { Ast.fn_body; _ } -> body_refs_perform fn_body
          | Ast.If { Ast.cond; Ast.then_; Ast.else_ } ->
            body_refs_perform cond || body_refs_perform then_ ||
            body_refs_perform else_
          | Ast.Do stmts ->
            List.exists (function
              | Ast.StmtLet { value; _ } -> body_refs_perform value
              | Ast.StmtExpr e -> body_refs_perform e) stmts
          | Ast.Match { Ast.scrutinee; Ast.arms } ->
            body_refs_perform scrutinee ||
            List.exists (fun arm -> body_refs_perform arm.Ast.arm_body) arms
          | Ast.Letrec (bindings, body) ->
            List.exists (fun b -> body_refs_perform b.Ast.letrec_body) bindings ||
            body_refs_perform body
          | Ast.Record fields -> List.exists (fun (_, e) -> body_refs_perform e) fields
          | Ast.RecordUpdate (base, fields) ->
            body_refs_perform base ||
            List.exists (fun (_, e) -> body_refs_perform e) fields
          | Ast.Project (e, _) -> body_refs_perform e
          | Ast.Handle { Ast.handled; _ } -> body_refs_perform handled
          | Ast.Var _ | Ast.IntLit _ | Ast.FloatLit _ | Ast.StringLit _
          | Ast.BoolLit _ | Ast.UnitLit -> false
        in
        let rec body_refs_handle e =
          match e.Ast.desc with
          | Ast.Handle { Ast.handled; Ast.handlers } ->
            body_refs_handle handled ||
            List.exists (fun h -> h.Ast.effect_handler = name) handlers ||
            List.exists (fun h ->
              List.exists (fun op -> body_refs_handle op.Ast.op_handler_body)
                h.Ast.op_handlers) handlers
          | Ast.App (f, args) ->
            body_refs_handle f || List.exists body_refs_handle args
          | Ast.Let { Ast.value; Ast.body; _ } ->
            body_refs_handle value || body_refs_handle body
          | Ast.Fn { Ast.fn_body; _ } -> body_refs_handle fn_body
          | Ast.If { Ast.cond; Ast.then_; Ast.else_ } ->
            body_refs_handle cond || body_refs_handle then_ ||
            body_refs_handle else_
          | Ast.Do stmts ->
            List.exists (function
              | Ast.StmtLet { value; _ } -> body_refs_handle value
              | Ast.StmtExpr e -> body_refs_handle e) stmts
          | Ast.Match { Ast.scrutinee; Ast.arms } ->
            body_refs_handle scrutinee ||
            List.exists (fun arm -> body_refs_handle arm.Ast.arm_body) arms
          | Ast.Letrec (bindings, body) ->
            List.exists (fun b -> body_refs_handle b.Ast.letrec_body) bindings ||
            body_refs_handle body
          | Ast.Record fields -> List.exists (fun (_, e) -> body_refs_handle e) fields
          | Ast.RecordUpdate (base, fields) ->
            body_refs_handle base ||
            List.exists (fun (_, e) -> body_refs_handle e) fields
          | Ast.Project (e, _) -> body_refs_handle e
          | Ast.Perform { Ast.args; _ } -> List.exists body_refs_handle args
          | Ast.Var _ | Ast.IntLit _ | Ast.FloatLit _ | Ast.StringLit _
          | Ast.BoolLit _ | Ast.UnitLit -> false
        in
        let performs = body_refs_perform decl_body in
        let handles  = body_refs_handle  decl_body in
        if used_in_sig then
          results := (mod_name, fn_name, "uses") :: !results;
        if performs && not used_in_sig then
          results := (mod_name, fn_name, "performs") :: !results;
        if handles then
          results := (mod_name, fn_name, "handles") :: !results
      | _ -> ()
    ) ms.typed_ast
  ) state.modules;
  List.rev !results

(* ─── Graph traversal node types ─────────────────────────────────────────── *)

type gnode =
  | GnFn   of { gmod : string; gname : string; ghash : string }
  | GnParam of { gpfn : string; gpidx : int; gpname : string; gpty : string }
  | GnCall  of { gcaller : string; gcallee : string; gchash : string;
                 gcargs : Ast.expr list }
  | GnExpr  of { gefn : string; gehash : string; geexpr : Ast.expr;
                 gecall_site : string option }

let json_of_gnode state node =
  match node with
  | GnFn { gmod; gname; ghash } ->
    let sig_ =
      match Hashtbl.find_opt state.modules gmod with
      | None -> ""
      | Some ms ->
        (match List.assoc_opt gname ms.type_env with
         | None -> ""
         | Some sc -> Format.asprintf "%a" Typechecker.pp_ty sc.Typechecker.body)
    in
    Object [("kind", String "function"); ("id", String ghash);
            ("module", String gmod); ("name", String gname);
            ("signature", String sig_)]
  | GnParam { gpfn; gpidx = _; gpname; gpty } ->
    Object [("kind", String "param");
            ("id", String (gpfn ^ ":param:" ^ gpname));
            ("function", String gpfn); ("name", String gpname);
            ("type", String gpty)]
  | GnCall { gcaller; gcallee; gchash; _ } ->
    Object [("kind", String "call_site"); ("id", String gchash);
            ("function", String gcaller); ("callee", String gcallee)]
  | GnExpr { gefn; gehash; geexpr; gecall_site } ->
    let expr_str = Printer.print_expr geexpr in
    let fields = [("kind", String "expr"); ("id", String gehash);
                  ("function", String gefn); ("description", String expr_str)] in
    let fields = match gecall_site with
      | None    -> fields
      | Some cs -> ("call_site", String cs) :: fields
    in
    Object fields

(* Look up a function's param list from the combined program. *)
let fn_params_of prog fn_name =
  List.find_opt (fun d ->
    match d.Ast.decl_desc with
    | Ast.DeclFn { fn_name = n; _ } -> n = fn_name
    | _ -> false) prog
  |> Option.map (fun d ->
      match d.Ast.decl_desc with
      | Ast.DeclFn { params; _ } -> params
      | _ -> [])
  |> Option.value ~default:[]

(* Look up a function's body from the combined program. *)
let fn_body_of prog fn_name =
  List.find_opt (fun d ->
    match d.Ast.decl_desc with
    | Ast.DeclFn { fn_name = n; _ } -> n = fn_name
    | _ -> false) prog
  |> Option.map (fun d ->
      match d.Ast.decl_desc with
      | Ast.DeclFn { decl_body; _ } -> Some decl_body
      | _ -> None)
  |> Option.join

(* Apply a filter JSON object to a gnode; returns true iff node passes. *)
let gnode_passes_filter filter node =
  match filter with
  | Null | Object [] -> true
  | Object fields ->
    List.for_all (fun (k, v) ->
      match k, v, node with
      | "name",  String s, GnParam { gpname; _ }  -> gpname = s
      | "name",  String s, GnFn   { gname;  _ }   -> gname  = s
      | "param", String s, GnExpr _               ->
        (* filter {param: "loc"} on argument nodes is handled at collection time *)
        ignore s; true
      | _ -> true
    ) fields
  | _ -> true

(* Traverse one edge from one node; returns a list of result gnodes.
   [param_filter]: when Some name, only return the argument at that param position. *)
let traverse_one_edge state prog enc edge direction filter node =
  match edge, direction with

  | "HAS_PARAM", "out" ->
    (match node with
     | GnFn { gname; _ } ->
       fn_params_of prog gname
       |> List.mapi (fun i p ->
           GnParam { gpfn  = gname; gpidx = i;
                     gpname = p.Ast.param_name;
                     gpty  = Printer.print_type_expr p.Ast.param_type })
       |> List.filter (gnode_passes_filter filter)
     | _ -> [])

  | "HAS_PARAM", "in" ->
    (* Return the containing function for a param *)
    (match node with
     | GnParam { gpfn; _ } ->
       (match module_of_fn state gpfn with
        | None -> []
        | Some gmod ->
          let ghash = match Hashtbl.find_opt state.modules gmod with
            | None -> "" | Some ms ->
              Option.value ~default:"" (Hashtbl.find_opt ms.decl_hashes gpfn)
          in
          [GnFn { gmod; gname = gpfn; ghash }])
     | _ -> [])

  | "CALLS", "out" ->
    (* Call sites within the anchor function *)
    (match node with
     | GnFn { gname = caller; _ } ->
       (match fn_body_of prog caller with
        | None -> []
        | Some body ->
          let calls = ref [] in
          collect_calls_in calls body;
          List.filter_map (fun (callee, call_expr, args) ->
            let gchash = bytes_to_hex (Node_encoding.encode_expr enc call_expr) in
            let n = GnCall { gcaller = caller; gcallee = callee;
                             gchash; gcargs = args } in
            if gnode_passes_filter filter n then Some n else None
          ) (List.rev !calls))
     | _ -> [])

  | "CALLS", "in" ->
    (* Call sites that call the anchor function, across all modules *)
    (match node with
     | GnFn { gname = callee; _ } ->
       Hashtbl.fold (fun _mod_name ms acc ->
         let sites = Typechecker.collect_callers_in ms.typed_ast callee in
         List.filter_map (fun (s : Typechecker.caller_site) ->
           let gchash =
             bytes_to_hex (Node_encoding.encode_expr enc s.cs_call_expr) in
           let args = match s.cs_call_expr.Ast.desc with
             | Ast.App (_, a) -> a | _ -> []
           in
           let n = GnCall { gcaller = s.cs_caller; gcallee = callee;
                            gchash; gcargs = args } in
           if gnode_passes_filter filter n then Some n else None
         ) sites @ acc
       ) state.modules []
     | _ -> [])

  | "HAS_ARGUMENT", "out" ->
    (* Arguments of a call site; filter {param: "name"} selects by position *)
    (match node with
     | GnCall { gcaller; gcallee; gchash; gcargs } ->
       let param_filter =
         match filter with
         | Object fields ->
           (match List.assoc_opt "param" fields with
            | Some (String s) -> Some s | _ -> None)
         | _ -> None
       in
       let callee_params = fn_params_of prog gcallee in
       List.mapi (fun i arg ->
         let keep = match param_filter with
           | None -> true
           | Some pname ->
             (match List.nth_opt callee_params i with
              | Some p -> p.Ast.param_name = pname
              | None   -> false)
         in
         if keep then
           let gehash = bytes_to_hex (Node_encoding.encode_expr enc arg) in
           Some (GnExpr { gefn = gcaller; gehash; geexpr = arg;
                          gecall_site = Some gchash })
         else None
       ) gcargs |> List.filter_map Fun.id
     | _ -> [])

  | "HAS_TYPE", "out" ->
    (* Type annotation of a node *)
    (match node with
     | GnParam { gpfn; gpname; gpty; _ } ->
       let hash = gpfn ^ ":param:" ^ gpname ^ ":type" in
       [GnExpr { gefn = gpfn; gehash = hash;
                 geexpr = Ast.expr Ast.UnitLit;
                 gecall_site = Some gpty }]
     | GnFn { gmod; gname; _ } ->
       let sig_ =
         match Hashtbl.find_opt state.modules gmod with
         | None -> ""
         | Some ms ->
           (match List.assoc_opt gname ms.type_env with
            | None -> ""
            | Some sc -> Format.asprintf "%a" Typechecker.pp_ty sc.Typechecker.body)
       in
       [GnExpr { gefn = gname; gehash = gname ^ ":type";
                 geexpr = Ast.expr Ast.UnitLit;
                 gecall_site = Some sig_ }]
     | _ -> [])

  | "PERFORMS", "out" ->
    (* Perform sites within a function (direct, not transitive) *)
    (match node with
     | GnFn { gname; _ } ->
       let performs = ref [] in
       let rec walk e =
         match e.Ast.desc with
         | Ast.Perform { Ast.effect_name; _ } ->
           performs := effect_name :: !performs
         | Ast.App (f, args) -> walk f; List.iter walk args
         | Ast.Let { Ast.value; Ast.body; _ } -> walk value; walk body
         | Ast.Fn { Ast.fn_body; _ } -> walk fn_body
         | Ast.If { Ast.cond; Ast.then_; Ast.else_ } ->
           walk cond; walk then_; walk else_
         | Ast.Do stmts ->
           List.iter (function
             | Ast.StmtLet { value; _ } -> walk value
             | Ast.StmtExpr e -> walk e) stmts
         | Ast.Match { Ast.scrutinee; Ast.arms } ->
           walk scrutinee; List.iter (fun a -> walk a.Ast.arm_body) arms
         | Ast.Letrec (bindings, body) ->
           List.iter (fun b -> walk b.Ast.letrec_body) bindings; walk body
         | Ast.Record fields -> List.iter (fun (_, e) -> walk e) fields
         | Ast.RecordUpdate (base, fields) ->
           walk base; List.iter (fun (_, e) -> walk e) fields
         | Ast.Project (e, _) -> walk e
         | Ast.Handle { Ast.handled; Ast.handlers } ->
           walk handled;
           List.iter (fun h ->
             List.iter (fun op -> walk op.Ast.op_handler_body) h.Ast.op_handlers;
             Option.iter (fun r -> walk r.Ast.return_body) h.Ast.return_handler)
             handlers
         | Ast.Var _ | Ast.IntLit _ | Ast.FloatLit _ | Ast.StringLit _
         | Ast.BoolLit _ | Ast.UnitLit -> ()
       in
       (match fn_body_of prog gname with None -> () | Some b -> walk b);
       List.rev_map (fun eff_name ->
         let ghash = gname ^ ":performs:" ^ eff_name in
         GnExpr { gefn = gname; gehash = ghash;
                  geexpr = Ast.expr Ast.UnitLit;
                  gecall_site = Some eff_name }
       ) !performs
     | _ -> [])

  | "HANDLES", "out" ->
    (* Effect names handled (via Handle) within a function (direct) *)
    (match node with
     | GnFn { gname; _ } ->
       let handles = ref [] in
       let rec walk e =
         match e.Ast.desc with
         | Ast.Handle { Ast.handled; Ast.handlers } ->
           List.iter (fun h -> handles := h.Ast.effect_handler :: !handles) handlers;
           walk handled;
           List.iter (fun h ->
             List.iter (fun op -> walk op.Ast.op_handler_body) h.Ast.op_handlers;
             Option.iter (fun r -> walk r.Ast.return_body) h.Ast.return_handler)
             handlers
         | Ast.App (f, args) -> walk f; List.iter walk args
         | Ast.Let { Ast.value; Ast.body; _ } -> walk value; walk body
         | Ast.Fn { Ast.fn_body; _ } -> walk fn_body
         | Ast.If { Ast.cond; Ast.then_; Ast.else_ } ->
           walk cond; walk then_; walk else_
         | Ast.Do stmts ->
           List.iter (function
             | Ast.StmtLet { value; _ } -> walk value
             | Ast.StmtExpr e -> walk e) stmts
         | Ast.Match { Ast.scrutinee; Ast.arms } ->
           walk scrutinee; List.iter (fun a -> walk a.Ast.arm_body) arms
         | Ast.Letrec (bindings, body) ->
           List.iter (fun b -> walk b.Ast.letrec_body) bindings; walk body
         | Ast.Record fields -> List.iter (fun (_, e) -> walk e) fields
         | Ast.RecordUpdate (base, fields) ->
           walk base; List.iter (fun (_, e) -> walk e) fields
         | Ast.Project (e, _) -> walk e
         | Ast.Perform { Ast.args; _ } -> List.iter walk args
         | Ast.Var _ | Ast.IntLit _ | Ast.FloatLit _ | Ast.StringLit _
         | Ast.BoolLit _ | Ast.UnitLit -> ()
       in
       (match fn_body_of prog gname with None -> () | Some b -> walk b);
       List.rev_map (fun eff_name ->
         let ghash = gname ^ ":handles:" ^ eff_name in
         GnExpr { gefn = gname; gehash = ghash;
                  geexpr = Ast.expr Ast.UnitLit;
                  gecall_site = Some eff_name }
       ) !handles
     | _ -> [])

  | "REFERENCES", "out" ->
    (* Types and effects referenced by a function's signature *)
    (match node with
     | GnFn { gmod; gname; _ } ->
       let decl_opt = match Hashtbl.find_opt state.modules gmod with
         | None -> None
         | Some ms ->
           List.find_opt (fun d ->
             match d.Ast.decl_desc with
             | Ast.DeclFn { fn_name = n; _ } -> n = gname
             | _ -> false) ms.typed_ast
       in
       (match decl_opt with
        | None -> []
        | Some d ->
          (match d.Ast.decl_desc with
           | Ast.DeclFn { params; return_type; effects; _ } ->
             let refs = ref [] in
             let rec ty_ref t =
               match t with
               | Ast.TyName s -> refs := s :: !refs
               | Ast.TyApp (s, args) -> refs := s :: !refs; List.iter ty_ref args
               | Ast.TyTuple ts -> List.iter ty_ref ts
               | Ast.TyFun (ps, r, _) -> List.iter ty_ref ps; ty_ref r
             in
             List.iter (fun p -> ty_ref p.Ast.param_type) params;
             Option.iter ty_ref return_type;
             (match effects with
              | Some (Ast.Effects (ts, _)) -> List.iter ty_ref ts
              | _ -> ());
             List.filter_map (fun r_name ->
               let h = gname ^ ":ref:" ^ r_name in
               Some (GnExpr { gefn = gname; gehash = h;
                              geexpr = Ast.expr Ast.UnitLit;
                              gecall_site = Some r_name })
             ) (List.rev !refs)
           | _ -> []))
     | _ -> [])

  | "DATA_FLOW", "in" ->
    (* Trace backwards from an expression: Var → let-binding or parameter.
       v1: let-binding + parameter chains only. *)
    let data_flow_from gefn geexpr =
      match geexpr.Ast.desc with
      | Ast.Var var_name ->
        let params = fn_params_of prog gefn in
        let is_param = List.exists (fun p -> p.Ast.param_name = var_name) params in
        if is_param then
          [GnParam { gpfn   = gefn; gpidx = 0;
                     gpname = var_name;
                     gpty   = (match List.find_opt (fun p ->
                         p.Ast.param_name = var_name) params with
                       | Some p -> Printer.print_type_expr p.Ast.param_type
                       | None   -> "?") }]
        else begin
          let bindings = ref [] in
          (match fn_body_of prog gefn with
           | None -> ()
           | Some body -> collect_let_bindings bindings body);
          match List.assoc_opt var_name !bindings with
          | None -> []
          | Some value ->
            let gehash = bytes_to_hex (Node_encoding.encode_expr enc value) in
            [GnExpr { gefn; gehash; geexpr = value; gecall_site = None }]
        end
      | _ ->
        let gehash = bytes_to_hex (Node_encoding.encode_expr enc geexpr) in
        [GnExpr { gefn; gehash; geexpr; gecall_site = None }]
    in
    (match node with
     | GnExpr { gefn; geexpr; _ } -> data_flow_from gefn geexpr
     | _ -> [])

  | _ -> []

(* Execute one traversal step (including recursive follow) against a list of
   starting nodes, accumulating results into [collections]. *)
let rec execute_traverse state prog enc (collections : (string, json list) Hashtbl.t)
    step nodes =
  let edge      = match obj_get "edge"      step with String s -> s | _ -> "" in
  let direction = match obj_get "direction" step with String s -> s | _ -> "out" in
  let filter    = obj_get "filter" step in
  let collect   = match obj_get "collect"   step with String s -> s | _ -> "_" in
  let depth     = match obj_get "depth"     step with String s -> s | _ -> "single" in
  let follow    = obj_get "follow" step in
  let next = ref [] in
  let seen_hashes : (string, unit) Hashtbl.t = Hashtbl.create 8 in
  let add_node n =
    let id = match n with
      | GnFn   { ghash;  _ } -> ghash
      | GnParam { gpfn; gpname; _ } -> gpfn ^ ":param:" ^ gpname
      | GnCall  { gchash; _ } -> gchash
      | GnExpr  { gehash; _ } -> gehash
    in
    if not (Hashtbl.mem seen_hashes id) then begin
      Hashtbl.replace seen_hashes id ();
      next := n :: !next
    end
  in
  let traverse_from n =
    List.iter add_node (traverse_one_edge state prog enc edge direction filter n)
  in
  List.iter traverse_from nodes;
  (* Transitive: keep traversing until no new nodes found *)
  if depth = "transitive" then begin
    let frontier = ref !next in
    let keep_going = ref true in
    while !keep_going do
      let before = List.length !next in
      List.iter traverse_from !frontier;
      let after = List.length !next in
      if after = before then keep_going := false
      else
        frontier := List.filteri (fun i _ -> i < after - before)
                      (List.rev !next |> List.rev)
    done
  end;
  (* Collect result JSON *)
  let json_nodes = List.filter_map (fun n ->
      match json_of_gnode state n with
      | Object _ as j -> Some j
      | _ -> None
    ) (List.rev !next) in
  let existing = Option.value ~default:[] (Hashtbl.find_opt collections collect) in
  Hashtbl.replace collections collect (existing @ json_nodes);
  (* Apply follow to the collected nodes *)
  match follow with
  | Null | Object [] -> ()
  | follow_step -> execute_traverse state prog enc collections follow_step (List.rev !next)

(* ─── Query tool ─────────────────────────────────────────────────────────── *)

let dispatch_query state cmd =
  let op   = match obj_get "op"   cmd with String s -> s | _ -> "" in
  let args = obj_get "args" cmd in
  match op with
  | "signature" ->
    (match resolve_anchor state args with
     | Error d -> Cmd_halt d
     | Ok (_mod_name, fn_name, ms) ->
       match List.assoc_opt fn_name ms.type_env with
       | None ->
         Cmd_halt (Mcp_types.make_error "anchor/not-found"
           (Printf.sprintf "Function '%s' not found" fn_name))
       | Some scheme ->
         let sig_str = Format.asprintf "%a" Typechecker.pp_ty scheme.Typechecker.body in
         let node = Hashtbl.find_opt ms.decl_hashes fn_name in
         Cmd_success (Object [("signature", String sig_str);
                               ("node", match node with None -> Null | Some h -> String h)],
                      []))

  | "interface" ->
    let mod_name = match obj_get "module" args with String s -> s | _ -> "" in
    (match Hashtbl.find_opt state.modules mod_name with
     | None ->
       Cmd_halt (Mcp_types.make_error "anchor/not-found"
         (Printf.sprintf "Module '%s' not found" mod_name))
     | Some ms ->
       let lines = List.filter_map render_interface_decl ms.typed_ast in
       Cmd_success (Object [("interface", String (String.concat "\n" lines));
                             ("node", String ms.root_hash)],
                    []))

  | "effects" ->
    let mod_name = match obj_get "module" args with String s -> s | _ -> "" in
    (match Hashtbl.find_opt state.modules mod_name with
     | None ->
       Cmd_halt (Mcp_types.make_error "anchor/not-found"
         (Printf.sprintf "Module '%s' not found" mod_name))
     | Some ms ->
       let tbl : (string, string list) Hashtbl.t = Hashtbl.create 8 in
       let add eff fn =
         let xs = Option.value ~default:[] (Hashtbl.find_opt tbl eff) in
         if not (List.mem fn xs) then Hashtbl.replace tbl eff (xs @ [fn])
       in
       List.iter (fun decl ->
         match decl.Ast.decl_desc with
         | Ast.DeclFn { fn_name; effects; _ } ->
           let effs =
             match List.assoc_opt fn_name ms.type_env with
             | None        -> []
             | Some scheme ->
               (match fn_effect_row scheme.Typechecker.body with
                | Some row -> collect_row_effects row
                | None     -> effect_names_of_decl_annotation effects)
           in
           List.iter (fun e -> add e fn_name) effs
         | _ -> ()
       ) ms.typed_ast;
       let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b)
           (Hashtbl.fold (fun e fns acc ->
              (e, Array (List.map (fun n -> String n) fns)) :: acc) tbl []) in
       Cmd_success (Object [("effects", Object sorted);
                             ("node", String ms.root_hash)],
                    []))

  | "callers" ->
    (* Cross-module callers: find all call sites to (module, name) across every
       submitted module.  If module is omitted, the defining module is located
       first.  Each result carries a "module" field identifying the call site's
       owning module. *)
    let mod_name = match obj_get "module" args with String s -> s | _ -> "" in
    let fn_name  = match obj_get "name"   args with String s -> s | _ -> "" in
    (* Verify the function is defined in the specified (or any) module. *)
    let defining_mod =
      if mod_name <> "" then
        (match Hashtbl.find_opt state.modules mod_name with
         | None ->
           Error (Printf.sprintf "Module '%s' not found" mod_name)
         | Some ms ->
           let exists = List.exists (fun d ->
             match d.Ast.decl_desc with
             | Ast.DeclFn { fn_name = n; _ } -> n = fn_name
             | _ -> false) ms.typed_ast in
           if exists then Ok mod_name
           else Error (Printf.sprintf "Function '%s' not defined in module '%s'" fn_name mod_name))
      else
        match module_of_fn state fn_name with
        | Some mn -> Ok mn
        | None    -> Error (Printf.sprintf "Function '%s' not found in any submitted module" fn_name)
    in
    (match defining_mod with
     | Error msg ->
       Cmd_halt (Mcp_types.make_error "anchor/not-found" msg)
     | Ok _ ->
       let enc = Node_store.as_encoding_store state.node_store in
       (* Search every submitted module for call sites. *)
       let json_sites = Hashtbl.fold (fun caller_mod ms acc ->
           let sites = Typechecker.collect_callers_in ms.typed_ast fn_name in
           let module_sites = List.map (fun (s : Typechecker.caller_site) ->
               let caller_node = Hashtbl.find_opt ms.decl_hashes s.cs_caller in
               let call_site_node =
                 bytes_to_hex (Node_encoding.encode_expr enc s.cs_call_expr)
               in
               Object [
                 ("module",        String caller_mod);
                 ("caller_node",   match caller_node with None -> Null | Some h -> String h);
                 ("call_site_node", String call_site_node);
                 ("location",      Object [("line", Int s.cs_line); ("col", Int s.cs_col)]);
               ]) sites in
           acc @ module_sites
         ) state.modules [] in
       Cmd_success (Object [("callers", Array json_sites)], []))

  | "effect_flow" ->
    (* Trace all effects performed transitively from an entry function,
       annotating each with whether it is ultimately handled. *)
    (match resolve_anchor state args with
     | Error d -> Cmd_halt d
     | Ok (_mod_name, fn_name, ms) ->
       let prog = combined_program state in
       (try
          let unhandled = Typechecker.collect_unhandled_effects prog fn_name in
          let unhandled_set =
            List.map (fun (s : Typechecker.effect_site) ->
              (s.es_effect, s.es_function)) unhandled
          in
          let all_performs = collect_all_performs_in_body prog fn_name in
          let fn_node = Hashtbl.find_opt ms.decl_hashes fn_name in
          let json_performs = List.map (fun (eff, fn) ->
              let is_unhandled = List.mem (eff, fn) unhandled_set in
              let owner_ms_opt = match module_of_fn state fn with
                | None -> None | Some mn -> Hashtbl.find_opt state.modules mn
              in
              let fn_hash = match owner_ms_opt with
                | None -> Null
                | Some ms2 ->
                  (match Hashtbl.find_opt ms2.decl_hashes fn with
                   | None -> Null | Some h -> String h)
              in
              Object [("effect",  String eff);
                      ("function", String fn);
                      ("handled", Bool (not is_unhandled));
                      ("node",    fn_hash)]
            ) all_performs
          in
          Cmd_success (Object [
            ("entry",    String fn_name);
            ("node",     match fn_node with None -> Null | Some h -> String h);
            ("performs", Array json_performs)], [])
        with Failure msg ->
          Cmd_halt (Mcp_types.make_error "anchor/not-found" msg)))

  | "unhandled" ->
    (* Project-wide unhandled effects from a specified entry point.
       Returns structured data (as opposed to verify/effects which uses diagnostics). *)
    let anchor_json = obj_get "anchor" args in
    let (entry_point, prog) =
      match obj_get "name" anchor_json with
      | String ep ->
        (ep, combined_program state)
      | _ ->
        (* Try direct fields on args *)
        let ep = match obj_get "name" args with
          | String s -> s
          | _ -> ""
        in
        (ep, combined_program state)
    in
    if entry_point = "" then
      Cmd_halt (Mcp_types.make_error "anchor/not-found"
        "unhandled requires an anchor with a function name")
    else
      (try
         let sites = Typechecker.collect_unhandled_effects prog entry_point in
         let json_sites = List.map (fun (s : Typechecker.effect_site) ->
             let owner_mod = module_of_fn state s.es_function in
             let fn_hash = match owner_mod with
               | None -> Null
               | Some mn ->
                 (match Hashtbl.find_opt state.modules mn with
                  | None -> Null
                  | Some ms ->
                    (match Hashtbl.find_opt ms.decl_hashes s.es_function with
                     | None -> Null | Some h -> String h))
             in
             Object [
               ("effect",   String s.es_effect);
               ("function", String s.es_function);
               ("module",   match owner_mod with None -> Null | Some m -> String m);
               ("node",     fn_hash);
             ]
           ) sites in
         Cmd_success (Object [
           ("entry",  String entry_point);
           ("sites",  Array json_sites)], [])
       with Failure msg ->
         Cmd_halt (Mcp_types.make_error "anchor/not-found" msg))

  | "dependents" ->
    (* List all declarations across submitted modules that reference a named
       type or effect.  "name" is the only required arg. *)
    let name = match obj_get "name" args with String s -> s | _ -> "" in
    if name = "" then
      Cmd_halt (Mcp_types.make_error "anchor/not-found"
        "dependents requires a 'name' argument")
    else begin
      let deps = find_dependents_of state name in
      let json_deps = List.map (fun (mod_name, decl_n, kind) ->
          let hash = match Hashtbl.find_opt state.modules mod_name with
            | None -> Null
            | Some ms ->
              (match Hashtbl.find_opt ms.decl_hashes decl_n with
               | None -> Null | Some h -> String h)
          in
          Object [("module",      String mod_name);
                  ("declaration", String decl_n);
                  ("kind",        String kind);
                  ("node",        hash)]
        ) deps in
      Cmd_success (Object [("name", String name); ("dependents", Array json_deps)], [])
    end

  | "pattern_coverage" ->
    (* Check exhaustiveness of match expressions in the specified function. *)
    (match resolve_anchor state args with
     | Error d -> Cmd_halt d
     | Ok (_mod_name, fn_name, ms) ->
       let warnings = Typechecker.collect_match_warnings ms.typed_ast in
       let fn_warnings =
         List.filter (fun (w : Typechecker.match_warning) ->
           w.mw_fn_name = Some fn_name) warnings
       in
       let fn_node = Hashtbl.find_opt ms.decl_hashes fn_name in
       (* Produce one entry per match: exhaustive or not. *)
       let non_exhaustive = List.map (fun (w : Typechecker.match_warning) ->
           Object [("exhaustive", Bool false);
                   ("missing",    Array (List.map (fun s -> String s) w.mw_missing));
                   ("node",       match fn_node with None -> Null | Some h -> String h)]
         ) fn_warnings in
       (* If no warnings, report a single exhaustive result. *)
       let matches =
         if fn_warnings = [] then
           [Object [("exhaustive", Bool true); ("missing", Array []);
                    ("node", match fn_node with None -> Null | Some h -> String h)]]
         else non_exhaustive
       in
       Cmd_success (Object [
         ("function", String fn_name);
         ("node",     match fn_node with None -> Null | Some h -> String h);
         ("matches",  Array matches)], []))

  | "graph" ->
    (* Structured graph traversal anchored on a function.
       args: { anchor: {...}, traverse: [{edge, direction, filter?, collect, depth?,
               terminal_when?, follow?}] }
       Returns named collections of graph nodes. *)
    let anchor_json  = obj_get "anchor"   args in
    let traverse_arr = match obj_get "traverse" args with
      | Array steps -> steps | _ -> []
    in
    (match resolve_anchor state anchor_json with
     | Error d -> Cmd_halt d
     | Ok (gmod, gname, ms) ->
       let ghash = Option.value ~default:"" (Hashtbl.find_opt ms.decl_hashes gname) in
       let start  = [GnFn { gmod; gname; ghash }] in
       let prog   = combined_program state in
       let enc    = Node_store.as_encoding_store state.node_store in
       let cols : (string, json list) Hashtbl.t = Hashtbl.create 8 in
       List.iter (fun step ->
         execute_traverse state prog enc cols step start
       ) traverse_arr;
       let result_fields = Hashtbl.fold (fun k v acc ->
           (k, Array v) :: acc) cols []
       in
       Cmd_success (Object result_fields, []))

  | _ ->
    Cmd_halt (Mcp_types.make_error "command/unknown"
      (Printf.sprintf "Unknown query op '%s'" op))

(* ─── Write tool ─────────────────────────────────────────────────────────── *)

(* Encode an AST to root hash + per-decl hashes. *)
let encode_ast state ast =
  let enc  = Node_store.as_encoding_store state.node_store in
  let root = Node_encoding.encode_program enc ast in
  let root_hash = bytes_to_hex root in
  let enc2 = Node_store.as_encoding_store state.node_store in
  let decl_hashes = Hashtbl.create 16 in
  List.iter (fun decl ->
    match decl_name decl with
    | Some n -> Hashtbl.replace decl_hashes n
                  (bytes_to_hex (Node_encoding.encode_decl enc2 decl))
    | None   -> ()) ast;
  (root_hash, decl_hashes)

(* Build the {root, nodes} result object from encoded hashes. *)
let make_write_result root_hash decl_hashes =
  let nodes_list   = Hashtbl.fold (fun n h acc -> (n, String h) :: acc) decl_hashes [] in
  let nodes_sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) nodes_list in
  Object [("root", String root_hash); ("nodes", Object nodes_sorted)]

(* Store a module, persisting the source. *)
let store_module state name source ms =
  Hashtbl.replace state.modules name ms;
  Hashtbl.replace state.sources name source;
  save_module_registry state.store_dir state.sources

(* ─── Reusable verify library functions ──────────────────────────────────── *)
(* These are called both from dispatch_verify and from run_post_batch_verify
   (after write/transform mutations), ensuring identical diagnostic shapes. *)

let diags_of_match_warnings ms =
  List.map (fun (w : Typechecker.match_warning) ->
    let msg  = Printf.sprintf "Non-exhaustive match; missing: %s"
                 (String.concat ", " w.mw_missing) in
    let node = match w.mw_fn_name with
      | Some fn -> (match Hashtbl.find_opt ms.decl_hashes fn with
          | Some h -> h | None -> ms.root_hash)
      | None -> ms.root_hash
    in
    Mcp_types.make_warning ~node
      ~location:(make_loc w.mw_line w.mw_col)
      "match/non-exhaustive" msg
  ) (Typechecker.collect_match_warnings ms.typed_ast)

let diags_of_unused_items ms =
  List.map (fun (u : Typechecker.unused_item) ->
    let node = Hashtbl.find_opt ms.decl_hashes u.ui_name in
    let msg  = Printf.sprintf "Unused %s '%s'" u.ui_kind u.ui_name in
    Mcp_types.make_warning ?node
      ~location:(make_loc u.ui_line u.ui_col)
      "decl/unused" msg
  ) (Typechecker.collect_unused ms.typed_ast)

let diags_of_type_errors_project state =
  let prog = combined_program state in
  if prog = [] then []
  else
    match (try Ok (Typechecker.check_program prog) with Failure msg -> Error msg) with
    | Ok _      -> []
    | Error msg -> [Mcp_types.make_error "type/error" msg]

(* Run the post-batch verification checks on all modules in state.
   Handles "exhaustive", "unused", and "types" (project-wide).
   "effects" and "tail_calls" are skipped without an explicit entry point / anchor. *)
let run_post_batch_verify state verify_checks =
  let diags = ref [] in
  if List.mem "types" verify_checks then
    diags := !diags @ diags_of_type_errors_project state;
  Hashtbl.iter (fun _name ms ->
    if List.mem "exhaustive" verify_checks then
      diags := !diags @ diags_of_match_warnings ms;
    if List.mem "unused" verify_checks then
      diags := !diags @ diags_of_unused_items ms
  ) state.modules;
  !diags

let dispatch_write state cmd =
  let op   = match obj_get "op"   cmd with String s -> s | _ -> "" in
  let args = obj_get "args" cmd in
  match op with

  (* ── module: parse + elaborate + store a full working-form module ──────── *)
  | "module" ->
    let name   = match obj_get "name"   args with String s -> s | _ -> "" in
    let source = match obj_get "source" args with String s -> s | _ -> "" in
    if name = "" then
      Cmd_halt (Mcp_types.make_error "command/invalid"
        "module op requires a non-empty 'name' field")
    else
    let parse_result =
      try let tokens = Lexer.tokenize source in
          Ok (Parser.parse_program tokens)
      with Failure msg -> Error msg
    in
    (match parse_result with
     | Error msg -> Cmd_halt (Mcp_types.make_error "parse/error" msg)
     | Ok ast ->
       let (root_hash, decl_hashes) = encode_ast state ast in
       let result = make_write_result root_hash decl_hashes in
       let type_result =
         try Ok (Typechecker.check_program ast) with Failure msg -> Error msg
       in
       (match type_result with
        | Error msg ->
          let ms = { typed_ast = ast; type_env = []; effect_env = [];
                     root_hash; decl_hashes } in
          store_module state name source ms;
          Cmd_success (result, [Mcp_types.make_error "type/error" msg])
        | Ok (type_env, effect_env) ->
          let ms = { typed_ast = ast; type_env; effect_env; root_hash; decl_hashes } in
          store_module state name source ms;
          Cmd_success (result, [])))

  (* ── function: add / replace a single declaration in an existing module ── *)
  | "function" ->
    let module_name = match obj_get "module" args with String s -> s | _ -> "" in
    let source      = match obj_get "source" args with String s -> s | _ -> "" in
    (match Hashtbl.find_opt state.modules module_name with
     | None ->
       Cmd_halt (Mcp_types.make_error "anchor/not-found"
         (Printf.sprintf "Module '%s' not found" module_name))
     | Some ms ->
       let parse_result =
         try let tokens = Lexer.tokenize source in
             Ok (Parser.parse_program tokens)
         with Failure msg -> Error msg
       in
       (match parse_result with
        | Error msg -> Cmd_halt (Mcp_types.make_error "parse/error" msg)
        | Ok new_decls ->
          let new_names  = List.filter_map decl_name new_decls in
          let kept       = List.filter (fun d ->
            match decl_name d with
            | Some n -> not (List.mem n new_names)
            | None   -> true) ms.typed_ast in
          let combined   = kept @ new_decls in
          let (root_hash, decl_hashes) = encode_ast state combined in
          let result     = make_write_result root_hash decl_hashes in
          let type_result =
            try Ok (Typechecker.check_program combined) with Failure msg -> Error msg
          in
          let new_source = Printer.print_program combined in
          (match type_result with
           | Error msg ->
             let updated = { typed_ast = combined; root_hash; decl_hashes;
                             type_env = []; effect_env = [] } in
             store_module state module_name new_source updated;
             Cmd_success (result, [Mcp_types.make_error "type/error" msg])
           | Ok (type_env, effect_env) ->
             let updated = { typed_ast = combined; type_env; effect_env;
                             root_hash; decl_hashes } in
             store_module state module_name new_source updated;
             Cmd_success (result, []))))

  (* ── replace: substitute the declaration identified by anchor ─────────── *)
  | "replace" ->
    let anchor_json = obj_get "anchor" args in
    let source      = match obj_get "source" args with String s -> s | _ -> "" in
    (match resolve_anchor state anchor_json with
     | Error d -> Cmd_halt d
     | Ok (module_name, decl_n, ms) ->
       let parse_result =
         try let tokens = Lexer.tokenize source in
             Ok (Parser.parse_program tokens)
         with Failure msg -> Error msg
       in
       (match parse_result with
        | Error msg -> Cmd_halt (Mcp_types.make_error "parse/error" msg)
        | Ok new_decls ->
          let without_old = List.filter (fun d ->
            match decl_name d with
            | Some n -> n <> decl_n
            | None   -> true) ms.typed_ast in
          let new_ast  = without_old @ new_decls in
          let (root_hash, decl_hashes) = encode_ast state new_ast in
          let result   = make_write_result root_hash decl_hashes in
          let type_result =
            try Ok (Typechecker.check_program new_ast) with Failure msg -> Error msg
          in
          let new_source = Printer.print_program new_ast in
          (match type_result with
           | Error msg ->
             let updated = { typed_ast = new_ast; root_hash; decl_hashes;
                             type_env = []; effect_env = [] } in
             store_module state module_name new_source updated;
             Cmd_success (result, [Mcp_types.make_error "type/error" msg])
           | Ok (type_env, effect_env) ->
             let updated = { typed_ast = new_ast; type_env; effect_env;
                             root_hash; decl_hashes } in
             store_module state module_name new_source updated;
             Cmd_success (result, []))))

  | _ ->
    Cmd_halt (Mcp_types.make_error "command/unknown"
      (Printf.sprintf "Unknown write op '%s'" op))

(* ─── Transform tool ─────────────────────────────────────────────────────── *)

let dispatch_transform _state cmd =
  let op = match obj_get "op" cmd with String s -> s | _ -> "" in
  Cmd_halt (Mcp_types.make_error "command/unimplemented"
    (Printf.sprintf "Transform op '%s' is not yet implemented" op))

(* ─── Verify tool ────────────────────────────────────────────────────────── *)

let dispatch_verify state cmd =
  let op   = match obj_get "op"   cmd with String s -> s | _ -> "" in
  let args = obj_get "args" cmd in
  match op with
  | "types" ->
    (* Three modes:
       1. scope = "project"         → typecheck combined program of all modules
       2. scope = {"module": "..."}  → typecheck just that module
       3. source = "..."            → typecheck ad-hoc source string (legacy) *)
    let scope = obj_get "scope" args in
    (match scope with
     | String "project" ->
       let diags = diags_of_type_errors_project state in
       Cmd_success (Object [("scope", String "project")], diags)

     | Object fields when List.mem_assoc "module" fields ->
       let mod_name = match List.assoc "module" fields with String s -> s | _ -> "" in
       (match Hashtbl.find_opt state.modules mod_name with
        | None ->
          Cmd_halt (Mcp_types.make_error "anchor/not-found"
            (Printf.sprintf "Module '%s' not found" mod_name))
        | Some ms ->
          let diags =
            match (try Ok (Typechecker.check_program ms.typed_ast)
                   with Failure msg -> Error msg) with
            | Ok _      -> []
            | Error msg -> [Mcp_types.make_error ~node:ms.root_hash "type/error" msg]
          in
          Cmd_success (Object [("scope", Object [("module", String mod_name)])], diags))

     | _ ->
       (* Legacy: typecheck source string without updating state. *)
       let source = match obj_get "source" args with String s -> s | _ -> "" in
       (try
          let tokens = Lexer.tokenize source in
          let ast    = Parser.parse_program tokens in
          ignore (Typechecker.check_program ast);
          Cmd_success (Object [], [])
        with Failure msg ->
          Cmd_success (Object [], [Mcp_types.make_error "type/error" msg])))

  | "exhaustive" ->
    (* scope = "project" checks all modules; default (or module specified) checks one. *)
    let scope = obj_get "scope" args in
    (match scope with
     | String "project" ->
       let diags = Hashtbl.fold (fun _name ms acc ->
           acc @ diags_of_match_warnings ms
         ) state.modules [] in
       Cmd_success (Object [("scope", String "project")], diags)
     | _ ->
       let mod_name = match obj_get "module" args with String s -> s | _ -> "" in
       (match Hashtbl.find_opt state.modules mod_name with
        | None ->
          Cmd_halt (Mcp_types.make_error "anchor/not-found"
            (Printf.sprintf "Module '%s' not found" mod_name))
        | Some ms ->
          Cmd_success (Object [], diags_of_match_warnings ms)))

  | "effects" ->
    (* Resolve entry point via:
       1. args.anchor (hash or symbolic ref) → use resolve_anchor
       2. args.entry_point string (legacy)    → look up by name directly *)
    let (entry_point, prog) =
      match obj_get "anchor" args with
      | Null ->
        let mod_name    = match obj_get "module"      args with String s -> s | _ -> "" in
        let entry_point = match obj_get "entry_point" args with String s -> s | _ -> "" in
        let prog =
          if mod_name <> "" then
            let others = Hashtbl.fold (fun mn ms acc ->
              if mn = mod_name then acc else acc @ ms.typed_ast) state.modules [] in
            (match Hashtbl.find_opt state.modules mod_name with
             | None -> []
             | Some ms -> ms.typed_ast @ others)
          else
            combined_program state
        in
        (entry_point, prog)
      | anchor_json ->
        (match resolve_anchor state anchor_json with
         | Error _ -> ("", [])  (* will be caught below *)
         | Ok (_mod_name, fn_name, _ms) ->
           (fn_name, combined_program state))
    in
    (* Validate anchor resolution failure (only arises from hash path) *)
    let anchor_err =
      match obj_get "anchor" args with
      | Null -> None
      | anchor_json ->
        (match resolve_anchor state anchor_json with
         | Error d -> Some d
         | Ok _ -> None)
    in
    (match anchor_err with
     | Some d -> Cmd_halt d
     | None ->
       if prog = [] then
         Cmd_halt (Mcp_types.make_error "anchor/not-found" "No modules submitted")
       else if entry_point = "" then
         Cmd_halt (Mcp_types.make_error "command/invalid"
           "effects requires an entry_point or anchor")
       else
         (try
            let sites = Typechecker.collect_unhandled_effects prog entry_point in
            let diags = List.map (fun (s : Typechecker.effect_site) ->
                let owner_mod = module_of_fn state s.es_function in
                let node = match owner_mod with
                  | None -> None
                  | Some mn ->
                    (match Hashtbl.find_opt state.modules mn with
                     | None -> None
                     | Some ms -> Hashtbl.find_opt ms.decl_hashes s.es_function)
                in
                let msg = Printf.sprintf "Unhandled effect '%s' in function '%s'"
                    s.es_effect s.es_function in
                Mcp_types.make_warning ?node
                  ~location:(make_loc ?module_name:(module_of_fn state s.es_function)
                               s.es_line s.es_col)
                  "effect/unhandled" msg
              ) sites in
            Cmd_success (Object [], diags)
          with Failure msg ->
            Cmd_halt (Mcp_types.make_error "anchor/not-found" msg)))

  | "unused" ->
    (* scope = "project" checks all modules; default checks one. *)
    let scope = obj_get "scope" args in
    (match scope with
     | String "project" ->
       let diags = Hashtbl.fold (fun _name ms acc ->
           acc @ diags_of_unused_items ms
         ) state.modules [] in
       Cmd_success (Object [("scope", String "project")], diags)
     | _ ->
       let mod_name = match obj_get "module" args with String s -> s | _ -> "" in
       (match Hashtbl.find_opt state.modules mod_name with
        | None ->
          Cmd_halt (Mcp_types.make_error "anchor/not-found"
            (Printf.sprintf "Module '%s' not found" mod_name))
        | Some ms ->
          Cmd_success (Object [], diags_of_unused_items ms)))

  | "tail_calls" ->
    (* Verify that all recursive self-calls in the specified function are in
       tail position.  Args: { anchor: <hash-or-symbolic> } *)
    let anchor_json = obj_get "anchor" args in
    (match resolve_anchor state anchor_json with
     | Error d -> Cmd_halt d
     | Ok (mod_name, fn_name, ms) ->
       let prog = combined_program state in
       let violations = Typechecker.collect_tail_call_violations prog fn_name in
       let fn_node = Hashtbl.find_opt ms.decl_hashes fn_name in
       let diags = List.map (fun (v : Typechecker.tail_call_violation) ->
           let msg = Printf.sprintf
               "Recursive call to '%s' is not in tail position" v.tv_fn_name in
           Mcp_types.make_warning ?node:fn_node
             ~location:(make_loc ?module_name:(Some mod_name) v.tv_line v.tv_col)
             "tail_call/non-tail" msg
         ) violations in
       let ok = violations = [] in
       Cmd_success (Object [
         ("function",  String fn_name);
         ("module",    String mod_name);
         ("node",      match fn_node with None -> Null | Some h -> String h);
         ("all_tail",  Bool ok)],
         diags))

  | _ ->
    Cmd_halt (Mcp_types.make_error "command/unknown"
      (Printf.sprintf "Unknown verify op '%s'" op))

(* ─── Tool schemas ───────────────────────────────────────────────────────── *)

let command_schema ops =
  Object [
    ("type", String "array");
    ("items", Object [
       ("type", String "object");
       ("properties", Object [
          ("op",   Object [("type", String "string");
                           ("enum", Array (List.map (fun s -> String s) ops))]);
          ("args", Object [("type", String "object")]);
        ]);
       ("required", Array [String "op"; String "args"]);
     ]);
  ]

let batch_input_schema ops =
  Object [
    ("type", String "object");
    ("properties", Object [("commands", command_schema ops)]);
    ("required", Array [String "commands"]);
  ]

let tool_query_schema =
  Object [
    ("name", String "query");
    ("description", String
       "Read the IR graph: function signatures, module interfaces, effect maps, \
        caller graphs, effect flow, dependents, pattern coverage, and structured \
        graph traversal.  Accepts a batch of commands executed in order.");
    ("inputSchema", batch_input_schema
       ["signature"; "interface"; "effects"; "callers";
        "effect_flow"; "unhandled"; "dependents"; "pattern_coverage"; "graph"]);
  ]

let tool_write_schema =
  Object [
    ("name", String "write");
    ("description", String
       "Author working-form Axiom source; elaborate, typecheck, store, and \
        automatically verify the resulting project state.  Accepts a batch of \
        commands followed by an optional 'verify' array \
        (default: [\"types\",\"effects\",\"exhaustive\"]).  \
        Parse errors halt the batch; type/effect diagnostics are recoverable.");
    ("inputSchema", batch_input_schema ["module"; "function"; "replace"]);
  ]

let tool_transform_schema =
  Object [
    ("name", String "transform");
    ("description", String
       "Apply mechanical, deterministic refactors (rename, extract-function, \
        mock-effects, add-effect-logging, inline-handler).  Automatically \
        verifies the resulting state.  Accepts a batch of commands.");
    ("inputSchema", batch_input_schema []);
  ]

let tool_verify_schema =
  Object [
    ("name", String "verify");
    ("description", String
       "On-demand invariant checks: types (single source or project-wide), \
        exhaustive pattern matching, effect handling, unused declarations, \
        and tail-call position.  Accepts a batch of commands.");
    ("inputSchema", batch_input_schema
       ["types"; "exhaustive"; "effects"; "unused"; "tail_calls"]);
  ]

(* ─── Request handler ────────────────────────────────────────────────────── *)

let wrap_batch_result br =
  Object [
    ("content", Array [
       Object [("type", String "text");
               ("text", String (json_to_string (json_of_batch_response br)))]])
  ]

let handle state msg =
  let id   = obj_get "id" msg in
  let meth = match obj_get "method" msg with String s -> s | _ -> "" in
  Printf.eprintf "[mcp] method=%s\n%!" meth;
  match meth with
  | "initialize" ->
    state.initialized <- true;
    send (response id (Object [
      ("protocolVersion", String "2024-11-05");
      ("serverInfo", Object [("name", String "axiom"); ("version", String "0.1.0")]);
      ("capabilities", Object [("tools", Object [])]);
    ]))

  | "notifications/initialized" -> ()

  | "tools/list" ->
    send (response id (Object [
      ("tools", Array [tool_query_schema; tool_write_schema;
                       tool_transform_schema; tool_verify_schema])
    ]))

  | "tools/call" ->
    let params    = obj_get "params" msg in
    let tool_name = match obj_get "name" params with String s -> s | _ -> "" in
    let args      = obj_get "arguments" params in
    let cmds      = get_commands args in
    (match tool_name with
     | "query" ->
       send (response id (wrap_batch_result
           (batch_execute (dispatch_query state) cmds)))
     | "write" ->
       let verify_checks = match obj_get "verify" args with
         | Array vs -> List.filter_map (function String s -> Some s | _ -> None) vs
         | _        -> ["types"; "effects"; "exhaustive"]
       in
       let br        = batch_execute (dispatch_write state) cmds in
       let post_diags = run_post_batch_verify state verify_checks in
       let br'       = { br with br_diagnostics = br.br_diagnostics @ post_diags } in
       send (response id (wrap_batch_result br'))
     | "transform" ->
       send (response id (wrap_batch_result
           (batch_execute (dispatch_transform state) cmds)))
     | "verify" ->
       send (response id (wrap_batch_result
           (batch_execute (dispatch_verify state) cmds)))
     | _ ->
       send (error_response id (-32601) ("Unknown tool: " ^ tool_name)))

  | _ ->
    (match id with
     | Null -> ()
     | _    -> send (error_response id (-32601) ("Method not found: " ^ meth)))

(* ─── Entry point ────────────────────────────────────────────────────────── *)

let () =
  (* Determine a stable image directory.  Checked in order:
       1. AXIOM_IMAGE_DIR environment variable
       2. ~/.axiom/image
       3. ./.axiom/image  *)
  let store_dir =
    match Sys.getenv_opt "AXIOM_IMAGE_DIR" with
    | Some d -> d
    | None ->
      let base =
        match Sys.getenv_opt "HOME" with
        | Some h -> Filename.concat h ".axiom"
        | None   -> ".axiom"
      in
      Filename.concat base "image"
  in
  mkdir_p store_dir;
  let node_store = Node_store.open_store store_dir in
  let sources    = Hashtbl.create 16 in
  let modules    = Hashtbl.create 16 in
  (* Re-hydrate modules from the registry saved during the last run. *)
  let registry = load_module_registry store_dir in
  List.iter (fun (name, src) ->
    Hashtbl.replace sources name src;
    (try
       let tokens    = Lexer.tokenize src in
       let ast       = Parser.parse_program tokens in
       let (type_env, effect_env) = Typechecker.check_program ast in
       let enc       = Node_store.as_encoding_store node_store in
       let root      = Node_encoding.encode_program enc ast in
       let root_hash = bytes_to_hex root in
       let enc2      = Node_store.as_encoding_store node_store in
       let decl_hashes = Hashtbl.create 16 in
       List.iter (fun decl ->
         match decl_name decl with
         | Some n ->
           let h = Node_encoding.encode_decl enc2 decl in
           Hashtbl.replace decl_hashes n (bytes_to_hex h)
         | None -> ()
       ) ast;
       let ms = { typed_ast = ast; type_env; effect_env; root_hash; decl_hashes } in
       Hashtbl.replace modules name ms
     with Failure msg ->
       Printf.eprintf "[mcp] warning: could not rehydrate module '%s': %s\n%!" name msg)
  ) registry;
  let state = { initialized = false; modules; sources; node_store; store_dir } in
  (try
     while true do
       let line = String.trim (input_line stdin) in
       if line <> "" then
         match parse_json line with
         | Object _ as msg ->
           (try handle state msg
            with Json_error e ->
              Printf.eprintf "[mcp] handler error: %s\n%!" e)
         | _ -> Printf.eprintf "[mcp] non-object message ignored\n%!"
         | exception Json_error e ->
           Printf.eprintf "[mcp] JSON parse error: %s\n%!" e
     done
   with End_of_file -> ());
  Node_store.close_store state.node_store
