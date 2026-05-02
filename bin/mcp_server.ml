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
  node_store          : Node_store.t;
}

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

(* ─── Type pretty-printing helpers (carried over from old implementation) ── *)

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
   (symbolic) and resolves to (module_name, decl_name, module_state). *)
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
    (match Hashtbl.find_opt state.modules mod_name with
     | None ->
       Error (Mcp_types.make_error "anchor/not-found"
         (Printf.sprintf "Module '%s' not found" mod_name))
     | Some ms -> Ok (mod_name, fn_name, ms))

(* ─── Decl-name extraction (for hash look-up) ────────────────────────────── *)

let decl_name decl =
  match decl.Ast.decl_desc with
  | Ast.DeclFn     { fn_name;     _ } -> Some fn_name
  | Ast.DeclType   { type_name;   _ } -> Some type_name
  | Ast.DeclEffect { effect_name; _ } -> Some effect_name
  | Ast.DeclModule { module_name; _ } -> Some module_name
  | _                                  -> None

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
    let mod_name = match obj_get "module" args with String s -> s | _ -> "" in
    let fn_name  = match obj_get "name"   args with String s -> s | _ -> "" in
    (match Hashtbl.find_opt state.modules mod_name with
     | None ->
       Cmd_halt (Mcp_types.make_error "anchor/not-found"
         (Printf.sprintf "Module '%s' not found" mod_name))
     | Some ms ->
       (match Typechecker.collect_callers ms.typed_ast fn_name with
        | Error msg ->
          Cmd_halt (Mcp_types.make_error "anchor/not-found" msg)
        | Ok sites ->
          let enc = Node_store.as_encoding_store state.node_store in
          let json_sites = List.map (fun (s : Typechecker.caller_site) ->
              let caller_node = Hashtbl.find_opt ms.decl_hashes s.cs_caller in
              let call_site_node =
                bytes_to_hex (Node_encoding.encode_expr enc s.cs_call_expr)
              in
              Object [
                ("caller_node",
                 match caller_node with None -> Null | Some h -> String h);
                ("call_site_node", String call_site_node);
                ("location", Object [("line", Int s.cs_line); ("col", Int s.cs_col)]);
              ]) sites in
          Cmd_success (Object [("callers", Array json_sites)], [])))

  | _ ->
    Cmd_halt (Mcp_types.make_error "command/unknown"
      (Printf.sprintf "Unknown query op '%s'" op))

(* ─── Write tool ─────────────────────────────────────────────────────────── *)

(* After storing a module, run all verify passes and collect diagnostics. *)
let post_write_verify ms =
  let diags = ref [] in
  (* match exhaustiveness warnings *)
  List.iter (fun (w : Typechecker.match_warning) ->
    let msg = Printf.sprintf "Non-exhaustive match; missing: %s"
        (String.concat ", " w.mw_missing) in
    let node = match w.mw_fn_name with
      | Some fn -> (match Hashtbl.find_opt ms.decl_hashes fn with
          | Some h -> h | None -> ms.root_hash)
      | None -> ms.root_hash
    in
    diags := !diags @ [Mcp_types.make_warning
        ~node
        ~location:(make_loc w.mw_line w.mw_col)
        "match/non-exhaustive" msg]
  ) (Typechecker.collect_match_warnings ms.typed_ast);
  (* unused declarations *)
  List.iter (fun (u : Typechecker.unused_item) ->
    let node = Hashtbl.find_opt ms.decl_hashes u.ui_name in
    let msg = Printf.sprintf "Unused %s '%s'" u.ui_kind u.ui_name in
    diags := !diags @ [Mcp_types.make_warning
        ?node
        ~location:(make_loc u.ui_line u.ui_col)
        "decl/unused" msg]
  ) (Typechecker.collect_unused ms.typed_ast);
  !diags

let dispatch_write state cmd =
  let op   = match obj_get "op"   cmd with String s -> s | _ -> "" in
  let args = obj_get "args" cmd in
  match op with
  | "submit_module" ->
    let source      = match obj_get "source"      args with String s -> s | _ -> "" in
    let module_name = match obj_get "module_name" args with String s -> s | _ -> "" in
    (try
       let tokens    = Lexer.tokenize source in
       let ast       = Parser.parse_program tokens in
       let (type_env, effect_env) = Typechecker.check_program ast in
       let enc       = Node_store.as_encoding_store state.node_store in
       let root      = Node_encoding.encode_program enc ast in
       let root_hash = bytes_to_hex root in
       let enc2      = Node_store.as_encoding_store state.node_store in
       let decl_hashes = Hashtbl.create 16 in
       List.iter (fun decl ->
         match decl_name decl with
         | Some n ->
           let h = Node_encoding.encode_decl enc2 decl in
           Hashtbl.replace decl_hashes n (bytes_to_hex h)
         | None -> ()
       ) ast;
       let ms = { typed_ast = ast; type_env; effect_env; root_hash; decl_hashes } in
       Hashtbl.replace state.modules module_name ms;
       let verify_diags = post_write_verify ms in
       let nodes_list = Hashtbl.fold (fun name hash acc ->
           (name, String hash) :: acc) decl_hashes [] in
       let nodes_sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) nodes_list in
       Cmd_success (Object [
           ("root",  String root_hash);
           ("nodes", Object nodes_sorted);
         ], verify_diags)
     with Failure msg ->
       Cmd_halt (Mcp_types.make_error "parse/error" msg))

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
    let source = match obj_get "source" args with String s -> s | _ -> "" in
    (try
       let tokens = Lexer.tokenize source in
       let ast    = Parser.parse_program tokens in
       ignore (Typechecker.check_program ast);
       Cmd_success (Object [], [])
     with Failure msg ->
       let diag = Mcp_types.make_error "type/error" msg in
       Cmd_success (Object [], [diag]))

  | "exhaustive" ->
    let mod_name = match obj_get "module" args with String s -> s | _ -> "" in
    (match Hashtbl.find_opt state.modules mod_name with
     | None ->
       Cmd_halt (Mcp_types.make_error "anchor/not-found"
         (Printf.sprintf "Module '%s' not found" mod_name))
     | Some ms ->
       let warnings = Typechecker.collect_match_warnings ms.typed_ast in
       let diags = List.map (fun (w : Typechecker.match_warning) ->
           let msg = Printf.sprintf "Non-exhaustive match; missing: %s"
               (String.concat ", " w.mw_missing) in
           let node = match w.mw_fn_name with
             | Some fn -> (match Hashtbl.find_opt ms.decl_hashes fn with
                 | Some h -> h | None -> ms.root_hash)
             | None -> ms.root_hash
           in
           Mcp_types.make_warning
             ~node
             ~location:(make_loc w.mw_line w.mw_col)
             "match/non-exhaustive" msg
         ) warnings in
       Cmd_success (Object [], diags))

  | "effects" ->
    let mod_name   = match obj_get "module"      args with String s -> s | _ -> "" in
    let entry_point = match obj_get "entry_point" args with String s -> s | _ -> "" in
    (match Hashtbl.find_opt state.modules mod_name with
     | None ->
       Cmd_halt (Mcp_types.make_error "anchor/not-found"
         (Printf.sprintf "Module '%s' not found" mod_name))
     | Some ms ->
       (try
          let sites = Typechecker.collect_unhandled_effects ms.typed_ast entry_point in
          let diags = List.map (fun (s : Typechecker.effect_site) ->
              let node = Hashtbl.find_opt ms.decl_hashes s.es_function in
              let msg  = Printf.sprintf "Unhandled effect '%s' in function '%s'"
                  s.es_effect s.es_function in
              Mcp_types.make_warning ?node
                ~location:(make_loc s.es_line s.es_col)
                "effect/unhandled" msg
            ) sites in
          Cmd_success (Object [], diags)
        with Failure msg ->
          Cmd_halt (Mcp_types.make_error "anchor/not-found" msg)))

  | "unused" ->
    let mod_name = match obj_get "module" args with String s -> s | _ -> "" in
    (match Hashtbl.find_opt state.modules mod_name with
     | None ->
       Cmd_halt (Mcp_types.make_error "anchor/not-found"
         (Printf.sprintf "Module '%s' not found" mod_name))
     | Some ms ->
       let items = Typechecker.collect_unused ms.typed_ast in
       let diags = List.map (fun (u : Typechecker.unused_item) ->
           let node = Hashtbl.find_opt ms.decl_hashes u.ui_name in
           let msg  = Printf.sprintf "Unused %s '%s'" u.ui_kind u.ui_name in
           Mcp_types.make_warning ?node
             ~location:(make_loc u.ui_line u.ui_col)
             "decl/unused" msg
         ) items in
       Cmd_success (Object [], diags))

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
        and caller graphs.  Accepts a batch of commands executed in order.");
    ("inputSchema", batch_input_schema ["signature"; "interface"; "effects"; "callers"]);
  ]

let tool_write_schema =
  Object [
    ("name", String "write");
    ("description", String
       "Submit working-form Axiom source; elaborate, typecheck, store, and \
        automatically verify the resulting state.  Accepts a batch of commands.");
    ("inputSchema", batch_input_schema ["submit_module"]);
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
       "On-demand invariant checks: types, exhaustive pattern matching, effect \
        handling, and unused declarations.  Accepts a batch of commands.");
    ("inputSchema", batch_input_schema ["types"; "exhaustive"; "effects"; "unused"]);
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
       send (response id (wrap_batch_result
           (batch_execute (dispatch_write state) cmds)))
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
  let store_dir =
    let path = Filename.temp_file "axiom_mcp_" "_dir" in
    Sys.remove path;
    Unix.mkdir path 0o700;
    path
  in
  let node_store = Node_store.open_store store_dir in
  let state      = { initialized = false; modules = Hashtbl.create 16; node_store } in
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
