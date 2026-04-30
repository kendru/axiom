open Axiom_lib

(* Minimal JSON type *)
type json =
  | Null
  | Bool of bool
  | Int of int
  | String of string
  | Array of json list
  | Object of (string * json) list

(* JSON serializer *)
let rec json_to_string = function
  | Null -> "null"
  | Bool b -> if b then "true" else "false"
  | Int n -> string_of_int n
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

(* Minimal recursive-descent JSON parser *)
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
           (* skip 4 hex digits; replace with '?' for simplicity *)
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

(* JSON-RPC output helpers *)
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

(* Per-module state stored after a successful submit_module call *)
type module_state = {
  typed_ast  : Ast.program;
  type_env   : Typechecker.env;
  effect_env : Typechecker.effect_env;
}

type state = {
  mutable initialized : bool;
  modules             : (string, module_state) Hashtbl.t;
  node_store          : Node_store.t;
}

let format_type_error msg =
  Object [("message", String msg); ("line", Int 0); ("col", Int 0)]

let tool_submit_module_schema =
  Object [
    ("name", String "submit_module");
    ("description", String "Parse, typecheck, and store a working-form Axiom module");
    ("inputSchema", Object [
      ("type", String "object");
      ("properties", Object [
        ("source",      Object [("type", String "string")]);
        ("module_name", Object [("type", String "string")]);
      ]);
      ("required", Array [String "source"; String "module_name"]);
    ]);
  ]

let tool_verify_types_schema =
  Object [
    ("name", String "verify_types");
    ("description", String "Typecheck a working-form Axiom module and return any type errors; does not update server state");
    ("inputSchema", Object [
      ("type", String "object");
      ("properties", Object [
        ("source", Object [("type", String "string")]);
      ]);
      ("required", Array [String "source"]);
    ]);
  ]

let tool_query_signature_schema =
  Object [
    ("name", String "query_signature");
    ("description", String "Return the type and effect signature of a named function from a previously submitted module");
    ("inputSchema", Object [
      ("type", String "object");
      ("properties", Object [
        ("module_name",   Object [("type", String "string")]);
        ("function_name", Object [("type", String "string")]);
      ]);
      ("required", Array [String "module_name"; String "function_name"]);
    ]);
  ]

let tool_query_interface_schema =
  Object [
    ("name", String "query_interface");
    ("description", String "Return the public API of a previously submitted module: pub type aliases, pub effect declarations, and pub function signatures");
    ("inputSchema", Object [
      ("type", String "object");
      ("properties", Object [
        ("module_name", Object [("type", String "string")]);
      ]);
      ("required", Array [String "module_name"]);
    ]);
  ]

let handle_submit_module state args =
  let source      = match obj_get "source"      args with String s -> s | _ -> "" in
  let module_name = match obj_get "module_name" args with String s -> s | _ -> "" in
  try
    let tokens   = Lexer.tokenize source in
    let ast      = Parser.parse_program tokens in
    let (type_env, effect_env) = Typechecker.check_program ast in
    let enc      = Node_store.as_encoding_store state.node_store in
    let root     = Node_encoding.encode_program enc ast in
    let hash_hex = bytes_to_hex root in
    Hashtbl.replace state.modules module_name { typed_ast = ast; type_env; effect_env };
    Object [("ok", Bool true); ("hash", String hash_hex)]
  with Failure msg ->
    Object [("ok", Bool false); ("error", format_type_error msg)]

let handle_verify_types args =
  let source = match obj_get "source" args with String s -> s | _ -> "" in
  try
    let tokens = Lexer.tokenize source in
    let ast    = Parser.parse_program tokens in
    ignore (Typechecker.check_program ast);
    Object [("errors", Array [])]
  with Failure msg ->
    Object [("errors", Array [format_type_error msg])]

let handle_query_signature state args =
  let module_name   = match obj_get "module_name"   args with String s -> s | _ -> "" in
  let function_name = match obj_get "function_name" args with String s -> s | _ -> "" in
  match Hashtbl.find_opt state.modules module_name with
  | None ->
    Object [("ok", Bool false);
            ("error", String (Printf.sprintf "Module '%s' not found" module_name))]
  | Some ms ->
    match List.assoc_opt function_name ms.type_env with
    | None ->
      Object [("ok", Bool false);
              ("error", String (Printf.sprintf "Function '%s' not found in module '%s'" function_name module_name))]
    | Some scheme ->
      let sig_str = Format.asprintf "%a" Typechecker.pp_ty scheme.body in
      Object [("ok", Bool true); ("signature", String sig_str)]

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
      | _ -> ""
    in
    Some ("pub fn " ^ fn_name ^ tp_s ^ "(" ^
          String.concat ", " (List.map Printer.print_param params) ^ ")" ^ ann)
  | Ast.DeclType { pub = true; _ } ->
    Some (Printer.print_decl decl)
  | Ast.DeclEffect { pub = true; _ } ->
    Some (Printer.print_decl decl)
  | _ -> None

let handle_query_interface state args =
  let module_name = match obj_get "module_name" args with String s -> s | _ -> "" in
  match Hashtbl.find_opt state.modules module_name with
  | None ->
    Object [("ok", Bool false);
            ("error", String (Printf.sprintf "Module '%s' not found" module_name))]
  | Some ms ->
    let lines = List.filter_map render_interface_decl ms.typed_ast in
    let iface = String.concat "\n" lines in
    Object [("interface", String iface)]

let handle state msg =
  let id     = obj_get "id" msg in
  let meth   = match obj_get "method" msg with String s -> s | _ -> "" in
  Printf.eprintf "[mcp] method=%s\n%!" meth;
  match meth with
  | "initialize" ->
    state.initialized <- true;
    send (response id (Object [
      ("protocolVersion", String "2024-11-05");
      ("serverInfo", Object [("name", String "axiom"); ("version", String "0.1.0")]);
      ("capabilities", Object [("tools", Object [])]);
    ]))
  | "notifications/initialized" ->
    ()
  | "tools/list" ->
    send (response id (Object [("tools", Array [tool_submit_module_schema; tool_verify_types_schema; tool_query_signature_schema; tool_query_interface_schema])]))
  | "tools/call" ->
    let params    = obj_get "params" msg in
    let tool_name = match obj_get "name" params with String s -> s | _ -> "" in
    let args      = obj_get "arguments" params in
    (match tool_name with
     | "submit_module" ->
       let result = handle_submit_module state args in
       send (response id (Object [
         ("content", Array [
           Object [("type", String "text"); ("text", String (json_to_string result))]
         ])
       ]))
     | "verify_types" ->
       let result = handle_verify_types args in
       send (response id (Object [
         ("content", Array [
           Object [("type", String "text"); ("text", String (json_to_string result))]
         ])
       ]))
     | "query_signature" ->
       let result = handle_query_signature state args in
       send (response id (Object [
         ("content", Array [
           Object [("type", String "text"); ("text", String (json_to_string result))]
         ])
       ]))
     | "query_interface" ->
       let result = handle_query_interface state args in
       send (response id (Object [
         ("content", Array [
           Object [("type", String "text"); ("text", String (json_to_string result))]
         ])
       ]))
     | _ ->
       send (error_response id (-32601) ("Unknown tool: " ^ tool_name)))
  | _ ->
    (match id with
     | Null -> ()
     | _ -> send (error_response id (-32601) ("Method not found: " ^ meth)))

let () =
  let store_dir  =
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
