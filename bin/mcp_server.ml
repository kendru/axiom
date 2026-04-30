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

(* Server state — will grow as tools are added in later issues *)
type state = {
  mutable initialized: bool;
}

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
    send (response id (Object [("tools", Array [])]))
  | "tools/call" ->
    send (error_response id (-32601) "Method not found")
  | _ ->
    (match id with
     | Null -> ()
     | _ -> send (error_response id (-32601) ("Method not found: " ^ meth)))

let () =
  let state = { initialized = false } in
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
   with End_of_file -> ())
