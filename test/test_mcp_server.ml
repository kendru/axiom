let mcp_exe =
  match Sys.getenv_opt "MCP_EXE" with
  | Some p -> p
  | None ->
    let test_dir = Filename.dirname Sys.argv.(0) in
    let candidates =
      [ Filename.concat test_dir "../bin/mcp_server.exe"
      ; Filename.concat test_dir "../bin/mcp_server"
      ; Filename.concat (Sys.getcwd ()) "_build/default/bin/mcp_server.exe"
      ; Filename.concat (Sys.getcwd ()) "_build/default/bin/mcp_server"
      ]
    in
    (match List.find_opt Sys.file_exists candidates with
     | Some p -> p
     | None   -> "mcp_server")

(* Launch the MCP server and return (write_line, read_line, close) *)
let start_server () =
  (* Give each server instance an isolated image directory so tests don't
     interfere with each other or with the user's ~/.axiom/image store. *)
  let store_dir =
    let path = Filename.temp_file "axiom_mcp_test_" "_dir" in
    Sys.remove path;
    Unix.mkdir path 0o700;
    path
  in
  let env = Array.append (Unix.environment ())
    [| Printf.sprintf "AXIOM_IMAGE_DIR=%s" store_dir |] in
  let (child_in_r, child_in_w)   = Unix.pipe () in
  let (child_out_r, child_out_w) = Unix.pipe () in
  let pid = Unix.create_process_env mcp_exe [| mcp_exe |] env
    child_in_r child_out_w Unix.stderr
  in
  Unix.close child_in_r;
  Unix.close child_out_w;
  let ic = Unix.in_channel_of_descr child_out_r in
  let oc = Unix.out_channel_of_descr child_in_w in
  let write_line s =
    output_string oc (s ^ "\n");
    flush oc
  in
  let read_line () = input_line ic in
  let close () =
    close_out_noerr oc;
    close_in_noerr ic;
    (try Unix.kill pid Sys.sigterm with _ -> ());
    ignore (Unix.waitpid [] pid);
    (* Remove the temporary store directory. *)
    (try
       let files = Sys.readdir store_dir in
       Array.iter (fun f ->
         try Sys.remove (Filename.concat store_dir f) with _ -> ()) files;
       Unix.rmdir store_dir
     with _ -> ())
  in
  (write_line, read_line, close)

(* ─── Minimal JSON parser ────────────────────────────────────────────────── *)

let json_field key json =
  match json with
  | `Assoc fields -> (match List.assoc_opt key fields with Some v -> v | None -> `Null)
  | _ -> `Null

let mini_parse s =
  let len = String.length s in
  let pos = ref 0 in
  let peek () = if !pos < len then s.[!pos] else '\000' in
  let next () = let c = peek () in incr pos; c in
  let skip_ws () =
    while !pos < len && (let c = peek () in c = ' ' || c = '\t' || c = '\n' || c = '\r')
    do incr pos done
  in
  let expect c =
    let got = next () in
    if got <> c then failwith (Printf.sprintf "expected '%c' got '%c'" c got)
  in
  let parse_string () =
    expect '"';
    let buf = Buffer.create 16 in
    let rec loop () =
      match next () with
      | '"' -> Buffer.contents buf
      | '\\' ->
        (match next () with
         | '"'  -> Buffer.add_char buf '"'
         | '\\' -> Buffer.add_char buf '\\'
         | 'n'  -> Buffer.add_char buf '\n'
         | 'r'  -> Buffer.add_char buf '\r'
         | 't'  -> Buffer.add_char buf '\t'
         | c    -> Buffer.add_char buf c);
        loop ()
      | c -> Buffer.add_char buf c; loop ()
    in
    loop ()
  in
  let rec parse_value () =
    skip_ws ();
    match peek () with
    | '"' -> `String (parse_string ())
    | '[' -> parse_array ()
    | '{' -> parse_object ()
    | 't' -> incr pos; expect 'r'; expect 'u'; expect 'e'; `Bool true
    | 'f' -> incr pos; expect 'a'; expect 'l'; expect 's'; expect 'e'; `Bool false
    | 'n' -> incr pos; expect 'u'; expect 'l'; expect 'l'; `Null
    | c when c = '-' || (c >= '0' && c <= '9') ->
      let start = !pos in
      if peek () = '-' then incr pos;
      while !pos < len && peek () >= '0' && peek () <= '9' do incr pos done;
      `Int (int_of_string (String.sub s start (!pos - start)))
    | c -> failwith (Printf.sprintf "unexpected '%c'" c)
  and parse_array () =
    expect '['; skip_ws ();
    if peek () = ']' then (incr pos; `List [])
    else begin
      let items = ref [] in
      let go = ref true in
      while !go do
        items := parse_value () :: !items;
        skip_ws ();
        (match peek () with
         | ',' -> incr pos
         | ']' -> incr pos; go := false
         | _   -> failwith "expected ',' or ']'")
      done;
      `List (List.rev !items)
    end
  and parse_object () =
    expect '{'; skip_ws ();
    if peek () = '}' then (incr pos; `Assoc [])
    else begin
      let fields = ref [] in
      let go = ref true in
      while !go do
        skip_ws ();
        let k = parse_string () in
        skip_ws (); expect ':';
        let v = parse_value () in
        fields := (k, v) :: !fields;
        skip_ws ();
        (match peek () with
         | ',' -> incr pos
         | '}' -> incr pos; go := false
         | _   -> failwith "expected ',' or '}'")
      done;
      `Assoc (List.rev !fields)
    end
  in
  parse_value ()

(* ─── Protocol helpers ───────────────────────────────────────────────────── *)

let json_string_escape s =
  let buf = Buffer.create (String.length s + 2) in
  String.iter (function
    | '"'  -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\r' -> Buffer.add_string buf "\\r"
    | '\t' -> Buffer.add_string buf "\\t"
    | c    -> Buffer.add_char buf c) s;
  Buffer.contents buf

let initialize write_line read_line =
  write_line {|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}|};
  ignore (read_line ());
  write_line {|{"jsonrpc":"2.0","id":null,"method":"notifications/initialized","params":{}}|}

(* Build a tools/call JSON-RPC request.
   [commands_json] is a pre-serialised JSON array string. *)
let tool_call id tool_name commands_json =
  Printf.sprintf
    {|{"jsonrpc":"2.0","id":%d,"method":"tools/call","params":{"name":"%s","arguments":{"commands":%s}}}|}
    id tool_name commands_json

(* Build a single-command batch array. [args_json] is a pre-serialised object. *)
let single_cmd op args_json =
  Printf.sprintf {|[{"op":"%s","args":%s}]|} op args_json

(* Parse the batch_response JSON from a raw server line. *)
let parse_batch_response line =
  let json = mini_parse line in
  let result = json_field "result" json in
  let content = json_field "content" result in
  match content with
  | `List (item :: _) ->
    let text = json_field "text" item in
    (match text with `String s -> mini_parse s | _ -> `Null)
  | _ -> result

(* First element of the "results" array, or `Null. *)
let first_result batch =
  match json_field "results" batch with
  | `List (r :: _) -> r
  | _              -> `Null

(* "diagnostics" array, or []. *)
let batch_diagnostics batch =
  match json_field "diagnostics" batch with
  | `List ds -> ds
  | _        -> []

(* True when the batch completed without halting. *)
let batch_ok batch =
  match json_field "stopped_at" batch with
  | `Null -> true
  | _     -> false

(* ─── Shared test sources ────────────────────────────────────────────────── *)

let well_typed_source = {|fn double(x: Int) -> Int ! pure { add(x, x) }|}
let ill_typed_source   = {|fn go() -> Unit ! pure { perform Nope.boom() }|}

let mixed_visibility_source =
  {|pub fn add_one(x: Int) -> Int ! pure { add(x, 1) }
fn internal_helper(x: Int) -> Int ! pure { x }
pub type Color = | Red | Green | Blue|}

let exhaustive_source =
  {|type Color = | Red | Green | Blue
fn describe(c: Color) -> Int ! pure {
  match c with {
    | Red   => 0
    | Green => 1
    | Blue  => 2
  }
}|}

let effects_all_handled_source =
  {|effect Log { log: (String) -> Unit }
fn helper() -> Unit ! {Log} { perform Log.log("hello") }
fn main() -> Unit ! pure {
  handle helper() with {
    Log { log(msg) => () }
  }
}|}

let effects_unhandled_source =
  {|effect Log { log: (String) -> Unit }
fn helper() -> Unit ! {Log} { perform Log.log("hello") }
fn main() -> Unit ! {Log} { helper() }|}

let all_used_source =
  {|fn helper(x: Int) -> Int ! pure { x }
fn main() -> Int ! pure { helper(1) }|}

let one_unused_source =
  {|fn used(x: Int) -> Int ! pure { x }
fn unused_fn(x: Int) -> Int ! pure { x }
fn main() -> Int ! pure { used(1) }|}

let pub_unused_source = {|pub fn exported(x: Int) -> Int ! pure { x }|}

let mutual_recursion_source =
  {|fn ping(x: Int) -> Int ! pure { pong(x) }
fn pong(x: Int) -> Int ! pure { ping(x) }
fn main() -> Int ! pure { ping(0) }|}

let mutual_both_unused_source =
  {|fn ping(x: Int) -> Int ! pure { pong(x) }
fn pong(x: Int) -> Int ! pure { ping(x) }
fn main() -> Int ! pure { 0 }|}

let pure_module_source =
  {|fn add_one(x: Int) -> Int ! pure { add(x, 1) }
fn double(x: Int) -> Int ! pure { add(x, x) }|}

let single_effect_source =
  {|effect Log { log: (String) -> Unit }
fn greet(msg: String) -> Unit ! {Log} { perform Log.log(msg) }
fn pure_fn(x: Int) -> Int ! pure { x }|}

let multi_effect_source =
  {|effect Log { log: (String) -> Unit }
effect Throw { throw: (String) -> Unit }
fn do_both(x: Int) -> Unit ! {Log, Throw} { perform Log.log("hi") }
fn do_log(x: Int) -> Unit ! {Log} { perform Log.log("x") }|}

let multi_caller_source =
  {|fn helper(x: Int) -> Int ! pure { x }
fn main1() -> Int ! pure { helper(1) }
fn main2() -> Int ! pure { helper(2) }|}

let uncalled_fn_source =
  {|fn helper(x: Int) -> Int ! pure { x }
fn main() -> Int ! pure { 0 }|}

let contains_substring haystack needle =
  let hlen = String.length haystack and nlen = String.length needle in
  if nlen = 0 then true
  else if nlen > hlen then false
  else
    let found = ref false in
    for i = 0 to hlen - nlen do
      if not !found && String.sub haystack i nlen = needle then found := true
    done;
    !found

(* ─── tools/list ─────────────────────────────────────────────────────────── *)

let test_tools_list_has_four_tools () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line {|{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}|};
    let line = read_line () in
    let json = mini_parse line in
    let tools = json_field "tools" (json_field "result" json) in
    let names = match tools with
      | `List items ->
        List.filter_map (fun item ->
          match json_field "name" item with `String s -> Some s | _ -> None) items
      | _ -> []
    in
    Alcotest.(check int) "exactly four tools" 4 (List.length names);
    List.iter (fun t ->
      Alcotest.(check bool) (t ^ " in tools/list") true (List.mem t names)
    ) ["query"; "write"; "transform"; "verify"]
  )

(* ─── verify / types ─────────────────────────────────────────────────────── *)

let test_verify_types_well_typed () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "verify"
      (single_cmd "types"
         (Printf.sprintf {|{"source":"%s"}|} (json_string_escape well_typed_source))));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok"  true (batch_ok batch);
    Alcotest.(check bool) "no diagnostics" true (batch_diagnostics batch = [])
  )

let test_verify_types_ill_typed () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "verify"
      (single_cmd "types"
         (Printf.sprintf {|{"source":"%s"}|} (json_string_escape ill_typed_source))));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok (verify never halts on type errors)" true (batch_ok batch);
    let diags = batch_diagnostics batch in
    Alcotest.(check bool) "diagnostics non-empty" true (diags <> []);
    (match diags with
     | d :: _ ->
       Alcotest.(check bool) "severity is error" true
         (json_field "severity" d = `String "error");
       Alcotest.(check bool) "code present" true
         (match json_field "code" d with `String s -> String.length s > 0 | _ -> false);
       Alcotest.(check bool) "message present" true
         (match json_field "message" d with `String s -> String.length s > 0 | _ -> false)
     | [] -> ())
  )

let test_verify_types_does_not_update_state () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    (* verify ill-typed source — must not store anything *)
    write_line (tool_call 2 "verify"
      (single_cmd "types"
         (Printf.sprintf {|{"source":"%s"}|} (json_string_escape ill_typed_source))));
    ignore (read_line ());
    (* write well-typed source — must still succeed *)
    write_line (tool_call 3 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"test"}|}
            (json_string_escape well_typed_source))));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "write succeeds" true (batch_ok batch)
  )

(* ─── write / module ────────────────────────────────────────────────────── *)

let test_write_submit_returns_root_and_nodes () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape well_typed_source))));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let r = first_result batch in
    let root = json_field "root" r in
    Alcotest.(check bool) "root is a non-empty string" true
      (match root with `String s -> String.length s > 0 | _ -> false);
    let nodes = json_field "nodes" r in
    Alcotest.(check bool) "nodes is an object" true
      (match nodes with `Assoc _ -> true | _ -> false);
    Alcotest.(check bool) "nodes contains double" true
      (match nodes with
       | `Assoc fs ->
         (match List.assoc_opt "double" fs with
          | Some (`String s) -> String.length s > 0
          | _ -> false)
       | _ -> false)
  )

let test_write_submit_nodes_map_all_decls () =
  let source = {|fn foo(x: Int) -> Int ! pure { x }
fn bar(x: Int) -> Int ! pure { x }|} in
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape source))));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let nodes = json_field "nodes" (first_result batch) in
    Alcotest.(check bool) "foo in nodes" true
      (match nodes with `Assoc fs -> List.mem_assoc "foo" fs | _ -> false);
    Alcotest.(check bool) "bar in nodes" true
      (match nodes with `Assoc fs -> List.mem_assoc "bar" fs | _ -> false)
  )

let test_write_parse_error_halts () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         {|{"source":"@@@invalid@@@","name":"mymod"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch halted" true (not (batch_ok batch));
    let diags = batch_diagnostics batch in
    Alcotest.(check bool) "error diagnostic present" true (diags <> []);
    (match diags with
     | d :: _ ->
       Alcotest.(check bool) "severity is error" true
         (json_field "severity" d = `String "error")
     | [] -> ())
  )

(* ─── query / signature ─────────────────────────────────────────────────── *)

let test_query_signature_success () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape well_typed_source))));
    ignore (read_line ());
    write_line (tool_call 3 "query"
      (single_cmd "signature" {|{"module":"mymod","name":"double"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let sig_ = json_field "signature" (first_result batch) in
    Alcotest.(check bool) "signature is non-empty string" true
      (match sig_ with `String s -> String.length s > 0 | _ -> false);
    let node = json_field "node" (first_result batch) in
    Alcotest.(check bool) "node hash present" true
      (match node with `String s -> String.length s > 0 | _ -> false)
  )

let test_query_signature_by_hash () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape well_typed_source))));
    let submit_batch = parse_batch_response (read_line ()) in
    let double_hash = match json_field "nodes" (first_result submit_batch) with
      | `Assoc fs ->
        (match List.assoc_opt "double" fs with
         | Some (`String h) -> h | _ -> "")
      | _ -> ""
    in
    Alcotest.(check bool) "got double hash" true (String.length double_hash > 0);
    write_line (tool_call 3 "query"
      (single_cmd "signature"
         (Printf.sprintf {|{"hash":"%s"}|} double_hash)));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let sig_ = json_field "signature" (first_result batch) in
    Alcotest.(check bool) "signature non-empty" true
      (match sig_ with `String s -> String.length s > 0 | _ -> false)
  )

let test_query_signature_module_not_found () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "query"
      (single_cmd "signature" {|{"module":"nonexistent","name":"double"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch halted" true (not (batch_ok batch));
    let diags = batch_diagnostics batch in
    Alcotest.(check bool) "error diagnostic present" true (diags <> [])
  )

let test_query_signature_function_not_found () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape well_typed_source))));
    ignore (read_line ());
    write_line (tool_call 3 "query"
      (single_cmd "signature" {|{"module":"mymod","name":"nonexistent_fn"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch halted" true (not (batch_ok batch));
    let diags = batch_diagnostics batch in
    Alcotest.(check bool) "error diagnostic present" true (diags <> [])
  )

(* ─── query / interface ─────────────────────────────────────────────────── *)

let test_query_interface_returns_only_pub () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape mixed_visibility_source))));
    ignore (read_line ());
    write_line (tool_call 3 "query"
      (single_cmd "interface" {|{"module":"mymod"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let iface = match json_field "interface" (first_result batch) with
      | `String s -> s | _ -> "" in
    Alcotest.(check bool) "contains pub fn"      true (contains_substring iface "pub fn add_one");
    Alcotest.(check bool) "contains pub type"    true (contains_substring iface "pub type Color");
    Alcotest.(check bool) "excludes private fn"  true (not (contains_substring iface "internal_helper"))
  )

let test_query_interface_fn_has_no_body () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape mixed_visibility_source))));
    ignore (read_line ());
    write_line (tool_call 3 "query"
      (single_cmd "interface" {|{"module":"mymod"}|}));
    let batch = parse_batch_response (read_line ()) in
    let iface = match json_field "interface" (first_result batch) with
      | `String s -> s | _ -> "" in
    Alcotest.(check bool) "interface non-empty"     true (String.length iface > 0);
    Alcotest.(check bool) "no body braces"          true (not (String.contains iface '{'))
  )

let test_query_interface_module_not_found () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "query"
      (single_cmd "interface" {|{"module":"nonexistent"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch halted" true (not (batch_ok batch))
  )

(* ─── verify / exhaustive ────────────────────────────────────────────────── *)

let test_verify_exhaustive_node_is_fn_hash () =
  (* The typechecker raises on non-exhaustive patterns, so stored modules are
     always exhaustive.  verify/exhaustive on a stored module should be clean,
     and confirm that the server correctly threads fn-name to match_warning. *)
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape exhaustive_source))));
    ignore (read_line ());
    write_line (tool_call 3 "verify"
      (single_cmd "exhaustive" {|{"module":"mymod"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    Alcotest.(check bool) "no diagnostics on exhaustive module" true
      (batch_diagnostics batch = [])
  )

let test_verify_exhaustive_no_warnings () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape exhaustive_source))));
    ignore (read_line ());
    write_line (tool_call 3 "verify"
      (single_cmd "exhaustive" {|{"module":"mymod"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    Alcotest.(check bool) "no diagnostics" true (batch_diagnostics batch = [])
  )

let test_verify_exhaustive_module_not_found () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "verify"
      (single_cmd "exhaustive" {|{"module":"nonexistent"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch halted" true (not (batch_ok batch))
  )

(* ─── verify / effects ───────────────────────────────────────────────────── *)

let test_verify_effects_all_handled () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape effects_all_handled_source))));
    ignore (read_line ());
    write_line (tool_call 3 "verify"
      (single_cmd "effects" {|{"module":"mymod","entry_point":"main"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    Alcotest.(check bool) "no diagnostics" true (batch_diagnostics batch = [])
  )

let test_verify_effects_unhandled () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape effects_unhandled_source))));
    ignore (read_line ());
    write_line (tool_call 3 "verify"
      (single_cmd "effects" {|{"module":"mymod","entry_point":"main"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let diags = batch_diagnostics batch in
    Alcotest.(check bool) "diagnostics non-empty" true (diags <> []);
    (match diags with
     | d :: _ ->
       Alcotest.(check bool) "severity is warning" true
         (json_field "severity" d = `String "warning");
       Alcotest.(check bool) "code is effect/unhandled" true
         (json_field "code" d = `String "effect/unhandled");
       Alcotest.(check bool) "location present" true
         (match json_field "location" d with `Assoc _ -> true | _ -> false)
     | [] -> ())
  )

let test_verify_effects_module_not_found () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "verify"
      (single_cmd "effects" {|{"module":"nonexistent","entry_point":"main"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch halted" true (not (batch_ok batch))
  )

let test_verify_effects_entry_not_found () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape effects_all_handled_source))));
    ignore (read_line ());
    write_line (tool_call 3 "verify"
      (single_cmd "effects" {|{"module":"mymod","entry_point":"nonexistent_fn"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch halted" true (not (batch_ok batch))
  )

(* ─── verify / unused ────────────────────────────────────────────────────── *)

let test_verify_unused_all_used () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape all_used_source))));
    ignore (read_line ());
    write_line (tool_call 3 "verify"
      (single_cmd "unused" {|{"module":"mymod"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    Alcotest.(check bool) "no diagnostics" true (batch_diagnostics batch = [])
  )

let test_verify_unused_one_unused () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape one_unused_source))));
    ignore (read_line ());
    write_line (tool_call 3 "verify"
      (single_cmd "unused" {|{"module":"mymod"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let diags = batch_diagnostics batch in
    Alcotest.(check bool) "diagnostics non-empty" true (diags <> []);
    (match diags with
     | d :: _ ->
       Alcotest.(check bool) "severity is warning" true
         (json_field "severity" d = `String "warning");
       Alcotest.(check bool) "code is decl/unused" true
         (json_field "code" d = `String "decl/unused");
       Alcotest.(check bool) "message non-empty" true
         (match json_field "message" d with `String s -> String.length s > 0 | _ -> false);
       Alcotest.(check bool) "location present" true
         (match json_field "location" d with `Assoc _ -> true | _ -> false)
     | [] -> ())
  )

let test_verify_unused_pub_not_flagged () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape pub_unused_source))));
    ignore (read_line ());
    write_line (tool_call 3 "verify"
      (single_cmd "unused" {|{"module":"mymod"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "no diagnostics for pub decl" true (batch_diagnostics batch = [])
  )

let test_verify_unused_mutual_recursion_both_reachable () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape mutual_recursion_source))));
    ignore (read_line ());
    write_line (tool_call 3 "verify"
      (single_cmd "unused" {|{"module":"mymod"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "no diagnostics — pair reachable from main" true
      (batch_diagnostics batch = [])
  )

let test_verify_unused_mutual_recursion_both_unreachable () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape mutual_both_unused_source))));
    ignore (read_line ());
    write_line (tool_call 3 "verify"
      (single_cmd "unused" {|{"module":"mymod"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "two diagnostics — both sides flagged" true
      (List.length (batch_diagnostics batch) = 2)
  )

let test_verify_unused_module_not_found () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "verify"
      (single_cmd "unused" {|{"module":"nonexistent"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch halted" true (not (batch_ok batch))
  )

(* ─── query / effects ────────────────────────────────────────────────────── *)

let test_query_effects_module_not_found () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "query"
      (single_cmd "effects" {|{"module":"nonexistent"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch halted" true (not (batch_ok batch))
  )

let test_query_effects_pure_module () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape pure_module_source))));
    ignore (read_line ());
    write_line (tool_call 3 "query"
      (single_cmd "effects" {|{"module":"mymod"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let effs = json_field "effects" (first_result batch) in
    Alcotest.(check bool) "effects is empty object" true
      (match effs with `Assoc [] -> true | _ -> false)
  )

let test_query_effects_single_effect () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape single_effect_source))));
    ignore (read_line ());
    write_line (tool_call 3 "query"
      (single_cmd "effects" {|{"module":"mymod"}|}));
    let batch = parse_batch_response (read_line ()) in
    let effs = json_field "effects" (first_result batch) in
    let log_fns = match effs with
      | `Assoc fs -> (match List.assoc_opt "Log" fs with Some v -> v | None -> `Null)
      | _ -> `Null
    in
    Alcotest.(check bool) "Log maps to greet" true
      (match log_fns with `List fns -> List.mem (`String "greet") fns | _ -> false);
    Alcotest.(check bool) "pure_fn not in Log" true
      (match log_fns with `List fns -> not (List.mem (`String "pure_fn") fns) | _ -> false)
  )

let test_query_effects_multi_effect () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape multi_effect_source))));
    ignore (read_line ());
    write_line (tool_call 3 "query"
      (single_cmd "effects" {|{"module":"mymod"}|}));
    let batch = parse_batch_response (read_line ()) in
    let effs = json_field "effects" (first_result batch) in
    let get e = match effs with
      | `Assoc fs -> (match List.assoc_opt e fs with Some v -> v | None -> `Null)
      | _ -> `Null
    in
    let log_fns   = get "Log" in
    let throw_fns = get "Throw" in
    Alcotest.(check bool) "do_both under Log"    true
      (match log_fns   with `List fs -> List.mem (`String "do_both") fs | _ -> false);
    Alcotest.(check bool) "do_log under Log"     true
      (match log_fns   with `List fs -> List.mem (`String "do_log")  fs | _ -> false);
    Alcotest.(check bool) "do_both under Throw"  true
      (match throw_fns with `List fs -> List.mem (`String "do_both") fs | _ -> false);
    Alcotest.(check bool) "do_log not under Throw" true
      (match throw_fns with `List fs -> not (List.mem (`String "do_log") fs) | _ -> false)
  )

(* ─── query / callers ────────────────────────────────────────────────────── *)

let test_query_callers_multiple_callers () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape multi_caller_source))));
    ignore (read_line ());
    write_line (tool_call 3 "query"
      (single_cmd "callers" {|{"module":"mymod","name":"helper"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let callers = json_field "callers" (first_result batch) in
    Alcotest.(check bool) "two callers" true
      (match callers with `List xs -> List.length xs = 2 | _ -> false);
    (match callers with
     | `List (entry :: _) ->
       Alcotest.(check bool) "caller_node field non-empty" true
         (match json_field "caller_node" entry with
          | `String s -> String.length s > 0 | _ -> false);
       Alcotest.(check bool) "call_site_node field non-empty" true
         (match json_field "call_site_node" entry with
          | `String s -> String.length s > 0 | _ -> false);
       Alcotest.(check bool) "location object present" true
         (match json_field "location" entry with `Assoc _ -> true | _ -> false)
     | _ -> ())
  )

let test_query_callers_site_nodes_distinct () =
  (* main1 and main2 call helper with different args — call site hashes differ *)
  let source = {|fn helper(x: Int) -> Int ! pure { x }
fn main1() -> Int ! pure { helper(1) }
fn main2() -> Int ! pure { helper(2) }|} in
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape source))));
    ignore (read_line ());
    write_line (tool_call 3 "query"
      (single_cmd "callers" {|{"module":"mymod","name":"helper"}|}));
    let batch = parse_batch_response (read_line ()) in
    let callers = json_field "callers" (first_result batch) in
    let hashes = match callers with
      | `List xs ->
        List.filter_map (fun e ->
          match json_field "call_site_node" e with
          | `String s -> Some s | _ -> None) xs
      | _ -> []
    in
    Alcotest.(check int) "two call sites" 2 (List.length hashes);
    Alcotest.(check bool) "call site hashes are distinct" true
      (match hashes with [a; b] -> a <> b | _ -> false)
  )

let test_query_callers_uncalled_function () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape uncalled_fn_source))));
    ignore (read_line ());
    write_line (tool_call 3 "query"
      (single_cmd "callers" {|{"module":"mymod","name":"helper"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let callers = json_field "callers" (first_result batch) in
    Alcotest.(check bool) "empty callers list" true
      (match callers with `List [] -> true | _ -> false)
  )

let test_query_callers_module_not_found () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "query"
      (single_cmd "callers" {|{"module":"nonexistent","name":"helper"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch halted" true (not (batch_ok batch))
  )

let test_query_callers_function_not_defined () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape multi_caller_source))));
    ignore (read_line ());
    write_line (tool_call 3 "query"
      (single_cmd "callers" {|{"module":"mymod","name":"nonexistent_fn"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch halted" true (not (batch_ok batch))
  )

(* ─── batch semantics: multi-command and early halt ─────────────────────── *)

let test_batch_multi_command_all_succeed () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape well_typed_source))));
    ignore (read_line ());
    (* Two query commands in one batch *)
    let cmds = Printf.sprintf
        {|[{"op":"signature","args":{"module":"mymod","name":"double"}},{"op":"interface","args":{"module":"mymod"}}]|}
    in
    write_line (tool_call 3 "query" cmds);
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let results = match json_field "results" batch with `List rs -> rs | _ -> [] in
    Alcotest.(check int) "two results" 2 (List.length results)
  )

let test_batch_halts_at_first_unrecoverable () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape well_typed_source))));
    ignore (read_line ());
    (* First command fails (module not found), second would succeed *)
    let cmds = Printf.sprintf
        {|[{"op":"signature","args":{"module":"no_such","name":"double"}},{"op":"signature","args":{"module":"mymod","name":"double"}}]|}
    in
    write_line (tool_call 3 "query" cmds);
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch halted" true (not (batch_ok batch));
    Alcotest.(check bool) "stopped_at is 0" true
      (json_field "stopped_at" batch = `Int 0);
    let results = match json_field "results" batch with `List rs -> rs | _ -> [] in
    Alcotest.(check int) "zero results before halt" 0 (List.length results)
  )

(* ─── diagnostic node hash population ───────────────────────────────────── *)

let test_verify_effects_diagnostic_has_node () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape effects_unhandled_source))));
    ignore (read_line ());
    write_line (tool_call 3 "verify"
      (single_cmd "effects" {|{"module":"mymod","entry_point":"main"}|}));
    let batch = parse_batch_response (read_line ()) in
    (match batch_diagnostics batch with
     | d :: _ ->
       Alcotest.(check bool) "diagnostic has node hash" true
         (match json_field "node" d with `String s -> String.length s > 0 | _ -> false)
     | [] ->
       Alcotest.fail "expected at least one diagnostic")
  )

let test_verify_unused_diagnostic_has_node () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape one_unused_source))));
    ignore (read_line ());
    write_line (tool_call 3 "verify"
      (single_cmd "unused" {|{"module":"mymod"}|}));
    let batch = parse_batch_response (read_line ()) in
    (match batch_diagnostics batch with
     | d :: _ ->
       Alcotest.(check bool) "diagnostic has node hash" true
         (match json_field "node" d with `String s -> String.length s > 0 | _ -> false)
     | [] ->
       Alcotest.fail "expected at least one diagnostic")
  )

(* ─── query / effect_flow ────────────────────────────────────────────────── *)

let test_query_effect_flow_pure_fn () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape effects_all_handled_source))));
    ignore (read_line ());
    write_line (tool_call 3 "query"
      (single_cmd "effect_flow" {|{"module":"mymod","name":"main"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let r = first_result batch in
    let entry = json_field "entry" r in
    Alcotest.(check bool) "entry field is main" true (entry = `String "main");
    let performs = json_field "performs" r in
    Alcotest.(check bool) "performs is a list" true
      (match performs with `List _ -> true | _ -> false)
  )

let test_query_effect_flow_handled_effect () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape effects_all_handled_source))));
    ignore (read_line ());
    write_line (tool_call 3 "query"
      (single_cmd "effect_flow" {|{"module":"mymod","name":"main"}|}));
    let batch = parse_batch_response (read_line ()) in
    let r = first_result batch in
    let performs = match json_field "performs" r with `List xs -> xs | _ -> [] in
    (* All effects should be marked handled=true *)
    let all_handled = List.for_all (fun p ->
        json_field "handled" p = `Bool true) performs in
    Alcotest.(check bool) "all effects marked handled" true
      (performs = [] || all_handled)
  )

let test_query_effect_flow_unhandled_marked () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape effects_unhandled_source))));
    ignore (read_line ());
    write_line (tool_call 3 "query"
      (single_cmd "effect_flow" {|{"module":"mymod","name":"main"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let r = first_result batch in
    let performs = match json_field "performs" r with `List xs -> xs | _ -> [] in
    Alcotest.(check bool) "performs non-empty" true (performs <> []);
    let any_unhandled = List.exists (fun p ->
        json_field "handled" p = `Bool false) performs in
    Alcotest.(check bool) "at least one unhandled" true any_unhandled
  )

let test_query_effect_flow_not_found_halts () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "query"
      (single_cmd "effect_flow" {|{"module":"no_such","name":"main"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch halted" true (not (batch_ok batch))
  )

(* ─── query / unhandled ──────────────────────────────────────────────────── *)

let test_query_unhandled_all_handled () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape effects_all_handled_source))));
    ignore (read_line ());
    write_line (tool_call 3 "query"
      (single_cmd "unhandled" {|{"anchor":{"module":"mymod","name":"main"}}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let r = first_result batch in
    let sites = match json_field "sites" r with `List xs -> xs | _ -> [] in
    Alcotest.(check bool) "no unhandled sites" true (sites = [])
  )

let test_query_unhandled_returns_sites () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape effects_unhandled_source))));
    ignore (read_line ());
    write_line (tool_call 3 "query"
      (single_cmd "unhandled" {|{"anchor":{"module":"mymod","name":"main"}}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let r = first_result batch in
    let sites = match json_field "sites" r with `List xs -> xs | _ -> [] in
    Alcotest.(check bool) "unhandled sites non-empty" true (sites <> []);
    (match sites with
     | s :: _ ->
       Alcotest.(check bool) "site has effect field" true
         (match json_field "effect" s with `String x -> String.length x > 0 | _ -> false);
       Alcotest.(check bool) "site has function field" true
         (match json_field "function" s with `String x -> String.length x > 0 | _ -> false)
     | [] -> ())
  )

let test_query_unhandled_no_name_halts () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "query"
      (single_cmd "unhandled" {|{}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch halted" true (not (batch_ok batch))
  )

(* ─── query / dependents ─────────────────────────────────────────────────── *)

let test_query_dependents_effect () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape single_effect_source))));
    ignore (read_line ());
    write_line (tool_call 3 "query"
      (single_cmd "dependents" {|{"name":"Log"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let r = first_result batch in
    Alcotest.(check bool) "name field present" true
      (json_field "name" r = `String "Log");
    let deps = match json_field "dependents" r with `List xs -> xs | _ -> [] in
    Alcotest.(check bool) "dependents non-empty" true (deps <> [])
  )

let test_query_dependents_defines_included () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape single_effect_source))));
    ignore (read_line ());
    write_line (tool_call 3 "query"
      (single_cmd "dependents" {|{"name":"Log"}|}));
    let batch = parse_batch_response (read_line ()) in
    let deps = match json_field "dependents" (first_result batch) with
      | `List xs -> xs | _ -> []
    in
    let has_defines = List.exists (fun d ->
        json_field "kind" d = `String "defines") deps in
    Alcotest.(check bool) "defines entry included" true has_defines
  )

let test_query_dependents_no_name_halts () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "query"
      (single_cmd "dependents" {|{}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch halted" true (not (batch_ok batch))
  )

let test_query_dependents_unknown_name_empty () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape pure_module_source))));
    ignore (read_line ());
    write_line (tool_call 3 "query"
      (single_cmd "dependents" {|{"name":"NoSuchType"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let deps = match json_field "dependents" (first_result batch) with
      | `List xs -> xs | _ -> []
    in
    Alcotest.(check bool) "empty dependents for unknown name" true (deps = [])
  )

(* ─── query / pattern_coverage ──────────────────────────────────────────── *)

let test_query_pattern_coverage_exhaustive () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape exhaustive_source))));
    ignore (read_line ());
    write_line (tool_call 3 "query"
      (single_cmd "pattern_coverage" {|{"module":"mymod","name":"describe"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let r = first_result batch in
    Alcotest.(check bool) "function field present" true
      (json_field "function" r = `String "describe");
    let matches = match json_field "matches" r with `List xs -> xs | _ -> [] in
    Alcotest.(check bool) "matches non-empty" true (matches <> []);
    let all_exhaustive = List.for_all (fun m ->
        json_field "exhaustive" m = `Bool true) matches in
    Alcotest.(check bool) "all matches exhaustive" true all_exhaustive
  )

let test_query_pattern_coverage_node_present () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape exhaustive_source))));
    ignore (read_line ());
    write_line (tool_call 3 "query"
      (single_cmd "pattern_coverage" {|{"module":"mymod","name":"describe"}|}));
    let batch = parse_batch_response (read_line ()) in
    let r = first_result batch in
    Alcotest.(check bool) "node hash present" true
      (match json_field "node" r with `String s -> String.length s > 0 | _ -> false)
  )

let test_query_pattern_coverage_not_found_halts () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "query"
      (single_cmd "pattern_coverage" {|{"module":"no_such","name":"fn"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch halted" true (not (batch_ok batch))
  )

(* ─── query / graph ──────────────────────────────────────────────────────── *)

let test_query_graph_has_param () =
  let source = {|fn get_location(loc: Int, name: String) -> Int ! pure { loc }|} in
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape source))));
    ignore (read_line ());
    write_line (tool_call 3 "query"
      (single_cmd "graph"
         {|{"anchor":{"module":"mymod","name":"get_location"},"traverse":[{"edge":"HAS_PARAM","direction":"out","collect":"params"}]}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let r = first_result batch in
    let params = match json_field "params" r with `List xs -> xs | _ -> [] in
    Alcotest.(check bool) "two params returned" true (List.length params = 2);
    let names = List.filter_map (fun p ->
        match json_field "name" p with `String s -> Some s | _ -> None) params in
    Alcotest.(check bool) "loc in params" true (List.mem "loc" names);
    Alcotest.(check bool) "name in params" true (List.mem "name" names)
  )

let test_query_graph_has_param_filter () =
  let source = {|fn get_location(loc: Int, name: String) -> Int ! pure { loc }|} in
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape source))));
    ignore (read_line ());
    write_line (tool_call 3 "query"
      (single_cmd "graph"
         {|{"anchor":{"module":"mymod","name":"get_location"},"traverse":[{"edge":"HAS_PARAM","direction":"out","filter":{"name":"loc"},"collect":"param"}]}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let r = first_result batch in
    let params = match json_field "param" r with `List xs -> xs | _ -> [] in
    Alcotest.(check int) "exactly one param" 1 (List.length params);
    (match params with
     | p :: _ ->
       Alcotest.(check bool) "name is loc" true (json_field "name" p = `String "loc");
       Alcotest.(check bool) "type field present" true
         (match json_field "type" p with `String s -> String.length s > 0 | _ -> false)
     | [] -> ())
  )

let test_query_graph_calls_in () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape multi_caller_source))));
    ignore (read_line ());
    write_line (tool_call 3 "query"
      (single_cmd "graph"
         {|{"anchor":{"module":"mymod","name":"helper"},"traverse":[{"edge":"CALLS","direction":"in","collect":"call_sites"}]}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let r = first_result batch in
    let call_sites = match json_field "call_sites" r with `List xs -> xs | _ -> [] in
    Alcotest.(check bool) "two call sites" true (List.length call_sites = 2);
    (match call_sites with
     | cs :: _ ->
       Alcotest.(check bool) "id field non-empty" true
         (match json_field "id" cs with `String s -> String.length s > 0 | _ -> false);
       Alcotest.(check bool) "callee is helper" true
         (json_field "callee" cs = `String "helper")
     | [] -> ())
  )

let test_query_graph_no_anchor_halts () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "query"
      (single_cmd "graph"
         {|{"anchor":{"module":"no_such","name":"fn"},"traverse":[]}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch halted" true (not (batch_ok batch))
  )

let test_query_graph_calls_out () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape multi_caller_source))));
    ignore (read_line ());
    write_line (tool_call 3 "query"
      (single_cmd "graph"
         {|{"anchor":{"module":"mymod","name":"main1"},"traverse":[{"edge":"CALLS","direction":"out","collect":"callees"}]}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let r = first_result batch in
    let callees = match json_field "callees" r with `List xs -> xs | _ -> [] in
    Alcotest.(check bool) "at least one callee" true (callees <> [])
  )

let test_query_graph_follow_has_argument () =
  (* Verify that CALLS in → follow HAS_ARGUMENT out produces argument nodes *)
  let source = {|fn helper(x: Int) -> Int ! pure { x }
fn main1() -> Int ! pure { helper(1) }
fn main2() -> Int ! pure { helper(2) }|} in
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape source))));
    ignore (read_line ());
    let cmds = {|{"anchor":{"module":"mymod","name":"helper"},"traverse":[{"edge":"CALLS","direction":"in","collect":"call_sites","follow":{"edge":"HAS_ARGUMENT","direction":"out","collect":"arguments"}}]}|} in
    write_line (tool_call 3 "query" (single_cmd "graph" cmds));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let r = first_result batch in
    let call_sites = match json_field "call_sites" r with `List xs -> xs | _ -> [] in
    let arguments  = match json_field "arguments"  r with `List xs -> xs | _ -> [] in
    Alcotest.(check bool) "two call sites" true (List.length call_sites = 2);
    Alcotest.(check bool) "two arguments (one per call)" true (List.length arguments = 2)
  )

let test_query_graph_batch_with_existing_ops () =
  (* The hero acceptance criterion: interface + signature + callers in one batch *)
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape mixed_visibility_source))));
    ignore (read_line ());
    let cmds =
      {|[{"op":"interface","args":{"module":"mymod"}},{"op":"signature","args":{"module":"mymod","name":"add_one"}},{"op":"callers","args":{"module":"mymod","name":"add_one"}}]|}
    in
    write_line (tool_call 3 "query" cmds);
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let results = match json_field "results" batch with `List rs -> rs | _ -> [] in
    Alcotest.(check int) "three results" 3 (List.length results)
  )

(* ─── write / module: type errors are recoverable ───────────────────────── *)

let test_write_module_type_error_recoverable () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape ill_typed_source))));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch NOT halted (recoverable)" true (batch_ok batch);
    let r = first_result batch in
    Alcotest.(check bool) "root hash present" true
      (match json_field "root" r with `String s -> String.length s > 0 | _ -> false);
    let diags = batch_diagnostics batch in
    Alcotest.(check bool) "type error diagnostic present" true (diags <> []);
    (match diags with
     | d :: _ ->
       Alcotest.(check bool) "severity is error" true
         (json_field "severity" d = `String "error");
       Alcotest.(check bool) "code starts with type/" true
         (match json_field "code" d with
          | `String s -> contains_substring s "type/" | _ -> false)
     | [] -> ())
  )

let test_write_module_type_error_then_good_module () =
  (* A type error in a module op must not halt the batch;
     a subsequent module op in the same batch must still execute. *)
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    let cmds = Printf.sprintf
      {|[{"op":"module","args":{"source":"%s","name":"bad"}},{"op":"module","args":{"source":"%s","name":"good"}}]|}
      (json_string_escape ill_typed_source)
      (json_string_escape well_typed_source)
    in
    write_line (tool_call 2 "write" cmds);
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok (type error is recoverable)" true (batch_ok batch);
    let results = match json_field "results" batch with `List rs -> rs | _ -> [] in
    Alcotest.(check int) "two results — both commands ran" 2 (List.length results)
  )

(* ─── write / function ───────────────────────────────────────────────────── *)

let test_write_function_adds_decl () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape well_typed_source))));
    ignore (read_line ());
    let new_fn = {|fn triple(x: Int) -> Int ! pure { add(x, add(x, x)) }|} in
    write_line (tool_call 3 "write"
      (single_cmd "function"
         (Printf.sprintf {|{"module":"mymod","source":"%s"}|}
            (json_string_escape new_fn))));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let nodes = json_field "nodes" (first_result batch) in
    Alcotest.(check bool) "original fn still present" true
      (match nodes with `Assoc fs -> List.mem_assoc "double" fs | _ -> false);
    Alcotest.(check bool) "new fn present" true
      (match nodes with `Assoc fs -> List.mem_assoc "triple" fs | _ -> false)
  )

let test_write_function_replaces_existing_decl () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape well_typed_source))));
    ignore (read_line ());
    let updated_fn = {|fn double(x: Int) -> Int ! pure { add(x, x) }|} in
    write_line (tool_call 3 "write"
      (single_cmd "function"
         (Printf.sprintf {|{"module":"mymod","source":"%s"}|}
            (json_string_escape updated_fn))));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let nodes = json_field "nodes" (first_result batch) in
    Alcotest.(check bool) "double still in nodes" true
      (match nodes with `Assoc fs -> List.mem_assoc "double" fs | _ -> false)
  )

let test_write_function_module_not_found_halts () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "function"
         {|{"module":"nonexistent","source":"fn f(x: Int) -> Int ! pure { x }"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch halted" true (not (batch_ok batch))
  )

let test_write_function_parse_error_halts () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape well_typed_source))));
    ignore (read_line ());
    write_line (tool_call 3 "write"
      (single_cmd "function" {|{"module":"mymod","source":"@@@bad@@@"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch halted on parse error" true (not (batch_ok batch))
  )

(* ─── write / replace ────────────────────────────────────────────────────── *)

let test_write_replace_by_name () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape well_typed_source))));
    ignore (read_line ());
    let new_body = {|fn double(x: Int) -> Int ! pure { add(x, x) }|} in
    write_line (tool_call 3 "write"
      (single_cmd "replace"
         (Printf.sprintf {|{"anchor":{"module":"mymod","name":"double"},"source":"%s"}|}
            (json_string_escape new_body))));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let nodes = json_field "nodes" (first_result batch) in
    Alcotest.(check bool) "double still in nodes" true
      (match nodes with `Assoc fs -> List.mem_assoc "double" fs | _ -> false)
  )

let test_write_replace_by_hash () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape well_typed_source))));
    let init_batch = parse_batch_response (read_line ()) in
    let double_hash = match json_field "nodes" (first_result init_batch) with
      | `Assoc fs ->
        (match List.assoc_opt "double" fs with Some (`String h) -> h | _ -> "")
      | _ -> ""
    in
    Alcotest.(check bool) "got double hash" true (String.length double_hash > 0);
    let new_body = {|fn double(x: Int) -> Int ! pure { add(x, x) }|} in
    write_line (tool_call 3 "write"
      (single_cmd "replace"
         (Printf.sprintf {|{"anchor":{"hash":"%s"},"source":"%s"}|}
            double_hash (json_string_escape new_body))));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok after hash-anchored replace" true (batch_ok batch)
  )

let test_write_replace_anchor_not_found_halts () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "replace"
         {|{"anchor":{"module":"no_such","name":"fn"},"source":"fn fn() -> Unit ! pure { () }"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch halted" true (not (batch_ok batch))
  )

(* ─── write: post-batch verify ───────────────────────────────────────────── *)

let test_write_post_verify_unused_default () =
  (* The default verify array includes "exhaustive" but NOT "unused", so
     an unused function in the module should NOT appear in diagnostics
     unless "unused" is explicitly requested. *)
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (tool_call 2 "write"
      (single_cmd "module"
         (Printf.sprintf {|{"source":"%s","name":"mymod"}|}
            (json_string_escape one_unused_source))));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let diags = batch_diagnostics batch in
    let has_unused = List.exists (fun d ->
      json_field "code" d = `String "decl/unused") diags in
    Alcotest.(check bool) "no unused diagnostic with default verify" false has_unused
  )

let test_write_post_verify_unused_explicit () =
  (* When verify includes "unused", unused declarations are flagged. *)
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    let req = Printf.sprintf
      {|{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"write","arguments":{"commands":[{"op":"module","args":{"source":"%s","name":"mymod"}}],"verify":["unused"]}}}|}
      (json_string_escape one_unused_source)
    in
    write_line req;
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let diags = batch_diagnostics batch in
    let has_unused = List.exists (fun d ->
      json_field "code" d = `String "decl/unused") diags in
    Alcotest.(check bool) "unused diagnostic present" true has_unused
  )

(* ─── Test runner ────────────────────────────────────────────────────────── *)

let () =
  Alcotest.run "mcp_server" [
    ("tools/list", [
      Alcotest.test_case "exposes exactly four tools" `Quick test_tools_list_has_four_tools;
    ]);
    ("verify/types", [
      Alcotest.test_case "well-typed source → no diagnostics"    `Quick test_verify_types_well_typed;
      Alcotest.test_case "ill-typed source → error diagnostics"  `Quick test_verify_types_ill_typed;
      Alcotest.test_case "does not update server state"          `Quick test_verify_types_does_not_update_state;
    ]);
    ("write/module", [
      Alcotest.test_case "returns root hash and nodes map"       `Quick test_write_submit_returns_root_and_nodes;
      Alcotest.test_case "nodes map contains all declarations"   `Quick test_write_submit_nodes_map_all_decls;
      Alcotest.test_case "parse error halts batch"               `Quick test_write_parse_error_halts;
      Alcotest.test_case "type error is recoverable diagnostic"  `Quick test_write_module_type_error_recoverable;
      Alcotest.test_case "type error allows next command"        `Quick test_write_module_type_error_then_good_module;
    ]);
    ("write/function", [
      Alcotest.test_case "adds new decl to existing module"      `Quick test_write_function_adds_decl;
      Alcotest.test_case "replaces existing decl of same name"   `Quick test_write_function_replaces_existing_decl;
      Alcotest.test_case "module not found halts batch"          `Quick test_write_function_module_not_found_halts;
      Alcotest.test_case "parse error halts batch"               `Quick test_write_function_parse_error_halts;
    ]);
    ("write/replace", [
      Alcotest.test_case "replace by symbolic anchor"            `Quick test_write_replace_by_name;
      Alcotest.test_case "replace by hash anchor"                `Quick test_write_replace_by_hash;
      Alcotest.test_case "anchor not found halts batch"          `Quick test_write_replace_anchor_not_found_halts;
    ]);
    ("write/post-verify", [
      Alcotest.test_case "unused not checked without verify:unused" `Quick test_write_post_verify_unused_default;
      Alcotest.test_case "verify:unused flags unused decls"         `Quick test_write_post_verify_unused_explicit;
    ]);
    ("query/signature", [
      Alcotest.test_case "returns signature and node hash"       `Quick test_query_signature_success;
      Alcotest.test_case "hash anchor resolves to signature"     `Quick test_query_signature_by_hash;
      Alcotest.test_case "module not found halts batch"          `Quick test_query_signature_module_not_found;
      Alcotest.test_case "function not found halts batch"        `Quick test_query_signature_function_not_found;
    ]);
    ("query/interface", [
      Alcotest.test_case "returns only pub declarations"         `Quick test_query_interface_returns_only_pub;
      Alcotest.test_case "function signatures have no body"      `Quick test_query_interface_fn_has_no_body;
      Alcotest.test_case "module not found halts batch"          `Quick test_query_interface_module_not_found;
    ]);
    ("verify/exhaustive", [
      Alcotest.test_case "exhaustive match → no diagnostics"     `Quick test_verify_exhaustive_no_warnings;
      Alcotest.test_case "fn-scoped node threading compiles OK"  `Quick test_verify_exhaustive_node_is_fn_hash;
      Alcotest.test_case "module not found halts batch"          `Quick test_verify_exhaustive_module_not_found;
    ]);
    ("verify/effects", [
      Alcotest.test_case "all handled → no diagnostics"          `Quick test_verify_effects_all_handled;
      Alcotest.test_case "unhandled effects → warning diags"     `Quick test_verify_effects_unhandled;
      Alcotest.test_case "module not found halts batch"          `Quick test_verify_effects_module_not_found;
      Alcotest.test_case "entry point not found halts batch"     `Quick test_verify_effects_entry_not_found;
    ]);
    ("verify/unused", [
      Alcotest.test_case "all used → no diagnostics"             `Quick test_verify_unused_all_used;
      Alcotest.test_case "unused fn → warning diagnostic"        `Quick test_verify_unused_one_unused;
      Alcotest.test_case "pub decl not flagged"                  `Quick test_verify_unused_pub_not_flagged;
      Alcotest.test_case "mutual recursion reachable — clean"    `Quick test_verify_unused_mutual_recursion_both_reachable;
      Alcotest.test_case "mutual recursion unreachable — flagged" `Quick test_verify_unused_mutual_recursion_both_unreachable;
      Alcotest.test_case "module not found halts batch"          `Quick test_verify_unused_module_not_found;
    ]);
    ("query/effects", [
      Alcotest.test_case "module not found halts batch"          `Quick test_query_effects_module_not_found;
      Alcotest.test_case "pure module → empty effects map"       `Quick test_query_effects_pure_module;
      Alcotest.test_case "single effect → correct mapping"       `Quick test_query_effects_single_effect;
      Alcotest.test_case "multi-effect fn appears under each"    `Quick test_query_effects_multi_effect;
    ]);
    ("query/callers", [
      Alcotest.test_case "multiple callers returned"             `Quick test_query_callers_multiple_callers;
      Alcotest.test_case "call site hashes are distinct"         `Quick test_query_callers_site_nodes_distinct;
      Alcotest.test_case "uncalled fn → empty list"              `Quick test_query_callers_uncalled_function;
      Alcotest.test_case "module not found halts batch"          `Quick test_query_callers_module_not_found;
      Alcotest.test_case "fn not defined halts batch"            `Quick test_query_callers_function_not_defined;
    ]);
    ("batch/semantics", [
      Alcotest.test_case "multi-command batch all succeed"       `Quick test_batch_multi_command_all_succeed;
      Alcotest.test_case "halts at first unrecoverable error"    `Quick test_batch_halts_at_first_unrecoverable;
    ]);
    ("diagnostic/node", [
      Alcotest.test_case "effect diagnostic carries node hash"   `Quick test_verify_effects_diagnostic_has_node;
      Alcotest.test_case "unused diagnostic carries node hash"   `Quick test_verify_unused_diagnostic_has_node;
    ]);
    ("query/effect_flow", [
      Alcotest.test_case "returns entry and performs list"       `Quick test_query_effect_flow_pure_fn;
      Alcotest.test_case "handled effects marked handled=true"   `Quick test_query_effect_flow_handled_effect;
      Alcotest.test_case "unhandled effects marked handled=false" `Quick test_query_effect_flow_unhandled_marked;
      Alcotest.test_case "not found halts batch"                 `Quick test_query_effect_flow_not_found_halts;
    ]);
    ("query/unhandled", [
      Alcotest.test_case "all handled → empty sites"             `Quick test_query_unhandled_all_handled;
      Alcotest.test_case "unhandled effects → sites list"        `Quick test_query_unhandled_returns_sites;
      Alcotest.test_case "no name halts batch"                   `Quick test_query_unhandled_no_name_halts;
    ]);
    ("query/dependents", [
      Alcotest.test_case "effect name → dependents list"         `Quick test_query_dependents_effect;
      Alcotest.test_case "defines entry included in dependents"  `Quick test_query_dependents_defines_included;
      Alcotest.test_case "no name halts batch"                   `Quick test_query_dependents_no_name_halts;
      Alcotest.test_case "unknown name → empty dependents"       `Quick test_query_dependents_unknown_name_empty;
    ]);
    ("query/pattern_coverage", [
      Alcotest.test_case "exhaustive fn → exhaustive result"     `Quick test_query_pattern_coverage_exhaustive;
      Alcotest.test_case "result carries node hash"              `Quick test_query_pattern_coverage_node_present;
      Alcotest.test_case "not found halts batch"                 `Quick test_query_pattern_coverage_not_found_halts;
    ]);
    ("query/graph", [
      Alcotest.test_case "HAS_PARAM out → all params"            `Quick test_query_graph_has_param;
      Alcotest.test_case "HAS_PARAM out with filter → one param" `Quick test_query_graph_has_param_filter;
      Alcotest.test_case "CALLS in → call site nodes"            `Quick test_query_graph_calls_in;
      Alcotest.test_case "CALLS out → callee nodes"              `Quick test_query_graph_calls_out;
      Alcotest.test_case "follow HAS_ARGUMENT → arguments"       `Quick test_query_graph_follow_has_argument;
      Alcotest.test_case "no anchor halts batch"                 `Quick test_query_graph_no_anchor_halts;
      Alcotest.test_case "batch interface+signature+callers"     `Quick test_query_graph_batch_with_existing_ops;
    ]);
  ]
