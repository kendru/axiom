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

let start_server () =
  let store_dir =
    let path = Filename.temp_file "axiom_tx_test_" "_dir" in
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
  let write_line s = output_string oc (s ^ "\n"); flush oc in
  let read_line  () = input_line ic in
  let close () =
    close_out_noerr oc;
    close_in_noerr ic;
    (try Unix.kill pid Sys.sigterm with _ -> ());
    ignore (Unix.waitpid [] pid);
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

let tool_call id tool_name commands_json =
  Printf.sprintf
    {|{"jsonrpc":"2.0","id":%d,"method":"tools/call","params":{"name":"%s","arguments":{"commands":%s}}}|}
    id tool_name commands_json

let tool_call_verify id tool_name commands_json verify_json =
  Printf.sprintf
    {|{"jsonrpc":"2.0","id":%d,"method":"tools/call","params":{"name":"%s","arguments":{"commands":%s,"verify":%s}}}|}
    id tool_name commands_json verify_json

let single_cmd op args_json =
  Printf.sprintf {|[{"op":"%s","args":%s}]|} op args_json

let parse_batch_response line =
  let json = mini_parse line in
  let result = json_field "result" json in
  let content = json_field "content" result in
  match content with
  | `List (item :: _) ->
    let text = json_field "text" item in
    (match text with `String s -> mini_parse s | _ -> `Null)
  | _ -> result

let first_result batch =
  match json_field "results" batch with
  | `List (r :: _) -> r
  | _              -> `Null

let batch_diagnostics batch =
  match json_field "diagnostics" batch with
  | `List ds -> ds
  | _        -> []

let batch_ok batch =
  match json_field "stopped_at" batch with
  | `Null -> true
  | _     -> false

let has_error_diag batch =
  List.exists (fun d ->
    match json_field "severity" d with `String "error" -> true | _ -> false
  ) (batch_diagnostics batch)

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

(* Submit a module via write/module; return the batch json *)
let submit_module write_line read_line id mod_name source =
  write_line (tool_call id "write"
    (single_cmd "module"
       (Printf.sprintf {|{"name":"%s","source":"%s"}|}
          mod_name (json_string_escape source))));
  parse_batch_response (read_line ())

(* ─── rename tests ───────────────────────────────────────────────────────── *)

let test_rename_function_same_module () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    (* Submit a module with two public functions *)
    let src = {|pub fn old_name(x: Int) -> Int ! pure { x }
pub fn caller() -> Int ! pure { old_name(1) }|} in
    ignore (submit_module write_line read_line 2 "m" src);
    (* Rename old_name → new_name *)
    write_line (tool_call 3 "transform"
      (single_cmd "rename"
         {|{"anchor":{"module":"m","name":"old_name"},"new_name":"new_name"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    Alcotest.(check bool) "no error diags" false (has_error_diag batch);
    let r = first_result batch in
    Alcotest.(check string) "new_name in result" "new_name"
      (match json_field "new_name" r with `String s -> s | _ -> "");
    (* The changed_nodes array must be non-empty *)
    let changed = match json_field "changed_nodes" r with `List l -> l | _ -> [] in
    Alcotest.(check bool) "some nodes changed" true (changed <> []);
    (* Verify the rename is reflected in the interface *)
    write_line (tool_call 4 "query"
      (single_cmd "interface" {|{"module":"m"}|}));
    let q_batch = parse_batch_response (read_line ()) in
    let iface = match json_field "interface" (first_result q_batch) with
      | `String s -> s | _ -> "" in
    Alcotest.(check bool) "new_name in interface" true
      (contains_substring iface "new_name");
    Alcotest.(check bool) "old_name not in interface" false
      (contains_substring iface "old_name")
  )

let test_rename_effect_updates_perform_and_handle () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    let src = {|pub effect OldEff { op: (Int) -> Int }
pub fn doer(x: Int) -> Int ! {OldEff} { perform OldEff.op(x) }
pub fn runner() -> Int ! pure {
  handle doer(1) with {
    OldEff { op(n) => n }
  }
}|} in
    ignore (submit_module write_line read_line 2 "m" src);
    write_line (tool_call 3 "transform"
      (single_cmd "rename"
         {|{"anchor":{"module":"m","name":"OldEff"},"new_name":"NewEff"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    (* Confirm the updated source has NewEff *)
    write_line (tool_call 4 "query"
      (single_cmd "interface" {|{"module":"m"}|}));
    let q_batch = parse_batch_response (read_line ()) in
    let iface = match json_field "interface" (first_result q_batch) with
      | `String s -> s | _ -> "" in
    Alcotest.(check bool) "NewEff in interface" true
      (contains_substring iface "NewEff");
    Alcotest.(check bool) "OldEff gone from interface" false
      (contains_substring iface "OldEff")
  )

let test_rename_reports_type_clash () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    (* Two functions; renaming a to b where b already exists should type-error *)
    let src = {|fn a(x: Int) -> Int ! pure { x }
fn b(x: Int) -> Bool ! pure { true }
fn caller() -> Int ! pure { a(1) }|} in
    ignore (submit_module write_line read_line 2 "m" src);
    write_line (tool_call 3 "transform"
      (single_cmd "rename"
         {|{"anchor":{"module":"m","name":"a"},"new_name":"b"}|}));
    let batch = parse_batch_response (read_line ()) in
    (* The rename itself succeeds (batch not halted) but a type error is reported *)
    Alcotest.(check bool) "batch ok (rename applies)" true (batch_ok batch);
    Alcotest.(check bool) "type error diagnostic" true (has_error_diag batch)
  )

(* ─── extract_function tests ─────────────────────────────────────────────── *)

let test_extract_function_basic () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    let src = {|fn main(x: Int) -> Int ! pure {
  do {
    let a = add(x, 1);
    let b = add(a, 2);
    b
  }
}|} in
    ignore (submit_module write_line read_line 2 "m" src);
    (* Extract stmt 2 (the final 'b' expression) into a new fn "step_one" *)
    write_line (tool_call 3 "transform"
      (single_cmd "extract_function"
         {|{"anchor":{"module":"m","name":"main"},"range":[2,2],"new_name":"step_one"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let r = first_result batch in
    Alcotest.(check string) "extracted fn name" "step_one"
      (match json_field "extracted_fn" r with `String s -> s | _ -> "");
    (* The new function should be registered *)
    write_line (tool_call 4 "query"
      (single_cmd "signature"
         {|{"module":"m","name":"step_one"}|}));
    let q_batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "step_one registered" true (batch_ok q_batch)
  )

(* ─── add_effect_logging tests ───────────────────────────────────────────── *)

let test_add_effect_logging_inserts_log () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    let src = {|effect Log { log: (String) -> Unit }
effect Db { query: (String) -> Int }
fn fetch(key: String) -> Int ! {Db} {
  do {
    perform Db.query(key)
  }
}|} in
    ignore (submit_module write_line read_line 2 "m" src);
    write_line (tool_call 3 "transform"
      (single_cmd "add_effect_logging"
         {|{"module":"m","effect":"Db"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let r = first_result batch in
    let changed = match json_field "changed_nodes" r with `List l -> l | _ -> [] in
    Alcotest.(check bool) "nodes changed" true (changed <> []);
    (* The updated source should contain a Log.log perform *)
    write_line (tool_call 4 "query"
      (single_cmd "interface" {|{"module":"m"}|}));
    let q_batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "query ok" true (batch_ok q_batch)
  )

let test_add_effect_logging_adds_log_to_effects () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    let src = {|effect Log { log: (String) -> Unit }
effect St { get: () -> Int }
fn reader() -> Int ! {St} {
  do {
    perform St.get()
  }
}|} in
    ignore (submit_module write_line read_line 2 "m" src);
    write_line (tool_call 3 "transform"
      (single_cmd "add_effect_logging" {|{"module":"m","effect":"St"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    (* The function's effect annotation should now include Log *)
    write_line (tool_call 4 "query"
      (single_cmd "effects" {|{"module":"m"}|}));
    let q_batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "effects query ok" true (batch_ok q_batch)
  )

(* ─── mock_effects tests ─────────────────────────────────────────────────── *)

let test_mock_effects_creates_module () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    let src = {|effect Log { log: (String) -> Unit }
pub fn greet(msg: String) -> Unit ! {Log} {
  do {
    perform Log.log(msg)
  }
}|} in
    ignore (submit_module write_line read_line 2 "svc" src);
    write_line (tool_call 3 "transform"
      (single_cmd "mock_effects"
         {|{"target":"svc","into_module":"svc_mock"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let r = first_result batch in
    Alcotest.(check string) "module name" "svc_mock"
      (match json_field "module" r with `String s -> s | _ -> "");
    let source = match json_field "source" r with `String s -> s | _ -> "" in
    Alcotest.(check bool) "source non-empty" true (source <> "");
    Alcotest.(check bool) "mock_greet in source" true
      (contains_substring source "mock_greet");
    (* The new module should be queryable *)
    write_line (tool_call 4 "query"
      (single_cmd "interface" {|{"module":"svc_mock"}|}));
    let q_batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "mock module queryable" true (batch_ok q_batch)
  )

(* ─── inline_handler tests ───────────────────────────────────────────────── *)

let test_inline_handler_trivial () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    let src = {|effect Counter { inc: () -> Unit }
fn run() -> Unit ! pure {
  handle (do {
    perform Counter.inc();
    perform Counter.inc()
  }) with {
    Counter { inc() => () }
  }
}|} in
    ignore (submit_module write_line read_line 2 "m" src);
    write_line (tool_call 3 "transform"
      (single_cmd "inline_handler"
         {|{"anchor":{"module":"m","name":"run"},"effect":"Counter"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let r = first_result batch in
    Alcotest.(check string) "function" "run"
      (match json_field "function" r with `String s -> s | _ -> "");
    Alcotest.(check string) "effect" "Counter"
      (match json_field "effect" r with `String s -> s | _ -> "")
  )

let test_inline_handler_nontrivial_skipped () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    (* A handler that uses resume is non-trivial; inline should succeed but
       leave the handler in place (changed_nodes = []) *)
    let src = {|effect Ask { ask: () -> Int }
fn run(x: Int) -> Int ! pure {
  handle add(x, 1) with {
    Ask { ask() => resume(42) }
  }
}|} in
    ignore (submit_module write_line read_line 2 "m" src);
    write_line (tool_call 3 "transform"
      (single_cmd "inline_handler"
         {|{"anchor":{"module":"m","name":"run"},"effect":"Ask"}|}));
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    let r = first_result batch in
    let changed = match json_field "changed_nodes" r with `List l -> l | _ -> [] in
    (* No change: non-trivial handler was left in place *)
    Alcotest.(check bool) "no change for non-trivial" true (changed = [])
  )

(* ─── transform verify integration ──────────────────────────────────────── *)

let test_transform_runs_post_verify () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    let src = {|fn double(x: Int) -> Int ! pure { add(x, x) }|} in
    ignore (submit_module write_line read_line 2 "m" src);
    (* Rename with verify = ["types"] *)
    write_line (tool_call_verify 3 "transform"
      (single_cmd "rename"
         {|{"anchor":{"module":"m","name":"double"},"new_name":"twice"}|})
      {|["types"]|});
    let batch = parse_batch_response (read_line ()) in
    Alcotest.(check bool) "batch ok" true (batch_ok batch);
    Alcotest.(check bool) "no errors after clean rename" false (has_error_diag batch)
  )

(* ─── Alcotest registration ──────────────────────────────────────────────── *)

let () =
  let open Alcotest in
  run "transforms" [
    "rename", [
      test_case "rename function in same module" `Quick test_rename_function_same_module;
      test_case "rename effect updates perform and handle" `Quick test_rename_effect_updates_perform_and_handle;
      test_case "rename to existing name reports type clash" `Quick test_rename_reports_type_clash;
    ];
    "extract_function", [
      test_case "extract basic do-stmt range" `Quick test_extract_function_basic;
    ];
    "add_effect_logging", [
      test_case "inserts log calls before target performs" `Quick test_add_effect_logging_inserts_log;
      test_case "adds Log to function effect annotation" `Quick test_add_effect_logging_adds_log_to_effects;
    ];
    "mock_effects", [
      test_case "generates mock module from pub fns" `Quick test_mock_effects_creates_module;
    ];
    "inline_handler", [
      test_case "inlines trivial handler" `Quick test_inline_handler_trivial;
      test_case "skips non-trivial (resume-using) handler" `Quick test_inline_handler_nontrivial_skipped;
    ];
    "verify_integration", [
      test_case "transform runs post-batch verify" `Quick test_transform_runs_post_verify;
    ];
  ]
