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
  let (child_in_r, child_in_w)   = Unix.pipe () in
  let (child_out_r, child_out_w) = Unix.pipe () in
  let pid = Unix.create_process mcp_exe [| mcp_exe |]
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
    ignore (Unix.waitpid [] pid)
  in
  (write_line, read_line, close)

(* Minimal JSON accessor helpers *)
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

let parse_response line =
  let json = mini_parse line in
  let result = json_field "result" json in
  let content = json_field "content" result in
  (match content with
   | `List (item :: _) ->
     let text = json_field "text" item in
     (match text with `String s -> mini_parse s | _ -> `Null)
   | _ -> result)

let initialize write_line read_line =
  write_line {|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}|};
  ignore (read_line ());
  write_line {|{"jsonrpc":"2.0","id":null,"method":"notifications/initialized","params":{}}|}

let well_typed_source = {|fn double(x: Int) -> Int ! pure { add(x, x) }|}

let ill_typed_source = {|fn go() -> Unit ! pure { perform Nope.boom() }|}

(* ------------------------------------------------------------------ *)
(* Tests                                                               *)
(* ------------------------------------------------------------------ *)

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

let verify_types_call id src =
  Printf.sprintf
    {|{"jsonrpc":"2.0","id":%d,"method":"tools/call","params":{"name":"verify_types","arguments":{"source":"%s"}}}|}
    id (json_string_escape src)

let submit_module_call id src name =
  Printf.sprintf
    {|{"jsonrpc":"2.0","id":%d,"method":"tools/call","params":{"name":"submit_module","arguments":{"source":"%s","module_name":"%s"}}}|}
    id (json_string_escape src) (json_string_escape name)

let query_signature_call id module_name function_name =
  Printf.sprintf
    {|{"jsonrpc":"2.0","id":%d,"method":"tools/call","params":{"name":"query_signature","arguments":{"module_name":"%s","function_name":"%s"}}}|}
    id (json_string_escape module_name) (json_string_escape function_name)

let query_interface_call id module_name =
  Printf.sprintf
    {|{"jsonrpc":"2.0","id":%d,"method":"tools/call","params":{"name":"query_interface","arguments":{"module_name":"%s"}}}|}
    id (json_string_escape module_name)

let test_verify_types_well_typed () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (verify_types_call 2 well_typed_source);
    let result = parse_response (read_line ()) in
    let errors = json_field "errors" result in
    Alcotest.(check bool) "errors is empty list" true
      (match errors with `List [] -> true | _ -> false)
  )

let test_verify_types_type_error () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (verify_types_call 2 ill_typed_source);
    let result = parse_response (read_line ()) in
    let errors = json_field "errors" result in
    Alcotest.(check bool) "errors is non-empty" true
      (match errors with `List (_ :: _) -> true | _ -> false);
    (match errors with
     | `List (err :: _) ->
       let msg = json_field "message" err in
       Alcotest.(check bool) "error has message field" true
         (match msg with `String s -> String.length s > 0 | _ -> false);
       let line = json_field "line" err in
       Alcotest.(check bool) "error has line field" true
         (match line with `Int _ -> true | _ -> false);
       let col = json_field "col" err in
       Alcotest.(check bool) "error has col field" true
         (match col with `Int _ -> true | _ -> false)
     | _ -> ())
  )

let test_verify_types_does_not_update_state () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (verify_types_call 2 ill_typed_source);
    ignore (read_line ());
    write_line (submit_module_call 3 well_typed_source "test");
    let result = parse_response (read_line ()) in
    let ok = json_field "ok" result in
    Alcotest.(check bool) "submit_module still succeeds after verify_types" true
      (match ok with `Bool true -> true | _ -> false)
  )

let test_tools_list_includes_verify_types () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line {|{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}|};
    let line = read_line () in
    let json = mini_parse line in
    let result = json_field "result" json in
    let tools = json_field "tools" result in
    let names = match tools with
      | `List items ->
        List.filter_map (fun item ->
          match json_field "name" item with
          | `String s -> Some s
          | _ -> None) items
      | _ -> []
    in
    Alcotest.(check bool) "verify_types in tools/list" true
      (List.mem "verify_types" names)
  )

let test_query_signature_success () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (submit_module_call 2 well_typed_source "mymod");
    ignore (read_line ());
    write_line (query_signature_call 3 "mymod" "double");
    let result = parse_response (read_line ()) in
    let ok = json_field "ok" result in
    Alcotest.(check bool) "ok is true" true
      (match ok with `Bool true -> true | _ -> false);
    let sig_ = json_field "signature" result in
    Alcotest.(check bool) "signature is a non-empty string" true
      (match sig_ with `String s -> String.length s > 0 | _ -> false)
  )

let test_query_signature_module_not_found () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (query_signature_call 2 "nonexistent" "double");
    let result = parse_response (read_line ()) in
    let ok = json_field "ok" result in
    Alcotest.(check bool) "ok is false" true
      (match ok with `Bool false -> true | _ -> false);
    let err = json_field "error" result in
    Alcotest.(check bool) "error message is a non-empty string" true
      (match err with `String s -> String.length s > 0 | _ -> false)
  )

let test_query_signature_function_not_found () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (submit_module_call 2 well_typed_source "mymod");
    ignore (read_line ());
    write_line (query_signature_call 3 "mymod" "nonexistent_fn");
    let result = parse_response (read_line ()) in
    let ok = json_field "ok" result in
    Alcotest.(check bool) "ok is false" true
      (match ok with `Bool false -> true | _ -> false);
    let err = json_field "error" result in
    Alcotest.(check bool) "error message is a non-empty string" true
      (match err with `String s -> String.length s > 0 | _ -> false)
  )

let test_query_signature_in_tools_list () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line {|{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}|};
    let line = read_line () in
    let json = mini_parse line in
    let result = json_field "result" json in
    let tools = json_field "tools" result in
    let names = match tools with
      | `List items ->
        List.filter_map (fun item ->
          match json_field "name" item with
          | `String s -> Some s
          | _ -> None) items
      | _ -> []
    in
    Alcotest.(check bool) "query_signature in tools/list" true
      (List.mem "query_signature" names)
  )

let mixed_visibility_source =
  {|pub fn add_one(x: Int) -> Int ! pure { add(x, 1) }
fn internal_helper(x: Int) -> Int ! pure { x }
pub type Color = | Red | Green | Blue|}

let contains_substring haystack needle =
  let hlen = String.length haystack and nlen = String.length needle in
  if nlen = 0 then true
  else if nlen > hlen then false
  else
    let found = ref false in
    for i = 0 to hlen - nlen do
      if not !found &&
         String.sub haystack i nlen = needle then
        found := true
    done;
    !found

let test_query_interface_returns_only_pub () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (submit_module_call 2 mixed_visibility_source "mymod");
    ignore (read_line ());
    write_line (query_interface_call 3 "mymod");
    let result = parse_response (read_line ()) in
    let iface = json_field "interface" result in
    let iface_str = match iface with `String s -> s | _ -> "" in
    Alcotest.(check bool) "interface contains pub fn" true
      (contains_substring iface_str "pub fn add_one");
    Alcotest.(check bool) "interface contains pub type" true
      (contains_substring iface_str "pub type Color");
    Alcotest.(check bool) "interface excludes private fn" true
      (not (contains_substring iface_str "internal_helper"))
  )

let test_query_interface_fn_has_no_body () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (submit_module_call 2 mixed_visibility_source "mymod");
    ignore (read_line ());
    write_line (query_interface_call 3 "mymod");
    let result = parse_response (read_line ()) in
    let iface = json_field "interface" result in
    let iface_str = match iface with `String s -> s | _ -> "" in
    Alcotest.(check bool) "interface is a non-empty string" true
      (String.length iface_str > 0);
    Alcotest.(check bool) "interface has no fn body braces" true
      (not (String.contains iface_str '{'))
  )

let test_query_interface_module_not_found () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (query_interface_call 2 "nonexistent");
    let result = parse_response (read_line ()) in
    let ok = json_field "ok" result in
    Alcotest.(check bool) "ok is false" true
      (match ok with `Bool false -> true | _ -> false);
    let err = json_field "error" result in
    Alcotest.(check bool) "error is non-empty string" true
      (match err with `String s -> String.length s > 0 | _ -> false)
  )

let test_query_interface_in_tools_list () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line {|{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}|};
    let line = read_line () in
    let json = mini_parse line in
    let result = json_field "result" json in
    let tools = json_field "tools" result in
    let names = match tools with
      | `List items ->
        List.filter_map (fun item ->
          match json_field "name" item with
          | `String s -> Some s
          | _ -> None) items
      | _ -> []
    in
    Alcotest.(check bool) "query_interface in tools/list" true
      (List.mem "query_interface" names)
  )

let verify_exhaustive_call id module_name =
  Printf.sprintf
    {|{"jsonrpc":"2.0","id":%d,"method":"tools/call","params":{"name":"verify_exhaustive","arguments":{"module_name":"%s"}}}|}
    id (json_string_escape module_name)

let exhaustive_source =
  {|type Color = | Red | Green | Blue
fn describe(c: Color) -> Int ! pure {
  match c with {
    | Red   => 0
    | Green => 1
    | Blue  => 2
  }
}|}

let test_verify_exhaustive_in_tools_list () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line {|{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}|};
    let line = read_line () in
    let json = mini_parse line in
    let result = json_field "result" json in
    let tools = json_field "tools" result in
    let names = match tools with
      | `List items ->
        List.filter_map (fun item ->
          match json_field "name" item with
          | `String s -> Some s
          | _ -> None) items
      | _ -> []
    in
    Alcotest.(check bool) "verify_exhaustive in tools/list" true
      (List.mem "verify_exhaustive" names)
  )

let test_verify_exhaustive_exhaustive_module () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (submit_module_call 2 exhaustive_source "mymod");
    ignore (read_line ());
    write_line (verify_exhaustive_call 3 "mymod");
    let result = parse_response (read_line ()) in
    let warnings = json_field "warnings" result in
    Alcotest.(check bool) "warnings is empty list" true
      (match warnings with `List [] -> true | _ -> false)
  )

let test_verify_exhaustive_module_not_found () =
  let (write_line, read_line, close) = start_server () in
  Fun.protect ~finally:close (fun () ->
    initialize write_line read_line;
    write_line (verify_exhaustive_call 2 "nonexistent");
    let result = parse_response (read_line ()) in
    let ok = json_field "ok" result in
    Alcotest.(check bool) "ok is false" true
      (match ok with `Bool false -> true | _ -> false);
    let err = json_field "error" result in
    Alcotest.(check bool) "error is non-empty string" true
      (match err with `String s -> String.length s > 0 | _ -> false)
  )

let () =
  ignore well_typed_source;
  ignore ill_typed_source;
  Alcotest.run "mcp_server" [
    ("verify_types", [
      Alcotest.test_case "well-typed program returns empty errors"    `Quick test_verify_types_well_typed;
      Alcotest.test_case "ill-typed program returns non-empty errors" `Quick test_verify_types_type_error;
      Alcotest.test_case "does not update server state"               `Quick test_verify_types_does_not_update_state;
      Alcotest.test_case "appears in tools/list"                      `Quick test_tools_list_includes_verify_types;
    ]);
    ("query_signature", [
      Alcotest.test_case "returns signature for known function"              `Quick test_query_signature_success;
      Alcotest.test_case "returns error when module not found"               `Quick test_query_signature_module_not_found;
      Alcotest.test_case "returns error when function not found in module"   `Quick test_query_signature_function_not_found;
      Alcotest.test_case "appears in tools/list"                             `Quick test_query_signature_in_tools_list;
    ]);
    ("query_interface", [
      Alcotest.test_case "returns only pub declarations"                     `Quick test_query_interface_returns_only_pub;
      Alcotest.test_case "function signatures have no body"                  `Quick test_query_interface_fn_has_no_body;
      Alcotest.test_case "returns error when module not found"               `Quick test_query_interface_module_not_found;
      Alcotest.test_case "appears in tools/list"                             `Quick test_query_interface_in_tools_list;
    ]);
    ("verify_exhaustive", [
      Alcotest.test_case "appears in tools/list"                             `Quick test_verify_exhaustive_in_tools_list;
      Alcotest.test_case "returns empty warnings for exhaustive module"      `Quick test_verify_exhaustive_exhaustive_module;
      Alcotest.test_case "returns error when module not found"               `Quick test_verify_exhaustive_module_not_found;
    ]);
  ]
