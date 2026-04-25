(** End-to-end test harness for the axiom build pipeline.

    Each test:
    1. Writes a small .axm source to a temp file.
    2. Invokes the compiled [axiom] binary with [build <src> -o <dst>].
    3. Validates the resulting .wasm binary.
    4. Optionally runs the .wasm under an available engine and checks the result.

    Validation uses [wasm-validate] when installed, falling back to Node.js
    [WebAssembly.validate].  Execution uses [wasmtime] when installed, falling
    back to Node.js [WebAssembly.instantiate].  When neither is available for
    a given step, the test prints a skip notice and passes, satisfying the CI
    graceful-skip requirement. *)

(* ------------------------------------------------------------------ *)
(* Locate the compiled axiom binary                                    *)
(* ------------------------------------------------------------------ *)

(** [dune test] sets AXIOM_EXE via the [(setenv ...)] action in test/dune;
    fall back to path-relative heuristics for ad-hoc runs. *)
let axiom_exe =
  match Sys.getenv_opt "AXIOM_EXE" with
  | Some p -> p
  | None ->
    let test_dir = Filename.dirname Sys.argv.(0) in
    let candidates =
      [ Filename.concat test_dir "../bin/main.exe"
      ; Filename.concat test_dir "../bin/main"
      ; Filename.concat (Sys.getcwd ()) "_build/default/bin/main.exe"
      ; Filename.concat (Sys.getcwd ()) "_build/default/bin/main"
      ]
    in
    (match List.find_opt Sys.file_exists candidates with
     | Some p -> p
     | None   -> "axiom")

(* ------------------------------------------------------------------ *)
(* Engine detection                                                    *)
(* ------------------------------------------------------------------ *)

let command_exists cmd =
  Sys.command (Printf.sprintf "command -v %s >/dev/null 2>&1" cmd) = 0

let has_wasm_validate = lazy (command_exists "wasm-validate")
let has_wasmtime      = lazy (command_exists "wasmtime")
let has_node          = lazy (command_exists "node")

(* ------------------------------------------------------------------ *)
(* Temp-file / IO helpers                                              *)
(* ------------------------------------------------------------------ *)

let with_tmp_file suffix f =
  let path = Filename.temp_file "axiom_pipeline_" suffix in
  Fun.protect
    (fun () -> f path)
    ~finally:(fun () -> try Sys.remove path with _ -> ())

let write_file path contents =
  let oc = open_out path in
  output_string oc contents;
  close_out oc

let read_file_first_bytes path n =
  let ic = open_in_bin path in
  let buf = Bytes.create n in
  (try really_input ic buf 0 n with End_of_file -> ());
  close_in ic;
  buf

(* ------------------------------------------------------------------ *)
(* Invoke the axiom binary                                             *)
(* ------------------------------------------------------------------ *)

let axiom_build ~src ~dst =
  let cmd =
    Printf.sprintf "%s build %s -o %s 2>/dev/null"
      (Filename.quote axiom_exe)
      (Filename.quote src)
      (Filename.quote dst)
  in
  Sys.command cmd

(* ------------------------------------------------------------------ *)
(* Validation                                                          *)
(* ------------------------------------------------------------------ *)

type check_result = Pass | Fail of string | Skip of string

let validate_wasm path =
  if Lazy.force has_wasm_validate then
    let rc =
      Sys.command
        (Printf.sprintf "wasm-validate %s >/dev/null 2>&1" (Filename.quote path))
    in
    if rc = 0 then Pass else Fail "wasm-validate rejected the binary"
  else if Lazy.force has_node then
    let script =
      Printf.sprintf
        "const fs=require('fs');\
         const buf=fs.readFileSync(%s);\
         process.exit(WebAssembly.validate(buf)?0:1)"
        (Filename.quote path)
    in
    let rc =
      Sys.command
        (Printf.sprintf "node -e %s 2>/dev/null" (Filename.quote script))
    in
    if rc = 0 then Pass else Fail "WebAssembly.validate returned false"
  else
    Skip "no wasm validator available (install wasm-validate or node)"

(* ------------------------------------------------------------------ *)
(* Execution                                                           *)
(* ------------------------------------------------------------------ *)

type run_result = Got of int | RunFail of string | RunSkip of string

let run_via_wasmtime path =
  let tmp = Filename.temp_file "axiom_wasmtime_" ".txt" in
  let rc =
    Sys.command
      (Printf.sprintf "wasmtime --invoke main %s >%s 2>&1"
         (Filename.quote path) (Filename.quote tmp))
  in
  let out =
    let ic = open_in tmp in
    let s = try input_line ic with End_of_file -> "" in
    close_in ic;
    (try Sys.remove tmp with _ -> ());
    String.trim s
  in
  (* wasmtime prints the return value; a 0-result shows as "0". *)
  if rc = 0 then
    (match int_of_string_opt out with
     | Some n -> Got n
     | None   -> Got 0)   (* no output means void / success *)
  else RunFail (Printf.sprintf "wasmtime exited %d" rc)

let run_via_node path =
  let script =
    Printf.sprintf
      "const fs=require('fs');\
       const buf=fs.readFileSync(%s);\
       WebAssembly.instantiate(buf).then(({instance})=>{\
         const r=instance.exports.main();\
         console.log(r);\
       }).catch(e=>{console.error(e.message);process.exit(1);})"
      (Filename.quote path)
  in
  let tmp = Filename.temp_file "axiom_node_" ".txt" in
  let rc =
    Sys.command
      (Printf.sprintf "node -e %s >%s 2>/dev/null"
         (Filename.quote script) (Filename.quote tmp))
  in
  let out =
    let ic = open_in tmp in
    let s = try input_line ic with End_of_file -> "" in
    close_in ic;
    (try Sys.remove tmp with _ -> ());
    String.trim s
  in
  if rc = 0 then
    (match int_of_string_opt out with
     | Some n -> Got n
     | None   -> RunFail (Printf.sprintf "unexpected output: %S" out))
  else RunFail (Printf.sprintf "node exited %d" rc)

let execute_main path =
  if Lazy.force has_wasmtime then run_via_wasmtime path
  else if Lazy.force has_node then run_via_node path
  else RunSkip "no wasm runtime available (install wasmtime or node)"

(* ------------------------------------------------------------------ *)
(* Source fixtures                                                     *)
(* ------------------------------------------------------------------ *)

let src_empty = ""

let src_simple_fn =
  {|fn add(x: Int, y: Int) -> Int ! pure {
  add(x, y)
}
|}

let src_type_decl =
  {|type Color = | Red | Green | Blue

fn color_id(c: Color) -> Color ! pure {
  match c with {
    | Red => Red
    | Green => Green
    | Blue => Blue
  }
}
|}

(* ------------------------------------------------------------------ *)
(* Build tests                                                         *)
(* ------------------------------------------------------------------ *)

let test_build_produces_file src () =
  with_tmp_file ".axm" (fun s ->
    with_tmp_file ".wasm" (fun d ->
      write_file s src;
      let rc = axiom_build ~src:s ~dst:d in
      Alcotest.(check int)  "exit 0"           0    rc;
      Alcotest.(check bool) "output exists" true (Sys.file_exists d)))

let test_bad_input_file () =
  with_tmp_file ".wasm" (fun dst ->
    let rc = axiom_build ~src:"/nonexistent/file.axm" ~dst in
    Alcotest.(check bool) "non-zero exit" true (rc <> 0))

(* ------------------------------------------------------------------ *)
(* Output format tests                                                 *)
(* ------------------------------------------------------------------ *)

let test_wasm_magic_bytes src () =
  with_tmp_file ".axm" (fun s ->
    with_tmp_file ".wasm" (fun d ->
      write_file s src;
      let rc = axiom_build ~src:s ~dst:d in
      if rc <> 0 then Alcotest.fail "axiom build failed";
      let magic = read_file_first_bytes d 4 in
      Alcotest.(check bytes) "WASM magic \\x00asm"
        (Bytes.of_string "\x00asm") magic))

(* ------------------------------------------------------------------ *)
(* Validation tests                                                    *)
(* ------------------------------------------------------------------ *)

let test_validates src () =
  with_tmp_file ".axm" (fun s ->
    with_tmp_file ".wasm" (fun d ->
      write_file s src;
      let rc = axiom_build ~src:s ~dst:d in
      if rc <> 0 then Alcotest.fail "axiom build failed";
      match validate_wasm d with
      | Pass     -> ()
      | Fail msg -> Alcotest.fail msg
      | Skip msg -> Printf.printf "[SKIP] %s\n%!" msg))

(* ------------------------------------------------------------------ *)
(* Execution tests                                                     *)
(* ------------------------------------------------------------------ *)

let test_main_returns_zero src () =
  with_tmp_file ".axm" (fun s ->
    with_tmp_file ".wasm" (fun d ->
      write_file s src;
      let rc = axiom_build ~src:s ~dst:d in
      if rc <> 0 then Alcotest.fail "axiom build failed";
      match execute_main d with
      | Got 0      -> ()
      | Got n      -> Alcotest.failf "main returned %d, expected 0" n
      | RunFail m  -> Alcotest.fail ("execution failed: " ^ m)
      | RunSkip m  -> Printf.printf "[SKIP] %s\n%!" m))

(* ------------------------------------------------------------------ *)
(* Suite                                                               *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "build_pipeline"
    [ ( "build",
        [ Alcotest.test_case "empty program"  `Quick (test_build_produces_file src_empty)
        ; Alcotest.test_case "simple fn"      `Quick (test_build_produces_file src_simple_fn)
        ; Alcotest.test_case "type decl"      `Quick (test_build_produces_file src_type_decl)
        ; Alcotest.test_case "bad input file" `Quick test_bad_input_file
        ] )
    ; ( "output_format",
        [ Alcotest.test_case "wasm magic (empty)"     `Quick (test_wasm_magic_bytes src_empty)
        ; Alcotest.test_case "wasm magic (simple fn)" `Quick (test_wasm_magic_bytes src_simple_fn)
        ] )
    ; ( "validation",
        [ Alcotest.test_case "empty program"  `Quick (test_validates src_empty)
        ; Alcotest.test_case "simple fn"      `Quick (test_validates src_simple_fn)
        ; Alcotest.test_case "type decl"      `Quick (test_validates src_type_decl)
        ] )
    ; ( "execution",
        [ Alcotest.test_case "main returns 0 (empty)"     `Quick (test_main_returns_zero src_empty)
        ; Alcotest.test_case "main returns 0 (simple fn)" `Quick (test_main_returns_zero src_simple_fn)
        ] )
    ]
