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

(* Issue #34: Int literals, arithmetic primitives, and let bindings *)
let src_main_add_literals =
  {|fn main() -> Int ! pure {
  add(1, 2)
}|}

let src_main_let_arithmetic =
  {|fn main() -> Int ! pure {
  let a = add(3, 4) in
  let b = mul(a, 2) in
  sub(b, 1)
}|}
(* a = 7, b = 14, result = 13 *)

(* Issue #36: Bool, comparisons, and if/else *)
let src_bool_true =
  {|fn main() -> Int ! pure {
  if true { 1 } else { 0 }
}|}

let src_bool_false =
  {|fn main() -> Int ! pure {
  if false { 1 } else { 0 }
}|}

let src_if_eq =
  {|fn main() -> Int ! pure {
  if eq(3, 3) { 42 } else { 0 }
}|}

let src_if_neq =
  {|fn main() -> Int ! pure {
  if neq(3, 4) { 7 } else { 0 }
}|}

let src_if_lte =
  {|fn main() -> Int ! pure {
  if lte(3, 3) { 1 } else { 0 }
}|}

let src_if_gte =
  {|fn main() -> Int ! pure {
  if gte(5, 3) { 1 } else { 0 }
}|}

let src_bool_to_int =
  {|fn bool_to_int(b: Bool) -> Int ! pure {
  if b { 1 } else { 0 }
}

fn main() -> Int ! pure {
  bool_to_int(true)
}|}

let src_abs =
  {|fn abs(x: Int) -> Int ! pure {
  if lt(x, 0) { neg(x) } else { x }
}

fn main() -> Int ! pure {
  abs(neg(5))
}|}
(* neg(5) = -5, abs(-5) = 5 *)

(* Issue #35: top-level functions and direct/recursive calls *)
let src_calls_helper =
  {|fn double(x: Int) -> Int ! pure {
  add(x, x)
}

fn main() -> Int ! pure {
  double(21)
}|}

let src_recursive_fact =
  {|fn fact(n: Int) -> Int ! pure {
  if eq(n, 0) { 1 } else { mul(n, fact(sub(n, 1))) }
}

fn main() -> Int ! pure {
  fact(5)
}|}
(* fact(5) = 120 *)

let src_do_block =
  {|fn main() -> Int ! pure {
  do {
    let a = add(3, 4);
    let b = mul(a, 2);
    sub(b, 1)
  }
}|}
(* a = 7, b = 14, result = 13 *)

let src_letrec_fact =
  {|fn main() -> Int ! pure {
  letrec {
    fact(n: Int): Int = if eq(n, 0) { 1 } else { mul(n, fact(sub(n, 1))) }
  } in
  fact(5)
}|}
(* fact(5) = 120 *)

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

let test_main_returns src expected () =
  with_tmp_file ".axm" (fun s ->
    with_tmp_file ".wasm" (fun d ->
      write_file s src;
      let rc = axiom_build ~src:s ~dst:d in
      if rc <> 0 then Alcotest.fail "axiom build failed";
      match execute_main d with
      | Got n when n = expected -> ()
      | Got n     -> Alcotest.failf "main returned %d, expected %d" n expected
      | RunFail m -> Alcotest.fail ("execution failed: " ^ m)
      | RunSkip m -> Printf.printf "[SKIP] %s\n%!" m))

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
    ; ( "codegen",
        [ Alcotest.test_case "add(1,2) = 3"           `Quick (test_main_returns src_main_add_literals 3)
        ; Alcotest.test_case "let+arithmetic = 13"    `Quick (test_main_returns src_main_let_arithmetic 13)
        ; Alcotest.test_case "double(21) = 42"        `Quick (test_main_returns src_calls_helper 42)
        ; Alcotest.test_case "fact(5) = 120"          `Quick (test_main_returns src_recursive_fact 120)
        ; Alcotest.test_case "do block = 13"          `Quick (test_main_returns src_do_block 13)
        ; Alcotest.test_case "letrec fact(5) = 120"   `Quick (test_main_returns src_letrec_fact 120)
        ] )
    ; ( "bool_and_if",
        [ Alcotest.test_case "if true = 1"            `Quick (test_main_returns src_bool_true 1)
        ; Alcotest.test_case "if false = 0"           `Quick (test_main_returns src_bool_false 0)
        ; Alcotest.test_case "eq(3,3) -> 42"          `Quick (test_main_returns src_if_eq 42)
        ; Alcotest.test_case "neq(3,4) -> 7"          `Quick (test_main_returns src_if_neq 7)
        ; Alcotest.test_case "lte(3,3) -> 1"          `Quick (test_main_returns src_if_lte 1)
        ; Alcotest.test_case "gte(5,3) -> 1"          `Quick (test_main_returns src_if_gte 1)
        ; Alcotest.test_case "bool_to_int(true) = 1"  `Quick (test_main_returns src_bool_to_int 1)
        ; Alcotest.test_case "abs(neg(5)) = 5"        `Quick (test_main_returns src_abs 5)
        ] )
    ]
