open Wasm_encode

(* ------------------------------------------------------------------ *)
(* Helpers                                                             *)
(* ------------------------------------------------------------------ *)

(** Write bytes to a temp file, run [f path], delete the file. *)
let with_tmp_wasm bytes f =
  let path = Filename.temp_file "axiom_wasm_" ".wasm" in
  let oc = open_out_bin path in
  output_bytes oc bytes;
  close_out oc;
  let result = f path in
  (try Sys.remove path with _ -> ());
  result

(** Validate a wasm binary using Node.js WebAssembly.validate.
    Returns Ok () on success, Error msg on failure. *)
let node_validate bytes =
  with_tmp_wasm bytes (fun path ->
    let quoted = Filename.quote path in
    let script =
      Printf.sprintf
        "const fs=require('fs');\
         const buf=fs.readFileSync(%s);\
         process.exit(WebAssembly.validate(buf)?0:1)"
        quoted
    in
    let cmd = Printf.sprintf "node -e %s 2>/dev/null" (Filename.quote script) in
    let rc = Sys.command cmd in
    if rc = 0 then Ok ()
    else Error (Printf.sprintf "WebAssembly.validate returned false (exit %d)" rc))

(** Instantiate a wasm module with Node.js and call [func_name] (no args).
    Returns the i32 result as an int, or Error msg. *)
let node_run bytes func_name =
  with_tmp_wasm bytes (fun path ->
    let quoted = Filename.quote path in
    let script =
      Printf.sprintf
        "const fs=require('fs');\
         const buf=fs.readFileSync(%s);\
         WebAssembly.instantiate(buf).then(({instance})=>{\
           const r=instance.exports[%s]();\
           console.log(r);\
           process.exit(0);\
         }).catch(e=>{console.error(e.message);process.exit(1);})"
        quoted
        (Printf.sprintf "'%s'" func_name)
    in
    let tmp_out = Filename.temp_file "axiom_wasm_out_" ".txt" in
    let cmd =
      Printf.sprintf "node -e %s >%s 2>/dev/null"
        (Filename.quote script)
        (Filename.quote tmp_out)
    in
    let rc = Sys.command cmd in
    let output =
      let ic = open_in tmp_out in
      let s = try input_line ic with End_of_file -> "" in
      close_in ic;
      (try Sys.remove tmp_out with _ -> ());
      s
    in
    if rc <> 0 then Error (Printf.sprintf "node exited with %d" rc)
    else
      match int_of_string_opt (String.trim output) with
      | Some n -> Ok n
      | None   -> Error (Printf.sprintf "unexpected output: %S" output))

(* ------------------------------------------------------------------ *)
(* LEB128 unit tests                                                   *)
(* ------------------------------------------------------------------ *)

let leb_bytes f n =
  let buf = Buffer.create 8 in
  f buf n;
  Buffer.to_bytes buf

let test_uleb128_single () =
  Alcotest.(check bytes) "uleb 0" (Bytes.of_string "\x00") (leb_bytes put_uleb128 0);
  Alcotest.(check bytes) "uleb 1" (Bytes.of_string "\x01") (leb_bytes put_uleb128 1);
  Alcotest.(check bytes) "uleb 63" (Bytes.of_string "\x3F") (leb_bytes put_uleb128 63);
  Alcotest.(check bytes) "uleb 127" (Bytes.of_string "\x7F") (leb_bytes put_uleb128 127)

let test_uleb128_multi () =
  (* 128 = 0x80 0x01 in LEB128 *)
  Alcotest.(check bytes) "uleb 128"
    (Bytes.of_string "\x80\x01") (leb_bytes put_uleb128 128);
  (* 300 = 0xAC 0x02 *)
  Alcotest.(check bytes) "uleb 300"
    (Bytes.of_string "\xAC\x02") (leb_bytes put_uleb128 300)

let test_sleb128_positive () =
  Alcotest.(check bytes) "sleb 0"  (Bytes.of_string "\x00") (leb_bytes put_sleb128 0);
  Alcotest.(check bytes) "sleb 42" (Bytes.of_string "\x2A") (leb_bytes put_sleb128 42);
  (* 64 needs two bytes: 0xC0 0x00 *)
  Alcotest.(check bytes) "sleb 64"
    (Bytes.of_string "\xC0\x00") (leb_bytes put_sleb128 64)

let test_sleb128_negative () =
  (* -1 = 0x7F *)
  Alcotest.(check bytes) "sleb -1"  (Bytes.of_string "\x7F") (leb_bytes put_sleb128 (-1));
  (* -128 = 0x80 0x7F *)
  Alcotest.(check bytes) "sleb -128"
    (Bytes.of_string "\x80\x7F") (leb_bytes put_sleb128 (-128))

(* ------------------------------------------------------------------ *)
(* Test 1: constant function  (func (result i32) i32.const 42)         *)
(* ------------------------------------------------------------------ *)

let build_answer_module () =
  let m = create () in
  let _ = add_function m ~export:"answer" [] [I32] [] [I32Const 42] in
  encode m

let test_constant_func_bytes () =
  let wasm = build_answer_module () in
  (* Magic + version *)
  Alcotest.(check bytes) "magic"
    (Bytes.of_string "\x00asm")
    (Bytes.sub wasm 0 4);
  Alcotest.(check int) "version word"
    1
    (Char.code (Bytes.get wasm 4) lor
     (Char.code (Bytes.get wasm 5) lsl 8) lor
     (Char.code (Bytes.get wasm 6) lsl 16) lor
     (Char.code (Bytes.get wasm 7) lsl 24));
  (* Section ids should appear in order: 1 (type), 3 (func), 7 (export), 10 (code) *)
  let b i = Char.code (Bytes.get wasm i) in
  Alcotest.(check int) "type section id"   1 (b 8);
  (* Find function section after type section *)
  let type_sec_size = b 9 in
  let func_sec_offset = 8 + 2 + type_sec_size in
  Alcotest.(check int) "func section id"   3 (b func_sec_offset);
  ignore func_sec_offset

let test_constant_func_valid () =
  let wasm = build_answer_module () in
  match node_validate wasm with
  | Ok ()     -> ()
  | Error msg -> Alcotest.fail ("wasm invalid: " ^ msg)

let test_constant_func_runs () =
  let wasm = build_answer_module () in
  match node_run wasm "answer" with
  | Ok 42     -> ()
  | Ok n      -> Alcotest.failf "expected 42, got %d" n
  | Error msg -> Alcotest.fail ("execution failed: " ^ msg)

(* ------------------------------------------------------------------ *)
(* Test 2: function with locals + arithmetic                            *)
(*   (func (export "compute") (result i32)                             *)
(*     (local i32)      ;; local 0                                     *)
(*     i32.const 10                                                     *)
(*     local.set 0                                                      *)
(*     local.get 0                                                      *)
(*     i32.const 32                                                     *)
(*     i32.add)         ;; result: 42                                  *)
(* ------------------------------------------------------------------ *)

let build_locals_module () =
  let m = create () in
  let _ =
    add_function m ~export:"compute" [] [I32]
      [{ count = 1; ty = I32 }]
      [ I32Const 10
      ; LocalSet 0
      ; LocalGet 0
      ; I32Const 32
      ; I32Add
      ]
  in
  encode m

let test_locals_valid () =
  let wasm = build_locals_module () in
  match node_validate wasm with
  | Ok ()     -> ()
  | Error msg -> Alcotest.fail ("wasm invalid: " ^ msg)

let test_locals_runs () =
  let wasm = build_locals_module () in
  match node_run wasm "compute" with
  | Ok 42     -> ()
  | Ok n      -> Alcotest.failf "expected 42, got %d" n
  | Error msg -> Alcotest.fail ("execution failed: " ^ msg)

(* ------------------------------------------------------------------ *)
(* Test 3: control flow — if/else                                       *)
(*   (func (export "abs_diff") (param i32 i32) (result i32)            *)
(*     local.get 0                                                      *)
(*     local.get 1                                                      *)
(*     i32.gt_s                                                         *)
(*     if (result i32)                                                  *)
(*       local.get 0  local.get 1  i32.sub                             *)
(*     else                                                             *)
(*       local.get 1  local.get 0  i32.sub                             *)
(*     end)                                                             *)
(* ------------------------------------------------------------------ *)

let build_if_module () =
  let m = create () in
  let _ =
    add_function m ~export:"abs_diff" [I32; I32] [I32] []
      [ LocalGet 0
      ; LocalGet 1
      ; I32GtS
      ; If (ValType I32,
            [LocalGet 0; LocalGet 1; I32Sub],
            Some [LocalGet 1; LocalGet 0; I32Sub])
      ]
  in
  encode m

let test_if_valid () =
  let wasm = build_if_module () in
  match node_validate wasm with
  | Ok ()     -> ()
  | Error msg -> Alcotest.fail ("wasm invalid: " ^ msg)

let test_if_runs_pos () =
  let wasm = build_if_module () in
  (* abs_diff(10, 3) = 7 *)
  with_tmp_wasm wasm (fun path ->
    let quoted = Filename.quote path in
    let script =
      Printf.sprintf
        "const fs=require('fs');\
         const buf=fs.readFileSync(%s);\
         WebAssembly.instantiate(buf).then(({instance})=>{\
           console.log(instance.exports.abs_diff(10,3));\
         });"
        quoted
    in
    let tmp = Filename.temp_file "axiom_wasm_if_" ".txt" in
    ignore (Sys.command (Printf.sprintf "node -e %s >%s 2>/dev/null"
                           (Filename.quote script) (Filename.quote tmp)));
    let ic = open_in tmp in
    let s = (try input_line ic with End_of_file -> "") in
    close_in ic;
    (try Sys.remove tmp with _ -> ());
    Alcotest.(check string) "abs_diff(10,3)=7" "7" (String.trim s))

(* ------------------------------------------------------------------ *)
(* Test 4: loop / br_if — count down                                   *)
(*   (func (export "sum_n") (param i32) (result i32)                   *)
(*     (local i32)   ;; acc in local 1                                 *)
(*     block $exit                                                      *)
(*       loop $top                                                      *)
(*         local.get 0  i32.const 0  i32.eq  br_if 1   ;; $exit        *)
(*         local.get 1  local.get 0  i32.add  local.set 1              *)
(*         local.get 0  i32.const 1  i32.sub  local.set 0              *)
(*         br 0         ;; $top                                         *)
(*       end                                                            *)
(*     end                                                              *)
(*     local.get 1)   ;; return acc                                     *)
(* ------------------------------------------------------------------ *)

let build_loop_module () =
  let m = create () in
  let _ =
    add_function m ~export:"sum_n" [I32] [I32]
      [{ count = 1; ty = I32 }]   (* local 1: accumulator *)
      [ Block (Void,
          [ Loop (Void,
              [ LocalGet 0; I32Const 0; I32Eq; BrIf 1   (* exit block *)
              ; LocalGet 1; LocalGet 0; I32Add; LocalSet 1
              ; LocalGet 0; I32Const 1; I32Sub; LocalSet 0
              ; Br 0                                     (* top of loop *)
              ])
          ])
      ; LocalGet 1
      ]
  in
  encode m

let test_loop_valid () =
  let wasm = build_loop_module () in
  match node_validate wasm with
  | Ok ()     -> ()
  | Error msg -> Alcotest.fail ("wasm invalid: " ^ msg)

let test_loop_runs () =
  (* sum_n(10) = 55 *)
  let wasm = build_loop_module () in
  with_tmp_wasm wasm (fun path ->
    let quoted = Filename.quote path in
    let script =
      Printf.sprintf
        "const fs=require('fs');\
         const buf=fs.readFileSync(%s);\
         WebAssembly.instantiate(buf).then(({instance})=>{\
           console.log(instance.exports.sum_n(10));\
         });"
        quoted
    in
    let tmp = Filename.temp_file "axiom_wasm_loop_" ".txt" in
    ignore (Sys.command (Printf.sprintf "node -e %s >%s 2>/dev/null"
                           (Filename.quote script) (Filename.quote tmp)));
    let ic = open_in tmp in
    let s = (try input_line ic with End_of_file -> "") in
    close_in ic;
    (try Sys.remove tmp with _ -> ());
    Alcotest.(check string) "sum_n(10)=55" "55" (String.trim s))

(* ------------------------------------------------------------------ *)
(* Test 5: call instruction                                            *)
(*   (func $double (param i32) (result i32)                            *)
(*     local.get 0  local.get 0  i32.add)                              *)
(*   (func (export "quad") (param i32) (result i32)                    *)
(*     local.get 0  call $double  call $double)                        *)
(* ------------------------------------------------------------------ *)

let build_call_module () =
  let m = create () in
  let double_idx =
    add_function m [I32] [I32] []
      [LocalGet 0; LocalGet 0; I32Add]
  in
  let _ =
    add_function m ~export:"quad" [I32] [I32] []
      [LocalGet 0; Call double_idx; Call double_idx]
  in
  encode m

let test_call_valid () =
  let wasm = build_call_module () in
  match node_validate wasm with
  | Ok ()     -> ()
  | Error msg -> Alcotest.fail ("wasm invalid: " ^ msg)

let test_call_runs () =
  let wasm = build_call_module () in
  with_tmp_wasm wasm (fun path ->
    let quoted = Filename.quote path in
    let script =
      Printf.sprintf
        "const fs=require('fs');\
         const buf=fs.readFileSync(%s);\
         WebAssembly.instantiate(buf).then(({instance})=>{\
           console.log(instance.exports.quad(3));\
         });"
        quoted
    in
    let tmp = Filename.temp_file "axiom_wasm_call_" ".txt" in
    ignore (Sys.command (Printf.sprintf "node -e %s >%s 2>/dev/null"
                           (Filename.quote script) (Filename.quote tmp)));
    let ic = open_in tmp in
    let s = (try input_line ic with End_of_file -> "") in
    close_in ic;
    (try Sys.remove tmp with _ -> ());
    Alcotest.(check string) "quad(3)=12" "12" (String.trim s))

(* ------------------------------------------------------------------ *)
(* Test 6: globals and memory (bump allocator)                        *)
(*   (memory 1)                                                        *)
(*   (global $g (mut i32) (i32.const 1024))                           *)
(*   (func (export "alloc") (param i32) (result i32)                  *)
(*     global.get $g  local.tee 1  local.get 0  i32.add               *)
(*     global.set $g  local.get 1)                                     *)
(*   (func (export "store_load") (result i32)                          *)
(*     ;; alloc 4 bytes, store 99, reload and return it               *)
(*     i32.const 4  call $alloc  local.tee 0                          *)
(*     i32.const 99  i32.store align=2 offset=0                        *)
(*     local.get 0  i32.load align=2 offset=0)                         *)
(* ------------------------------------------------------------------ *)

let build_alloc_module () =
  let m = create () in
  add_memory m { min = 1; max = None };
  add_export m "memory" (ExportMem 0);
  let g = add_global m I32 true (GlobI32 1024) in
  let alloc_idx =
    add_function m ~export:"alloc" [I32] [I32]
      [{ count = 1; ty = I32 }]
      [ GlobalGet g; LocalTee 1; LocalGet 0; I32Add; GlobalSet g; LocalGet 1 ]
  in
  let _ =
    add_function m ~export:"store_load" [] [I32]
      [{ count = 1; ty = I32 }]
      [ I32Const 4; Call alloc_idx; LocalTee 0
      ; I32Const 99; I32Store (2, 0)
      ; LocalGet 0;  I32Load  (2, 0)
      ]
  in
  encode m

let test_alloc_valid () =
  match node_validate (build_alloc_module ()) with
  | Ok ()     -> ()
  | Error msg -> Alcotest.fail ("wasm invalid: " ^ msg)

let test_alloc_runs () =
  with_tmp_wasm (build_alloc_module ()) (fun path ->
    let quoted = Filename.quote path in
    let script =
      Printf.sprintf
        "const fs=require('fs');\
         const buf=fs.readFileSync(%s);\
         WebAssembly.instantiate(buf).then(({instance})=>{\
           const e=instance.exports;\
           const ptr=e.alloc(8);\
           if(ptr<1024){process.exit(1);}\
           const mem=new Uint32Array(e.memory.buffer);\
           mem[ptr>>2]=0xCAFE;\
           if(mem[ptr>>2]!==0xCAFE){process.exit(2);}\
           const ptr2=e.alloc(4);\
           if(ptr2!==ptr+8){process.exit(3);}\
           console.log(e.store_load());\
         }).catch(e=>{console.error(e.message);process.exit(4);});"
        quoted
    in
    let tmp = Filename.temp_file "axiom_wasm_alloc_" ".txt" in
    ignore (Sys.command (Printf.sprintf "node -e %s >%s 2>/dev/null"
                           (Filename.quote script) (Filename.quote tmp)));
    let ic = open_in tmp in
    let s = (try input_line ic with End_of_file -> "") in
    close_in ic;
    (try Sys.remove tmp with _ -> ());
    Alcotest.(check string) "store_load()=99" "99" (String.trim s))

(* ------------------------------------------------------------------ *)
(* Test suite registration                                             *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "wasm_encode"
    [ ( "leb128",
        [ Alcotest.test_case "uleb128 single byte" `Quick test_uleb128_single
        ; Alcotest.test_case "uleb128 multi byte"  `Quick test_uleb128_multi
        ; Alcotest.test_case "sleb128 positive"    `Quick test_sleb128_positive
        ; Alcotest.test_case "sleb128 negative"    `Quick test_sleb128_negative
        ] )
    ; ( "constant_func",
        [ Alcotest.test_case "byte layout"  `Quick test_constant_func_bytes
        ; Alcotest.test_case "wasm-validate" `Quick test_constant_func_valid
        ; Alcotest.test_case "executes 42"   `Quick test_constant_func_runs
        ] )
    ; ( "locals_and_arithmetic",
        [ Alcotest.test_case "wasm-validate" `Quick test_locals_valid
        ; Alcotest.test_case "executes 42"   `Quick test_locals_runs
        ] )
    ; ( "if_else",
        [ Alcotest.test_case "wasm-validate"      `Quick test_if_valid
        ; Alcotest.test_case "abs_diff(10,3)=7"   `Quick test_if_runs_pos
        ] )
    ; ( "loop_and_br",
        [ Alcotest.test_case "wasm-validate"  `Quick test_loop_valid
        ; Alcotest.test_case "sum_n(10)=55"   `Quick test_loop_runs
        ] )
    ; ( "call",
        [ Alcotest.test_case "wasm-validate" `Quick test_call_valid
        ; Alcotest.test_case "quad(3)=12"    `Quick test_call_runs
        ] )
    ; ( "globals_and_memory",
        [ Alcotest.test_case "wasm-validate"   `Quick test_alloc_valid
        ; Alcotest.test_case "alloc/store/load" `Quick test_alloc_runs
        ] )
    ]
