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

(* Path to runtime.wasm produced by `zig build` in runtime/.
   This is a build artifact, not a tracked file; tests skip when absent. *)
let runtime_wasm_path =
  let here = Filename.dirname Sys.argv.(0) in
  let candidates =
    [ Filename.concat here "../../runtime/zig-out/bin/runtime.wasm"
    ; Filename.concat (Sys.getcwd ()) "runtime/zig-out/bin/runtime.wasm"
    ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some p -> p
  | None   -> "runtime/zig-out/bin/runtime.wasm"   (* absent => tests skip *)

let has_runtime = lazy (Sys.file_exists runtime_wasm_path)

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

(** Run [axiom_path] via the Axiom runtime:
    1. Instantiate [axiom_path] normally (it exports [main]).
    2. Instantiate [runtime.wasm] with the Axiom module's [main] as the
       [axiom_program.main] import.
    3. Call [_start] on the runtime instance.
    4. Return the value from [get_exit_code]. *)
let run_via_runtime axiom_path =
  if not (Lazy.force has_node) then
    RunSkip "no node.js runtime available"
  else if not (Lazy.force has_runtime) then
    RunSkip (Printf.sprintf "runtime.wasm not found at %s (run 'zig build' in runtime/)" runtime_wasm_path)
  else
    let script =
      Printf.sprintf
        "const fs=require('fs');\
         const axiomBuf=fs.readFileSync(%s);\
         const rtBuf=fs.readFileSync(%s);\
         WebAssembly.instantiate(axiomBuf)\
           .then(({instance:ai})=>{\
             const axiomMain=ai.exports.main;\
             return WebAssembly.instantiate(rtBuf,{axiom_program:{main:axiomMain}});\
           })\
           .then(({instance:ri})=>{\
             ri.exports._start();\
             console.log(ri.exports.get_exit_code());\
           })\
           .catch(e=>{console.error(e.message);process.exit(1);})"
        (Filename.quote axiom_path)
        (Filename.quote runtime_wasm_path)
    in
    let tmp = Filename.temp_file "axiom_runtime_" ".txt" in
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

let test_main_returns_via_runtime src expected () =
  with_tmp_file ".axm" (fun s ->
    with_tmp_file ".wasm" (fun d ->
      write_file s src;
      let rc = axiom_build ~src:s ~dst:d in
      if rc <> 0 then Alcotest.fail "axiom build failed";
      match run_via_runtime d with
      | Got n when n = expected -> ()
      | Got n     -> Alcotest.failf "runtime returned %d, expected %d" n expected
      | RunFail m -> Alcotest.fail ("runtime execution failed: " ^ m)
      | RunSkip m -> Printf.printf "[SKIP] %s\n%!" m))

(* ------------------------------------------------------------------ *)
(* Multi-file build helpers                                            *)
(* ------------------------------------------------------------------ *)

(** Create a temporary directory, write multiple source files into it, build
    the named entry file, and run [f] on the resulting .wasm path. *)
let with_tmp_dir f =
  let dir = Filename.temp_file "axiom_multifile_" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  Fun.protect
    (fun () -> f dir)
    ~finally:(fun () ->
      (* Remove all files in dir, then the dir itself. *)
      (try
         let entries = Sys.readdir dir in
         Array.iter (fun name ->
           (try Sys.remove (Filename.concat dir name) with _ -> ())
         ) entries;
         Unix.rmdir dir
       with _ -> ()))

let axiom_build_in_dir ~dir ~entry ~dst =
  let src = Filename.concat dir entry in
  axiom_build ~src ~dst

let test_multifile_returns files entry expected () =
  with_tmp_dir (fun dir ->
    List.iter (fun (name, contents) ->
      write_file (Filename.concat dir name) contents
    ) files;
    with_tmp_file ".wasm" (fun d ->
      let rc = axiom_build_in_dir ~dir ~entry ~dst:d in
      if rc <> 0 then Alcotest.fail "axiom build failed";
      match execute_main d with
      | Got n when n = expected -> ()
      | Got n     -> Alcotest.failf "main returned %d, expected %d" n expected
      | RunFail m -> Alcotest.fail ("execution failed: " ^ m)
      | RunSkip m -> Printf.printf "[SKIP] %s\n%!" m))

let test_multifile_validates files entry () =
  with_tmp_dir (fun dir ->
    List.iter (fun (name, contents) ->
      write_file (Filename.concat dir name) contents
    ) files;
    with_tmp_file ".wasm" (fun d ->
      let rc = axiom_build_in_dir ~dir ~entry ~dst:d in
      if rc <> 0 then Alcotest.fail "axiom build failed";
      match validate_wasm d with
      | Pass     -> ()
      | Fail msg -> Alcotest.fail msg
      | Skip msg -> Printf.printf "[SKIP] %s\n%!" msg))

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

(* Issue #38: ADT constructors and pattern matching *)

(* Nullary enum: match returns tag as Int *)
let src_color_match =
  {|type Color = | Red | Green | Blue

fn color_to_int(c: Color) -> Int ! pure {
  match c with {
    | Red   => 0
    | Green => 1
    | Blue  => 2
  }
}

fn main() -> Int ! pure {
  color_to_int(Green)
}|}

(* Point(x, y) constructor + field projection *)
let src_point_get_x =
  {|type Point = | Point(Int, Int)

fn get_x(p: Point) -> Int ! pure {
  match p with {
    | Point(x, y) => x
  }
}

fn main() -> Int ! pure {
  get_x(Point(42, 7))
}|}

(* Wildcard arm *)
let src_wildcard_match =
  {|type Shape = | Circle | Square | Triangle

fn is_circle(s: Shape) -> Int ! pure {
  match s with {
    | Circle => 1
    | _      => 0
  }
}

fn main() -> Int ! pure {
  is_circle(Square)
}|}

(* Match on constructor with two fields, return second field *)
let src_point_get_y =
  {|type Point = | Point(Int, Int)

fn get_y(p: Point) -> Int ! pure {
  match p with {
    | Point(x, y) => y
  }
}

fn main() -> Int ! pure {
  get_y(Point(3, 99))
}|}

(* ------------------------------------------------------------------ *)
(* Allocator execution helper                                          *)
(* ------------------------------------------------------------------ *)

(** Instantiate the module at [path] via Node.js, call __alloc twice,
    write/read through the exported memory, and return the first
    allocation address. *)
let run_alloc_check path =
  if not (Lazy.force has_node) then
    RunSkip "no node.js runtime available"
  else
    let script =
      Printf.sprintf
        "const fs=require('fs');\
         const buf=fs.readFileSync(%s);\
         WebAssembly.instantiate(buf).then(({instance})=>{\
           const e=instance.exports;\
           const ptr=e.__alloc(8);\
           if(ptr<1024){console.error('ptr too low:'+ptr);process.exit(1);}\
           const mem=new Uint32Array(e.memory.buffer);\
           mem[ptr>>2]=0xABCD;\
           if(mem[ptr>>2]!==0xABCD){console.error('rw mismatch');process.exit(2);}\
           const ptr2=e.__alloc(4);\
           if(ptr2!==ptr+8){console.error('bump wrong:'+ptr2);process.exit(3);}\
           console.log(ptr);\
         }).catch(e=>{console.error(e.message);process.exit(4);});"
        (Filename.quote path)
    in
    let tmp = Filename.temp_file "axiom_alloc_" ".txt" in
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
    if rc <> 0 then RunFail (Printf.sprintf "node exited %d" rc)
    else
      match int_of_string_opt out with
      | Some n -> Got n
      | None   -> RunFail (Printf.sprintf "unexpected output: %S" out)

(* ------------------------------------------------------------------ *)
(* Issue #39: decision-tree pattern matching fixtures                  *)
(* ------------------------------------------------------------------ *)

(* Recursive ADT: length of a linked list *)
let src_list_length =
  {|type List = | Nil | Cons(Int, List)

fn length(l: List) -> Int ! pure {
  match l with {
    | Nil => 0
    | Cons(_, t) => add(1, length(t))
  }
}

fn main() -> Int ! pure {
  length(Cons(1, Cons(2, Cons(3, Nil))))
}|}
(* length([1,2,3]) = 3 *)

(* Or-patterns: two constructors share one arm *)
let src_or_pattern =
  {|type Color = | Red | Green | Blue

fn is_warm(c: Color) -> Int ! pure {
  match c with {
    | Red | Green => 1
    | Blue => 0
  }
}

fn main() -> Int ! pure {
  is_warm(Green)
}|}
(* Green is warm => 1 *)

(* Nested constructor patterns: extract the head of the tail *)
let src_nested_pattern =
  {|type List = | Nil | Cons(Int, List)

fn head_of_tail(l: List) -> Int ! pure {
  match l with {
    | Cons(_, Cons(h, _)) => h
    | _ => 0
  }
}

fn main() -> Int ! pure {
  head_of_tail(Cons(1, Cons(42, Cons(3, Nil))))
}|}
(* second element = 42 *)

(* 3-level nested constructor pattern *)
let src_triple_nested =
  {|type List = | Nil | Cons(Int, List)

fn third(l: List) -> Int ! pure {
  match l with {
    | Cons(_, Cons(_, Cons(h, _))) => h
    | _ => 0
  }
}

fn main() -> Int ! pure {
  third(Cons(1, Cons(2, Cons(99, Nil))))
}|}
(* third element = 99 *)

(* ------------------------------------------------------------------ *)
(* Issue #41: perform / handle codegen                                 *)
(* ------------------------------------------------------------------ *)

(* Single-op effect: handler doubles its argument; perform returns it *)
let src_perform_double =
  {|effect Arith {
  double: (Int) -> Int
}

fn apply(x: Int) -> Int ! {Arith} {
  perform Arith.double(x)
}

fn main() -> Int ! pure {
  handle apply(5) with {
    Arith {
      double(n) => resume(add(n, n))
    }
  }
}|}
(* double(5) = 5+5 = 10 *)

(* Two-op effect: handler adds or multiplies *)
let src_two_op_effect =
  {|effect Math {
  add_one: (Int) -> Int,
  square: (Int) -> Int
}

fn compute(x: Int) -> Int ! {Math} {
  add(perform Math.add_one(x), perform Math.square(x))
}

fn main() -> Int ! pure {
  handle compute(3) with {
    Math {
      add_one(n) => resume(add(n, 1))
      square(n)  => resume(mul(n, n))
    }
  }
}|}
(* add_one(3)=4, square(3)=9, add(4,9)=13 *)

(* Perform inside a do block *)
let src_perform_in_do =
  {|effect Counter {
  tick: () -> Int
}

fn run() -> Int ! {Counter} {
  do {
    let a = perform Counter.tick();
    let b = perform Counter.tick();
    add(a, b)
  }
}

fn main() -> Int ! pure {
  let count = 0 in
  handle run() with {
    Counter {
      tick() => resume(42)
    }
  }
}|}
(* tick()=42 twice; add(42,42)=84 *)

(* ------------------------------------------------------------------ *)
(* Issue #42: handle ... with return clause and abort handlers         *)
(* ------------------------------------------------------------------ *)

(* State-style: two-op effect (get/put); get returns constant 42; return clause
   is identity.  put's result is discarded by the do-block Drop.
   Final result: get() = 42. *)
let src_state_get_put =
  {|effect State {
  get: () -> Int,
  put: (Int) -> Int
}

fn program() -> Int ! {State} {
  do {
    perform State.put(10);
    perform State.get()
  }
}

fn main() -> Int ! pure {
  handle program() with {
    State {
      get() => resume(42)
      put(s) => resume(s)
      return v => v
    }
  }
}|}
(* put(10) -> handler returns 10, dropped; get() -> handler returns 42;
   return v => v is identity; result = 42 *)

(* State-style: single get() with a non-trivial return clause.
   get() = 41; return v => add(v, 1); result = 42. *)
let src_state_return_clause =
  {|effect State {
  get: () -> Int
}

fn program() -> Int ! {State} {
  perform State.get()
}

fn main() -> Int ! pure {
  handle program() with {
    State {
      get() => resume(41)
      return v => add(v, 1)
    }
  }
}|}
(* get() = 41; return clause adds 1; result = 42 *)

(* Throw-style abort: perform Throw.abort(0) in the zero-division branch.
   Handler returns code directly (no resume).  Result = 0. *)
let src_throw_abort =
  {|effect Throw {
  abort: (Int) -> Int
}

fn safe_div(a: Int, b: Int) -> Int ! {Throw} {
  if eq(b, 0) { perform Throw.abort(0) } else { a }
}

fn main() -> Int ! pure {
  handle safe_div(10, 0) with {
    Throw {
      abort(code) => code
    }
  }
}|}
(* b = 0 -> abort handler returns code = 0; result = 0 *)

(* Throw-style: no abort triggered; normal path returns a = 10. *)
let src_throw_no_abort =
  {|effect Throw {
  abort: (Int) -> Int
}

fn safe_div(a: Int, b: Int) -> Int ! {Throw} {
  if eq(b, 0) { perform Throw.abort(0) } else { a }
}

fn main() -> Int ! pure {
  handle safe_div(10, 2) with {
    Throw {
      abort(code) => code
    }
  }
}|}
(* b != 0 -> else branch returns a = 10; result = 10 *)

(* ------------------------------------------------------------------ *)
(* Issue #104: string literals and basic string ops                    *)
(* ------------------------------------------------------------------ *)

(* string_length(concat("Hello, ", "world")) = 12 *)
let src_string_concat_length =
  {|fn main() -> Int ! pure {
  string_length(concat("Hello, ", "world"))
}|}

(* string_length("") = 0 *)
let src_string_empty_length =
  {|fn main() -> Int ! pure {
  string_length("")
}|}

(* string_length("abc") = 3 *)
let src_string_literal_length =
  {|fn main() -> Int ! pure {
  string_length("abc")
}|}

(* string_eq("hi", "hi") = 1 (true) *)
let src_string_eq_true =
  {|fn main() -> Int ! pure {
  if string_eq("hi", "hi") { 1 } else { 0 }
}|}

(* string_eq("hi", "bye") = 0 (false) *)
let src_string_eq_false =
  {|fn main() -> Int ! pure {
  if string_eq("hi", "bye") { 1 } else { 0 }
}|}

(* string_head("ABC") = 65 (ASCII 'A') *)
let src_string_head =
  {|fn main() -> Int ! pure {
  string_head("ABC")
}|}

(* string_head("") = 0 (empty) *)
let src_string_head_empty =
  {|fn main() -> Int ! pure {
  string_head("")
}|}

(* Repeated concat to check heap allocation *)
let src_string_triple_concat =
  {|fn main() -> Int ! pure {
  string_length(concat(concat("a", "b"), "c"))
}|}

(* ------------------------------------------------------------------ *)
(* Issue #45: 01_basics.axm end-to-end integration                    *)
(* ------------------------------------------------------------------ *)

(* Find examples/ relative to the test binary or CWD. *)
let examples_dir =
  let here = Filename.dirname Sys.argv.(0) in
  let candidates =
    [ Filename.concat here "../../../examples"  (* from _build/default/test/ *)
    ; Filename.concat here "../../examples"
    ; Filename.concat (Sys.getcwd ()) "examples"
    ; "examples"
    ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some d -> d
  | None   -> "examples"

(* Subset of 01_basics.axm: non-String, non-HOF functions.
   Exercises features from sub-issues #34–#39 in one program.

   Expected: square(5)=25, distance_sq(0,0,3,4)=25, bool_to_int(true)=1,
             abs(neg(7))=7, get_x(move_right(make_point(10,20),5))=15
             total = 25+25+1+7+15 = 73 *)
let src_01_basics_subset =
  {|type Point = | Point(Int, Int)

fn square(x: Int) -> Int ! pure {
  let result = mul(x, x) in
  result
}

fn distance_sq(x1: Int, y1: Int, x2: Int, y2: Int) -> Int ! pure {
  let dx = sub(x2, x1) in
  let dy = sub(y2, y1) in
  add(mul(dx, dx), mul(dy, dy))
}

fn bool_to_int(b: Bool) -> Int ! pure {
  match b with {
    | true  => 1
    | false => 0
  }
}

fn abs(x: Int) -> Int ! pure {
  if lt(x, 0) { neg(x) } else { x }
}

fn make_point(x: Int, y: Int) -> Point ! pure {
  Point(x, y)
}

fn get_x(p: Point) -> Int ! pure {
  match p with {
    | Point(x, _) => x
  }
}

fn move_right(p: Point, dx: Int) -> Point ! pure {
  match p with {
    | Point(x, y) => Point(add(x, dx), y)
  }
}

fn main() -> Int ! pure {
  let s = square(5) in
  let d = distance_sq(0, 0, 3, 4) in
  let b = bool_to_int(true) in
  let a = abs(neg(7)) in
  let p = make_point(10, 20) in
  let x = get_x(move_right(p, 5)) in
  add(add(s, d), add(add(b, a), x))
}|}

(* Test the full 01_basics.axm file from disk (no main → empty stub). *)
let test_build_file_from_disk path () =
  if not (Sys.file_exists path) then
    Printf.printf "[SKIP] %s not found\n%!" path
  else
    with_tmp_file ".wasm" (fun d ->
      let rc = axiom_build ~src:path ~dst:d in
      Alcotest.(check int)  "exit 0"       0    rc;
      Alcotest.(check bool) "wasm exists"  true (Sys.file_exists d))

let test_validate_file_from_disk path () =
  if not (Sys.file_exists path) then
    Printf.printf "[SKIP] %s not found\n%!" path
  else
    with_tmp_file ".wasm" (fun d ->
      let rc = axiom_build ~src:path ~dst:d in
      if rc <> 0 then Alcotest.fail "axiom build failed";
      match validate_wasm d with
      | Pass     -> ()
      | Fail msg -> Alcotest.fail msg
      | Skip msg -> Printf.printf "[SKIP] %s\n%!" msg)

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

(** Check that every compiled module exports __alloc, and that calling
    it twice with sizes 8 and 4 returns consecutive addresses >= 1024. *)
let test_allocator_exported src () =
  with_tmp_file ".axm" (fun s ->
    with_tmp_file ".wasm" (fun d ->
      write_file s src;
      let rc = axiom_build ~src:s ~dst:d in
      if rc <> 0 then Alcotest.fail "axiom build failed";
      match validate_wasm d with
      | Fail msg -> Alcotest.fail msg
      | Skip msg -> Printf.printf "[SKIP] %s\n%!" msg
      | Pass ->
        match run_alloc_check d with
        | Got n when n >= 1024 -> ()
        | Got n    -> Alcotest.failf "__alloc returned %d, want >= 1024" n
        | RunFail m -> Alcotest.fail ("allocator check failed: " ^ m)
        | RunSkip m -> Printf.printf "[SKIP] %s\n%!" m))

(* ------------------------------------------------------------------ *)
(* Issue #43: tail-call codegen                                        *)
(* ------------------------------------------------------------------ *)

(* Tail call in the if-then branch *)
let src_tail_if =
  {|fn count_down(n: Int) -> Int ! pure {
  if eq(n, 0) { 42 } else { count_down(sub(n, 1)) }
}

fn main() -> Int ! pure {
  count_down(10)
}|}
(* count_down(10) ... count_down(0) = 42 *)

(* Tail call in the if-else branch *)
let src_tail_else =
  {|fn count_up(n: Int, limit: Int) -> Int ! pure {
  if eq(n, limit) { n } else { count_up(add(n, 1), limit) }
}

fn main() -> Int ! pure {
  count_up(0, 7)
}|}
(* count_up(0,7) = 7 *)

(* Tail call in the last statement of a do block *)
let src_tail_do =
  {|fn go(n: Int) -> Int ! pure {
  do {
    let m = sub(n, 1);
    if eq(m, 0) { 99 } else { go(m) }
  }
}

fn main() -> Int ! pure {
  go(5)
}|}
(* go(5)->go(4)->go(3)->go(2)->go(1)->99 *)

(* Tail call in match arms *)
let src_tail_match =
  {|type Step = | Zero | Pos(Int)

fn walk(s: Step) -> Int ! pure {
  match s with {
    | Zero    => 55
    | Pos(n)  => walk(if eq(n, 0) { Zero } else { Pos(sub(n, 1)) })
  }
}

fn main() -> Int ! pure {
  walk(Pos(4))
}|}
(* walk(Pos(4))->walk(Pos(3))->...->walk(Zero)=55 *)

(* Counting to 1,000,000 via tail recursion — must not stack-overflow *)
let src_count_million =
  {|fn count(n: Int) -> Int ! pure {
  if eq(n, 0) { 0 } else { count(sub(n, 1)) }
}

fn main() -> Int ! pure {
  count(1000000)
}|}
(* result = 0 *)

(* letrec tail-recursive loop *)
let src_letrec_tail_loop =
  {|fn main() -> Int ! pure {
  letrec {
    loop(n: Int): Int = if eq(n, 0) { 7 } else { loop(sub(n, 1)) }
  } in
  loop(100)
}|}
(* loop(100) ... loop(0) = 7 *)

(* ------------------------------------------------------------------ *)
(* Helpers for tests with extra CLI flags                              *)
(* ------------------------------------------------------------------ *)

let axiom_build_flags ~flags ~src ~dst =
  let cmd =
    Printf.sprintf "%s build %s -o %s %s 2>/dev/null"
      (Filename.quote axiom_exe)
      (Filename.quote src)
      (Filename.quote dst)
      flags
  in
  Sys.command cmd

let test_main_returns_flags ~flags src expected () =
  with_tmp_file ".axm" (fun s ->
    with_tmp_file ".wasm" (fun d ->
      write_file s src;
      let rc = axiom_build_flags ~flags ~src:s ~dst:d in
      if rc <> 0 then Alcotest.fail "axiom build failed";
      match execute_main d with
      | Got n when n = expected -> ()
      | Got n     -> Alcotest.failf "main returned %d, expected %d" n expected
      | RunFail m -> Alcotest.fail ("execution failed: " ^ m)
      | RunSkip m -> Printf.printf "[SKIP] %s\n%!" m))

let test_validates_flags ~flags src () =
  with_tmp_file ".axm" (fun s ->
    with_tmp_file ".wasm" (fun d ->
      write_file s src;
      let rc = axiom_build_flags ~flags ~src:s ~dst:d in
      if rc <> 0 then Alcotest.fail "axiom build failed";
      match validate_wasm d with
      | Pass     -> ()
      | Fail msg -> Alcotest.fail msg
      | Skip msg -> Printf.printf "[SKIP] %s\n%!" msg))

(* ------------------------------------------------------------------ *)
(* Issue #103: examples 02–10 triage helpers                          *)
(* ------------------------------------------------------------------ *)

let axiom_build_capture_stderr ~src ~dst =
  let err_tmp = Filename.temp_file "axiom_stderr_" ".txt" in
  let cmd =
    Printf.sprintf "%s build %s -o %s 2>%s"
      (Filename.quote axiom_exe)
      (Filename.quote src)
      (Filename.quote dst)
      (Filename.quote err_tmp)
  in
  let rc = Sys.command cmd in
  let stderr_out =
    let ic = open_in err_tmp in
    let buf = Buffer.create 128 in
    (try while true do Buffer.add_channel buf ic 1 done
     with End_of_file -> ());
    close_in ic;
    (try Sys.remove err_tmp with _ -> ());
    Buffer.contents buf
  in
  (rc, stderr_out)

let string_contains haystack needle =
  let hlen = String.length haystack and nlen = String.length needle in
  if nlen = 0 then true
  else if nlen > hlen then false
  else
    let rec loop i =
      if i > hlen - nlen then false
      else if String.sub haystack i nlen = needle then true
      else loop (i + 1)
    in
    loop 0

let test_example_builds path () =
  if not (Sys.file_exists path) then
    Printf.printf "[SKIP] %s not found\n%!" path
  else
    with_tmp_file ".wasm" (fun d ->
      let rc, stderr = axiom_build_capture_stderr ~src:path ~dst:d in
      if rc <> 0 then
        Alcotest.failf "expected build to succeed, got exit %d\nstderr: %S" rc stderr)

let test_example_fails_with path expected_fragment () =
  if not (Sys.file_exists path) then
    Printf.printf "[SKIP] %s not found\n%!" path
  else
    with_tmp_file ".wasm" (fun d ->
      let rc, stderr = axiom_build_capture_stderr ~src:path ~dst:d in
      Alcotest.(check bool) "non-zero exit" true (rc <> 0);
      if not (string_contains stderr expected_fragment) then
        Alcotest.failf "expected stderr to contain %S\ngot: %S"
          expected_fragment stderr)

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
    ; ( "allocator",
        [ Alcotest.test_case "__alloc exported (empty)"     `Quick (test_allocator_exported src_empty)
        ; Alcotest.test_case "__alloc exported (simple fn)" `Quick (test_allocator_exported src_main_add_literals)
        ] )
    ; ( "adt",
        [ Alcotest.test_case "build: color enum"            `Quick (test_build_produces_file src_color_match)
        ; Alcotest.test_case "validate: color enum"         `Quick (test_validates src_color_match)
        ; Alcotest.test_case "color_to_int(Green) = 1"      `Quick (test_main_returns src_color_match 1)
        ; Alcotest.test_case "build: Point get_x"           `Quick (test_build_produces_file src_point_get_x)
        ; Alcotest.test_case "validate: Point get_x"        `Quick (test_validates src_point_get_x)
        ; Alcotest.test_case "get_x(Point(42,7)) = 42"      `Quick (test_main_returns src_point_get_x 42)
        ; Alcotest.test_case "get_y(Point(3,99)) = 99"      `Quick (test_main_returns src_point_get_y 99)
        ; Alcotest.test_case "wildcard: is_circle(Square)=0" `Quick (test_main_returns src_wildcard_match 0)
        ] )
    ; ( "pattern_matching",
        [ Alcotest.test_case "build: list length"           `Quick (test_build_produces_file src_list_length)
        ; Alcotest.test_case "validate: list length"        `Quick (test_validates src_list_length)
        ; Alcotest.test_case "length([1,2,3]) = 3"          `Quick (test_main_returns src_list_length 3)
        ; Alcotest.test_case "build: or-pattern"            `Quick (test_build_produces_file src_or_pattern)
        ; Alcotest.test_case "validate: or-pattern"         `Quick (test_validates src_or_pattern)
        ; Alcotest.test_case "is_warm(Green) = 1"           `Quick (test_main_returns src_or_pattern 1)
        ; Alcotest.test_case "build: nested pattern"        `Quick (test_build_produces_file src_nested_pattern)
        ; Alcotest.test_case "validate: nested pattern"     `Quick (test_validates src_nested_pattern)
        ; Alcotest.test_case "head_of_tail([1,42,3]) = 42" `Quick (test_main_returns src_nested_pattern 42)
        ; Alcotest.test_case "build: triple-nested pattern" `Quick (test_build_produces_file src_triple_nested)
        ; Alcotest.test_case "validate: triple-nested"      `Quick (test_validates src_triple_nested)
        ; Alcotest.test_case "third([1,2,99]) = 99"         `Quick (test_main_returns src_triple_nested 99)
        ] )
    ; ( "effects",
        [ Alcotest.test_case "build: perform double"        `Quick (test_build_produces_file src_perform_double)
        ; Alcotest.test_case "validate: perform double"     `Quick (test_validates src_perform_double)
        ; Alcotest.test_case "double(5) = 10"               `Quick (test_main_returns src_perform_double 10)
        ; Alcotest.test_case "build: two-op effect"         `Quick (test_build_produces_file src_two_op_effect)
        ; Alcotest.test_case "validate: two-op effect"      `Quick (test_validates src_two_op_effect)
        ; Alcotest.test_case "add_one(3)+square(3) = 13"    `Quick (test_main_returns src_two_op_effect 13)
        ; Alcotest.test_case "build: perform in do"         `Quick (test_build_produces_file src_perform_in_do)
        ; Alcotest.test_case "validate: perform in do"      `Quick (test_validates src_perform_in_do)
        ; Alcotest.test_case "tick()+tick() = 84"           `Quick (test_main_returns src_perform_in_do 84)
        (* Issue #42: return clause and abort handlers *)
        ; Alcotest.test_case "build: state get/put"         `Quick (test_build_produces_file src_state_get_put)
        ; Alcotest.test_case "validate: state get/put"      `Quick (test_validates src_state_get_put)
        ; Alcotest.test_case "state get/put = 42"           `Quick (test_main_returns src_state_get_put 42)
        ; Alcotest.test_case "build: state return clause"   `Quick (test_build_produces_file src_state_return_clause)
        ; Alcotest.test_case "validate: state return clause" `Quick (test_validates src_state_return_clause)
        ; Alcotest.test_case "state get(41)+1 = 42"         `Quick (test_main_returns src_state_return_clause 42)
        ; Alcotest.test_case "build: throw abort"           `Quick (test_build_produces_file src_throw_abort)
        ; Alcotest.test_case "validate: throw abort"        `Quick (test_validates src_throw_abort)
        ; Alcotest.test_case "throw abort: code = 0"        `Quick (test_main_returns src_throw_abort 0)
        ; Alcotest.test_case "build: throw no-abort"        `Quick (test_build_produces_file src_throw_no_abort)
        ; Alcotest.test_case "validate: throw no-abort"     `Quick (test_validates src_throw_no_abort)
        ; Alcotest.test_case "throw no-abort: a = 10"       `Quick (test_main_returns src_throw_no_abort 10)
        ] )
    ; ( "tail_calls",
        (* return_call mode (default) *)
        [ Alcotest.test_case "build: tail if"               `Quick (test_build_produces_file src_tail_if)
        ; Alcotest.test_case "validate: tail if"            `Quick (test_validates src_tail_if)
        ; Alcotest.test_case "tail if = 42"                 `Quick (test_main_returns src_tail_if 42)
        ; Alcotest.test_case "build: tail else"             `Quick (test_build_produces_file src_tail_else)
        ; Alcotest.test_case "validate: tail else"          `Quick (test_validates src_tail_else)
        ; Alcotest.test_case "tail else = 7"                `Quick (test_main_returns src_tail_else 7)
        ; Alcotest.test_case "build: tail do"               `Quick (test_build_produces_file src_tail_do)
        ; Alcotest.test_case "validate: tail do"            `Quick (test_validates src_tail_do)
        ; Alcotest.test_case "tail do = 99"                 `Quick (test_main_returns src_tail_do 99)
        ; Alcotest.test_case "build: tail match"            `Quick (test_build_produces_file src_tail_match)
        ; Alcotest.test_case "validate: tail match"         `Quick (test_validates src_tail_match)
        ; Alcotest.test_case "tail match = 55"              `Quick (test_main_returns src_tail_match 55)
        ; Alcotest.test_case "build: letrec tail loop"      `Quick (test_build_produces_file src_letrec_tail_loop)
        ; Alcotest.test_case "validate: letrec tail loop"   `Quick (test_validates src_letrec_tail_loop)
        ; Alcotest.test_case "letrec loop(100) = 7"         `Quick (test_main_returns src_letrec_tail_loop 7)
        (* count to 1,000,000 — verifies no stack overflow in return_call mode *)
        ; Alcotest.test_case "build: count 1M"              `Quick (test_build_produces_file src_count_million)
        ; Alcotest.test_case "validate: count 1M"           `Quick (test_validates src_count_million)
        ; Alcotest.test_case "count(1000000) = 0"           `Quick (test_main_returns src_count_million 0)
        (* --no-tail-calls trampoline mode *)
        ; Alcotest.test_case "validate: tail if (trampoline)"    `Quick
            (test_validates_flags ~flags:"--no-tail-calls" src_tail_if)
        ; Alcotest.test_case "tail if = 42 (trampoline)"         `Quick
            (test_main_returns_flags ~flags:"--no-tail-calls" src_tail_if 42)
        ; Alcotest.test_case "validate: tail else (trampoline)"  `Quick
            (test_validates_flags ~flags:"--no-tail-calls" src_tail_else)
        ; Alcotest.test_case "tail else = 7 (trampoline)"        `Quick
            (test_main_returns_flags ~flags:"--no-tail-calls" src_tail_else 7)
        ; Alcotest.test_case "validate: tail do (trampoline)"    `Quick
            (test_validates_flags ~flags:"--no-tail-calls" src_tail_do)
        ; Alcotest.test_case "tail do = 99 (trampoline)"         `Quick
            (test_main_returns_flags ~flags:"--no-tail-calls" src_tail_do 99)
        ; Alcotest.test_case "validate: count 1M (trampoline)"   `Quick
            (test_validates_flags ~flags:"--no-tail-calls" src_count_million)
        ; Alcotest.test_case "count(1M)=0 (trampoline)"          `Quick
            (test_main_returns_flags ~flags:"--no-tail-calls" src_count_million 0)
        ; Alcotest.test_case "letrec loop(100)=7 (trampoline)"   `Quick
            (test_main_returns_flags ~flags:"--no-tail-calls" src_letrec_tail_loop 7)
        ] )
    ; ( "runtime",
        (* Issue #44: programs run unchanged via the Axiom runtime.
           Each test compiles an Axiom program, then runs it through the
           runtime module (runtime/runtime.wasm) using the two-module host
           pattern: the Axiom module's `main` export is passed as
           `axiom_program.main` to the runtime, then `_start` is called and
           `get_exit_code` is read back.  Tests skip gracefully when
           runtime.wasm is absent (run `make` in runtime/ to build it). *)
        [ Alcotest.test_case "runtime: add(1,2) = 3"        `Quick
            (test_main_returns_via_runtime src_main_add_literals 3)
        ; Alcotest.test_case "runtime: let+arithmetic = 13" `Quick
            (test_main_returns_via_runtime src_main_let_arithmetic 13)
        ; Alcotest.test_case "runtime: fact(5) = 120"       `Quick
            (test_main_returns_via_runtime src_recursive_fact 120)
        ; Alcotest.test_case "runtime: abs(neg(5)) = 5"     `Quick
            (test_main_returns_via_runtime src_abs 5)
        ; Alcotest.test_case "runtime: color_to_int(Green)=1" `Quick
            (test_main_returns_via_runtime src_color_match 1)
        ; Alcotest.test_case "runtime: double(5) = 10 (effect)" `Quick
            (test_main_returns_via_runtime src_perform_double 10)
        ; Alcotest.test_case "runtime: tail count(10) = 42" `Quick
            (test_main_returns_via_runtime src_tail_if 42)
        ] )
    ; ( "cross_file_import",
        (* Issue #120: import X / import X as Y across files *)
        [ Alcotest.test_case "validates: import lib.add"         `Quick
            (test_multifile_validates
               [ ("lib.axm",
                  {|pub fn add(a: Int, b: Int) -> Int ! pure { add(a, b) }|})
               ; ("app.axm",
                  {|import lib
fn main() -> Int ! pure { lib.add(10, 32) }|})
               ]
               "app.axm")
        ; Alcotest.test_case "lib.add(10,32) = 42"               `Quick
            (test_multifile_returns
               [ ("lib.axm",
                  {|pub fn add(a: Int, b: Int) -> Int ! pure { add(a, b) }|})
               ; ("app.axm",
                  {|import lib
fn main() -> Int ! pure { lib.add(10, 32) }|})
               ]
               "app.axm" 42)
        ; Alcotest.test_case "import alias: m.double(21) = 42"   `Quick
            (test_multifile_returns
               [ ("math.axm",
                  {|pub fn double(x: Int) -> Int ! pure { add(x, x) }|})
               ; ("app.axm",
                  {|import math as m
fn main() -> Int ! pure { m.double(21) }|})
               ]
               "app.axm" 42)
        ; Alcotest.test_case "import missing file fails"          `Quick
            (fun () ->
               with_tmp_dir (fun dir ->
                 write_file (Filename.concat dir "app.axm")
                   {|import no_such_module
fn main() -> Int ! pure { 0 }|};
                 with_tmp_file ".wasm" (fun d ->
                   let rc = axiom_build_in_dir ~dir ~entry:"app.axm" ~dst:d in
                   Alcotest.(check bool) "non-zero exit" true (rc <> 0))))
        ] )
    ; ( "basics_e2e",
        (* Issue #45: 01_basics.axm end-to-end.  Wires together every
           preceding sub-issue (#34–#44) in a single integration test.

           Covers:
           - Int arithmetic and let-bindings    (#34)
           - Top-level function calls           (#35)
           - Bool pattern match, if/else, neg   (#36-#37)
           - ADT construction + projection      (#38)
           - Nested constructor patterns        (#39)
           - Bump allocator (implicit)          (#40)

           The disk-file tests additionally verify that examples/01_basics.axm
           (which uses String and HOF generics) compiles without error: those
           functions are silently skipped by codegen, producing valid WASM for
           the supportable subset.  wasmtime/node execution uses the inline
           subset fixture which exports a runnable main. *)
        [ Alcotest.test_case "build: 01_basics subset"        `Quick
            (test_build_produces_file src_01_basics_subset)
        ; Alcotest.test_case "validate: 01_basics subset"     `Quick
            (test_validates src_01_basics_subset)
        ; Alcotest.test_case "01_basics subset = 73"          `Quick
            (test_main_returns src_01_basics_subset 73)
        ; Alcotest.test_case "build: 01_basics.axm from disk" `Quick
            (test_build_file_from_disk
               (Filename.concat examples_dir "01_basics.axm"))
        ; Alcotest.test_case "validate: 01_basics.axm from disk" `Quick
            (test_validate_file_from_disk
               (Filename.concat examples_dir "01_basics.axm"))
        ] )
    ; ( "examples_02_10",
        (* Issue #103: triage examples 02–10.
           Each test locks in the exact current build outcome so that CI flags
           both silent fixes (a broken example starts passing) and unexpected
           new breakage (a passing example starts failing).
           Update this suite when an example is genuinely fixed. *)
        let ex name = Filename.concat examples_dir (name ^ ".axm") in
        [ Alcotest.test_case "02_data_types builds" `Quick
            (test_example_builds (ex "02_data_types"))
          (* 03: missing lowercase constructor wrappers for user-defined ADTs
             ('info' for Info, etc.) — blocked by #109 *)
        ; Alcotest.test_case "03_effects: unbound 'info'" `Quick
            (test_example_fails_with (ex "03_effects") "unbound variable 'info'")
          (* 04: missing lowercase constructor wrapper 'transitioned' — blocked by #109 *)
        ; Alcotest.test_case "04_state_machine: unbound 'transitioned'" `Quick
            (test_example_fails_with (ex "04_state_machine") "unbound variable 'transitioned'")
          (* 05: missing stdlib 'pair' constructor — blocked by #109 *)
        ; Alcotest.test_case "05_collections: unbound 'pair'" `Quick
            (test_example_fails_with (ex "05_collections") "unbound variable 'pair'")
          (* 06: concat/string_length/string_eq are now built-in (#104);
             eq_char and related char ops still unresolved — blocked by #110 *)
        ; Alcotest.test_case "06_string_processing: unbound 'eq_char'" `Quick
            (test_example_fails_with (ex "06_string_processing") "unbound variable 'eq_char'")
          (* 07: missing lowercase constructor wrappers 'valid'/'invalid' — blocked by #109 *)
        ; Alcotest.test_case "07_validation: unbound 'valid'" `Quick
            (test_example_fails_with (ex "07_validation") "unbound variable 'valid'")
          (* 08: parser bug — 'perform' not accepted as match scrutinee — blocked by #101 *)
        ; Alcotest.test_case "08_config: parse error on Perform" `Quick
            (test_example_fails_with (ex "08_config") "unexpected token Perform")
          (* 09: 'Log' effect used but not declared in file scope — blocked by #109 *)
        ; Alcotest.test_case "09_pipeline: unknown effect 'Log'" `Quick
            (test_example_fails_with (ex "09_pipeline") "unknown effect 'Log'")
          (* 10: missing lowercase constructor wrappers for Json ADT — blocked by #109 *)
        ; Alcotest.test_case "10_json: unbound 'j_object'" `Quick
            (test_example_fails_with (ex "10_json") "unbound variable 'j_object'")
        ] )
    ; ( "strings",
        (* Issue #104: string literals and basic string ops.
           String layout: [i32 byte-length][UTF-8 bytes], i32 pointer.
           Primitives: concat, string_length, string_eq, string_head. *)
        [ Alcotest.test_case "validates: concat literal"            `Quick
            (test_validates src_string_concat_length)
        ; Alcotest.test_case "string_length(concat) = 12"          `Quick
            (test_main_returns src_string_concat_length 12)
        ; Alcotest.test_case "string_length(\"\") = 0"             `Quick
            (test_main_returns src_string_empty_length 0)
        ; Alcotest.test_case "string_length(\"abc\") = 3"          `Quick
            (test_main_returns src_string_literal_length 3)
        ; Alcotest.test_case "string_eq same = true"               `Quick
            (test_main_returns src_string_eq_true 1)
        ; Alcotest.test_case "string_eq different = false"         `Quick
            (test_main_returns src_string_eq_false 0)
        ; Alcotest.test_case "string_head(\"ABC\") = 65"           `Quick
            (test_main_returns src_string_head 65)
        ; Alcotest.test_case "string_head(\"\") = 0"               `Quick
            (test_main_returns src_string_head_empty 0)
        ; Alcotest.test_case "triple concat length = 3"            `Quick
            (test_main_returns src_string_triple_concat 3)
        ] )
    ]
