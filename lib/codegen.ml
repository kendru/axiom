(** Stub code generator: produces a minimal valid WASM module from a
    type-checked Axiom program.  The emitted module exports a no-op [main]
    function that returns i32 0.  Real compilation logic lands in later
    sub-issues; this stub is enough to close the pipeline plumbing and let
    the test harness validate the binary format end-to-end. *)

let emit_stub (_prog : Ast.program) : bytes =
  let m = Wasm_encode.create () in
  let _ =
    Wasm_encode.add_function m ~export:"main" [] [Wasm_encode.I32] []
      [Wasm_encode.I32Const 0]
  in
  Wasm_encode.encode m
