(* Copyright 2026 Andrew Meredith
   SPDX-License-Identifier: Apache-2.0 *)

(** WebAssembly code generation.
    Compiles fully-elaborated IR to WASM binary format.
    See spec/axiom-overview-draft.md §9 for the compilation strategy.

    Effect compilation (§9.2):
    - Effects compile to evidence passing: a struct of function pointers,
      one per operation, allocated per handler installation.
    - 'perform E.op(args)' becomes: load evidence from handler stack,
      call the corresponding function pointer.
    - Linear handlers compile to direct jumps — no heap continuations.

    Tail calls (§9.3):
    - Tail positions emit WASM 'return_call' / 'return_call_indirect'.
    - For WASM engines without tail-call support: trampoline fallback
      (explicit stack, CPS-transformed loops).

    Memory management (§9.4):
    - Escape analysis determines stack vs. heap allocation per value.
    - GC calls are inserted at allocation sites; barrier calls at stores.
    - GC implementation lives in the Zig runtime (runtime/src/gc.zig).
*)

(* TODO: define wasm_module type: functions, tables, memories, globals, exports *)
(* TODO: define wasm_func type: locals, body (instruction list) *)
(* TODO: implement compile         : Ir.ir_node list -> bytes *)
(* TODO: implement emit_function   : Ir.ir_node -> wasm_func *)
(* TODO: implement emit_evidence   : effect_decl -> wasm_global list *)
(* TODO: implement escape_analysis : Ir.ir_node -> alloc_map *)
