# Code Generation (`lib/codegen.ml` + `lib/wasm/wasm_encode.ml`)

The code generator translates a type-checked `Ast.program` into a
WebAssembly binary. It is split into two layers:

- **`lib/codegen.ml`** — Axiom-specific: walks the AST and emits
  `Wasm_encode` instruction sequences.
- **`lib/wasm/wasm_encode.ml`** — WebAssembly-specific: a self-contained
  binary WASM encoder with no Axiom dependencies.

---

## Current scope (Milestone 1)

`lib/codegen.ml` is deliberately minimal. It handles:

| AST form | WASM output |
|----------|-------------|
| `IntLit n` | `i32.const n` |
| `Var x` | `local.get idx` (where `idx` is the local index for `x`) |
| `App(Var "add", [a, b])` | compile `a`, compile `b`, `i32.add` |
| `App(Var "sub", [a, b])` | … `i32.sub` |
| `App(Var "mul", [a, b])` | … `i32.mul` |
| `Let { pat = PVar x; value; body }` | compile value, `local.set fresh`, compile body |

Everything else raises `Failure "Codegen: unsupported expression: …"`.

Only the top-level function named `main` is emitted; all other
declarations are ignored. If no `main` is found, an empty stub
`main` that returns `i32.const 0` is emitted instead.

---

## Public API (`lib/codegen.ml`)

| Function | Signature | Location |
|----------|-----------|----------|
| `emit` | `Ast.program -> bytes` | `lib/codegen.ml:81` |
| `emit_stub` | `Ast.program -> bytes` | `:95` (alias for `emit`) |

`emit` is what `bin/main.ml` calls (`bin/main.ml:53`). It returns the
complete `.wasm` binary.

---

## Internal structure

### Compilation context (`ctx`, lines 14–18)

```ocaml
type ctx = {
  env          : (string * int) list;   (* variable → local index *)
  next_local   : int ref;               (* next free slot *)
  extra_locals : local_decl list ref;   (* let-allocated locals *)
}
```

Parameters are pre-loaded into `env` at position 0..n-1 by `make_ctx`
(`:20`). `alloc_local` (`:27`) appends a fresh `i32` local to
`extra_locals` and returns its index.

### Compiling a function

`compile_fn` (`:70`) takes a list of named parameters and a body
expression, builds a `ctx`, calls `compile_expr`, and passes the
resulting instruction list to `Wasm_encode.add_function`.

### `compile_expr` (`:37`, recursive)

Dispatches on `expr.Ast.desc`. Unsupported forms call `Format.asprintf`
with `Ast.pp_expr_desc` and raise `Failure`.

---

## WASM encoder (`lib/wasm/wasm_encode.ml`)

A self-contained binary encoder for a WebAssembly MVP-level module. It
has no dependencies outside the OCaml standard library.

### Public types

| Type | Location | Purpose |
|------|----------|---------|
| `val_type` | `:14` | `I32 \| I64 \| F32 \| F64` |
| `block_type` | `:17` | `Void \| ValType vt \| TypeIdx i` |
| `instr` | `:23` | Instruction ADT (see below) |
| `local_decl` | `:45` | `{ count; ty }` — run-length encoded locals |
| `func_type` | `:51` | `{ params; results }` |
| `export_desc` | `:57` | `ExportFunc \| ExportTable \| ExportMem \| ExportGlobal` |
| `import_desc` | `:64` | `ImportFunc of int` (func imports only) |
| `limits` | `:75` | `{ min; max : int option }` |
| `data_segment` | `:81` | `{ offset; data }` (active, memory 0) |
| `elem_segment` | `:87` | `{ elem_offset; func_indices }` (active, table 0) |
| `module_builder` | `:93` | Mutable accumulator for all sections |

### Supported instructions

```
I32Const int          LocalGet idx        Call func_idx
I32Add                LocalSet idx        If (bt, then_, else_)
I32Sub                LocalTee idx        Block (bt, body)
I32Mul                Return              Loop (bt, body)
I32Eq                 Drop                Br label
I32LtS                                    BrIf label
I32GtS                                    BrTable (labels, default)
```

No floating-point instructions, no memory load/store, no SIMD, no
GC proposal types.

### Public functions

| Function | Signature | Location |
|----------|-----------|----------|
| `create` | `unit -> module_builder` | `lib/wasm/wasm_encode.ml:401` |
| `add_type` | `module_builder -> func_type -> int` | `:408` |
| `add_import` | `module_builder -> string -> string -> import_desc -> unit` | `:415` |
| `add_func` | `module_builder -> int -> local_decl list -> instr list -> int` | `:420` |
| `add_function` | `module_builder -> ?export:string -> val_type list -> val_type list -> local_decl list -> instr list -> int` | `:458` |
| `add_table` | `module_builder -> limits -> unit` | `:427` |
| `add_memory` | `module_builder -> limits -> unit` | `:430` |
| `add_global` | `module_builder -> val_type -> bool -> global_init -> unit` | `:433` |
| `add_export` | `module_builder -> string -> export_desc -> unit` | `:437` |
| `set_start` | `module_builder -> int -> unit` | `:441` |
| `add_element` | `module_builder -> int -> int list -> unit` | `:444` |
| `add_data` | `module_builder -> int -> bytes -> unit` | `:449` |
| `encode` | `module_builder -> bytes` | `:372` |

`add_function` is the convenience wrapper used by `codegen.ml`: it
calls `add_type`, `add_func`, and (if `~export` is given) `add_export`
in one call.

### Supported WASM sections

Sections are emitted only when non-empty (`lib/wasm/wasm_encode.ml:376–394`):

| Section | Emitted when |
|---------|-------------|
| Type | any function types declared |
| Import | any imports |
| Function | any functions defined |
| Table | any tables |
| Memory | any memories |
| Global | any globals |
| Export | any exports |
| Start | start index set |
| Element | any element segments |
| Code | any function bodies |
| Data | any data segments |

### LEB128 encoding

Both unsigned (`put_uleb128`, `:121`) and signed (`put_sleb128`, `:132`)
LEB128 are implemented. They are used for all counts, offsets, type
indices, and integer immediates throughout the binary format.

### Limitations

- **Instruction set**: only the MVP subset listed above; no `i64`/`f32`/`f64`
  constants as instructions (though globals can hold those values), no
  memory access, no atomics, no SIMD, no multi-value blocks beyond the
  `results` array on `func_type`.
- **Imports**: function imports only (`ImportFunc`); table/memory/global
  imports are not encoded.
- **Segments**: active segments only (memory 0 for data, table 0 for
  elements); passive segments are not supported.
- **No Memory64, GC proposal, or exception-handling proposal.**

---

## Extending the code generator

When adding a new Axiom expression to codegen:

1. Add a case to `compile_expr` in `lib/codegen.ml`.
2. If new WASM instructions are needed, add them to the `instr` type in
   `lib/wasm/wasm_encode.ml`, implement their encoding in
   `encode_instr`, and add a test in `test/test_wasm_encode.ml`.
3. Add integration coverage in `test/test_build_pipeline.ml`.
