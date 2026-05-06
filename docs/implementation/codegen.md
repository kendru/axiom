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
I32LtS                GlobalGet idx       BrIf label
I32GtS                GlobalSet idx       BrTable (labels, default)
                      I32Load (al, off)
                      I32Store (al, off)
```

No floating-point instructions, no SIMD, no GC proposal types.

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
  atomics, no SIMD, no multi-value blocks beyond the `results` array on
  `func_type`.
- **Imports**: function imports only (`ImportFunc`); table/memory/global
  imports are not encoded.
- **Segments**: active segments only (memory 0 for data, table 0 for
  elements); passive segments are not supported.
- **No Memory64, GC proposal, or exception-handling proposal.**

---

---

## Linear memory and bump allocator (Milestone 2 prerequisite)

### Memory section

Every module emitted by `codegen.ml` includes a single linear memory of
**one page (65 536 bytes)** with no declared maximum.  The memory is
exported under the name `"memory"` so host embedders and tests can
inspect it.

```
(memory (export "memory") 1)
```

### `__heap_ptr` global

A mutable `i32` global named `__heap_ptr` (global index 0) is emitted
before any user-defined globals.  Its initial value is **1024**, leaving
the first 1 KiB as a reserved static zone (currently unused; reserved
for future static-data segments).

```
(global $__heap_ptr (mut i32) (i32.const 1024))
```

### `__alloc` function

`__alloc` is function index 0 in every module.  Its signature is:

```
(func $__alloc (export "__alloc") (param $size i32) (result i32)
  ;; Save old heap_ptr, bump by size, return old value.
  global.get $__heap_ptr
  local.tee $saved          ;; local[1]: saved old ptr
  local.get $size           ;; local[0]: size param
  i32.add
  global.set $__heap_ptr
  local.get $saved)
```

Calling `__alloc(n)` returns the address of a freshly allocated `n`-byte
region and advances `__heap_ptr` by `n`.  There is no alignment
guarantee beyond what the caller requests; callers allocating word-sized
fields should round `n` up to a multiple of 4 themselves.

No freeing, no GC, no growth beyond one page — out of scope for now.

### Boxed-value layout

ADT constructor values are represented as **tagged heap records**:

```
offset +0 : i32  tag        (constructor index, 0-based)
offset +4 : i32  field[0]   (first payload field)
offset +8 : i32  field[1]   ...
...
```

All payload fields are `i32` in v1.  A field holds either:

- A 32-bit integer or boolean value packed directly, or
- A heap pointer (i32 address) to another boxed value.

**Allocation size** for a constructor with `k` fields is `4 * (1 + k)`
bytes.  The emitter calls `__alloc(4 * (1 + k))`, then uses
`i32.store` at `ptr + 0` for the tag and `ptr + 4*i` for field `i`.

**Alignment rule**: every allocation is a multiple of 4 bytes and
`__heap_ptr` starts at 1024 (also a multiple of 4), so all word
accesses are naturally aligned.

---

## End-to-end pipeline (Milestone 5)

### Invoking the compiler

```bash
dune exec bin/main.exe -- build <input.axm> -o <output.wasm>
# Optional: use trampoline TCO instead of WASM tail-call proposal
dune exec bin/main.exe -- build <input.axm> -o <output.wasm> --no-tail-calls
```

Running the compiled module with `wasmtime`:

```bash
wasmtime --invoke main output.wasm   # prints the i32 return value of main
```

### `examples/01_basics.axm` — what compiles, what is skipped

`examples/01_basics.axm` compiles end-to-end without error.  Functions whose
parameter or return types are unsupported are silently filtered at codegen
time; functions whose *bodies* use unimplemented constructs (e.g. String
primitives) are compiled to a stub that returns `i32.const 0`.

| Function | Status | Notes |
|----------|--------|-------|
| `square` | **Compiled** | mul, let-binding |
| `distance` | **Compiled** | multiple params, nested let |
| `id<a>` | **Compiled** | type variable lowered to i32 |
| `bool_to_int` | **Compiled** | Bool pattern match |
| `abs` | **Compiled** | if/else, `neg` primitive |
| `make_point` | **Compiled** | ADT constructor |
| `get_x` | **Compiled** | constructor pattern match |
| `move_right` | **Compiled** | pattern match + constructor |
| `greet` | **Stub** | body uses `concat` (String not implemented) |
| `apply_twice<a>` | **Skipped** | function-type param (`TyFun` unsupported) |
| `first_match<a>` | **Compiled** | `List<a>` param — `TyApp` now lowered to i32 |

The file has no `main`, so the emitted module exports an empty `main` stub
returning 0.

### Integration test (issue #45)

`test/test_build_pipeline.ml`, suite `basics_e2e`:

| Test | What it checks |
|------|---------------|
| `build: 01_basics subset` | inline subset of `01_basics.axm` compiles |
| `validate: 01_basics subset` | output passes `wasm-validate` / Node WebAssembly.validate |
| `01_basics subset = 73` | `main()` returns `square(5)+distance_sq(0,0,3,4)+bool_to_int(true)+abs(neg(7))+get_x(move_right(make_point(10,20),5))` = 73 |
| `build: 01_basics.axm from disk` | the actual `examples/01_basics.axm` file compiles |
| `validate: 01_basics.axm from disk` | its output is valid WASM |

---

## String representation (issue #104)

### Layout in linear memory

Every Axiom string is a **length-prefixed UTF-8 byte sequence** stored in
linear memory.  The WASM value for a string is an `i32` pointer to its
first byte:

```
ptr+0 .. ptr+3 : i32  byte-length (little-endian)
ptr+4 .. ptr+4+len-1 : UTF-8 content
```

Total allocation: `4 + len` bytes, padded to a 4-byte boundary.

### Static string literals

String literals are allocated at **compile time** into a static data
segment.  The codegen pre-scans the elaborated program for all unique
string literals, assigns each an offset starting at byte **64** (the
first 64 bytes are a reserved zero page), then emits one WASM `data`
segment per string.

The heap bump-allocator global `__heap_ptr` is initialised to
`max(1024, rounded_static_end)` so that dynamic allocations never
overlap the static region.

### Built-in string primitives

Four WASM helper functions are emitted into every module and bound to
the corresponding Axiom names:

| Axiom name | WASM signature | Behaviour |
|-----------|---------------|-----------|
| `string_length` | `(i32) → i32` | `i32.load ptr` — returns byte count |
| `string_eq` | `(i32, i32) → i32` | byte-by-byte comparison; returns 1 (equal) or 0 |
| `concat` | `(i32, i32) → i32` | allocates a new string, copies both inputs |
| `string_head` | `(i32) → i32` | first byte as char code, or 0 if empty |

`concat` calls `__alloc` internally and uses `i32.load8_u` / `i32.store8`
for byte-level copying.

These names are pre-populated in `func_map` so ordinary Axiom call
syntax (e.g. `concat("Hello, ", "world")`) compiles to a plain `call`.

### New WASM instructions

Two instructions were added to `lib/wasm/wasm_encode.ml` to support
byte-granularity access:

| Instruction | Opcode | OCaml constructor |
|-------------|--------|-------------------|
| `i32.load8_u` | `0x2D` | `I32Load8U (align, offset)` |
| `i32.store8` | `0x3A` | `I32Store8 (align, offset)` |

### `examples/02_data_types.axm` — what compiles (issue #105)

`examples/02_data_types.axm` declares `Option<a>`, `Result<a,e>`, `List<a>`,
`Tree<a>`, `Pair<a,b>`, `NonEmpty<a>`, and `Ordering`.  All functions whose
parameters and return type are ADT or scalar (no `TyFun` function-type
arguments) are now fully compiled.

| Function | Status | Notes |
|----------|--------|-------|
| `unwrap_or` | **Compiled** | `Option<a>` param lowered to i32 |
| `length` | **Compiled** | recursive `List<a>` traversal |
| `append` | **Compiled** | recursive list concatenation |
| `tree_size` | **Compiled** | recursive Tree traversal |
| `tree_depth` | **Compiled** | uses built-in `max` helper |
| `tree_to_list` | **Compiled** | Tree → List conversion |
| `non_empty_head`, `non_empty_to_list` | **Compiled** | single-ctor match |
| `from_list` | **Compiled** | Option-returning list coercion |
| `result_to_option` | **Compiled** | Result → Option conversion |
| `map_option`, `flat_map_option`, `map_result`, `map_error` | **Skipped** | function-type param (`TyFun` unsupported) |
| `map`, `filter`, `fold_left`, `reverse` | **Skipped** | function-type param |
| `tree_map`, `tree_fold` | **Skipped** | function-type param |

`main()` exercises the compiled subset and returns `20` (verified by
`test/test_build_pipeline.ml` suite `examples_02_10`).

### Lowercase constructor aliases

When a `type` declaration is processed, the ctor_map automatically includes
both the canonical capitalised name (`None`, `Cons`, …) and its lowercase
counterpart (`none`, `cons`, …).  This lets stdlib-style helper calls like
`none()`, `cons(h, t)` resolve to the same tagged-allocation path as the
explicit uppercase constructor.

### Built-in `max` helper

A `max(a, b) -> Int` helper is always emitted alongside the string helpers.
Its WASM body uses `i32.gt_s` / `if` to return the larger argument.  It is
pre-populated in `func_map` so `max(x, y)` calls compile to a plain `call`.

### Known gaps

The following constructs are **not yet supported** by the code generator:

- **Higher-order parameters**: `TyFun` parameter types are filtered out;
  functions like `map`, `filter`, `fold_left` are silently skipped.
- **Algebraic effects in generics**: effect polymorphism works for monomorphic
  effects; polymorphic effect rows are not yet lowered.
- **Multi-value returns / tuples**: no tuple codegen; `TyTuple` params are filtered.
- **Unmatched pattern defensive trap**: the codegen emits `unreachable` when all
  match arms have been tried and none matched.  The type-checker guarantees
  exhaustiveness so this path should never execute.

---

## Extending the code generator

When adding a new Axiom expression to codegen:

1. Add a case to `compile_expr` in `lib/codegen.ml`.
2. If new WASM instructions are needed, add them to the `instr` type in
   `lib/wasm/wasm_encode.ml`, implement their encoding in
   `encode_instr`, and add a test in `test/test_wasm_encode.ml`.
3. Add integration coverage in `test/test_build_pipeline.ml`.
