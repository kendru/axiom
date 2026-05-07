# Axiom Runtime

The Axiom runtime (`runtime/`) is a WebAssembly library written in Zig that
Axiom-compiled modules import at the `"axm"` namespace.  It provides:

- A bump allocator backed by linear-memory growth
- I/O primitives for the `Console` effect (`axm_print`, `axm_read_line`)
- Stub functions that reserve the future GC API (`axm_gc_*`)
- Trap and division-guard helpers carried over from Milestone 1

## Memory layout

```
Byte offset    Contents
──────────────────────────────────────────
0 – 63         Reserved zero-page area
64 – 65535     Static string data (placed by Axiom module data segments)
65536 +        Heap managed by the bump allocator (axm_alloc)
```

Page 0 (0–65535) is reserved for the Axiom module's static string pool.
Strings must fit within 63 KiB (offset 64 through 65535).  The bump allocator
starts at page 1 (offset 65536) and grows the memory one page at a time as
needed.

## Import surface

The table below lists every symbol the Axiom-compiled module imports from the
`"axm"` module.

### Memory

| Import name | WASM type | Description |
|-------------|-----------|-------------|
| `memory`    | memory (min 2 pages) | Linear memory shared between the runtime and the Axiom module |

### Allocation

| Import name | Signature | Description |
|-------------|-----------|-------------|
| `axm_alloc(n)` | `(i32) → i32` | Allocate [n] bytes (4-byte aligned); return pointer |
| `axm_alloc_string(ptr, len)` | `(i32, i32) → i32` | Copy [len] bytes from [ptr] into a new length-prefixed heap string; return pointer |
| `axm_alloc_record(field_count)` | `(i32) → i32` | Allocate `field_count * 4` bytes; return pointer |
| `axm_alloc_ctor(tag, payload_count)` | `(i32, i32) → i32` | Allocate `(1 + payload_count) * 4` bytes, write [tag] into word 0; return pointer |

### I/O

| Import name | Signature | Description |
|-------------|-----------|-------------|
| `axm_print(ptr, len)` | `(i32, i32) → i32` | Write [len] bytes at [ptr] to stdout; returns 0 on success, −1 on error |
| `axm_read_line()` | `() → i32` | Read one line from stdin; allocate a length-prefixed heap string and return its pointer (0 on EOF or error) |

### GC stubs

| Import name | Signature | Description |
|-------------|-----------|-------------|
| `axm_gc_collect()` | `() → void` | No-op; reserves the GC collect API |
| `axm_gc_root(ptr)` | `(i32) → void` | No-op; reserves the GC root API |
| `axm_gc_unroot(ptr)` | `(i32) → void` | No-op; reserves the GC unroot API |

## String layout

Strings in Axiom are heap pointers to the following layout:

```
offset 0 – 3   i32 byte-length (little-endian)
offset 4 + i   UTF-8 byte i
```

`axm_print(ptr, len)` takes the **data pointer** (offset 4) and byte count.
The codegen wrapper `console_print(str_ptr)` extracts these automatically:

```
data_ptr = str_ptr + 4
len      = i32.load(str_ptr)
call axm_print(data_ptr, len)
```

## Building the runtime

Requires Zig 0.14+:

```sh
cd runtime
zig build -Doptimize=ReleaseSmall
# Output: zig-out/bin/runtime.wasm
```

`runtime.wasm` is a compiler artifact and is **not** tracked in the
repository.  Build it locally before running the runtime integration tests or
linking with wasmtime.

## Running under wasmtime

wasmtime resolves the `"axm"` imports via `--preload`:

```sh
# Compile an Axiom program
axiom build examples/01_basics.axm -o out.wasm

# Run with the runtime pre-loaded as the "axm" module
wasmtime --preload axm=runtime/zig-out/bin/runtime.wasm --invoke main out.wasm
```

wasmtime automatically satisfies the WASI imports (`wasi_snapshot_preview1`)
that the runtime itself requires for `axm_print`/`axm_read_line`.

## Running via Node.js (tests)

The test suite (`test/test_build_pipeline.ml`) provides in-process mock
implementations of the `"axm"` imports for all Node.js-based execution tests.
The mock uses a JavaScript bump allocator starting at 65536 and routes
`axm_print` to `process.stdout`.  No Zig toolchain is required for the test
suite to pass; the runtime integration tests skip gracefully when
`runtime/zig-out/bin/runtime.wasm` is absent.

## Out of scope

- Real garbage collection (tracked separately; `axm_gc_*` stubs reserve the API)
- Threads and shared memory
- Strings beyond UTF-8 byte sequences
