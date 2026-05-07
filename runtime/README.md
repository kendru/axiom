# Axiom Runtime

WebAssembly runtime library for Axiom programs.  Targets `wasm32-wasi`.

## What it provides

Axiom-compiled modules import these symbols from the `"axm"` namespace:

| Export | Signature | Purpose |
|--------|-----------|---------|
| `memory` | memory (min 2 pages) | Linear memory shared with the Axiom module |
| `axm_alloc` | `(i32) → i32` | Bump-allocate n bytes; return pointer |
| `axm_alloc_string` | `(i32, i32) → i32` | Copy raw bytes into a length-prefixed heap string |
| `axm_alloc_record` | `(i32) → i32` | Allocate n × 4 bytes for a record |
| `axm_alloc_ctor` | `(i32, i32) → i32` | Allocate ADT header + payload; write tag word |
| `axm_print` | `(i32, i32) → i32` | Write bytes to stdout via WASI |
| `axm_read_line` | `() → i32` | Read a line from stdin; return heap string pointer |
| `axm_gc_collect` | `() → void` | GC stub (no-op) |
| `axm_gc_root` | `(i32) → void` | GC root stub (no-op) |
| `axm_gc_unroot` | `(i32) → void` | GC unroot stub (no-op) |
| `__panic` | `() → noreturn` | Trap via WASM `unreachable` |
| `__div_check` | `(i32) → void` | Trap if divisor is zero |

## Memory layout

```
Page 0 (bytes 0–65535):  zero-page + Axiom static string data
Page 1+ (bytes 65536+):  heap managed by axm_alloc
```

## Building

Requires Zig 0.14+:

```sh
cd runtime
zig build -Doptimize=ReleaseSmall
# Output: zig-out/bin/runtime.wasm
```

`runtime.wasm` is not tracked in the repository.

## Running programs

Provide the runtime as the `"axm"` module when running Axiom programs.

**wasmtime:**

```sh
wasmtime --preload axm=zig-out/bin/runtime.wasm --invoke main program.wasm
```

**Node.js:**

```js
const { WASI } = require('wasi');
const wasi = new WASI({ version: 'preview1', env: process.env });

const rtBuf = require('fs').readFileSync('zig-out/bin/runtime.wasm');
const axiomBuf = require('fs').readFileSync('program.wasm');

const rtMod = new WebAssembly.Instance(
  new WebAssembly.Module(rtBuf),
  { wasi_snapshot_preview1: wasi.wasiImport }
);
wasi.initialize(rtMod);

const axiomMod = new WebAssembly.Instance(
  new WebAssembly.Module(axiomBuf),
  { axm: rtMod.exports }
);
console.log(axiomMod.exports.main());
```
