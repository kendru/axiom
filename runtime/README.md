# Axiom Runtime

Minimal WebAssembly runtime for Axiom programs.  Targets `wasm32-freestanding`.

## What it provides

| Export | Signature | Purpose |
|--------|-----------|---------|
| `_start` | `() → void` | Entry point; calls `axiom_program.main` and stores the result |
| `get_exit_code` | `() → i32` | Returns the value written by `_start` |
| `__panic` | `() → noreturn` | Traps via the WASM `unreachable` instruction |
| `__div_check` | `(i32) → void` | Traps if divisor is zero (integer-division guard) |

## Import contract

The runtime imports one symbol that must be resolved when the host
instantiates the module:

| Import module | Import name | Signature | Provided by |
|---------------|-------------|-----------|-------------|
| `axiom_program` | `main` | `() → i32` | Axiom-compiled application |

## Building

Requires Zig 0.16.0:

```sh
cd runtime
zig build -Doptimize=ReleaseSmall
# Output: zig-out/bin/runtime.wasm
```

`runtime.wasm` is a compiler artifact and is **not** tracked in the
repository.  Build it locally before running the runtime test group.

## Running programs via the runtime

The runtime and the Axiom module are linked by the host.  An example using
Node.js:

```js
const fs = require('fs');
const runtimeBuf = fs.readFileSync('runtime/zig-out/bin/runtime.wasm');
const axiomBuf   = fs.readFileSync('program.wasm');

const axiomMod   = await WebAssembly.instantiate(axiomBuf);
const axiomMain  = axiomMod.instance.exports.main;

const runtimeMod = await WebAssembly.instantiate(runtimeBuf, {
    axiom_program: { main: axiomMain }
});
runtimeMod.instance.exports._start();
const result = runtimeMod.instance.exports.get_exit_code();
console.log(result);
```

Under `wasmtime`, use static linking (wasm-ld) to merge both modules into a
single binary that exposes `_start`, then run with:

```sh
wasmtime linked.wasm
```

## Memory

The runtime does not declare or import linear memory.  Memory management
(bump allocator, heap pointer) is entirely the responsibility of the
Axiom-compiled module.
