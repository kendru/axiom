# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```bash
dune build          # Build the project
dune test           # Run all tests
dune exec test/test_typechecker.exe   # Run a single test file
dune exec bin/main.exe -- build <input.axm> -o <output.wasm>  # Run the CLI
```

Running a single test with alcotest filtering:
```bash
dune exec test/test_typechecker.exe -- --run "test name"
```

Dependencies: OCaml 4.14+, dune 3.x, alcotest. A C compiler is required for the vendored BLAKE3 sources in `lib/`.

## What This Is

Axiom is a compiled programming language designed for LLM code generation. It has:
- A Hindley-Milner type system with let-generalization and row-typed algebraic effects
- A binary IR that uses content-addressing (BLAKE3 hashes as node IDs)
- A **working form** (LLM-optimized textual syntax) that compiles to the binary IR
- A **review form** (human-readable pretty-printed output) generated from the binary IR
- WebAssembly as the compilation target

The primary design document is `axiom-overview-draft.md`.

## Compilation Pipeline

```
Source text (.axm)
  → Lexer (lexer.ml)          → token list
  → Parser (parser.ml)        → Ast.program
  → Typechecker (typechecker.ml) → typed Ast.program
  → Codegen (codegen.ml)      → Wasm instructions
  → Wasm_encode (lib/wasm/wasm_encode.ml) → .wasm binary
```

The CLI (`bin/main.ml`) wires these stages together with the `axiom build` subcommand.

## Key Architectural Concepts

**Effect system:** Effects are row-typed. A function's effect annotation is an *effect row* — either `RPure` (no effects), `RCons (eff, tail)` (one effect plus a tail row), or `RMeta` (a unification variable). During type inference, effect rows unify like record rows, allowing implicit effect polymorphism via row re-opening. The relevant types are `effect_row` and `effect_set` in `ast.ml`, and the inference logic is in `typechecker.ml`.

**Binary IR / Node store:** The AST can be serialized to a content-addressed binary IR where each node is identified by its BLAKE3 hash. `node_encoding.ml` converts AST → binary nodes, `node_decoding.ml` does the reverse, `node_hash.ml` wraps the vendored BLAKE3, and `node_store.ml` provides persistent storage. Roundtrip correctness is tested in `test/test_roundtrip.ml`.

**WASM encoder:** `lib/wasm/wasm_encode.ml` is a self-contained binary WASM encoder (LEB128, section emitters, instruction encoding). It is used by `codegen.ml` to emit the final module.

**Typechecker two-pass design:** `typechecker.ml` uses a two-pass walk over declarations — a first pass collects top-level type and value signatures, and a second pass checks bodies. This allows mutual recursion without pre-declarations.

## Examples

`examples/` contains 10 `.axm` programs (01_basics through 10_json) that demonstrate the working-form syntax and serve as integration tests for the pipeline. `examples/README.md` documents the code conventions used in those files.

## Pull Requests

When creating a pull request that implements a GitHub issue, always reference the issue in the PR body so it closes on merge:

```
Closes #<issue-number>
```

## Implementation Documentation

Detailed documentation for each major subsystem lives in `docs/implementation/`:

| Document | Covers |
|----------|--------|
| [front-end.md](docs/implementation/front-end.md) | Lexer, AST, Parser, Printer — the working-form ↔ AST pipeline |
| [typechecker.md](docs/implementation/typechecker.md) | Hindley–Milner inference, effect row unification, two-pass declaration checking |
| [ir.md](docs/implementation/ir.md) | Binary IR encoding/decoding, BLAKE3 hashing, structural sharing |
| [node-encoding.md](docs/implementation/node-encoding.md) | Byte-level wire format reference for every IR node type |
| [persistence.md](docs/implementation/persistence.md) | Node store: segmented flat-file layout, read/write paths, bloom filters, sealing |
| [node-store.md](docs/implementation/node-store.md) | On-disk format reference for segments, indexes, and the manifest |
| [codegen.md](docs/implementation/codegen.md) | Code generator and WASM encoder (current Milestone 1 scope) |
| [status.md](docs/implementation/status.md) | Feature implementation status vs. the design specification |
