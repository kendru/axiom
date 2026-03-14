# Axiom

A research programming language designed primarily for LLMs.

Axiom inverts the usual assumption: the LLM is the primary author and
day-to-day consumer of code, while humans set goals, review outputs, and
guide design. The language is optimized for LLM attention patterns —
keyword-rich syntax, explicit effect annotations, and structured query/
transform/verify commands that give LLMs targeted codebase access without
polluting context.

## Repository Layout

```
spec/        Language specification (CC BY 4.0)
compiler/    OCaml compiler frontend (Apache 2.0)
runtime/     Zig runtime and standard library (Apache 2.0)
tests/       End-to-end example programs
```

## Specification

See [spec/axiom-overview-draft.md](spec/axiom-overview-draft.md) for the
current v0.2 draft specification.

## Building

### Compiler (OCaml / Dune)

Requires OCaml ≥ 5.2 and opam.

```sh
cd compiler
opam install . --deps-only --with-test
dune build
dune test
```

### Runtime (Zig)

Requires Zig ~0.15.0.

```sh
cd runtime
zig build
zig build test
```

## License

- **Specification** (`spec/`): [CC BY 4.0](spec/LICENSE-CC-BY-4.0)
- **Implementation** (everything else): [Apache 2.0](LICENSE)
