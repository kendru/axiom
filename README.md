# axiom

A research PL designed primarily for LLMs.

## Building

The compiler builds with dune. You will need:

- **OCaml** (4.14 or later) and **dune** (3.x)
- **alcotest** (for the test suite)
- **A C compiler** (gcc or clang) — used to compile the vendored BLAKE3
  sources and the OCaml C stubs under `lib/`

No external C libraries are required. BLAKE3 (used for content-addressing
binary IR nodes) is vendored in `lib/` as portable-only C, so the build has
no link-time dependency on OpenSSL, libsodium, or a system BLAKE3 library.
See `lib/BLAKE3_VENDORED.md` for details on the vendored sources and
`lib/BLAKE3_LICENSE` for the upstream license.

### Commands

```sh
dune build    # compile
dune test     # run the full test suite
```
