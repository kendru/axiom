# Vendored BLAKE3

This directory contains a vendored copy of the official BLAKE3 C reference
implementation, used for content-addressing binary IR nodes.

## Provenance

- **Upstream:** https://github.com/BLAKE3-team/BLAKE3
- **Version:** 1.5.1
- **Source path in upstream:** `c/`
- **License:** CC0 1.0 / Apache License 2.0 (dual-licensed) — see `BLAKE3_LICENSE`

## Vendored files

| File                 | Purpose                                           |
|----------------------|---------------------------------------------------|
| `blake3.h`           | Public API header                                 |
| `blake3_impl.h`      | Internal implementation header                    |
| `blake3.c`           | Hasher state machine and tree hashing             |
| `blake3_dispatch.c`  | Runtime dispatch to portable / SIMD backends      |
| `blake3_portable.c`  | Pure-C compression and hash-many implementations  |

The SIMD source files (`blake3_sse2*`, `blake3_sse41*`, `blake3_avx2*`,
`blake3_avx512*`, `blake3_neon.c`, and the `.S` assembly variants) are
**not** vendored. See the build flags below for how SIMD is disabled.

## Build flags

The sources are compiled as `foreign_stubs` in `lib/dune` with these
preprocessor flags (portable-only, no SIMD):

```
-DBLAKE3_NO_SSE2
-DBLAKE3_NO_SSE41
-DBLAKE3_NO_AVX2
-DBLAKE3_NO_AVX512
-DBLAKE3_USE_NEON=0
```

These flags cause `blake3_dispatch.c` to short-circuit every SIMD code
path and fall through to the portable functions in `blake3_portable.c`,
so no SIMD translation units need to be linked and no CPU-specific
assembly is required. Performance is lower than a fully-optimized BLAKE3
build, but the tradeoff buys us portability and a zero-external-dependency
build.

If SIMD acceleration becomes worthwhile, drop the `BLAKE3_NO_*` flags
and add the corresponding `blake3_sse*.c` / `blake3_avx*.c` /
`blake3_neon.c` files (plus their asm counterparts on platforms that
need them) to the `foreign_stubs` `(names ...)` list.

## Updating

To sync with a newer upstream release:

1. Fetch the target version's files from
   `https://raw.githubusercontent.com/BLAKE3-team/BLAKE3/<version>/c/<file>`
   for each file listed above.
2. Fetch the upstream `LICENSE` into `BLAKE3_LICENSE`.
3. Update the **Version** line at the top of this document.
4. Run `dune test` — the BLAKE3 test vectors in
   `test/test_node_encoding.ml` will catch any regression in the binding.
