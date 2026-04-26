# Binary IR: Encoding, Decoding, Hashing

The binary IR is the canonical, content-addressed representation of an
Axiom program. Every node is identified by the BLAKE3 hash of its
encoded bytes, and structurally identical sub-trees automatically share
storage.

This document covers the *implementation* of the encoding/decoding
layer. For the byte-level wire format (every tag, field, and inline
sub-structure), see [node-encoding.md](node-encoding.md). For how
encoded payloads are persisted to disk, see
[persistence.md](persistence.md).

---

## Module layout

| File | Responsibility |
|------|----------------|
| `lib/node_tag.ml` | Tag-byte constants for every node, pattern, type expression, effect set, and `do`-statement variant |
| `lib/node_hash.ml` + `lib/node_hash_stubs.c` | Thin OCaml wrapper around the vendored BLAKE3 (`lib/blake3*.c`) |
| `lib/node_encoding.ml` | AST → payload bytes → hash, plus a pluggable storage callback |
| `lib/node_decoding.ml` | Hash + lookup callback → payload bytes → AST |

`node_tag.ml` contains constants only; keeping them in one file is what
prevents encoder/decoder drift.

---

## Hashing (`lib/node_hash.ml`)

| Symbol | Type | Meaning |
|--------|------|---------|
| `hash_size` | `int` (= 32) | BLAKE3 digest length |
| `digest` | `bytes -> bytes` | Hash a payload |
| `zero_hash` | `bytes` | 32 zero bytes (sentinel for "no root") |

The C stub (`lib/node_hash_stubs.c`) calls `blake3_hasher_init`,
`blake3_hasher_update`, and `blake3_hasher_finalize` and returns the
result as a fresh OCaml `bytes`. The portable BLAKE3 sources are built
without SIMD dispatch (see `lib/dune`).

---

## Encoding (`lib/node_encoding.ml`)

### Public API

| Function | Signature | Location |
|----------|-----------|----------|
| `encode_expr` | `store -> expr -> bytes` | `lib/node_encoding.ml:189` |
| `encode_decl` | `store -> decl -> bytes` | `:361` |
| `encode_program` | `store -> program -> bytes` | `:427` |
| `make_mem_store` | `unit -> store * (bytes, bytes) Hashtbl.t` | `:155` |

All three `encode_*` functions return the **hash** of the encoded
node, not the bytes. The bytes go to the `store` callback.

### Layered organisation

The file is structured as a stack of helpers:

1. **Primitives** (`:19–69`): `put_u8`, `put_u16`, `put_u32`, `put_i64`,
   `put_f64`, `put_bool`, `put_str`, `put_lstr`, `put_opt`, `put_list`.
   All multi-byte integers are little-endian. Bounds are enforced:
   `put_str` fails above 64 KiB, `put_list` fails above 65 535 items.
2. **Inline encoders** (`:75–141`): `put_type_expr`, `put_effect_set`,
   `put_param`, `put_pattern`, `put_comment`. These write
   sub-structures *inline* rather than creating addressable child
   nodes — patterns and types are not independently shared in v1.
3. **Store callback** (`:151–163`): the `store` type abstracts
   "hash + persist". `make_mem_store` returns a Hashtbl-backed
   implementation; `Node_store.as_encoding_store` returns the on-disk
   one.
4. **Node builder** (`:170–183`): `build_node` assembles the standard
   payload header (tag, child count, inline length), appends each child
   hash (32 bytes) and the inline data, calls the store, and returns
   the hash.
5. **Recursive encoders** (`:189–431`): `encode_expr`, `encode_decl`,
   `encode_program` pattern-match each AST variant and call
   `build_node` with the right children and inline closure.

### How recursion is handled

Sub-expressions and sub-declarations become *separate payloads* with
their own hashes; the parent stores those hashes in its child list.
This is what makes dedup work across the whole graph: every occurrence
of `Var "x"` in a program produces the same hash and is stored once.

Patterns and type expressions, by contrast, are inlined into the parent
payload. This is a deliberate trade-off documented in
[node-encoding.md](node-encoding.md) — they don't represent independent
units of computation, and the sharing benefit would be marginal.

### Determinism quirks

- **NaN canonicalization** (`:40`): every NaN bit pattern is rewritten
  to `0x7FF8000000000000` before hashing.
- **No alignment, no padding** anywhere in the format.
- **Comment is always the last field** in the inline data of every
  expression and declaration node, and in every inline pattern.

### No memoization in the encoder

Each call to `encode_expr` unconditionally re-encodes the node. The
**store** is the only dedup point — if the hash already exists, it
returns the existing one without rewriting the payload. In practice
this is cheap because the encoder is straight-line and BLAKE3 is fast.

---

## Decoding (`lib/node_decoding.ml`)

### Public API

| Function | Signature | Location |
|----------|-----------|----------|
| `decode_expr` | `lookup -> bytes -> expr` | `lib/node_decoding.ml:202` |
| `decode_decl` | `lookup -> bytes -> decl` | `:394` |
| `decode_program` | `lookup -> bytes -> program` | `:464` |
| `lookup_of_hashtbl` | `(bytes, bytes) Hashtbl.t -> lookup` | `:182` |

`lookup` is `bytes -> bytes` — given a hash, return the payload. It is
deliberately abstract so the same code works for in-memory tests and
the on-disk node store.

### Layered organisation

Mirrors the encoder:

1. **Cursor** (`:16–26`): a mutable read pointer over `bytes` with an
   `ensure` helper that bounds-checks every read.
2. **Primitives** (`:32–99`): `get_u8`, `get_u16`, …, `get_lstr`,
   `get_opt`, `get_list`, `get_hash`.
3. **Inline decoders** (`:105–171`): `get_type_expr`, `get_effect_set`,
   `get_param`, `get_pattern`.
4. **Header parser** (`:190–196`): reads tag, child-hash array, and
   inline cursor.
5. **Recursive decoders** (`:202–467`): each variant pattern-matches
   on the tag byte, reads inline data, and calls back through `lookup`
   for child payloads.

### Roundtrip property (tested)

`test/test_roundtrip.ml` builds an in-memory store, encodes every
example program, decodes from the resulting hashes, and checks for AST
equality. If you change either the encoder or the decoder, expect
failures there before anywhere else.

---

## When to extend the format

Adding a new AST variant means, in order:

1. Reserve a tag in `lib/node_tag.ml`.
2. Add the encoder case in `lib/node_encoding.ml` and the decoder case
   in `lib/node_decoding.ml`.
3. Update [node-encoding.md](node-encoding.md) — that document is the
   ground truth for tooling and external consumers.
4. Add roundtrip coverage in `test/test_roundtrip.ml`.

Any change to the byte layout invalidates every existing hash, so
plan for a store migration if you have persistent images.
