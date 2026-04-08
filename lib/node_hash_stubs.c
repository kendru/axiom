/* node_hash_stubs.c — BLAKE3 binding for content-addressing node hashes.

   Provides a single function: caml_node_hash_blake3(payload) -> digest
   where payload is an OCaml bytes value and digest is a 32-byte OCaml
   bytes value.

   BLAKE3 is vendored in blake3.c, blake3_dispatch.c, blake3_portable.c
   and compiled portable-only (no SIMD) — see dune flags. */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <string.h>

#include "blake3.h"

CAMLprim value caml_node_hash_blake3(value v_payload)
{
    CAMLparam1(v_payload);
    CAMLlocal1(v_digest);

    const void *data = (const void *)Bytes_val(v_payload);
    size_t len = caml_string_length(v_payload);

    blake3_hasher hasher;
    blake3_hasher_init(&hasher);
    blake3_hasher_update(&hasher, data, len);

    uint8_t out[BLAKE3_OUT_LEN];
    blake3_hasher_finalize(&hasher, out, BLAKE3_OUT_LEN);

    v_digest = caml_alloc_string(BLAKE3_OUT_LEN);
    memcpy(Bytes_val(v_digest), out, BLAKE3_OUT_LEN);

    CAMLreturn(v_digest);
}
