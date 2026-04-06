/* node_hash_stubs.c — SHA-256 binding via OpenSSL libcrypto.

   Provides a single function: caml_node_hash_sha256(payload) -> digest
   where payload is an OCaml bytes value and digest is a 32-byte OCaml
   bytes value.

   This is a placeholder for Blake3. When blake3-ocaml becomes available,
   delete this file and switch node_hash.ml to use blake3 directly. */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <string.h>
#include <openssl/evp.h>

CAMLprim value caml_node_hash_sha256(value v_payload)
{
    CAMLparam1(v_payload);
    CAMLlocal1(v_digest);

    const unsigned char *data = (const unsigned char *)Bytes_val(v_payload);
    size_t len = caml_string_length(v_payload);

    unsigned char md[32];
    unsigned int md_len = 0;

    EVP_MD_CTX *ctx = EVP_MD_CTX_new();
    EVP_DigestInit_ex(ctx, EVP_sha256(), NULL);
    EVP_DigestUpdate(ctx, data, len);
    EVP_DigestFinal_ex(ctx, md, &md_len);
    EVP_MD_CTX_free(ctx);

    v_digest = caml_alloc_string(32);
    memcpy(Bytes_val(v_digest), md, 32);

    CAMLreturn(v_digest);
}
