(** Content-addressing hash for binary IR nodes.

    Uses BLAKE3 (32-byte digests) via the vendored BLAKE3 C reference
    implementation (see blake3.c, blake3_dispatch.c, blake3_portable.c).
    The [hash_size] and [digest] function are the only points of contact
    with the hash algorithm; all encoding logic is independent of it. *)

let hash_size = 32

external blake3 : bytes -> bytes = "caml_node_hash_blake3"

let digest (payload : bytes) : bytes =
  blake3 payload

let zero_hash : bytes =
  Bytes.make hash_size '\x00'
