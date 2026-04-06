(** Content-addressing hash for binary IR nodes.

    Uses SHA-256 (32-byte digests) via OpenSSL libcrypto.
    To be replaced with Blake3 when the OCaml library is available.
    The hash_size and digest function are the only points of contact;
    all encoding logic is independent of the hash algorithm. *)

let hash_size = 32

external sha256 : bytes -> bytes = "caml_node_hash_sha256"

let digest (payload : bytes) : bytes =
  sha256 payload

let zero_hash : bytes =
  Bytes.make hash_size '\x00'
