(** Content-addressing hash for binary IR nodes.

    Currently uses MD5 doubled to 32 bytes as a placeholder.
    Will be replaced with Blake3 when the library is available.
    The hash_size and digest function are the only points of contact;
    all encoding logic is independent of the hash algorithm. *)

let hash_size = 32

let digest (payload : bytes) : bytes =
  let md5 = Digest.bytes payload in
  let h = Bytes.create hash_size in
  Bytes.blit_string md5 0 h 0 16;
  Bytes.blit_string md5 0 h 16 16;
  h

let zero_hash : bytes =
  Bytes.make hash_size '\x00'
