# Persistence: Node Store (`lib/node_store.ml`)

The node store is the on-disk repository for all content-addressed IR
payloads. It lives in a directory (`.axm-image/nodes/` by convention)
and is used by the compiler pipeline whenever a program is serialized
to or read from the binary IR.

For the exact on-disk format (manifest magic, segment headers, index
entries, bloom filter spec), see [node-store.md](node-store.md). This
document covers the *implementation* of the store.

---

## Public API

| Function | Signature | Location |
|----------|-----------|----------|
| `open_store` | `string -> t` | `lib/node_store.ml:421` |
| `close_store` | `t -> unit` | `:460` |
| `write` | `t -> bytes -> bytes` | `:477` |
| `lookup` | `t -> bytes -> bytes` | `:505` |
| `set_root` | `t -> bytes -> unit` | `:526` |
| `root_hash` | `t -> bytes` | `:531` |
| `seal` | `t -> unit` | `:413` |
| `as_encoding_store` | `t -> Node_encoding.store` | `:538` |
| `as_decoding_lookup` | `t -> Node_decoding.lookup` | `:542` |

`write` returns the BLAKE3 hash of the payload; if the same hash is
already present in any segment it returns immediately without writing
again. `lookup` raises `Not_found` if the hash is absent.

The two adaptor functions integrate cleanly with the IR layer:
`as_encoding_store` wraps `write` into the callback type the encoder
expects; `as_decoding_lookup` wraps `lookup` for the decoder.

---

## Internal types

```
type t = {
  dir               : string;
  mutable root_hash : bytes;              (* 32 bytes *)
  mutable sealed    : sealed_seg list;    (* oldest-first *)
  mutable active_id : int;
  mutable active_oc : out_channel;
  mutable active_off: int;
  active_idx        : (string, int) Hashtbl.t;
  mutable node_count   : int;
  mutable payload_bytes: int;
}

type sealed_seg = {
  ss_id        : int;
  ss_data_path : string;
  ss_bloom     : bytes;               (* 256 bytes, 2048-bit filter *)
  ss_index     : (bytes * int) array; (* (hash, offset), sorted by hash *)
}
```

The active segment is represented as the four mutable fields in `t`
(`active_oc`, `active_off`, `active_idx`, node/byte counters). Sealed
segments live in memory only as their bloom filter and sorted index; the
actual payloads stay on disk and are read on demand.

---

## Module organisation

| Lines | Content |
|-------|---------|
| 14–43 | Constants: magic strings (`AXNS`, `AXND`, `AXNI`), seal thresholds (65 536 nodes / 64 MB), bloom parameters (2048 bits, k=3) |
| 46–93 | Binary I/O helpers (`read_u8`, `write_u32`, etc., little-endian) |
| 99–147 | Bloom filter: bit-position function, set-bit, check-bit |
| 153–155 | Path helpers: manifest path, segment `.bin`/`.idx` paths |
| 161–176 | File utilities (`read_file`) |
| 182–228 | Manifest read / atomic write |
| 234–290 | Segment data scanner (builds in-memory index by scanning a `.bin` file) |
| 298–352 | Segment index read / write / binary search |
| 358–409 | `do_seal`: sort active index, build bloom filter, write `.idx`, open next segment, update manifest |
| 421–457 | `open_store`: create or resume, load sealed-segment indexes |
| 468–497 | `write` path |
| 505–520 | `lookup` path |
| 526–542 | Root hash, adaptors |

---

## Read path

`lookup t hash` (`lib/node_store.ml:505`):

1. Check `active_idx` (the in-memory Hashtbl for the current segment).
2. Walk `t.sealed` **newest-first**:
   a. Test the 2048-bit bloom filter (3 bit-positions, O(1)).
   b. If bloom positive, binary-search the sorted `ss_index` array.
   c. If found, `seek` to the recorded offset in `.bin` and read the
      payload.
3. Raise `Not_found` if no segment matched.

Bloom filters are loaded into memory once when the store is opened and
held for the lifetime of the store handle.

---

## Write path

`write t payload` (`lib/node_store.ml:477`):

1. Compute `hash = Node_hash.digest payload`.
2. If `hash` is already in `active_idx` or in any sealed segment (bloom
   then binary search), return `hash` immediately — no write.
3. Append `[hash:32][length:4][payload:N]` to the active `.bin` file.
4. Record `(hash, offset)` in `active_idx`.
5. Increment `node_count` and `payload_bytes`.
6. If either threshold is reached (65 536 nodes **or** 64 MB), call
   `do_seal`.

---

## Bloom filter implementation

The double-hash trick (`lib/node_store.ml:116–119`):

```
h0 = low  64 bits of the node's BLAKE3 hash
h1 = high 64 bits of the node's BLAKE3 hash
bit_i = (h0 + i × h1) mod 2048    for i ∈ {0, 1, 2}
```

Both halves are extracted as `int64` from the first 16 bytes of the
32-byte hash. Each of the three bit positions is set (on write) or
checked (on lookup) by masking into the 256-byte `ss_bloom` array. A
false positive causes a harmless extra binary search; the actual hash
comparison in the index resolves the ambiguity.

---

## Atomic manifest rewrite

`write_manifest` (`lib/node_store.ml:214`):

1. Write the new manifest to `manifest.tmp` in the store directory.
2. Close the file.
3. `Sys.rename "manifest.tmp" "manifest.bin"` — atomic on POSIX.

If the process crashes between steps 1 and 3, the `.tmp` file is
orphaned and the previous manifest remains intact. On resume, the
orphan is ignored.

---

## Sealing

`do_seal` (`lib/node_store.ml:381`):

1. Close the active `.bin` file.
2. Sort `active_idx` entries by hash (producing the `ss_index` array).
3. Build the bloom filter by iterating over all hashes.
4. Write the `.idx` file: header + bloom + sorted entries.
5. Open a fresh active segment (next ID, new `.bin` file).
6. Rewrite the manifest to include the new sealed segment.

After sealing, the former active segment becomes the newest entry in
`t.sealed`.

---

## Resuming an existing store

`open_store dir` (`lib/node_store.ml:436`): when the directory already
exists, the function reads the manifest to discover the segment list,
loads the `.idx` file for each sealed segment (bloom filter + full
index array), and scans the active `.bin` file to rebuild `active_idx`
in memory. Scanning is necessary because the index for the active
segment is only written at seal time.

---

## Compaction

Not yet implemented — there is no `compact` function. The design
specification describes a reachability-based GC that copies live nodes
into a fresh segment and drops the old ones; the protocol is documented
in [node-store.md](node-store.md).

---

## Error handling

The store raises OCaml `Failure` for format violations (bad magic
bytes, version mismatch) and `Not_found` from `lookup`. File I/O
errors propagate as `Sys_error`. There is no retry or recovery logic.
