# Node Store: Segmented Flat-File Design

The node store is the on-disk representation of the content-addressed IR graph.
It lives under `.axm-image/nodes/` in the image directory.

## Design Goals

- No per-node filesystem objects (avoids inode pressure at scale).
- O(log n) lookup by hash with bounded memory overhead.
- Bounded write amplification: sealing a segment does not rewrite existing
  segments.
- Simple GC: compaction is the only time nodes are removed, and it is a
  purely additive rewrite (copy referenced nodes into new segments, drop old
  segments).
- No external dependencies; implemented entirely in OCaml using standard I/O.

## Why Not a Full LSM Tree

LSM trees are designed for workloads with frequent updates to existing keys.
Content-addressed nodes are write-once and never updated — the only mutation
is GC, which is infrequent. Multi-level compaction, per-level bloom filters,
and tombstone handling are complexity that buys nothing for an immutable
key-value store.

The segmented design borrows LSM's core insight — bounded write amplification
via sealed immutable segment files — while discarding everything that exists
to handle mutable data.

---

## On-Disk Layout

```
.axm-image/nodes/
  manifest.bin          # active segment list, root hash, stats
  seg-000001.bin        # sealed segment: node data
  seg-000001.idx        # sealed segment: sorted hash index (frozen)
  seg-000002.bin        # active segment: node data (append-only)
  seg-000002.idx        # active segment: sorted hash index (in-memory, flushed on seal)
```

Segments are numbered with a monotonically increasing 6-digit decimal ID.
Exactly one segment is active (writeable) at any time; all others are sealed
(read-only). A new segment is opened after the previous one is sealed.

---

## Manifest Format (`manifest.bin`)

```
[magic    : 4 bytes]  0x41 0x58 0x4E 0x53  ("AXNS")
[version  : u16]      format version, currently 1
[reserved : u16]      padding, must be 0
[root_hash: 32 bytes] hash of the current program root node (all zeros if none)
[n_segs   : u32]      number of active segments
[seg_id_0 : u32]      segment IDs in creation order, oldest first
...
[seg_id_n : u32]
```

The manifest is rewritten atomically (write to `manifest.tmp`, then rename)
whenever a segment is sealed or compaction runs.

---

## Segment Data File (`seg-NNNNNN.bin`)

A sequence of back-to-back node records. Records are not aligned; the index
provides offsets.

```
[magic   : 4 bytes]  0x41 0x58 0x4E 0x44  ("AXND")
[seg_id  : u32]      segment identifier
[version : u16]      format version, currently 1
[reserved: u16]      padding, must be 0
--- repeated node records ---
[hash    : 32 bytes] Blake3 hash of (tag ‖ children ‖ inline_data)
[length  : u32]      byte length of node payload that follows
[payload : N bytes]  node encoding (see node-encoding.md)
```

The hash is stored redundantly in the data file so that a segment can be
validated or re-indexed without the `.idx` file.

---

## Segment Index File (`seg-NNNNNN.idx`)

A header followed by a sorted array of fixed-size entries. The array is sorted
ascending by hash, enabling O(log n) binary search.

```
[magic    : 4 bytes]  0x41 0x58 0x4E 0x49  ("AXNI")
[seg_id   : u32]      must match the corresponding .bin file
[version  : u16]      format version, currently 1
[reserved : u16]      padding, must be 0
[n_entries: u32]      number of index entries
[bloom    : 256 bytes] 2048-bit bloom filter (k=3, see below)
--- repeated index entries, sorted by hash ---
[hash     : 32 bytes]
[offset   : u64]      byte offset of the node record in the .bin file
```

Each index entry is 40 bytes. One million entries consume 40 MB of memory
when loaded. At typical program sizes (< 500 K nodes per segment) the full
index for a segment fits comfortably in memory.

### Bloom Filter

Each sealed segment carries a 2048-bit bloom filter with k=3 independent
hash functions derived from the node hash via double hashing:

```
h_i(x) = (h0(x) + i * h1(x)) mod 2048    for i in {0, 1, 2}
```

where `h0` and `h1` are the low and high 64-bit halves of the node's Blake3
hash. Before binary-searching a segment's index, the bloom filter is checked.
A negative result means the node is definitely absent from that segment; a
positive result means it is probably present (proceed to binary search).

This makes "node not in this segment" checks nearly free, which matters when
reading a node that was written many sessions ago and lives in an early segment
not in the in-memory cache.

---

## Read Path

1. Check the in-memory write buffer (a hash table of nodes written in the
   current session but not yet flushed to the active segment).
2. Check the in-memory index cache for the active segment (always loaded).
3. For each sealed segment, newest to oldest:
   a. Check the bloom filter (in-memory, 256 bytes per segment).
   b. If bloom positive, binary-search the segment's index (loaded on first
      use, then retained).
   c. If found, seek to the offset in the `.bin` file and read `length` bytes.
4. Return `Not_found` if no segment contains the hash.

In practice the vast majority of reads hit the active segment or the one
before it, so sealed segment indexes are rarely loaded.

---

## Write Path

1. Encode the node to bytes and compute its Blake3 hash.
2. If the hash is already in the write buffer or any segment index, return the
   existing hash (deduplication is automatic).
3. Append `[hash:32][length:4][payload:N]` to the active `.bin` file.
4. Insert `(hash, offset)` into the in-memory index for the active segment.
5. If the active segment now exceeds the seal threshold (default: 65,536 nodes
   or 64 MB of payload, whichever comes first), seal it:
   a. Sort the in-memory index by hash.
   b. Compute the bloom filter from all hashes.
   c. Write the `.idx` file.
   d. Open a new active segment and update the manifest.

---

## Compaction

Compaction merges two or more sealed segments into a single new segment,
retaining only nodes reachable from the current root hash. It is a GC pass
combined with segment consolidation.

```
axiom image compact
```

Algorithm:
1. Traverse the IR graph from the root hash, collecting all reachable hashes
   into a set.
2. Open a fresh segment as the output.
3. For each sealed segment (oldest first), copy each node whose hash is in the
   reachable set to the output segment.
4. Seal the output segment.
5. Update the manifest to replace the merged segments with the new one.
6. Delete the old `.bin` and `.idx` files.

Because nodes are immutable and content-addressed, compaction is always safe
to interrupt and retry: a partial output segment is simply discarded and the
original segments remain intact.

---

## Segment Seal Threshold Rationale

65,536 nodes per segment means:
- Index entries per segment: 65,536 × 40 bytes = ~2.5 MB per loaded index.
- With 16 sealed segments, all indexes loaded: ~40 MB — acceptable resident
  set overhead for a development tool.
- Binary search over 65,536 entries: at most 16 comparisons.

These defaults are tunable in the image manifest's configuration block (not
yet designed); they should be revisited once real workload data is available.

---

## Relationship to the Rest of the Image

The node store is the source of truth. All other image contents are derived:

- `indexes/graph.db` — SQLite graph index (edge types, caller maps) built by
  scanning the node store.
- `indexes/types.idx` — type index built from nodes with type-annotation tags.
- `cache/compiled.wasm` — output of the code generator; invalidated when the
  root hash changes.

If the node store is intact, every other file in the image can be regenerated.
