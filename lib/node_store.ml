(** On-disk node store.

    Persistent, content-addressed store for binary IR nodes.

    On-disk layout under [dir]:
      manifest.bin       — active segment list and root hash
      seg-NNNNNN.bin     — node data (one per segment)
      seg-NNNNNN.idx     — sorted hash index (sealed segments only)

    See docs/implementation/node-store.md for the format specification. *)

let hash_size = Node_hash.hash_size   (* 32 *)

(* ================================================================== *)
(* Constants                                                            *)
(* ================================================================== *)

let manifest_magic = "AXNS"
let data_magic     = "AXND"
let index_magic    = "AXNI"
let format_version = 1

(** Seal a segment after this many nodes. *)
let seal_threshold_nodes = 65536

(** Seal a segment after this many bytes of payload. *)
let seal_threshold_bytes = 64 * 1024 * 1024

(** Bloom filter size in bytes (2048 bits). *)
let bloom_bytes = 256

(** Number of bloom filter hash functions (k). *)
let bloom_k = 3

(** Data file header size: magic(4) + seg_id(4) + version(2) + reserved(2) = 12. *)
let data_header_size = 12

(** Index file header size: magic(4) + seg_id(4) + version(2) + reserved(2)
    + n_entries(4) + bloom(256) = 272. *)
let idx_header_size = 272

(** Index entry size: hash(32) + offset_u64(8) = 40. *)
let idx_entry_size = hash_size + 8

(* ================================================================== *)
(* Binary I/O helpers — little-endian                                  *)
(* ================================================================== *)

let get_u8 buf off = Char.code (Bytes.get buf off)

let get_u16_le buf off =
  get_u8 buf off
  lor ((get_u8 buf (off + 1)) lsl 8)

let get_u32_le buf off =
  get_u8 buf off
  lor ((get_u8 buf (off + 1)) lsl 8)
  lor ((get_u8 buf (off + 2)) lsl 16)
  lor ((get_u8 buf (off + 3)) lsl 24)

(** Read a little-endian u64 from [buf] at [off] as an OCaml [int].
    Safe on 64-bit systems for values up to [max_int] (~4.6×10¹⁸). *)
let get_u64_le buf off =
  get_u8 buf off
  lor ((get_u8 buf (off + 1)) lsl 8)
  lor ((get_u8 buf (off + 2)) lsl 16)
  lor ((get_u8 buf (off + 3)) lsl 24)
  lor ((get_u8 buf (off + 4)) lsl 32)
  lor ((get_u8 buf (off + 5)) lsl 40)
  lor ((get_u8 buf (off + 6)) lsl 48)
  lor ((get_u8 buf (off + 7)) lsl 56)

let write_u8 oc v = output_char oc (Char.chr (v land 0xFF))

let write_u16_le oc v =
  write_u8 oc  (v land 0xFF);
  write_u8 oc ((v lsr 8)  land 0xFF)

let write_u32_le oc v =
  write_u8 oc  (v land 0xFF);
  write_u8 oc ((v lsr 8)  land 0xFF);
  write_u8 oc ((v lsr 16) land 0xFF);
  write_u8 oc ((v lsr 24) land 0xFF)

let write_u64_le oc v =
  write_u8 oc  (v land 0xFF);
  write_u8 oc ((v lsr 8)  land 0xFF);
  write_u8 oc ((v lsr 16) land 0xFF);
  write_u8 oc ((v lsr 24) land 0xFF);
  write_u8 oc ((v lsr 32) land 0xFF);
  write_u8 oc ((v lsr 40) land 0xFF);
  write_u8 oc ((v lsr 48) land 0xFF);
  write_u8 oc ((v lsr 56) land 0xFF)

(* ================================================================== *)
(* Bloom filter                                                         *)
(* ================================================================== *)

(** Compute the [i]-th bloom filter bit position for [hash].
    Uses double-hashing: h_i = (h0 + i×h1) mod 2048, where h0 and h1
    are the low and high 64-bit halves of the BLAKE3 hash.
    Since 2048 = 2^11, mod 2048 is equivalent to masking the low 11 bits. *)
let bloom_bit_pos hash i =
  let read_u64_i64 off =
    let b k = Int64.of_int (get_u8 hash (off + k)) in
    let open Int64 in
    logor (b 0)
      (logor (shift_left (b 1)  8)
      (logor (shift_left (b 2) 16)
      (logor (shift_left (b 3) 24)
      (logor (shift_left (b 4) 32)
      (logor (shift_left (b 5) 40)
      (logor (shift_left (b 6) 48)
             (shift_left (b 7) 56)))))))
  in
  let h0 = read_u64_i64 0 in
  let h1 = read_u64_i64 8 in
  let sum = Int64.add h0 (Int64.mul (Int64.of_int i) h1) in
  Int64.to_int (Int64.logand sum 2047L)

(** Test whether [hash] is possibly present in [bloom].
    A [false] result means definitely absent; [true] means probably present. *)
let bloom_check bloom hash =
  let rec go i =
    if i = bloom_k then true
    else
      let bit = bloom_bit_pos hash i in
      if get_u8 bloom (bit lsr 3) land (1 lsl (bit land 7)) = 0
      then false
      else go (i + 1)
  in
  go 0

(** Set the bits for [hash] in [bloom] (mutates [bloom]). *)
let bloom_add bloom hash =
  for i = 0 to bloom_k - 1 do
    let bit = bloom_bit_pos hash i in
    let byte_idx = bit lsr 3 in
    Bytes.set bloom byte_idx
      (Char.chr (get_u8 bloom byte_idx lor (1 lsl (bit land 7))))
  done

(** Build a fresh bloom filter from an array of hashes. *)
let bloom_of_hashes hashes =
  let b = Bytes.make bloom_bytes '\x00' in
  Array.iter (bloom_add b) hashes;
  b

(* ================================================================== *)
(* File paths                                                           *)
(* ================================================================== *)

let manifest_path dir   = Filename.concat dir "manifest.bin"
let seg_data_path dir n = Filename.concat dir (Printf.sprintf "seg-%06d.bin" n)
let seg_idx_path  dir n = Filename.concat dir (Printf.sprintf "seg-%06d.idx" n)

(* ================================================================== *)
(* File utilities                                                       *)
(* ================================================================== *)

let file_size path =
  if not (Sys.file_exists path) then 0
  else begin
    let ic = open_in_bin path in
    let n = in_channel_length ic in
    close_in ic;
    n
  end

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  close_in ic;
  buf

(* ================================================================== *)
(* Manifest                                                             *)
(* ================================================================== *)

(** Manifest header (fixed part):
      magic(4) + version(2) + reserved(2) + root_hash(32) + n_segs(4) = 44 bytes *)
let manifest_fixed_size = 44

(** Read the manifest from [dir].
    Returns [(root_hash, seg_ids)].
    Returns [(zero_hash, [])] if no manifest exists (fresh store). *)
let read_manifest dir =
  let path = manifest_path dir in
  if not (Sys.file_exists path) then
    (Bytes.copy Node_hash.zero_hash, [])
  else begin
    let data = read_file path in
    if Bytes.length data < manifest_fixed_size then
      failwith "Node_store: manifest too short";
    let magic = Bytes.sub_string data 0 4 in
    if magic <> manifest_magic then
      failwith "Node_store: bad manifest magic";
    let version = get_u16_le data 4 in
    if version <> format_version then
      failwith (Printf.sprintf "Node_store: unsupported manifest version %d" version);
    let root_hash = Bytes.sub data 8 hash_size in
    let n_segs = get_u32_le data (8 + hash_size) in
    let seg_ids = List.init n_segs (fun i ->
      get_u32_le data (manifest_fixed_size + i * 4)
    ) in
    (root_hash, seg_ids)
  end

(** Atomically write the manifest to [dir].
    Writes to a .tmp file first then renames to ensure crash-safety. *)
let write_manifest dir root_hash seg_ids =
  let tmp = manifest_path dir ^ ".tmp" in
  let oc = open_out_bin tmp in
  (try
    output_string oc manifest_magic;
    write_u16_le oc format_version;
    write_u16_le oc 0;
    output_bytes oc root_hash;
    write_u32_le oc (List.length seg_ids);
    List.iter (write_u32_le oc) seg_ids;
    close_out oc
  with e ->
    close_out_noerr oc;
    (try Sys.remove tmp with _ -> ());
    raise e);
  Sys.rename tmp (manifest_path dir)

(* ================================================================== *)
(* Segment data file                                                    *)
(* ================================================================== *)

(** Write the 12-byte header to a newly created segment data file. *)
let write_data_header oc seg_id =
  output_string oc data_magic;
  write_u32_le oc seg_id;
  write_u16_le oc format_version;
  write_u16_le oc 0

(** Read the payload of a node at [record_offset] in [data_path].
    A record is: [hash:32][length:u32][payload:N]. The offset points to the hash,
    so the payload starts at [record_offset + hash_size + 4]. *)
let read_node_payload data_path record_offset =
  let ic = open_in_bin data_path in
  let payload = (try
    seek_in ic (record_offset + hash_size);
    let len_buf = Bytes.create 4 in
    really_input ic len_buf 0 4;
    let length = get_u32_le len_buf 0 in
    let p = Bytes.create length in
    really_input ic p 0 length;
    p
  with e ->
    close_in_noerr ic;
    raise e)
  in
  close_in ic;
  payload

(** Scan all complete records in [path], returning [(hash_key, record_offset)] pairs
    and the byte offset of the first byte after the last complete record.
    Partial records at the end (e.g. from a crash) are silently ignored. *)
let scan_data_file path =
  let ic = open_in_bin path in
  let entries, next_off =
    (try
      let hdr = Bytes.create data_header_size in
      really_input ic hdr 0 data_header_size;
      if Bytes.sub_string hdr 0 4 <> data_magic then
        failwith (Printf.sprintf "Node_store: bad data magic in %s" path);
      let acc = ref [] in
      let last_good = ref data_header_size in
      (try while true do
        let record_off = pos_in ic in
        let hash_buf = Bytes.create hash_size in
        really_input ic hash_buf 0 hash_size;
        let len_buf = Bytes.create 4 in
        really_input ic len_buf 0 4;
        let length = get_u32_le len_buf 0 in
        let payload_buf = Bytes.create length in
        really_input ic payload_buf 0 length;
        last_good := pos_in ic;
        acc := (Bytes.to_string hash_buf, record_off) :: !acc
      done with End_of_file -> ());
      (!acc, !last_good)
    with e -> close_in_noerr ic; raise e)
  in
  close_in ic;
  (entries, next_off)

(* ================================================================== *)
(* Segment index file                                                   *)
(* ================================================================== *)

(** Write a segment index file for a sealed segment.
    [sorted] must be sorted ascending by hash. *)
let write_segment_index dir seg_id (sorted : (bytes * int) array) bloom =
  let path = seg_idx_path dir seg_id in
  let oc = open_out_bin path in
  (try
    output_string oc index_magic;
    write_u32_le oc seg_id;
    write_u16_le oc format_version;
    write_u16_le oc 0;
    write_u32_le oc (Array.length sorted);
    output_bytes oc bloom;
    Array.iter (fun (h, off) ->
      output_bytes oc h;
      write_u64_le oc off
    ) sorted;
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e)

(** Read a segment index file.
    Returns [(bloom, entries)] where [entries] is sorted ascending by hash. *)
let read_segment_index dir seg_id =
  let path = seg_idx_path dir seg_id in
  let data = read_file path in
  if Bytes.length data < idx_header_size then
    failwith (Printf.sprintf "Node_store: index too short for seg %d" seg_id);
  if Bytes.sub_string data 0 4 <> index_magic then
    failwith (Printf.sprintf "Node_store: bad index magic for seg %d" seg_id);
  let version = get_u16_le data 8 in
  if version <> format_version then
    failwith (Printf.sprintf "Node_store: unsupported index version %d in seg %d" version seg_id);
  let n_entries = get_u32_le data 12 in
  let bloom = Bytes.sub data 16 bloom_bytes in
  let entries = Array.init n_entries (fun i ->
    let off = idx_header_size + i * idx_entry_size in
    let h = Bytes.sub data off hash_size in
    let file_off = get_u64_le data (off + hash_size) in
    (h, file_off)
  ) in
  (bloom, entries)

(** Binary-search [entries] (sorted ascending by hash) for [target].
    Returns [Some file_offset] if found, [None] otherwise. *)
let index_lookup (entries : (bytes * int) array) target =
  let lo = ref 0 and hi = ref (Array.length entries - 1) in
  let result = ref None in
  while !lo <= !hi && !result = None do
    let mid = (!lo + !hi) / 2 in
    let (h, off) = entries.(mid) in
    let c = Bytes.compare h target in
    if c = 0 then result := Some off
    else if c < 0 then lo := mid + 1
    else hi := mid - 1
  done;
  !result

(* ================================================================== *)
(* Store state                                                          *)
(* ================================================================== *)

type sealed_seg = {
  ss_id        : int;
  ss_data_path : string;
  ss_bloom     : bytes;                (* bloom_bytes *)
  ss_index     : (bytes * int) array;  (* sorted ascending by hash *)
}

type t = {
  dir               : string;
  mutable root_hash : bytes;
  mutable sealed    : sealed_seg list;  (* oldest first *)
  mutable active_id : int;
  mutable active_oc : out_channel;      (* write channel for active .bin *)
  mutable active_off: int;              (* byte offset of next write *)
  active_idx        : (string, int) Hashtbl.t;  (* Bytes.to_string hash -> record_offset *)
  mutable node_count   : int;
  mutable payload_bytes: int;
}

(* ================================================================== *)
(* Segment sealing                                                      *)
(* ================================================================== *)

let do_seal t =
  flush t.active_oc;
  let entries = Hashtbl.fold (fun h_str off acc ->
    (Bytes.of_string h_str, off) :: acc
  ) t.active_idx [] in
  let sorted = Array.of_list entries in
  Array.sort (fun (a, _) (b, _) -> Bytes.compare a b) sorted;
  let bloom = bloom_of_hashes (Array.map fst sorted) in
  write_segment_index t.dir t.active_id sorted bloom;
  let new_sealed = {
    ss_id        = t.active_id;
    ss_data_path = seg_data_path t.dir t.active_id;
    ss_bloom     = bloom;
    ss_index     = sorted;
  } in
  t.sealed <- t.sealed @ [new_sealed];
  let new_id = t.active_id + 1 in
  let new_path = seg_data_path t.dir new_id in
  let new_oc = open_out_bin new_path in
  write_data_header new_oc new_id;
  flush new_oc;
  t.active_id <- new_id;
  t.active_oc <- new_oc;
  t.active_off <- data_header_size;
  Hashtbl.reset t.active_idx;
  t.node_count    <- 0;
  t.payload_bytes <- 0;
  let all_ids = List.map (fun s -> s.ss_id) t.sealed @ [new_id] in
  write_manifest t.dir t.root_hash all_ids

(* ================================================================== *)
(* Opening / creating                                                   *)
(* ================================================================== *)

(** [open_store dir] opens or creates a node store in directory [dir].
    [dir] must exist; raises [Sys_error] if it does not. *)
let open_store dir =
  let root_hash, seg_ids = read_manifest dir in
  match seg_ids with
  | [] ->
    (* Fresh store: create the first active segment. *)
    let active_id = 1 in
    let data_path = seg_data_path dir active_id in
    let oc = open_out_bin data_path in
    write_data_header oc active_id;
    flush oc;
    write_manifest dir root_hash [active_id];
    { dir; root_hash; sealed = []; active_id; active_oc = oc;
      active_off = data_header_size;
      active_idx = Hashtbl.create 256;
      node_count = 0; payload_bytes = 0 }
  | _ ->
    (* Resume: last seg_id is active, all others are sealed. *)
    let sealed_ids, active_id =
      let rev = List.rev seg_ids in
      (List.rev (List.tl rev), List.hd rev)
    in
    let sealed = List.map (fun sid ->
      let (bloom, index) = read_segment_index dir sid in
      { ss_id = sid; ss_data_path = seg_data_path dir sid;
        ss_bloom = bloom; ss_index = index }
    ) sealed_ids in
    let active_path = seg_data_path dir active_id in
    let (entries, next_off) = scan_data_file active_path in
    let active_idx = Hashtbl.create (max 256 (List.length entries * 2)) in
    List.iter (fun (h_str, off) -> Hashtbl.replace active_idx h_str off) entries;
    let node_count = Hashtbl.length active_idx in
    (* Reopen for writing, positioned at next_off. *)
    let oc = open_out_gen [Open_wronly; Open_binary] 0o644 active_path in
    seek_out oc next_off;
    { dir; root_hash; sealed; active_id; active_oc = oc;
      active_off = next_off; active_idx;
      node_count; payload_bytes = next_off - data_header_size }

(** [close_store t] flushes and closes the store. Does not seal the active segment. *)
let close_store t =
  flush t.active_oc;
  close_out t.active_oc

(* ================================================================== *)
(* Write path                                                           *)
(* ================================================================== *)

(** Check whether [hash] already exists in any sealed segment. *)
let in_sealed_segs t hash =
  List.exists (fun ss ->
    bloom_check ss.ss_bloom hash &&
    index_lookup ss.ss_index hash <> None
  ) (List.rev t.sealed)  (* newest-first: more likely to find recent nodes *)

(** Write [payload] to the store, returning its BLAKE3 hash.
    If a node with the same hash already exists, returns the existing hash. *)
let write t payload =
  let hash = Node_hash.digest payload in
  let key = Bytes.to_string hash in
  if Hashtbl.mem t.active_idx key then hash
  else if in_sealed_segs t hash then hash
  else begin
    let record_off = t.active_off in
    output_bytes t.active_oc hash;
    write_u32_le t.active_oc (Bytes.length payload);
    output_bytes t.active_oc payload;
    flush t.active_oc;
    let payload_len = Bytes.length payload in
    t.active_off   <- record_off + hash_size + 4 + payload_len;
    Hashtbl.replace t.active_idx key record_off;
    t.node_count   <- t.node_count + 1;
    t.payload_bytes <- t.payload_bytes + payload_len;
    if t.node_count >= seal_threshold_nodes
    || t.payload_bytes >= seal_threshold_bytes
    then do_seal t;
    hash
  end

(* ================================================================== *)
(* Read path                                                            *)
(* ================================================================== *)

(** Look up [hash] in the store, returning its payload.
    Raises [Not_found] if the hash is not present. *)
let lookup t hash =
  let key = Bytes.to_string hash in
  match Hashtbl.find_opt t.active_idx key with
  | Some off ->
    read_node_payload (seg_data_path t.dir t.active_id) off
  | None ->
    let rec search = function
      | [] -> raise Not_found
      | ss :: rest ->
        if not (bloom_check ss.ss_bloom hash) then search rest
        else
          match index_lookup ss.ss_index hash with
          | Some off -> read_node_payload ss.ss_data_path off
          | None -> search rest
    in
    search (List.rev t.sealed)  (* search newest-first *)

(* ================================================================== *)
(* Root hash management                                                 *)
(* ================================================================== *)

let set_root t hash =
  t.root_hash <- Bytes.copy hash;
  let all_ids = List.map (fun s -> s.ss_id) t.sealed @ [t.active_id] in
  write_manifest t.dir t.root_hash all_ids

let root_hash t = Bytes.copy t.root_hash

(* ================================================================== *)
(* Adapters for Node_encoding / Node_decoding                          *)
(* ================================================================== *)

(** Return a [Node_encoding.store] that persists nodes in [t]. *)
let as_encoding_store t : Node_encoding.store =
  { Node_encoding.store = write t }

(** Return a [Node_decoding.lookup] that reads nodes from [t]. *)
let as_decoding_lookup t : Node_decoding.lookup =
  lookup t
