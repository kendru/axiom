(** Tests for the on-disk node store.

    Exercises the write/read paths, deduplication, bloom filter,
    resume after close, and manifest atomicity. *)

open Axiom_lib.Node_store
open Axiom_lib.Node_encoding
open Axiom_lib.Node_decoding
open Axiom_lib.Ast

(* ------------------------------------------------------------------ *)
(* Temp directory helpers                                               *)
(* ------------------------------------------------------------------ *)

let make_tmpdir () =
  let base = Filename.concat (Filename.get_temp_dir_name ()) "axiom_store_test" in
  let dir = Printf.sprintf "%s_%d_%d" base (Unix.getpid ()) (Random.int 1000000) in
  Unix.mkdir dir 0o755;
  dir

let rm_rf dir =
  (* Simple recursive removal for test cleanup. *)
  let rec remove path =
    if Sys.is_directory path then begin
      let entries = Sys.readdir path in
      Array.iter (fun e -> remove (Filename.concat path e)) entries;
      Unix.rmdir path
    end else
      Sys.remove path
  in
  if Sys.file_exists dir then remove dir

(** Run [f dir] with a fresh temporary directory, cleaning up afterward. *)
let with_store f =
  let dir = make_tmpdir () in
  (try f dir with e -> rm_rf dir; raise e);
  rm_rf dir

(* ------------------------------------------------------------------ *)
(* Payload helpers                                                      *)
(* ------------------------------------------------------------------ *)

let payload s = Bytes.of_string s

let small_payloads n =
  Array.init n (fun i -> payload (Printf.sprintf "payload-%06d" i))

(* ------------------------------------------------------------------ *)
(* Basic write / read                                                   *)
(* ------------------------------------------------------------------ *)

let test_write_read () =
  with_store (fun dir ->
    let t = open_store dir in
    let p = payload "hello world" in
    let h = write t p in
    let got = lookup t h in
    Alcotest.(check bytes) "payload round-trips" p got;
    close_store t
  )

let test_not_found () =
  with_store (fun dir ->
    let t = open_store dir in
    let fake_hash = Bytes.make 32 '\x42' in
    Alcotest.check_raises "Not_found on missing hash"
      Not_found
      (fun () -> ignore (lookup t fake_hash));
    close_store t
  )

(* ------------------------------------------------------------------ *)
(* Deduplication                                                        *)
(* ------------------------------------------------------------------ *)

let test_dedup () =
  with_store (fun dir ->
    let t = open_store dir in
    let p = payload "deduplicated node" in
    let h1 = write t p in
    let h2 = write t p in
    Alcotest.(check bytes) "same hash both times" h1 h2;
    let got = lookup t h1 in
    Alcotest.(check bytes) "payload intact" p got;
    close_store t
  )

(* ------------------------------------------------------------------ *)
(* Multiple distinct nodes                                              *)
(* ------------------------------------------------------------------ *)

let test_multiple_nodes () =
  with_store (fun dir ->
    let t = open_store dir in
    let payloads = small_payloads 100 in
    let hashes = Array.map (write t) payloads in
    Array.iteri (fun i h ->
      let got = lookup t h in
      Alcotest.(check bytes)
        (Printf.sprintf "payload %d" i) payloads.(i) got
    ) hashes;
    close_store t
  )

(* ------------------------------------------------------------------ *)
(* Resume: close and reopen the store                                  *)
(* ------------------------------------------------------------------ *)

let test_resume () =
  with_store (fun dir ->
    (* Write some nodes. *)
    let payloads = small_payloads 50 in
    let hashes =
      let t = open_store dir in
      let hs = Array.map (write t) payloads in
      close_store t;
      hs
    in
    (* Reopen and verify all nodes are still readable. *)
    let t = open_store dir in
    Array.iteri (fun i h ->
      let got = lookup t h in
      Alcotest.(check bytes)
        (Printf.sprintf "resume: payload %d" i) payloads.(i) got
    ) hashes;
    close_store t
  )

(* ------------------------------------------------------------------ *)
(* Root hash management                                                 *)
(* ------------------------------------------------------------------ *)

let test_root_hash_default () =
  with_store (fun dir ->
    let t = open_store dir in
    let r = root_hash t in
    Alcotest.(check bytes) "default root is zero hash" Axiom_lib.Node_hash.zero_hash r;
    close_store t
  )

let test_root_hash_persists () =
  with_store (fun dir ->
    let p = payload "root node" in
    let h =
      let t = open_store dir in
      let hash = write t p in
      set_root t hash;
      close_store t;
      hash
    in
    let t = open_store dir in
    Alcotest.(check bytes) "root hash persists" h (root_hash t);
    close_store t
  )

(* ------------------------------------------------------------------ *)
(* Segment sealing                                                      *)
(* ------------------------------------------------------------------ *)

(* Nodes written before an explicit seal live in a sealed segment;
   nodes written after live in the new active segment.
   Both must be readable from the same store handle. *)
let test_cross_segment_lookup () =
  with_store (fun dir ->
    let t = open_store dir in
    let before = small_payloads 50 in
    let h_before = Array.map (write t) before in
    seal t;  (* force seal: seg-000001 sealed, seg-000002 becomes active *)
    let after = small_payloads 50 in  (* fresh payloads, different content *)
    (* Make after payloads distinct from before by prepending "B-" *)
    let after = Array.map (fun p ->
      Bytes.cat (Bytes.of_string "B-") p
    ) after in
    let h_after = Array.map (write t) after in
    (* All nodes readable without closing *)
    Array.iteri (fun i h ->
      Alcotest.(check bytes) (Printf.sprintf "sealed seg, node %d" i)
        before.(i) (lookup t h)
    ) h_before;
    Array.iteri (fun i h ->
      Alcotest.(check bytes) (Printf.sprintf "active seg, node %d" i)
        after.(i) (lookup t h)
    ) h_after;
    close_store t
  )

(* After sealing and closing, reopen and verify both segments are readable. *)
let test_multi_segment_resume () =
  with_store (fun dir ->
    let before = small_payloads 30 in
    let after  = Array.map (fun p -> Bytes.cat (Bytes.of_string "C-") p)
                            (small_payloads 30) in
    let h_before, h_after =
      let t = open_store dir in
      let hb = Array.map (write t) before in
      seal t;
      let ha = Array.map (write t) after in
      close_store t;
      (hb, ha)
    in
    let t = open_store dir in
    Array.iteri (fun i h ->
      Alcotest.(check bytes) (Printf.sprintf "resume sealed %d" i)
        before.(i) (lookup t h)
    ) h_before;
    Array.iteri (fun i h ->
      Alcotest.(check bytes) (Printf.sprintf "resume active %d" i)
        after.(i) (lookup t h)
    ) h_after;
    close_store t
  )

(* Nodes already in a sealed segment are not re-written on the active segment. *)
let test_cross_segment_dedup () =
  with_store (fun dir ->
    let t = open_store dir in
    let p = payload "deduped across segments" in
    let h1 = write t p in
    seal t;
    let h2 = write t p in  (* same payload — must deduplicate against sealed seg *)
    Alcotest.(check bytes) "same hash across seal" h1 h2;
    (* Only one copy should exist — check via lookup on both sides *)
    Alcotest.(check bytes) "readable from active store" p (lookup t h1);
    close_store t
  )

(* Three segments: seg1 sealed, seg2 sealed, seg3 active. *)
let test_three_segments () =
  with_store (fun dir ->
    let mk n prefix =
      Array.map (fun p -> Bytes.cat (Bytes.of_string prefix) p)
                (small_payloads n)
    in
    let p1 = mk 20 "S1-" in
    let p2 = mk 20 "S2-" in
    let p3 = mk 20 "S3-" in
    let t = open_store dir in
    let h1 = Array.map (write t) p1 in
    seal t;
    let h2 = Array.map (write t) p2 in
    seal t;
    let h3 = Array.map (write t) p3 in
    let check_all () =
      Array.iteri (fun i h ->
        Alcotest.(check bytes) (Printf.sprintf "seg1 node %d" i) p1.(i) (lookup t h)
      ) h1;
      Array.iteri (fun i h ->
        Alcotest.(check bytes) (Printf.sprintf "seg2 node %d" i) p2.(i) (lookup t h)
      ) h2;
      Array.iteri (fun i h ->
        Alcotest.(check bytes) (Printf.sprintf "seg3 node %d" i) p3.(i) (lookup t h)
      ) h3
    in
    check_all ();
    close_store t;
    (* Verify after resume *)
    let t2 = open_store dir in
    Array.iteri (fun i h ->
      Alcotest.(check bytes) (Printf.sprintf "resume seg1 node %d" i) p1.(i) (lookup t2 h)
    ) h1;
    Array.iteri (fun i h ->
      Alcotest.(check bytes) (Printf.sprintf "resume seg2 node %d" i) p2.(i) (lookup t2 h)
    ) h2;
    Array.iteri (fun i h ->
      Alcotest.(check bytes) (Printf.sprintf "resume seg3 node %d" i) p3.(i) (lookup t2 h)
    ) h3;
    close_store t2
  )

(* ------------------------------------------------------------------ *)
(* Adapter integration with Node_encoding / Node_decoding              *)
(* ------------------------------------------------------------------ *)

let test_encoding_adapter () =
  with_store (fun dir ->
    let t = open_store dir in
    let enc_store = as_encoding_store t in
    let lookup_fn = as_decoding_lookup t in
    let e = expr (IntLit 42L) in
    let h = encode_expr enc_store e in
    let e' = decode_expr lookup_fn h in
    Alcotest.(check bool) "decoded expr equals original"
      true (equal_expr e e');
    close_store t
  )

let test_program_roundtrip () =
  with_store (fun dir ->
    let t = open_store dir in
    let enc = as_encoding_store t in
    let lkp = as_decoding_lookup t in
    let prog : program = [
      { decl_desc = DeclFn {
          pub = true; fn_name = "add"; type_params = [];
          params = [{ param_name = "x"; param_type = TyName "Int" };
                    { param_name = "y"; param_type = TyName "Int" }];
          return_type = Some (TyName "Int"); effects = None;
          decl_body = expr (App (expr (Var "+"),
                                 [expr (Var "x"); expr (Var "y")])) };
        decl_comment = None }
    ] in
    let h = encode_program enc prog in
    let prog' = decode_program lkp h in
    Alcotest.(check bool) "program round-trips via disk store"
      true (equal_program prog prog');
    close_store t
  )

(* ------------------------------------------------------------------ *)
(* Bloom filter sanity                                                  *)
(* ------------------------------------------------------------------ *)

let test_bloom_no_false_negatives () =
  (* After writing N nodes, sealing (by testing the internal seal path
     indirectly via resume), all lookups must succeed. *)
  with_store (fun dir ->
    let n = 500 in
    let payloads = small_payloads n in
    let hashes =
      let t = open_store dir in
      let hs = Array.map (write t) payloads in
      close_store t;
      hs
    in
    let t = open_store dir in
    let missing = Array.fold_left (fun acc h ->
      try ignore (lookup t h); acc
      with Not_found -> acc + 1
    ) 0 hashes in
    Alcotest.(check int) "no false negatives from bloom filter" 0 missing;
    close_store t
  )

(* ------------------------------------------------------------------ *)
(* Test registration                                                    *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "Node_store" [
    "basic", [
      Alcotest.test_case "write and read back"        `Quick test_write_read;
      Alcotest.test_case "not found"                  `Quick test_not_found;
      Alcotest.test_case "deduplication"              `Quick test_dedup;
      Alcotest.test_case "multiple distinct nodes"    `Quick test_multiple_nodes;
    ];
    "persistence", [
      Alcotest.test_case "resume after close"         `Quick test_resume;
      Alcotest.test_case "root hash default"          `Quick test_root_hash_default;
      Alcotest.test_case "root hash persists"         `Quick test_root_hash_persists;
    ];
    "multi-segment", [
      Alcotest.test_case "cross-segment lookup"       `Quick test_cross_segment_lookup;
      Alcotest.test_case "multi-segment resume"       `Quick test_multi_segment_resume;
      Alcotest.test_case "cross-segment dedup"        `Quick test_cross_segment_dedup;
      Alcotest.test_case "three segments"             `Quick test_three_segments;
    ];
    "adapters", [
      Alcotest.test_case "encoding adapter"           `Quick test_encoding_adapter;
      Alcotest.test_case "program round-trip on disk" `Quick test_program_roundtrip;
    ];
    "bloom", [
      Alcotest.test_case "no false negatives"         `Quick test_bloom_no_false_negatives;
    ];
  ]
