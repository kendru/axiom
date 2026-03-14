(* Copyright 2026 Andrew Meredith
   SPDX-License-Identifier: Apache-2.0 *)

(** Binary IR — the canonical, fully-elaborated representation.
    See spec/axiom-overview-draft.md §2.1 for design notes.

    The IR is a content-addressed, tagged tree structure:
    - Every node is prefixed with a 1-byte type tag.
    - Identifiers are stored as content-addressed hashes, not strings.
    - An index table maps hashes to byte offsets for random access.
    - The format is versioned; the first 4 bytes are a magic number + version.
*)

(** A content-addressed node identifier (32-byte SHA-256 hash). *)
type hash = bytes

(** Node tags (1 byte each). Values assigned during implementation. *)
type tag = int

(* TODO: define tag constants for each node kind:
     TAG_VAR, TAG_APP, TAG_LET, TAG_LETREC, TAG_MATCH, TAG_HANDLE,
     TAG_PERFORM, TAG_DO, TAG_IF, TAG_RECORD, TAG_FIELD, TAG_LIT_INT,
     TAG_LIT_FLOAT, TAG_LIT_STRING, TAG_LIT_BOOL, TAG_LIT_UNIT,
     TAG_CTOR, TAG_TYPE_VAR, TAG_TYPE_APP, TAG_TYPE_FUN, ... *)

(* TODO: define ir_node type — mirrors Ast but names are hashes, types carried *)
(* TODO: implement hash_of_bytes : bytes -> hash  (SHA-256) *)
(* TODO: implement serialize   : ir_node -> bytes *)
(* TODO: implement deserialize : bytes -> (ir_node, string) result *)
(* TODO: implement node_hash   : ir_node -> hash  (memoized) *)
