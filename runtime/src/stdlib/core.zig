// Copyright 2026 Andrew Meredith
// SPDX-License-Identifier: Apache-2.0

//! Core standard library types.
//! See spec/axiom-overview-draft.md §10 for type and operation specifications.
//!
//! Types to implement:
//!   Option(T)    §10.2  — Some(T) | None
//!   Result(A, E) §10.2  — Ok(A)   | Err(E)
//!   List(A)      §10.2  — singly-linked, GC-managed
//!                          ops: cons, head, tail, map, fold, filter, length
//!   Map(K, V)    §10.2  — persistent hash-array mapped trie (HAMT)
//!                          ops: lookup, insert, remove, keys, values, merge
//!   Set(A)       §10.2  — persistent hash set (HAMT leaves)
//!                          ops: member, insert, remove, union, intersection
//!
//! TODO: implement Option(T)
//! TODO: implement Result(A, E)
//! TODO: implement List(A)
//! TODO: implement Map(K, V)
//! TODO: implement Set(A)

test "core: placeholder compiles" {}
