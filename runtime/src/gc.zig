// Copyright 2026 Andrew Meredith
// SPDX-License-Identifier: Apache-2.0

//! Axiom garbage collector.
//! Strategy: incremental mark-and-sweep with a nursery generation.
//! See spec/axiom-overview-draft.md §9.4 for design notes.
//!
//! Implementation plan:
//!   - Nursery: bump allocator over a fixed-size arena; evacuated on minor GC.
//!   - Old generation: free-list allocator; swept on major GC.
//!   - Mark phase: tri-color marking (white / gray / black).
//!   - Write barrier: card table for inter-generational pointers.
//!
//! TODO: implement nursery bump allocator
//! TODO: implement mark phase (tri-color)
//! TODO: implement sweep phase
//! TODO: implement nursery evacuation / promotion
//! TODO: implement card-table write barriers

const std = @import("std");

/// Opaque pointer to a GC-managed heap object.
pub const GcPtr = *anyopaque;

/// Allocate `size` bytes on the GC heap.
/// Returns null on allocation failure; caller must handle OOM.
pub fn alloc(size: usize) ?GcPtr {
    _ = size;
    // TODO: implement — allocate from nursery; trigger minor GC if full
    return null;
}

/// Trigger a GC collection cycle.
/// Called automatically by alloc() when the nursery is exhausted.
pub fn collect() void {
    // TODO: implement
}

test "gc: alloc returns null (stub)" {
    try std.testing.expectEqual(@as(?GcPtr, null), alloc(8));
}
