// Copyright 2026 Andrew Meredith
// SPDX-License-Identifier: Apache-2.0

//! Effect handler machinery for the Axiom runtime.
//!
//! Effects compile to evidence passing: each handler installs a struct of
//! function pointers ("evidence") onto a per-fiber handler stack.
//! Performing an effect loads the innermost matching evidence and calls
//! the appropriate function pointer.
//!
//! See spec/axiom-overview-draft.md §9.2 for the compilation strategy.
//!
//! Implementation plan:
//!   - Each effect declaration generates a concrete Evidence struct type
//!     with one function-pointer field per operation.
//!   - The handler stack is a linked list of Evidence pointers, one list
//!     per fiber (stored in fiber-local storage).
//!   - 'resume' is a linear continuation: called exactly once per perform.
//!   - The Async effect (§5.7) requires a fiber scheduler; deferred.
//!
//! TODO: define EffectEvidence (struct of function pointers, one per op)
//! TODO: implement handler stack push / pop
//! TODO: implement resume continuation (assert linear: called exactly once)
//! TODO: define Fiber type for Async effect (§5.7)

const std = @import("std");

/// Placeholder for the per-effect evidence type.
/// The compiler generates a concrete version of this struct for each effect,
/// with one typed function pointer per operation.
pub const Evidence = *anyopaque;

test "effects: placeholder compiles" {
    // TODO: push a handler, perform an op, verify handler was called
    const _e: ?Evidence = null;
    try std.testing.expectEqual(@as(?Evidence, null), _e);
}
