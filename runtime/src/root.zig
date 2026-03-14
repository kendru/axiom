// Copyright 2026 Andrew Meredith
// SPDX-License-Identifier: Apache-2.0

//! Axiom runtime library root.
//! See spec/axiom-overview-draft.md §9 for the compilation strategy.

pub const gc      = @import("gc.zig");
pub const effects = @import("effects.zig");
pub const stdlib  = struct {
    pub const core = @import("stdlib/core.zig");
    pub const io   = @import("stdlib/io.zig");
};

test {
    // Pull all module tests into the test binary.
    _ = gc;
    _ = effects;
    _ = stdlib.core;
    _ = stdlib.io;
}
