// Copyright 2026 Andrew Meredith
// SPDX-License-Identifier: Apache-2.0

const std = @import("std");

pub fn build(b: *std.Build) void {
    const target   = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Static runtime library
    const lib = b.addStaticLibrary(.{
        .name             = "axiom_runtime",
        .root_source_file = b.path("src/root.zig"),
        .target           = target,
        .optimize         = optimize,
    });
    b.installArtifact(lib);

    // Unit tests
    const tests = b.addTest(.{
        .root_source_file = b.path("src/root.zig"),
        .target           = target,
        .optimize         = optimize,
    });
    const run_tests = b.addRunArtifact(tests);
    const test_step = b.step("test", "Run runtime unit tests");
    test_step.dependOn(&run_tests.step);
}
