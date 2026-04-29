const std = @import("std");

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});

    const target = b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .freestanding,
    });

    const exe = b.addExecutable(.{
        .name = "runtime",
        .root_source_file = b.path("src/runtime.zig"),
        .target = target,
        .optimize = optimize,
    });

    // We define our own `_start`; suppress the compiler-generated entry.
    exe.entry = .disabled;

    // Export every `pub export` symbol.
    exe.rdynamic = true;

    // Output goes to zig-out/bin/runtime.wasm.
    b.installArtifact(exe);
}
