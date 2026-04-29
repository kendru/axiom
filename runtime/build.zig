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

    // Do not emit a default `_start`; we define our own.
    exe.entry = .disabled;

    // Export all `pub export` symbols.
    exe.rdynamic = true;

    // Allow the `main` import from "axiom_program" to be unresolved at
    // library-build time; it is resolved when the host links this module
    // with a compiled Axiom application.
    exe.import_memory = false;

    b.installArtifact(exe);

    // zig build produces zig-out/bin/runtime.wasm
    // Copy it to runtime.wasm in the runtime/ directory for convenience.
    const install_step = b.addInstallFile(
        exe.getEmittedBin(),
        "../runtime.wasm",
    );
    b.getInstallStep().dependOn(&install_step.step);
}
