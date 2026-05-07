const std = @import("std");

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});

    // Target wasm32-wasi so that axm_print / axm_read_line can use WASI
    // fd_write / fd_read via std.io.  The Axiom-compiled module imports the
    // exports of this module as the "axm" namespace.
    const target = b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .wasi,
    });

    const lib = b.addExecutable(.{
        .name = "runtime",
        .root_source_file = b.path("src/runtime.zig"),
        .target = target,
        .optimize = optimize,
    });

    // No Zig-generated entry point; this is a library / side-module.
    lib.entry = .disabled;

    // Export every `pub export` symbol so the host / linker can see them.
    lib.rdynamic = true;

    // Output goes to zig-out/bin/runtime.wasm.
    b.installArtifact(lib);
}
