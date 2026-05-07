/// Axiom runtime for wasm32-wasi.
///
/// Provides allocation primitives and I/O operations that Axiom-compiled
/// modules import from the "axm" module namespace.
///
/// Memory layout (shared with the Axiom-compiled module):
///   Page 0 (bytes 0–65535): zero-page reserved area + static string data
///     Bytes 0–63:     reserved zero-page (never allocated)
///     Bytes 64–65535: static string literals placed by data segments
///   Page 1+ (bytes 65536+): heap managed by the bump allocator
///
/// The Axiom module places string literals in the first page via active data
/// segments; the runtime allocator starts at page 1 so there is no overlap.

const std = @import("std");

// ── Linear memory ──────────────────────────────────────────────────────────
// Exported as "memory" so that Axiom modules can import it.

export var memory: [*]u8 = undefined; // address zero; symbolic only in Zig

// ── Bump allocator state ────────────────────────────────────────────────────

/// Next free byte; initialised to the start of page 1.
var heap_ptr: u32 = 65536;

/// Allocate [n] bytes from the heap (4-byte aligned).
/// Grows linear memory by one page at a time if needed.
/// Returns the byte offset of the allocated block.
export fn axm_alloc(n: u32) u32 {
    const aligned = (n + 3) & ~@as(u32, 3);
    const ptr = heap_ptr;
    heap_ptr += aligned;
    // Grow memory if the new top of heap exceeds available pages.
    while (heap_ptr > @wasmMemorySize(0) * 65536) {
        const grew = @wasmMemoryGrow(0, 1);
        if (grew == std.math.maxInt(usize)) @trap(); // out of memory
    }
    return ptr;
}

/// Allocate a length-prefixed copy of the byte sequence [ptr..ptr+len].
/// Layout: i32 length at offset 0, then [len] bytes of content.
/// Returns the pointer to the new allocation.
export fn axm_alloc_string(ptr: u32, len: u32) u32 {
    const dest = axm_alloc(4 + len);
    const mem_bytes = @as([*]u8, @ptrFromInt(0));
    // Write length prefix (little-endian i32).
    mem_bytes[dest + 0] = @truncate(len & 0xFF);
    mem_bytes[dest + 1] = @truncate((len >> 8) & 0xFF);
    mem_bytes[dest + 2] = @truncate((len >> 16) & 0xFF);
    mem_bytes[dest + 3] = @truncate((len >> 24) & 0xFF);
    // Copy content bytes.
    var i: u32 = 0;
    while (i < len) : (i += 1) {
        mem_bytes[dest + 4 + i] = mem_bytes[ptr + i];
    }
    return dest;
}

/// Allocate [field_count * 4] bytes for a record.
export fn axm_alloc_record(field_count: u32) u32 {
    return axm_alloc(field_count * 4);
}

/// Allocate [(1 + payload_count) * 4] bytes for an ADT value and write
/// [tag] into the first word.
export fn axm_alloc_ctor(tag: u32, payload_count: u32) u32 {
    const ptr = axm_alloc((1 + payload_count) * 4);
    const mem_words = @as([*]u32, @ptrFromInt(0));
    mem_words[ptr / 4] = tag;
    return ptr;
}

// ── GC stubs ────────────────────────────────────────────────────────────────
// No GC yet; these reserve the API for future implementation.

export fn axm_gc_collect() void {}
export fn axm_gc_root(_ptr: u32) void {}
export fn axm_gc_unroot(_ptr: u32) void {}

// ── I/O ─────────────────────────────────────────────────────────────────────

/// Write [len] bytes from linear memory at [ptr] to stdout.
/// Returns 0 on success, -1 on error.
export fn axm_print(ptr: u32, len: u32) i32 {
    const mem_bytes = @as([*]const u8, @ptrFromInt(0));
    const slice = mem_bytes[ptr .. ptr + len];
    std.io.getStdOut().writer().writeAll(slice) catch return -1;
    return 0;
}

/// Read one line from stdin (up to 4096 bytes, excluding the newline).
/// Allocates a length-prefixed string in the heap and returns its pointer.
/// Returns 0 if stdin is closed or on error.
export fn axm_read_line() u32 {
    var buf: [4096]u8 = undefined;
    const maybe_line = std.io.getStdIn().reader().readUntilDelimiter(&buf, '\n');
    const line = maybe_line catch return 0;
    // Strip trailing CR for Windows-style line endings.
    const trimmed = if (line.len > 0 and line[line.len - 1] == '\r')
        line[0 .. line.len - 1]
    else
        line;
    return axm_alloc_string(
        @intFromPtr(trimmed.ptr) - @intFromPtr(@as(*u8, @ptrFromInt(0))),
        @intCast(trimmed.len),
    );
}

// ── Trap / division guard ────────────────────────────────────────────────────
// Retained from the original runtime for compatibility.

/// Unconditional trap.
pub export fn __panic() noreturn {
    @trap();
}

/// Trap if [divisor] is zero.
pub export fn __div_check(divisor: i32) void {
    if (divisor == 0) __panic();
}
