/// Minimal Axiom runtime for wasm32-freestanding.
///
/// Provides:
///   __panic        — trap on unrecoverable error
///   __div_check    — trap if divisor is zero (future integer-division guard)
///   _start         — entry point; calls the Axiom-emitted `main` and stores
///                    the return value so the host can read it with
///                    `get_exit_code()`.
///
/// `main` is imported from the `"axiom_program"` module, which is the
/// Axiom-compiled application.  When building a linked binary, the host
/// instantiates the Axiom module first and then instantiates this runtime
/// with `{ axiom_program: { main: axiomInstance.exports.main } }`.

/// Imported from the Axiom-compiled application module.
extern "axiom_program" fn main() i32;

/// Holds the return value written by `_start`.
var exit_code: i32 = 0;

/// Called by the host instead of invoking `main` directly.
/// Stores the result so the host can read it via `get_exit_code`.
pub export fn _start() void {
    exit_code = main();
}

/// Returns the value produced by the last `_start` call.
pub export fn get_exit_code() i32 {
    return exit_code;
}

/// Trap handler for unrecoverable errors (unreachable code, assertion
/// failures, etc.).  Uses the WASM `unreachable` instruction so the host
/// receives a trap rather than undefined behaviour.
pub export fn __panic() noreturn {
    @trap();
}

/// Division guard.  Called by the code generator before integer division
/// when safety checks are enabled.  Traps if `divisor` is zero.
pub export fn __div_check(divisor: i32) void {
    if (divisor == 0) __panic();
}
