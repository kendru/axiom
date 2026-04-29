/* Minimal Axiom runtime — C fallback for environments without Zig.
 *
 * Build:
 *   clang --target=wasm32 -O2 -nostdlib -c src/runtime.c -o /tmp/runtime.o
 *   wasm-ld --no-entry --allow-undefined \
 *           --export=_start --export=get_exit_code \
 *           --export=__panic --export=__div_check \
 *           /tmp/runtime.o -o runtime.wasm
 */

/* main() is imported from the Axiom-compiled application at link time. */
__attribute__((import_module("axiom_program"), import_name("main")))
extern int axiom_main(void);

static int _exit_code = 0;

__attribute__((export_name("_start")))
void start(void) {
    _exit_code = axiom_main();
}

__attribute__((export_name("get_exit_code")))
int get_exit_code(void) {
    return _exit_code;
}

__attribute__((export_name("__panic"), noreturn))
void axiom_panic(void) {
    __builtin_unreachable();
}

__attribute__((export_name("__div_check")))
void axiom_div_check(int divisor) {
    if (divisor == 0) axiom_panic();
}
