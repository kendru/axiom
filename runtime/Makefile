# Fallback build using clang + wasm-ld (no Zig required).
# Primary build: zig build -Doptimize=ReleaseSmall

CLANG   ?= clang
WASM_LD ?= wasm-ld

.PHONY: all clean

all: runtime.wasm

runtime.wasm: src/runtime.c
	$(CLANG) --target=wasm32 -O2 -nostdlib -c $< -o /tmp/_axiom_runtime.o
	$(WASM_LD) --no-entry --allow-undefined \
		--export=_start --export=get_exit_code \
		--export=__panic --export=__div_check \
		/tmp/_axiom_runtime.o -o $@
	rm -f /tmp/_axiom_runtime.o

clean:
	rm -f runtime.wasm
