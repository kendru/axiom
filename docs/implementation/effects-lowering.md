# Effects Lowering: CPS via Evidence-Passing Style

## Overview

Algebraic effects in Axiom are compiled using *evidence-passing style* (EPS),
a variant of continuation-passing that avoids heap-allocating continuations for
the common single-shot case.  The elaboration pass (`lib/elaboration.ml`)
produces an intermediate representation where:

- Every effectful function receives an extra *evidence parameter* per effect it
  uses (`__ev_EffectName : i32`).
- Every `perform E.op(args)` node is annotated with the evidence variable
  (`ev_var`) that names the handler to dispatch through.
- Every `handle e with { ... }` node carries a per-effect `ev_param` record
  that the code generator uses to wire up dispatch.

The code generator (`lib/codegen.ml`) then lowers this elaborated IR directly
to WebAssembly instructions.

## Evidence Struct Layout

An evidence value is a pointer into linear memory.  For an effect with *n*
operations the struct is `n × 8` bytes:

```
offset  size  field
──────  ────  ─────────────────────────────────────────────────
op₀×8    4    function-table slot for op₀ handler
op₀×8+4  4    closure environment pointer for op₀ handler
op₁×8    4    function-table slot for op₁ handler
op₁×8+4  4    closure environment pointer for op₁ handler
…
```

`EPerform` lowers to:

```wasm
local.get $__ev_E          ;; evidence pointer
i32.load offset=(op_idx*8+4)   ;; env_ptr
<arg₀> <arg₁> …
local.get $__ev_E
i32.load offset=(op_idx*8)     ;; fn_table_slot
call_indirect (type handler_sig)
```

The handler function signature is `(i32, arg₀_ty, …) → ret_ty`, where the
leading `i32` is the closure environment pointer.

## Closure Capture

Handler bodies in Axiom source can refer to variables from the enclosing
function scope.  In WebAssembly there are no closures, so the code generator:

1. **Computes free variables** in each handler body via `free_vars_in_eexpr`,
   collecting outer locals that the body references (including evidence
   variables for inner `perform` calls).

2. **Allocates a heap closure environment** at the `handle` site:
   `__alloc(n_captured × 4)` bytes.  Captured values are stored into this
   block in order.

3. **Stores the env pointer** in `ev_struct[op_idx*8+4]` alongside the
   function-table slot.

4. **Emits a top-level handler function** (appended to the module's function
   list at compile time) whose first parameter is `env_ptr : i32`.  The
   prologue of this function loads each captured value from `env_ptr` into a
   fresh local before executing the handler body.

## Abort / Throw Semantics

Effects whose operation returns type `Nothing` (e.g., `Throw`) model
short-circuiting control flow: `resume` is never called, so the handler must
bypass the return handler and jump to the `handle` result.

The abort mechanism uses a small *throw control block* (8 bytes) on the heap:

```
offset  size  field
──────  ────  ───────────────────────
0        4    abort_flag  (0 = normal, 1 = aborted)
4        4    abort_value (result to propagate)
```

At the `handle` site:

1. A throw control block is allocated and `abort_flag` set to 0.
2. Its pointer is stored as the first word in the closure environment for every
   abort-capable op handler.
3. The abort op handler, instead of returning its value directly, stores it
   into `throw_ctrl[4]`, sets `throw_ctrl[0] = 1`, and returns 0 (a dummy).
4. After the handled expression completes, the generated code checks
   `throw_ctrl[0]`: if 1, it loads `throw_ctrl[4]` and skips the return
   handler; otherwise it calls the return handler normally.

This scheme avoids non-local jumps or `br_table` across function boundaries.

## Handle Lowering Sketch

```
handle <expr> with {
  E {
    op(x) => <body>          (* may capture outer vars *)
    return v => <ret_body>
  }
}
```

Lowers to roughly (pseudocode):

```
throw_ctrl = __alloc(8)         (* only if abort effect *)
throw_ctrl[0] = 0

env = __alloc(n_captured * 4)
env[0] = throw_ctrl             (* if abort *)
env[1] = captured_var_1
…
ev_struct = __alloc(n_ops * 8)
ev_struct[op_idx * 8]     = fn_table_slot(op_handler_function)
ev_struct[op_idx * 8 + 4] = env

result = <expr with __ev_E = ev_struct>

if throw_ctrl[0] = 1 {
  throw_ctrl[4]
} else {
  <ret_body with v = result>
}
```

## `Nothing` as Bottom Type

The typechecker treats `Nothing` as a bottom type: `unify(Nothing, T)` succeeds
for any `T`.  This allows `perform Throw.throw(e)` (which returns `Nothing`)
to appear in any expression position regardless of the surrounding expected type.

## Pure Functions

Pure functions are unaffected.  The elaboration pass adds no evidence
parameters to functions with `! pure` effect annotation, and the code generator
emits no evidence-struct setup when there are no `EHandle` nodes.

## Limitations

- **Single-shot continuations only.**  `resume` may be called exactly once; the
  generated code does not capture a reified continuation value.
- **`State` effect is not truly stateful** in the current model.  The handler
  always receives the original captured value; proper mutable state requires
  either a mutable cell or rewriting the handler calling convention.
- **Recursive effectful tail calls** inside a handler body are not optimised
  as tail calls in the current implementation.
