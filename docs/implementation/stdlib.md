# Standard Library — Prelude

Axiom ships with a built-in prelude that is automatically imported into every
module unless `--no-prelude` is passed to the compiler.

## Primitive / compiler split

Not everything in the prelude is implemented in Axiom source.  Some names are
**compiler primitives** — the typechecker gives them a scheme but the
code-generator maps them directly to WebAssembly instructions or runtime
helpers.  The rest are **library functions** defined in `stdlib/prelude.axm`
and compiled the same way as user code.

### Compiler primitives (in `lib/typechecker.ml`)

| Name | Type | Notes |
|------|------|-------|
| `add` | `(Int, Int) -> Int` | WASM `i32.add` |
| `sub` | `(Int, Int) -> Int` | WASM `i32.sub` |
| `mul` | `(Int, Int) -> Int` | WASM `i32.mul` |
| `div` | `(Int, Int) -> Int ! {DivByZero}` | WASM `i32.div_s` |
| `neg` | `(Int) -> Int` | WASM `i32.sub 0 x` |
| `eq`, `neq` | `forall a. (a, a) -> Bool` | WASM `i32.eq` / `i32.ne` |
| `lt`, `gt`, `lte`, `gte`, `le`, `ge` | `forall a. (a, a) -> Bool` | WASM signed compare |
| `concat` | `(String, String) -> String` | Runtime helper |
| `string_length` | `(String) -> Int` | Runtime helper |
| `string_eq` | `(String, String) -> Bool` | Runtime helper |
| `string_head` | `(String) -> Int` | Runtime helper |
| `max` | `forall a. (a, a) -> a` | Runtime helper |
| `nil`, `Nil` | `forall a. List<a>` | ADT constructor |
| `cons`, `Cons` | `forall a. (a, List<a>) -> List<a>` | ADT constructor |
| `none`, `None` | `forall a. Option<a>` | ADT constructor |
| `some`, `Some` | `forall a. (a) -> Option<a>` | ADT constructor |
| `ok`, `Ok` | `forall a e. (a) -> Result<a, e>` | ADT constructor |
| `err`, `Err` | `forall a e. (e) -> Result<a, e>` | ADT constructor |

### Pre-declared effects

| Effect | Type params | Operation | Notes |
|--------|------------|-----------|-------|
| `DivByZero` | none | `throw: () -> Nothing` | Raised by `div` |
| `Throw` | `e` | `throw: (e) -> Nothing` | General error propagation |

### Library functions (in `stdlib/prelude.axm`)

These are compiled as regular Axiom functions and auto-imported before user
code.

**Types**

| Name | Kind | Constructors |
|------|------|--------------|
| `Pair<a, b>` | ADT | `Pair(a, b)` |

**List**

| Function | Type |
|----------|------|
| `length` | `forall a. (List<a>) -> Int` |
| `append` | `forall a. (List<a>, List<a>) -> List<a>` |
| `map` | `forall a b. ((a) -> b, List<a>) -> List<b>` |
| `filter` | `forall a. ((a) -> Bool, List<a>) -> List<a>` |
| `fold` | `forall a b. ((b, a) -> b, b, List<a>) -> b` |

**Option**

| Function | Type |
|----------|------|
| `map_option` | `forall a b. ((a) -> b, Option<a>) -> Option<b>` |
| `unwrap_or` | `forall a. (Option<a>, a) -> a` |

**Result**

| Function | Type |
|----------|------|
| `map_result` | `forall a b e. ((a) -> b, Result<a, e>) -> Result<b, e>` |
| `map_error` | `forall a e1 e2. ((e1) -> e2, Result<a, e1>) -> Result<a, e2>` |

## Lowercase constructor aliases

When the typechecker processes any `type` declaration it automatically adds a
**snake_case alias** for each constructor whose name differs from its alias.
For example, `type ConnState = | Disconnected | Connecting(String) | ...`
produces the extra bindings `disconnected`, `connecting`, etc. that can be
called as functions.

The alias is computed by the `camel_to_snake` function in
`lib/typechecker.ml`.  An alias is only added if the snake_case name is not
already bound — this prevents shadowing built-in functions such as `eq`, `lt`,
etc. when a type like `type Ordering = | LT | EQ | GT` is declared.

## Using the prelude

The prelude is loaded at the start of every `axiom build` invocation and its
declarations are prepended to the user program before type-checking begins.
Pass `--no-prelude` to suppress this:

```
axiom build input.axm -o out.wasm --no-prelude
```

The canonical source of the prelude is `stdlib/prelude.axm`.  The same text is
embedded in `lib/prelude_source.ml` so the compiler does not depend on the
file being present at runtime.
