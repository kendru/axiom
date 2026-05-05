# Implementation Status

This document tracks which features from the design specification
(`axiom-overview-draft.md`) are implemented, partially implemented, or not yet
started in the OCaml compiler frontend.

---

## Fully Implemented

| Feature | Components |
|---------|------------|
| Lexer (all tokens, keywords, literals, operators) | `lib/lexer.ml` |
| Recursive-descent parser (expressions + declarations) | `lib/parser.ml` |
| AST with node-attached comments | `lib/ast.ml` |
| Hindley-Milner type inference with let-generalization | `lib/typechecker.ml` |
| Program-level checking (`check_program`) — two-pass: collect decls, then check bodies against declared signatures | `lib/typechecker.ml` |
| `perform E.op(args)` type checking against declared effect operations | `lib/typechecker.ml` |
| `handle e with { E { op(…) => … ; return v => … } }` — op handlers checked against declared op signatures, `resume : op_return -> result`, return clause checked against the handled expression's type | `lib/typechecker.ml` |
| Effect rows on function types — `TyFun (a, b, row)` with Rémy-style row unification. Declared annotations (`! {E1, E2}` / `! pure`) close the innermost arrow's row; unannotated sites get a fresh open row. Each `perform` unifies its effect into the ambient row; full application unifies the callee's innermost row with the ambient; `handle` checks its handled expression under an ambient extended with the handled effects. Scheme instantiation re-opens rows so a closed row can be used in a larger ambient (implicit effect polymorphism). Two `perform`s of the same effect in one ambient row share their type arguments (e.g. `State<s>` get/put agree on `s`). | `lib/typechecker.ml` |
| Pattern matching (wildcards, vars, literals, ctors, records, or-patterns) | parser, AST |
| Algebraic data types (`type` declarations with constructors) | parser, AST |
| Effect declarations and `perform` / `handle` syntax | parser, AST |
| Module declarations with `pub` visibility | parser, AST |
| `require effect` declarations | parser, AST |
| `letrec` mutual recursion groups | parser, AST, typechecker |
| Records (construction, update, projection) | parser, AST |
| `do` blocks with statement sequencing | parser, AST |
| Node-attached comments (`@#...#@`) on exprs, patterns, decls | lexer, parser, AST |
| Binary IR node encoding with BLAKE3 content-addressing | `lib/node_encoding.ml`, `lib/node_decoding.ml`, `lib/node_tag.ml`, `lib/node_hash.ml` |
| On-disk node store with segmented flat-file layout | `lib/node_store.ml` |
| `DeclType` constructor registration — each constructor is added to the value env as a polymorphic scheme (`ctor_scheme_of_type`); constructors are usable both in expression position (`Some(x)`, `None`) and patterns with full type refinement | `lib/typechecker.ml`, `lib/parser.ml` |
| `DeclModule` body collection — both collect pass and body-checking pass now recurse into module bodies, making module-scoped functions available in the flat value env | `lib/typechecker.ml` |
| Structural record type inference — `TyRecord` with open tail (`rec_meta`); record construction, update, and projection infer and unify field types; record patterns bind sub-pattern variables at their field types; row-polymorphic unification via `unify_records` handles open/closed and open/open cases | `lib/typechecker.ml` |
| Explicit effect row variables in surface syntax — `{E₁, E₂ \| ρ}` parsed to `Effects (ts, Some v)` and converted to `RMeta` by `row_of_effect_set_env`; a shared `row_var_env` ensures the same name resolves to the same meta within a signature | `lib/parser.ml`, `lib/typechecker.ml` |
| **MCP server** — Unified result envelope and batch protocol with hash-anchored results. Query operations: `signature`, `interface`, `effects`, `callers`, `effect_flow`, `unhandled`, `dependents`, `pattern_coverage`, `graph`. Write operations: `submit_module`. Verify operations: `types`, `exhaustive`, `effects`, `unused`, `tail_calls`. Transform operations: `rename`, `extract_function`, `mock_effects`, `add_effect_logging`, `inline_handler`. Support for project-wide image, cross-module symbol resolution, and diagnostic payload encoding. See [docs/implementation/mcp.md](docs/implementation/mcp.md) | `lib/mcp.ml`, `lib/mcp_*` |

## Partially Implemented

| Feature | What exists | What is missing |
|---------|-------------|-----------------|
| Type checking | HM inference for core expressions (literals, let, fn, app, match, letrec). `check_program` collects `DeclFn`/`DeclEffect`/`DeclType`/`DeclModule` into envs before checking bodies. `perform E.op(args)` resolves to the declared operation, instantiates the effect's type parameters with fresh metas, unifies arg types, and returns the op's return type. Effect rows are threaded through inference as the ambient row; two `perform`s on the same effect in one ambient share their type arguments. Constructor patterns look up the ctor scheme in the env, unify the scrutinee type with the ctor's return type, and bind sub-pattern variables at the ctor's parameter types. | `DeclModule` body declarations are added to the flat value env only — no namespace separation or qualified access (e.g. `MyModule.foo`). |
| Effect system | Declarations, `perform`, and `handle` parse and round-trip through the IR. `perform` is type-checked against declared ops and unified into the enclosing ambient effect row. Handler clauses are type-checked: op-handler params match the declared op, `resume` is bound with type `op_return -> result`, all op bodies and the return clause (if present) must agree on the handle's result type. Row-typed arrows (`TyFun a b row`) cross-check declared vs performed effects at every function boundary: calling an effectful function from a context whose ambient doesn't admit that effect is a type error. `handle` discharges its handled effects by extending the ambient for the handled expression. `check_program` runs each function body under its declared effect row, so declared vs performed effects are cross-checked at the program level. | `effect_set` in the AST encodes effects as a list plus an optional row-variable name (`Effects of type_expr list * string option`). The internal checker row (`effect_row`) is richer — the AST form has no way to express nested row structure beyond a single tail variable. |
| Function type annotations | Parser accepts optional return type and effect annotations on `fn` and `DeclFn`. `check_program` unifies the body's inferred type against the declared return type AND runs the body under the declared effect row, so declared effects are cross-checked against those actually performed. | Design doc Section 4.3 calls for mandatory annotations at function boundaries; the parser currently makes them optional. |
| Code generation | WASM backend compiles integer and boolean literals, arithmetic primops (`add`, `sub`, `mul`, `neg`, `div`), comparison operators (`lt`, `gt`, `lte`, `gte`, `eq`), let-bindings, function definitions and calls, and pattern matching on simple constructors. Linear intermediate code walks the typed AST and emits WASM instruction sequences; function signatures are encoded in WASM type section. See [docs/implementation/codegen.md](docs/implementation/codegen.md) | Records, algebraic data types, effect operations (`perform`/`handle`), strings, list operations, and floating-point arithmetic not yet implemented. No runtime system or standard library support. |

## Not Yet Implemented

| Feature | Design doc section | Notes |
|---------|-------------------|-------|
| **Row-polymorphic records in type annotations** | §4.1 (`{ l₁: τ₁ \| ρ }`) | AST `type_expr` has no record variant — `TyName`, `TyApp`, `TyTuple`, `TyFun` are the only surface type forms. Record types are inferred from expressions but cannot appear in written type annotations or signatures. |
| **Recursive types** | §4.1 (`rec α . τ`) | Not in AST `type_expr`. |
| **Module imports** | §7.3 (`import X`, `import X as Y`) | `import` is not a keyword in the lexer. Only `require effect` exists for module dependencies. |
| **Positional shorthand** | §2.2 (`$0`, `$1` in closures) | Not in lexer or parser. |
| **Byte literals** | §10.1 (`Char` type) | No `Char` or byte literal in AST or lexer. |
| **Standard library** | §10 | No built-in functions or runtime. |
| **Image system** | §2.5 | No image archive, indexes, or operation history. |
