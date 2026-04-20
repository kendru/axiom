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

## Partially Implemented

| Feature | What exists | What is missing |
|---------|-------------|-----------------|
| Type checking | HM inference for core expressions (literals, let, fn, app, match, letrec). `check_program` collects `DeclFn`/`DeclEffect` into envs before checking bodies. `perform E.op(args)` resolves to the declared operation, instantiates the effect's type parameters with fresh metas, unifies arg types, and returns the op's return type. Effect rows are threaded through inference as the ambient row; two `perform`s on the same effect in one ambient share their type arguments. | `DeclType` constructors are not registered in the value env. `DeclModule` bodies are not recursed into. Record type inference (deferred). Constructor pattern type refinement. |
| Effect system | Declarations, `perform`, and `handle` parse and round-trip through the IR. `perform` is type-checked against declared ops and unified into the enclosing ambient effect row. Handler clauses are type-checked: op-handler params match the declared op, `resume` is bound with type `op_return -> result`, all op bodies and the return clause (if present) must agree on the handle's result type. Row-typed arrows (`TyFun a b row`) cross-check declared vs performed effects at every function boundary: calling an effectful function from a context whose ambient doesn't admit that effect is a type error. `handle` discharges its handled effects by extending the ambient for the handled expression. | Explicit effect-row variables in the surface syntax (`{E1, E2 \| ρ}` from §4.2) — row variables exist internally but the parser has no syntax for them. `effect_set` in the AST is still `Pure \| Effects of type_expr list`, a closed set. |
| Function type annotations | Parser accepts optional return type and effect annotations on `fn` and `DeclFn`. `check_program` unifies the body's inferred type against the declared return type AND runs the body under the declared effect row, so declared effects are cross-checked against those actually performed. | Design doc Section 4.3 calls for mandatory annotations at function boundaries; the parser currently makes them optional. |

## Not Yet Implemented

| Feature | Design doc section | Notes |
|---------|-------------------|-------|
| **Row-polymorphic records** | §4.1 (`{ l₁: τ₁ | ρ }`) | AST `type_expr` has no row variable slot. `TyName`, `TyApp`, `TyTuple`, `TyFun` are the only forms. |
| **Recursive types** | §4.1 (`rec α . τ`) | Not in AST `type_expr`. |
| **Explicit effect row variables in surface syntax** | §4.2 (`{ E₁ | ε' }`) | AST `effect_set` is `Pure \| Effects of type_expr list` — the parser has no syntax for open rows. The typechecker's internal rows already support row variables; what's missing is a way to write them on a function's declared effect set. |
| **Module imports** | §7.3 (`import X`, `import X as Y`) | `import` is not a keyword in the lexer. Only `require effect` exists for module dependencies. |
| **Positional shorthand** | §2.2 (`$0`, `$1` in closures) | Not in lexer or parser. |
| **Byte literals** | §10.1 (`Char` type) | No `Char` or byte literal in AST or lexer. |
| **Node store** | §2.5 | Specified in `docs/implementation/node-store.md` but not yet implemented in code. |
| **Code generation** | §9 | No backend. Compiler pipeline stops at type checking. |
| **Standard library** | §10 | No built-in functions or runtime. |
| **MCP server** | §11 | No query, transform, or verify tooling. |
| **Image system** | §2.5 | No image archive, indexes, or operation history. |
