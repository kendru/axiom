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
| Type checking | HM inference for core expressions (literals, let, fn, app, match, letrec). `check_program` collects `DeclFn`/`DeclEffect` into envs before checking bodies. `perform E.op(args)` resolves to the declared operation, instantiates the effect's type parameters with fresh metas, unifies arg types, and returns the op's return type. | `DeclType` constructors are not registered in the value env. `DeclModule` bodies are not recursed into. Record type inference (deferred). Constructor pattern type refinement. Each `perform` instantiates the effect's type parameters independently — there is no cross-`perform` sharing via an effect row yet, so e.g. two State operations in the same function do not share their `s` type. |
| Effect system | Declarations, `perform`, and `handle` parse and round-trip through the IR. `perform` is type-checked against declared ops. | Effect-row tracking on function types (`TyFun` has no effect slot), effect-row variables (`{E1, E2 \| ρ}`), handler-clause checking (`resume`, return-clause body vs. handled-expr result), and the constraint that every `perform`'s effect is declared in the enclosing function's effect row. |
| Function type annotations | Parser accepts optional return type and effect annotations on `fn` and `DeclFn`. `check_program` unifies the body's inferred type against the declared return type. | Design doc Section 4.3 calls for mandatory annotations at function boundaries; the parser currently makes them optional. The declared effect set is not yet checked against the effects actually performed in the body. |

## Not Yet Implemented

| Feature | Design doc section | Notes |
|---------|-------------------|-------|
| **Row-polymorphic records** | §4.1 (`{ l₁: τ₁ | ρ }`) | AST `type_expr` has no row variable slot. `TyName`, `TyApp`, `TyTuple`, `TyFun` are the only forms. |
| **Recursive types** | §4.1 (`rec α . τ`) | Not in AST `type_expr`. |
| **Effect row variables** | §4.2 (`{ E₁ | ε' }`) | AST `effect_set` is `Pure \| Effects of type_expr list` — a closed set with no row variable. |
| **Effect rows on function types** | §4.2, §5 | Type checker's internal `ty` has `TyFun of ty * ty` with no effect slot. `perform` is checked for op existence and arg types, but the declared effect row on a function is not checked against the effects actually performed in its body. Handler-clause bodies (including `resume`) are not type-checked. |
| **Module imports** | §7.3 (`import X`, `import X as Y`) | `import` is not a keyword in the lexer. Only `require effect` exists for module dependencies. |
| **Positional shorthand** | §2.2 (`$0`, `$1` in closures) | Not in lexer or parser. |
| **Byte literals** | §10.1 (`Char` type) | No `Char` or byte literal in AST or lexer. |
| **Node store** | §2.5 | Specified in `docs/implementation/node-store.md` but not yet implemented in code. |
| **Code generation** | §9 | No backend. Compiler pipeline stops at type checking. |
| **Standard library** | §10 | No built-in functions or runtime. |
| **MCP server** | §11 | No query, transform, or verify tooling. |
| **Image system** | §2.5 | No image archive, indexes, or operation history. |
