# Typechecker (`lib/typechecker.ml`)

A Hindley–Milner type inferencer with let-generalization, extended with
Rémy-style row polymorphism applied to both **records** and **algebraic
effect rows**. The whole pass is one self-contained file (~1500 lines, no
external type-system dependencies).

For the full feature matrix and what is implemented vs. deferred, see
[status.md](status.md).

---

## Internal type representation

The AST's `type_expr` is the *surface* type syntax. The typechecker
introduces its own representation (`lib/typechecker.ml:32–73`) that adds
unification variables and effect rows.

| Type | Lines | Purpose |
|------|-------|---------|
| `ty` | 32–40 | Internal types: `TyCon`, `TyVar`, `TyFun (param, ret, row)`, `TyForall`, `TyMeta`, `TyRecord (fields, tail)`, `TyApp` |
| `meta_var` | 42–45 | `{ id; mutable inst : ty option }` — a unification variable |
| `row` | 52–55 | Effect row: `RPure`, `RCons (effect_inst, row)`, `RMeta row_meta` |
| `effect_inst` | 57–60 | `{ eff_name; eff_args : ty list }` |
| `row_meta` | 62–65 | Row unification variable |
| `rec_meta` | 70–73 | Record-tail unification variable |
| `scheme` | 198–201 | Polymorphic scheme `{ bound : string list; body : ty }` |
| `env` | 203 | Value env: `(string * scheme) list` |
| `effect_op_scheme` | 317–321 | Declared operation: name, parameter types, return type |
| `effect_scheme` | 324–327 | `{ eff_type_params; eff_ops }` |
| `effect_env` | 329 | `(string * effect_scheme) list` |

A function type is always `TyFun (param, ret, row)` with the row sitting on
the **innermost** arrow — that is where declared effect annotations attach.

---

## Public entry points

| Function | Signature | Location |
|----------|-----------|----------|
| `check_program` | `program -> env * effect_env` | `lib/typechecker.ml:1405` |
| `infer_expr` | `expr -> ty` (empty envs) | `:1186` |
| `infer_expr_in` | `effect_env -> env -> row -> expr -> ty` | `:774` |
| `unify` | `ty -> ty -> unit` | `:348` |
| `unify_row` | `row -> row -> unit` | `:380` |
| `instantiate` | `scheme -> ty` | `:519` |
| `generalize` | `env -> ty -> scheme` | `:532` |
| `effect_scheme_of_decl` | `string list -> effect_op list -> effect_scheme` | `:1273` |
| `fn_scheme_of_decl` | `… -> scheme` | `:1288` |
| `ctor_scheme_of_type` | `string -> string list -> ctor_decl -> scheme` | `:1315` |

`check_program` is what `bin/main.ml` calls (`bin/main.ml:49`); the others
are exposed primarily for the test suite.

---

## Algorithm

### Hindley–Milner with let-generalization

`generalize` (`:532`) collects the metas free in a type but not in the
ambient environment, turns each into a fresh `TyVar`, and packages the
result as a `scheme`. `instantiate` (`:519`) is the inverse — it allocates
a fresh `TyMeta` per bound variable and substitutes through the body.

Crucially, **row metas are *not* generalized**. Instead, `instantiate`
calls `reopen_row` (`:491`) so each use site of a polymorphic function
gets its own fresh row tail. This is what gives Axiom *implicit* effect
polymorphism: a function declared `! pure` can still be called from an
effectful context because its row reopens to absorb the ambient.

### Unification

`unify` (`:348`) is single-dispatch over `ty`. It performs:

- Path compression on `TyMeta` chains.
- Occurs check (`:342`).
- Structural unification of `TyFun`, `TyApp`, `TyRecord`, `TyTuple`.
- Delegation to `unify_row` for effect rows on arrow types.

`unify_row` (`:380`) implements **Rémy's row unification**. The core
helper is `rewrite_row` (`:400`), which walks one row looking for an
effect with a given name; if found, it unifies the operation's type
arguments in place and returns the remainder; if it bottoms out at an
`RMeta`, it instantiates that meta to a fresh `RCons` extending the row.
This makes effect rows behave as *multisets* — order does not matter.

Records use the analogous machinery (`:426–480`) over `rec_meta`.

### Two-pass program checking

`check_program` (`:1405`) walks declarations twice:

1. **Collect pass** (`:1420–1453`): build `env` and `eenv` for every
   `DeclFn`, `DeclType`, `DeclEffect`, and `DeclModule`. Function
   schemes come from `fn_scheme_of_decl`; type declarations register
   each constructor with `ctor_scheme_of_type` (`:1431`).
2. **Body pass** (`:1458–1481`): for each `DeclFn`, run
   `infer_expr_in eenv env declared_row body`, then unify the inferred
   type against the declared return type.

This ordering is what makes mutual recursion work without forward
declarations — every top-level binding is in scope before any body runs.

### Constructor handling

`ctor_scheme_of_type` (`:1315`) builds a curried scheme
`τ₁ -> τ₂ -> … -> Type<α…>` with `RPure` rows and the type's parameters
as bound variables. The same scheme drives both expression-position uses
(`Some(x)`) and pattern matching, so refinement is consistent. `build_type_ctor_env` (`:1406`) is the helper that registers a whole
type's constructors at once.

### `perform` and `handle`

- **`perform`** (`:975`): look up `effect_name` in `eenv`, instantiate
  the effect's type parameters with fresh metas, unify each argument
  against the operation's parameter types, then unify the ambient row
  with `RCons (this_effect, fresh_tail)`. Two `perform`s of the same
  effect in one ambient share their type arguments through this
  unification — for example, `State<s>.get` and `State<s>.put` agree on
  `s`.
- **`handle`** (`:1014`): the handled expression is checked under an
  ambient extended with the handled effects (each instantiated with
  fresh metas, so handlers don't constrain their callers). Each op
  handler runs with a `resume : op_return_ty -> result_ty` binding; the
  result type is shared across all op bodies and the optional
  `return v => …` clause.

### Errors

The typechecker raises `Failure` with a human-readable string, surfaced
by `bin/main.ml` as `axiom build: type error: …`. Messages include:

- Mismatch reports from `unify` (`:360, 368, 375`).
- Unbound variable (`:210`).
- Unknown effect or operation (`:980, 989, 1040`).
- Row occurs check (`:386`).
- Pattern / constructor arity (`:1067, 1147`).
- Record field mismatch (`:446, 960`).

There is **no source span** in any of these messages — the AST does not
carry one.

---

## Limitations

- **No source positions** in any error message.
- **Tuple types in annotations are coerced to fresh metas** (`:601`);
  they are not yet a first-class form.
- **Modules are flat.** Both passes recurse into module bodies, but the
  resulting bindings sit in the same `env` (qualified by string
  concatenation). There is no scoped namespace.
- **Records have no surface row syntax.** The internal machinery
  supports row-polymorphic records, but the parser cannot write the
  `{ l : τ | ρ }` form from the design document.
- **Higher-rank polymorphism is not supported.** Schemes only quantify
  at the top level.
- **Effect rows have no surface variable syntax** (the Section 4.2
  `{ E₁ | ε' }` form is not yet in the parser; row variables exist only
  internally).
