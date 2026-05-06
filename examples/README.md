# Axiom Code Examples

These examples show the **working-form** code that an LLM would produce when
building various modules and functions in Axiom. They are written to be
consistent with the current compiler frontend (parser, type checker) while also
serving as design targets for features still under development (stdlib, effect
checking, codegen).

Each file is a self-contained program or module that an LLM might produce in
a single scope-focused interaction.

## Examples

| File | Description |
|------|-------------|
| `01_basics.axm` | Fundamental expressions: let-bindings, functions, pattern matching, if/else |
| `02_data_types.axm` | Algebraic data types: Option, Result, List, Tree |
| `03_effects.axm` | Effect declarations, performing effects, and writing handlers |
| `04_state_machine.axm` | Modeling a state machine with types and effects |
| `05_collections.axm` | List and Map operations — guides stdlib design |
| `06_string_processing.axm` | String manipulation and text transformation |
| `07_validation.axm` | Data validation with structured error accumulation |
| `08_config.axm` | Configuration loading with layered effect handling |
| `09_pipeline.axm` | Data transformation pipelines using function composition |
| `10_json.axm` | JSON-like data representation and traversal |

## Conventions

- Files use the `.axm` extension (Axiom working form).
- Line comments use `--`.
- Node-attached comments use `@# ... #@` (carried in the binary IR).
- Effect annotations appear at function boundaries: `-> ReturnType ! EffectSet`.
- `pure` means no effects; `{E1, E2}` lists concrete effects.

## Current parser constraints

These examples were validated against the current parser. A few constraints
shaped the code style — each is a candidate for future parser improvement:

1. **No constructor expressions.** The parser treats uppercase identifiers as
   types/patterns only. To "construct" a value like `Some(x)` in expression
   position, we call a lowercase wrapper: `some(x)`. The stdlib should provide
   these wrappers, or the parser should be extended.

2. **No record types in annotations.** Parameter types like `{ x: Int, y: Int }`
   are not supported; use a named type alias instead (e.g. `Point`).

3. **Function-type return values are tricky.** Returning `(x: a) -> b ! pure`
   from a function causes ambiguity with the outer `! EffectSet`. Prefer
   three-argument `compose_apply(f, g, x)` over curried `compose(f, g)` that
   returns a function, or use a named type alias for the function type.

## Build-status triage (issue \#103)

The table below records the current build status of each example, the exact
error produced, the failure category, and the GitHub issue(s) whose resolution
would unblock it.  The `examples_02_10` suite in `test/test_build_pipeline.ml`
asserts the exact failure mode of each broken example so that CI catches silent
fixes *and* unexpected new breakage.

| Example | Status | Failure | Category | Blocking issue(s) |
|---------|--------|---------|----------|-------------------|
| `01_basics.axm` | ✓ passes | — | — | — |
| `02_data_types.axm` | ✓ passes | — | — | — |
| `03_effects.axm` | ✗ fails | `unbound variable 'info'` | Missing lowercase constructor wrappers for user-defined ADTs (e.g. `info` for `Info`, `debug` for `Debug`) | [#109] |
| `04_state_machine.axm` | ✗ fails | `unbound variable 'transitioned'` | Missing lowercase constructor wrappers for user-defined ADTs | [#109] |
| `05_collections.axm` | ✗ fails | `unbound variable 'pair'` | Missing stdlib `pair` constructor wrapper | [#109] |
| `06_string_processing.axm` | ✗ fails | `unbound variable 'string_length'` | Missing stdlib string operations | [#110] |
| `07_validation.axm` | ✗ fails | `unbound variable 'valid'` | Missing lowercase constructor wrappers for user-defined ADTs | [#109] |
| `08_config.axm` | ✗ fails | `parse error: unexpected token Perform` | Parser bug — `perform` not accepted as the scrutinee of a `match` expression | [#101] |
| `09_pipeline.axm` | ✗ fails | `unknown effect 'Log'` | Missing `Log` effect declaration in file scope (declared in `03_effects.axm`; needs import or local redeclaration) | [#109] |
| `10_json.axm` | ✗ fails | `unbound variable 'j_object'` | Missing lowercase constructor wrappers for user-defined ADTs | [#109] |

[#101]: https://github.com/kendru/axiom/issues/101
[#109]: https://github.com/kendru/axiom/issues/109
[#110]: https://github.com/kendru/axiom/issues/110

## Stdlib functions assumed by examples

The examples call functions that don't exist yet. This list captures what
the stdlib needs to provide:

**Arithmetic:** `add`, `sub`, `mul`, `div`, `neg`, `max`, `min`
**Comparison:** `eq`, `lt`, `gt`, `lte`, `gte`
**Float:** `mul_f`, `lt_f`, `gt_f`
**String:** `concat`, `string_length`, `string_eq`, `string_empty`, `string_take`, `string_head`, `contains`, `replace_all`, `remove_between`
**Char:** `char_code`, `code_to_char`, `char_to_string`, `eq_char`, `is_digit`, `char_to_digit`, `space_char`, `tab_char`, `string_to_chars`
**List constructors:** `nil`, `cons`, `append`, `length`, `map`, `filter`
**Option constructors:** `none`, `some`
**Result constructors:** `ok`, `err`
**Other constructors:** `pair`, `leaf`, `node`, `valid`, `invalid` (one per ADT variant)
**Conversion:** `parse_int_or_throw`, `show_log_level`
