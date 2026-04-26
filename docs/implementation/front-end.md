# Front-End: Lexer, AST, Parser, Printer

The front-end turns Axiom source text (`.axm` files, the *working form*) into
an in-memory AST and back. Round-tripping `parse ∘ tokenize ∘ print` is a
correctness requirement — any change that breaks it is a bug.

The pipeline:

```
source string  ── Lexer.tokenize  ──▶  token list
token list     ── Parser.parse_program ──▶  Ast.program
Ast.program    ── Printer.print_program ──▶  source string
```

---

## Lexer (`lib/lexer.ml`)

A single-pass character state machine that produces a flat `token list`. The
lexer is position-free: tokens carry no source coordinates. Whitespace is
silently consumed.

### Public API

| Function | Signature | Location |
|----------|-----------|----------|
| `tokenize` | `string -> token list` | `lib/lexer.ml:269` |
| `pp_token` | `Format.formatter -> token -> unit` | `lib/lexer.ml:51` |
| `equal_token` | `token -> token -> bool` | `lib/lexer.ml:106` |

### Token categories

- **Keywords** (`lib/lexer.ml:112` `keyword_of_string`): `let`, `letrec`,
  `fn`, `pub`, `type`, `module`, `require`, `effect`, `perform`, `handle`,
  `match`, `do`, `in`, `if`, `else`, `with`, `return`, `resume`, `pure`, etc.
- **Identifiers**: `Ident` (lowercase or underscore-leading) versus
  `CtorIdent` (uppercase-leading). The distinction is lexical, not parsed.
- **Numeric literals**: integers (decimal and `0x…` hex) parsed to `int64`;
  floats trigger only when a `.` or exponent is present, so `1` is `Int` and
  `1.` is `Float`.
- **Strings**: scanned by `scan_string` (`lib/lexer.ml:223`). Recognised
  escapes are `\n`, `\t`, `\"`, `\\`. Unterminated literals raise `Failure`.
- **Operators / punctuation**: two-char operators (`->`, `=>`, `++`, `==`,
  `!=`, `..`) are dispatched ahead of their single-char prefixes via
  `peek2`.
- **Node-attached comments** (`@# … #@`): scanned by `scan_comment`
  (`lib/lexer.ml:246`) into `Comment of string`. The closing `#@` is
  required.

### Quirks worth knowing

- **No line/block comments.** `//` and `/* … */` are *not* tokens. Only the
  node-attached `@# … #@` form exists, by design — comments are first-class
  IR data, not throwaway annotations.
- **No source positions.** If you need them, layer them on top.
- **Unknown characters are silently skipped** (`lib/lexer.ml:337`); the
  parser will surface a downstream error.

---

## AST (`lib/ast.ml`)

The AST is a set of mutually recursive records and variants. Every
expression, pattern, and declaration carries an optional node-attached
comment, so the front-end can preserve LLM-authored reasoning across the
working-form ↔ IR round-trip.

### Top-level types

| Type | Lines | Summary |
|------|-------|---------|
| `type_expr` | 7–12 | `TyName`, `TyApp`, `TyTuple`, `TyFun (params, ret, eff)` |
| `effect_set` | 14–16 | `Pure` or `Effects of type_expr list` |
| `param` | 22–25 | Function parameter: name + type |
| `pattern` | 35–52 | `{ pat_desc; pat_comment }` over `PWild`, `PVar`, `PLit*`, `PCtor`, `PRecord (open?, fields)`, `POr` |
| `expr` | 61–85 | `{ desc; comment }` over `Var`, literals, `Let`, `App`, `Fn`, `Match`, `If`, `Do`, `Letrec`, `Record`, `RecordUpdate`, `Project`, `Perform`, `Handle` |
| `fn_data` | 86–91 | Lambda payload: params, optional return type, optional effects, body |
| `let_binding` | 93–97 | Pattern, value expression, body |
| `letrec_binding` | 99–104 | Mutually recursive function: name, params, **required** return type, body |
| `match_arm` | 106–109 | Pattern + arm body |
| `do_stmt` | 122–124 | `StmtLet of pattern * expr` or `StmtExpr of expr` |
| `perform_data` | 126–130 | Effect name, op name, arguments |
| `op_handler` / `return_handler` / `effect_handler` | 132–152 | One handler clause; an effect handler bundles ops with an optional `return` clause |
| `decl_desc` | 479–510 | `DeclFn`, `DeclType`, `DeclEffect`, `DeclModule`, `DeclRequire`, `DeclImport` |
| `program` | 515 | `decl list` |

Constructor functions `expr k`, `pat k`, `decl k` build nodes with a `None`
comment by default (`lib/ast.ml:55,155,513`).

### Notable invariants

- Every comment-bearing node round-trips its comment unchanged through the
  printer and the IR encoder; comments are part of node identity.
- `letrec` bindings require an explicit return type (`letrec_return_type :
  type_expr`, not optional). `DeclFn` makes the same annotation optional.
- `Effects` is a closed list — there is no surface syntax for an open
  effect-row variable yet (the typechecker supports them internally).
- The AST does **not** carry source locations; errors point at AST shapes,
  not source spans.

---

## Parser (`lib/parser.ml`)

A hand-written recursive-descent parser. There is no operator-precedence
climbing — application is left-associative, and statement-level constructs
(`let`, `letrec`, `handle`, `if`, `match`, `fn`, `perform`, `do`) are
recognised at a single statement layer.

### Public API

| Function | Signature | Location |
|----------|-----------|----------|
| `parse_program` | `token list -> program` | `lib/parser.ml:778` |
| `parse_expr` | `token list -> expr` | `lib/parser.ml:570` |

### Parsing layers

- `parse_program` (`lib/parser.ml:778`) loops `parse_decl` (`:659`) until
  EOF, dispatching on `pub?` followed by one of `fn`, `type`, `effect`,
  `module`, `require`, `import`.
- `parse_expr_state` (`:381`) recognises statement-level forms before
  falling through to `parse_app`.
- `parse_app` (`:483`) handles application and postfix (`.field`).
- `parse_atom` (`:523`) handles literals, variables, parenthesised
  expressions, record literals, and unit.

### Comment attachment

After every expression, pattern, and declaration, the parser peeks for a
trailing `Comment` token and attaches it via `maybe_comment_expr`,
`maybe_comment_pat`, `maybe_comment_decl` (`lib/parser.ml:35–50`). This is
the source of the round-trip property for IR comments.

### Quirks

- **`do`-block tail expression.** Because both statements and the trailing
  expression can begin with `let`, `parse_do` (`:315`) speculatively parses
  a `let`-statement, restoring state and re-parsing as an expression if no
  `;` follows.
- **Errors** are `Failure` strings produced by `consume`
  (`lib/parser.ml:20`); they include "expected X, got Y" but no source
  span.
- Function *declarations* require their body in `{ … }`. Function
  *expressions* (`fn (...) -> T { … }`) require the same; inline `=>`-style
  bodies are not accepted.

---

## Printer (`lib/printer.ml`)

Converts an AST back into working-form source text. The output is
*designed* to be re-parsable: `parse_program (tokenize (print_program p))`
should produce a program structurally equal to `p`.

### Public API

| Function | Signature | Location |
|----------|-----------|----------|
| `print_program` | `program -> string` | `lib/printer.ml:309` |
| `print_decl` | `decl -> string` | `lib/printer.ml:249` |
| `print_expr` | `expr -> string` | `lib/printer.ml:118` |
| `print_pattern`, `print_type_expr`, `print_effect_set`, `print_param` | helpers | — |

### Strategy

String concatenation with minimal whitespace; this is the *working form*
the LLM consumes, not the human-oriented *review form*. Specific choices:

- **Parenthesisation**: `needs_parens_as_base` (`lib/printer.ml:113`) wraps
  non-atomic expressions when they appear in callee, base, or projection
  position.
- **Float printing** (`lib/printer.ml:28`): `%.17g`, with a trailing `.`
  appended if neither `.` nor `e` would otherwise appear, so
  `Float 1.0` round-trips through the lexer's float discriminator.
- **Synthetic parameter names for `TyFun`** (`lib/printer.ml:47`): the
  parser stores arrow-type parameters by name but the AST drops them, so
  the printer emits `p0`, `p1`, … to keep the output parsable.
- **Or-patterns**: left-nested `POr` is parenthesised
  (`lib/printer.ml:101`) because parsing is right-associative.
- **Comments** are appended as ` @# … #@` after the construct they
  annotate.

### Round-trip property (tested)

`test/test_printer.ml` and `test/test_roundtrip.ml` exercise
print → tokenize → parse on every example and many synthesised cases. If
you change the printer or the lexer keyword set, expect failures there
first.
