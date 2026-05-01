# MCP Server — Batch Protocol and Diagnostic Schema

This document describes the wire protocol and OCaml type structure for the
Axiom MCP server (`bin/mcp_server.ml`).  It is the implementation reference
for issue #83 and the foundational ground-work for issue #82.

---

## Tool surface

The server exposes **exactly four tools**:

| Tool        | Purpose |
|-------------|---------|
| `query`     | Read the IR graph: signatures, interfaces, effect maps, caller graphs. |
| `write`     | Submit working-form source; elaborate, typecheck, store, and automatically verify. |
| `transform` | Mechanical deterministic refactors (rename, extract-function, …). |
| `verify`    | On-demand invariant checks: types, exhaustive patterns, effect handling, unused declarations. |

---

## Batch request schema

Every tool accepts a single `arguments` object:

```json
{
  "commands": [
    { "op": "<string>", "args": { ... } },
    ...
  ]
}
```

Commands execute in order within a single round-trip.

### Valid ops per tool

| Tool        | Valid `op` values |
|-------------|-------------------|
| `query`     | `"signature"`, `"interface"`, `"effects"`, `"callers"` |
| `write`     | `"submit_module"` |
| `transform` | *(none yet — stub)* |
| `verify`    | `"types"`, `"exhaustive"`, `"effects"`, `"unused"` |

### Command argument shapes

**`query` ops**

```json
// signature — resolve a function's type
{ "op": "signature", "args": { "module": "<name>", "name": "<fn-name>" } }

// interface — public API of a stored module
{ "op": "interface", "args": { "module": "<name>" } }

// effects — map effect names → performing functions
{ "op": "effects",   "args": { "module": "<name>" } }

// callers — direct call sites of a function
{ "op": "callers",   "args": { "module": "<name>", "name": "<fn-name>" } }
```

**`write` ops**

```json
// submit_module — parse, typecheck, store, and auto-verify
{ "op": "submit_module", "args": { "source": "<axiom source>", "module_name": "<name>" } }
```

**`verify` ops**

```json
// types — typecheck source without storing state
{ "op": "types",      "args": { "source": "<axiom source>" } }

// exhaustive — non-exhaustive match checks on a stored module
{ "op": "exhaustive", "args": { "module": "<name>" } }

// effects — unhandled-effect checks from an entry point
{ "op": "effects",    "args": { "module": "<name>", "entry_point": "<fn-name>" } }

// unused — unreferenced private declarations in a stored module
{ "op": "unused",     "args": { "module": "<name>" } }
```

---

## Batch response schema

```json
{
  "results":     [ <result-object>, ... ],
  "stopped_at":  <int> | null,
  "diagnostics": [ <diagnostic>, ... ]
}
```

- `results[i]` corresponds positionally to `commands[i]`.
- If execution halted at index *k* due to an **unrecoverable** failure,
  `results` has length *k* (the failed command's result is omitted) and
  `stopped_at` equals *k*.
- `diagnostics` accumulates diagnostics from all executed commands.

### Per-op result shapes

| Op             | Result fields |
|----------------|---------------|
| `signature`    | `{ "signature": "<type string>", "node": "<hash>" }` |
| `interface`    | `{ "interface": "<source text>", "node": "<hash>" }` |
| `effects`      | `{ "effects": { "<EffectName>": ["<fn>", ...] }, "node": "<hash>" }` |
| `callers`      | `{ "callers": [{ "caller": "...", "line": int, "col": int, "node": "<hash>" }] }` |
| `submit_module`| `{ "hash": "<root-hash>" }` |
| `types`        | `{}` (diagnostics carry the errors) |
| `exhaustive`   | `{}` (diagnostics carry any warnings) |
| `effects`      | `{}` (diagnostics carry any warnings) |
| `unused`       | `{}` (diagnostics carry any warnings) |

---

## Diagnostic schema

Every diagnostic produced by every tool shares one OCaml record type
(`Mcp_lib.Mcp_types.diagnostic`), making per-tool divergence a compile error.

```json
{
  "severity": "error" | "warning" | "info",
  "code":     "<category/slug>",
  "message":  "<human-readable text>",
  "node":     "<blake3-hex>" | null,
  "location": { "module": "<name>", "line": int, "col": int } | null,
  "related":  [ { "node": "<hash>", "message": "<text>" }, ... ]
}
```

### Field semantics

| Field      | Semantics |
|------------|-----------|
| `severity` | `"error"` for failures, `"warning"` for style/safety issues, `"info"` for informational notes. |
| `code`     | Slash-separated category and slug, e.g. `"type/error"`, `"effect/unhandled"`, `"match/non-exhaustive"`, `"decl/unused"`. |
| `message`  | Free-form human-readable description. |
| `node`     | BLAKE3 hex hash of the primary program element the diagnostic refers to.  **Required** whenever the diagnostic names a specific declaration or expression; `null` otherwise. |
| `location` | Supplementary source location.  Optional; prefer `node` for stable chaining. |
| `related`  | Secondary references, e.g. the conflicting declaration in a name-clash error. |

### Code catalogue

| Code                    | Severity | Halts batch? | Meaning |
|-------------------------|----------|--------------|---------|
| `anchor/not-found`      | error    | yes          | Named module or function does not exist in server state. |
| `command/unknown`       | error    | yes          | The `op` field is not a recognised command for this tool. |
| `command/unimplemented` | error    | yes          | The `op` is known but not yet implemented. |
| `parse/error`           | error    | yes          | Source failed to lex or parse; the module was not stored. |
| `type/error`            | error    | no           | Type-checking error (from `verify types`). |
| `effect/unhandled`      | warning  | no           | An effect escapes an entry-point boundary. |
| `match/non-exhaustive`  | warning  | no           | A `match` expression is missing one or more constructors. |
| `decl/unused`           | warning  | no           | A private declaration is never referenced. |

---

## Failure semantics

- **Unrecoverable** failures (halts the batch): malformed request structure,
  unknown/unimplemented command op, anchor not found.
- **Recoverable** diagnostics (batch continues): type errors from `verify
  types`, unhandled effects, non-exhaustive matches, unused declarations.

A `write submit_module` that fails to parse or typecheck is unrecoverable —
the module is not stored and subsequent commands that reference it will also
fail.  A successful `write` always runs all verify passes inline and surfaces
any warnings as recoverable diagnostics in the same response.

---

## OCaml type structure

The canonical types live in `lib/mcp/mcp_types.ml`:

```ocaml
type severity = Sev_error | Sev_warning | Sev_info

type location = {
  loc_module : string option;
  loc_line   : int;
  loc_col    : int;
}

type related_ref = {
  ref_node    : string option;
  ref_message : string;
}

type diagnostic = {
  diag_severity : severity;
  diag_code     : string;
  diag_message  : string;
  diag_node     : string option;
  diag_location : location option;
  diag_related  : related_ref list;
}
```

`bin/mcp_server.ml` defines the local `batch_response` type:

```ocaml
type batch_response = {
  br_results     : json list;
  br_stopped_at  : int option;
  br_diagnostics : Mcp_types.diagnostic list;
}
```

All four tool handlers (`dispatch_query`, `dispatch_write`, `dispatch_transform`,
`dispatch_verify`) return `cmd_outcome` values which the shared `batch_execute`
function folds into a `batch_response`.  A handler that returns a different type
will not compile.
