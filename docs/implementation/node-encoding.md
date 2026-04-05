# Binary IR: Node Encoding Format

This document specifies the exact byte-level encoding of every IR node type.
The encoding is the ground truth for hashing, storage, and tooling. Any
change to this spec invalidates all existing hashes and requires a store
migration.

---

## Design Decisions

**Hash function: Blake3.**
Blake3 produces 32-byte digests, is significantly faster than SHA-256, and
has no known weaknesses. The `blake3` OCaml library provides bindings.

**Byte order: little-endian.**
All multi-byte integers are encoded little-endian, consistent with LevelDB,
RocksDB, Parquet, and modern hardware. There are no network-transmission
requirements that would favor big-endian.

**Integer literals: fixed-width i64.**
All integer literal values are encoded as 8-byte little-endian signed
two's-complement integers. Variable-length encoding (e.g. LEB128) may be
worth revisiting if storage profiling shows integer literals are a
significant fraction of IR size, but fixed-width simplifies the
implementation for v1.

**Patterns and type expressions are inlined.**
Pattern and type-expression data is encoded directly inside the node that
contains it rather than as separately addressable child nodes. Patterns do
not represent an independent unit of computation or compilation, and the
sharing benefit would be marginal. If tooling later requires pattern-level
addressability, they can be promoted to first-class nodes in a future
format version.

**Structural sharing is automatic.**
Two semantically identical expressions (e.g. every occurrence of `Var "x"`)
encode to identical bytes, therefore have identical hashes, and are stored
exactly once. No explicit string-interning or deduplication mechanism is
needed.

**No desugaring before encoding.**
The binary IR preserves source structure faithfully. `If` is stored as `If`,
not lowered to `Match`. `Let` with a pattern stays `Let { pat; ... }`.
Lossless round-tripping between the working form and the binary IR is a
first-class correctness requirement; desugaring and optimization happen
_after_ elaboration into the IR.

---

## Primitive Types

All multi-byte integers are **little-endian**.

| Name    | Size     | Description                                                  |
|---------|----------|--------------------------------------------------------------|
| `u8`    | 1 byte   | Unsigned byte                                                |
| `u16`   | 2 bytes  | Unsigned 16-bit integer, little-endian                       |
| `u32`   | 4 bytes  | Unsigned 32-bit integer, little-endian                       |
| `i64`   | 8 bytes  | Signed 64-bit integer, little-endian, two's-complement       |
| `f64`   | 8 bytes  | IEEE 754 double, little-endian bit representation; all NaN values canonicalized to `0x7FF8000000000000` before encoding |
| `bool`  | 1 byte   | `0x00` = false, `0x01` = true                                |
| `str`   | variable | `[len : u16][utf-8 bytes : len]` — max 65,535 bytes; used for identifiers and names |
| `lstr`  | variable | `[len : u32][utf-8 bytes : len]` — max 4 GiB; used for string literal values |

**Compound types used in inline data:**

| Name        | Encoding                                              |
|-------------|-------------------------------------------------------|
| `opt(T)`    | `[present : bool][T if present]`                      |
| `list(T)`   | `[n : u16][T × n]`                                    |

---

## Inline Sub-Structures

These types appear embedded inside node inline data and are not separately
addressable in the store.

### Pattern (`pat`)

```
[tag : u8]
0x00  PWild        — (no further data)
0x01  PVar         — name:str
0x02  PLitInt      — value:i64
0x03  PLitFloat    — bits:f64
0x04  PLitString   — value:lstr
0x05  PLitTrue     — (no further data)
0x06  PLitFalse    — (no further data)
0x07  PLitUnit     — (no further data)
0x08  PCtor        — name:str  pats:list(pat)
0x09  PRecord      — is_open:bool  fields:list(str ‖ pat)
                     (each field entry is: field_name:str  field_pat:pat)
0x0A  POr          — left:pat  right:pat
```

### Type Expression (`ty`)

```
[tag : u8]
0x00  TyName    — name:str
0x01  TyApp     — name:str  args:list(ty)
0x02  TyTuple   — elems:list(ty)
0x03  TyFun     — params:list(ty)  ret:ty  eff:opt(eff_set)
```

### Effect Set (`eff_set`)

```
[tag : u8]
0x00  Pure      — (no further data)
0x01  Effects   — tys:list(ty)
```

### Function Parameter (`param`)

```
name:str  type:ty
```

### Node-Attached Comment (`comment`)

Every expression, pattern, and declaration node may carry an optional
node-attached comment. This is encoded as a trailing field in the node's
inline data:

```
opt(lstr)     -- None if no comment; Some(text) if @#...#@ was attached
```

The comment uses `lstr` (u32 length prefix) rather than `str` because
comments may contain substantial reasoning context for LLM consumption.

---

## Node Payload Format

Every IR node is stored as a self-contained payload:

```
[tag        : u8 ]
[n_children : u16]    number of child hashes that follow
[n_inline   : u32]    byte length of inline data that follows
[child_0    : 32 bytes]
...
[child_N    : 32 bytes]
[inline     : n_inline bytes]
```

**Hash computation:**
```
hash = Blake3(payload)
```

The hash is computed over the complete payload — tag, n_children, n_inline,
all child hashes, and all inline bytes. Because the encoding is fully
deterministic (canonical byte order, no padding, no optional fields
represented ambiguously), the hash is stable for any given AST node.

The payload length is:
```
length = 1 + 2 + 4 + (32 × n_children) + n_inline
       = 7 + 32 × n_children + n_inline
```

The stored record in a segment's `.bin` file is:
```
[hash    : 32 bytes]
[length  : u32     ]
[payload : length bytes]
```

The hash is stored redundantly in the data file so that segments can be
validated and re-indexed from the data file alone.

---

## Node Tag Table

| Tag    | Name          | Category    |
|--------|---------------|-------------|
| `0x00` | —             | Reserved    |
| `0x01` | `Var`         | Expression  |
| `0x02` | `IntLit`      | Expression  |
| `0x03` | `FloatLit`    | Expression  |
| `0x04` | `StringLit`   | Expression  |
| `0x05` | `BoolTrue`    | Expression  |
| `0x06` | `BoolFalse`   | Expression  |
| `0x07` | `UnitLit`     | Expression  |
| `0x08` | `Let`         | Expression  |
| `0x09` | `App`         | Expression  |
| `0x0A` | `Fn`          | Expression  |
| `0x0B` | `Match`       | Expression  |
| `0x0C` | `If`          | Expression  |
| `0x0D` | `Do`          | Expression  |
| `0x0E` | `Letrec`      | Expression  |
| `0x0F` | `Record`      | Expression  |
| `0x10` | `RecordUpdate`| Expression  |
| `0x11` | `Project`     | Expression  |
| `0x12` | `Perform`     | Expression  |
| `0x13` | `Handle`      | Expression  |
| `0x14`–`0x4F` | — | Reserved for future expression/pattern/type nodes |
| `0x50` | `DeclFn`      | Declaration |
| `0x51` | `DeclType`    | Declaration |
| `0x52` | `DeclEffect`  | Declaration |
| `0x53` | `DeclModule`  | Declaration |
| `0x54` | `DeclRequire` | Declaration |
| `0x55` | `Program`     | Declaration |
| `0x56`–`0xFF` | — | Reserved |

---

## Node Encoding Reference

For each node, **Children** lists the child hashes in order (index 0 first),
and **Inline** describes the inline data layout using the primitive types
defined above.

---

### Expressions

#### `0x01 Var`
A variable reference.
```
children: (none)
inline:   name:str
```

#### `0x02 IntLit`
An integer literal. Stored as a full 64-bit signed integer.
```
children: (none)
inline:   value:i64
```
> **Note:** LEB128 variable-length encoding is a candidate future optimisation
> if profiling shows integer literals dominate IR size. Fixed-width is used
> in v1 for simplicity.

#### `0x03 FloatLit`
A floating-point literal. The raw IEEE 754 bit pattern is stored
little-endian. All NaN payloads are normalised to quiet NaN
(`0x7FF8000000000000`) before hashing or storage to ensure canonical encoding.
```
children: (none)
inline:   bits:f64
```

#### `0x04 StringLit`
A string literal. Uses `lstr` (u32 length prefix) to support strings longer
than 64 KiB.
```
children: (none)
inline:   value:lstr
```

#### `0x05 BoolTrue` / `0x06 BoolFalse` / `0x07 UnitLit`
Constant-valued leaves with no data.
```
children: (none)
inline:   (none)
```

---

#### `0x08 Let`
A let-expression with a pattern binder. The pattern is inline; the value and
body are child expression nodes.
```
children: [0: value_expr, 1: body_expr]
inline:   pat:pat
```

---

#### `0x09 App`
Function application. The function and all arguments are children; order is
preserved.
```
children: [0: fn_expr, 1..n_children−1: arg_exprs]
inline:   (none)
```
`n_children = 1 + len(args)`

---

#### `0x0A Fn`
An anonymous function (lambda). The body is a child; parameter names, types,
return type annotation, and effect annotation are inline.
```
children: [0: body_expr]
inline:   params:list(param)  ret:opt(ty)  eff:opt(eff_set)
```

---

#### `0x0B Match`
Pattern match. The scrutinee and each arm body are children. Arm patterns are
inline, in the same order as the arm bodies.
```
children: [0: scrutinee, 1..n_arms: arm_body_exprs]
inline:   arm_pats:list(pat)
```
Arm `i` has body `child[i + 1]` and pattern `arm_pats[i]`.
`n_children = 1 + n_arms`

---

#### `0x0C If`
Conditional expression.
```
children: [0: cond, 1: then_, 2: else_]
inline:   (none)
```

---

#### `0x0D Do`
An effectful do-block. Each statement contributes exactly one child (its
expression); the inline data describes the statement structure.
```
children: [0..n_stmts−1: stmt_exprs]
inline:
  [n_stmts : u16]
  for each stmt in order:
    [stmt_tag : u8]
      0x00 = StmtExpr   — (no further inline data for this stmt)
      0x01 = StmtLet    — pat:pat
```
Child `i` is the expression for statement `i` (the bound value for `StmtLet`,
the expression itself for `StmtExpr`).

---

#### `0x0E Letrec`
Mutually recursive bindings. The outer body is child 0; each binding's body
is child `i + 1` (0-indexed). Binding metadata (name, params, required return
type) is inline.
```
children: [0: outer_body, 1..n_bindings: binding_body_exprs]
inline:
  [n_bindings : u16]
  for each binding in order:
    name:str
    params:list(param)
    ret_type:ty           ← required (not optional) in letrec
```
Note: unlike `DeclFn`, `letrec` bindings require a return type annotation in
the AST (`letrec_return_type : type_expr`, not `option`). This is reflected
in the encoding.

---

#### `0x0F Record`
A record literal. Field values are children; field names are inline in the
same order.
```
children: [0..n_fields−1: field_value_exprs]
inline:   field_names:list(str)
```
Field `i` has name `field_names[i]` and value `child[i]`.

---

#### `0x10 RecordUpdate`
A record update expression (`{ base with f = e, ... }`). The base record is
child 0; updated field values are children 1..n.
```
children: [0: base_expr, 1..n_fields: field_value_exprs]
inline:   field_names:list(str)
```
Updated field `i` has name `field_names[i]` and new value `child[i + 1]`.

---

#### `0x11 Project`
Field projection.
```
children: [0: record_expr]
inline:   field:str
```

---

#### `0x12 Perform`
Perform an effect operation. Arguments are children; effect and operation
names are inline.
```
children: [0..n_args−1: arg_exprs]
inline:   effect_name:str  op_name:str
```

---

#### `0x13 Handle`
Effect handler. The handled expression is child 0. Handler body expressions
follow in a deterministic order determined by parsing the inline data: for
each handler, all op handler bodies come first (in declaration order), then
the return handler body if present.
```
children:
  [0: handled_expr]
  [then, for each handler in order:
    op_handler_body_0 .. op_handler_body_N
    return_handler_body  (only if has_return = true)]

inline:
  [n_handlers : u16]
  for each handler in order:
    effect_name:str
    [n_ops : u16]
    for each op handler in order:
      op_name:str
      param_names:list(str)    ← names only; types come from DeclEffect
    has_return:bool
    if has_return:
      return_var:str
```

Child index is computed by accumulating: 1 (for `handled`) + for each
preceding handler, `n_ops + (1 if has_return)`. Inline data is
self-describing so no separate length fields are needed to locate children.

---

### Declarations

#### `0x50 DeclFn`
A top-level function declaration. The body is the sole child.
```
children: [0: body_expr]
inline:
  pub:bool
  name:str
  type_params:list(str)
  params:list(param)
  ret:opt(ty)
  eff:opt(eff_set)
```

---

#### `0x51 DeclType`
A sum-type declaration. All data is inline because constructor declarations
are small and carry no independently-addressable children.
```
children: (none)
inline:
  pub:bool
  name:str
  type_params:list(str)
  [n_ctors : u16]
  for each constructor in order:
    ctor_name:str
    ctor_params:list(ty)
```

---

#### `0x52 DeclEffect`
An effect declaration.
```
children: (none)
inline:
  pub:bool
  name:str
  type_params:list(str)
  [n_ops : u16]
  for each op in order:
    op_name:str
    op_params:list(ty)
    op_ret:ty
```

---

#### `0x53 DeclModule`
A module declaration. Each member declaration is a child (enabling
structural sharing across modules that contain identical declarations).
```
children: [0..n_decls−1: decl_hashes]
inline:
  pub:bool
  name:str
```

---

#### `0x54 DeclRequire`
A require-effect declaration. The required type is encoded inline.
```
children: (none)
inline:   ty:ty
```

---

#### `0x55 Program`
The root node of a complete program. Each top-level declaration is a child.
The root hash stored in the image manifest is the hash of this node.
```
children: [0..n_decls−1: decl_hashes]
inline:   (none)
```

---

## Worked Example

Source:
```
fn double(x: Int) -> Int { x }
```

### Node 1 — `Var "x"` (tag `0x01`)

```
tag:        01
n_children: 00 00          (0)
n_inline:   03 00 00 00    (3)
inline:     01 00          (str len = 1)
            78             ("x")
```
Payload (10 bytes): `01 00 00 03 00 00 00 01 00 78`
Hash: `Blake3(payload)` → call this **H_var_x**

---

### Node 2 — `DeclFn` (tag `0x50`)

```
tag:        50
n_children: 01 00          (1 child: H_var_x)
n_inline:   1e 00 00 00    (30 bytes)
child[0]:   [H_var_x : 32 bytes]

inline:
  00                       pub = false
  06 00 64 6f 75 62 6c 65  name = "double"
  00 00                    type_params: [] (n=0)
  01 00                    params: [1 param]
    01 00 78               param name = "x"
    00 03 00 49 6e 74      param type = TyName "Int"
  01                       ret: Some
    00 03 00 49 6e 74        TyName "Int"
  00                       eff: None
```

Inline byte count:
- pub: 1
- name "double": 2 + 6 = 8
- type_params: 2
- params list header: 2
- param "x: Int": (2+1) + (1+2+3) = 9
- ret Some(TyName "Int"): 1 + (1+2+3) = 7
- eff None: 1
- **total inline: 30**  ✓

Hash: `Blake3(payload)` → **H_declfn_double**

---

## Appendix: Inline Data Size Reference

| Node        | Children | Minimum inline | Notes                             |
|-------------|----------|----------------|-----------------------------------|
| `Var`       | 0        | ≥ 3            | name str (2 len + ≥1 char)        |
| `IntLit`    | 0        | 8              | fixed i64                         |
| `FloatLit`  | 0        | 8              | fixed f64                         |
| `StringLit` | 0        | 4              | lstr (u32 len, may be empty)      |
| `BoolTrue`  | 0        | 0              | —                                 |
| `BoolFalse` | 0        | 0              | —                                 |
| `UnitLit`   | 0        | 0              | —                                 |
| `Let`       | 2        | 1              | at least PWild (1 byte pat tag)   |
| `App`       | ≥ 1      | 0              | n_children encodes arity          |
| `Fn`        | 1        | 2              | empty param list + no ret + no eff|
| `Match`     | ≥ 1      | 2              | arm_pats list header (n=0 valid)  |
| `If`        | 3        | 0              | —                                 |
| `Do`        | ≥ 0      | 2              | n_stmts header                    |
| `Letrec`    | ≥ 1      | 2              | n_bindings header                 |
| `Record`    | ≥ 0      | 2              | field_names list header           |
| `RecordUpdate` | ≥ 1   | 2              | field_names list header           |
| `Project`   | 1        | ≥ 3            | field name str                    |
| `Perform`   | ≥ 0      | ≥ 6            | two name strs                     |
| `Handle`    | ≥ 1      | 2              | n_handlers header                 |
| `DeclFn`    | 1        | ≥ 5            | pub + name + 3 empty lists        |
| `DeclType`  | 0        | ≥ 5            | pub + name + type_params + ctors  |
| `DeclEffect`| 0        | ≥ 5            | pub + name + type_params + ops    |
| `DeclModule`| ≥ 0      | ≥ 3            | pub + name                        |
| `DeclRequire`| 0       | ≥ 2            | TyName ty (tag + name str)        |
| `Program`   | ≥ 0      | 0              | —                                 |
