# Axiom: A Language System Designed for LLM Reasoning

**Version 0.2 — Draft Specification**
**License: Apache 2.0 (implementation) / CC BY 4.0 (specification)**

---

## 1. Introduction

Axiom is a programming language *system* — not merely a language, but an integrated
environment of representations, tools, and runtime — designed from first principles
for reasoning and code generation by large language models.

Traditional programming systems assume the human is the primary reader and writer
of code. Axiom inverts this assumption: the LLM is the primary author and
day-to-day consumer of code, while humans set goals, review outputs, and guide
design decisions. In the same spirit that systems like Smalltalk provided
interaction modes beyond raw file editing for human programmers, Axiom provides
structured commands that give LLMs targeted access to codebase information
without polluting context with noise. Text remains the primary authoring medium,
but reading, reasoning about, and refactoring code is split between reading
working form text and invoking the system's built-in query, transform, and
verification commands.

### 1.1 Core Observations

1. **Attention is the bottleneck.** Every token an LLM must produce or consume
   costs finite capacity. The language should maximize semantic density — but not
   at the cost of reasoning clarity.

2. **LLMs are not humans.** They excel at structural recursion, pattern completion,
   and maintaining tree-shaped invariants. They struggle with sequential simulation,
   precise counting, and tracking implicit mutable state. The language should play
   to these strengths and away from these weaknesses.

3. **Effects are the hard problem.** Most real-world programming complexity lives
   at system boundaries. Effects should be the central organizing concept.

4. **Representation is not fixed.** The same program can and should exist in
   multiple forms optimized for different consumers. A compact binary IR for
   storage and tooling. A keyword-rich working form for LLM reasoning. A
   familiar review form for human auditing.

### 1.2 Design Principles

- **Locality of reasoning.** The meaning of any expression is determinable from
  bounded context. No action-at-a-distance.
- **Explicit data flow.** Values are produced once and consumed by explicit
  reference. No mutable variables in the core calculus.
- **Effects as boundaries.** Every interaction with the external world is typed,
  declared, and mediated by handlers.
- **Keyword anchors over structural minimalism.** Visual chunk boundaries and
  semantic markers aid LLM attention even when they add tokens.
- **Explicit types at boundaries, inferred locally.** Function signatures carry
  full type and effect annotations. Function bodies infer types freely.
- **Compilation-friendly semantics.** Tail calls, pattern matching, and algebraic
  types map directly to efficient machine code.

### 1.3 Implementation Strategy

- **Compiler frontend:** OCaml. Best-in-class for type checker and effect
  inference implementation. Close semantic alignment with Axiom's own semantics
  eases eventual bootstrapping.
- **Runtime and standard library:** Zig. Precise memory control for GC
  implementation, seamless C FFI, minimal runtime overhead.
- **Initial compilation target:** WebAssembly. Highly portable, well-specified,
  straightforward to target directly without existing backend infrastructure.
- **Future targets:** ARM64, AMD64 native code generation; LLVM IR.

### 1.4 Licensing

- **This specification and all documentation:** Creative Commons Attribution 4.0
  International (CC BY 4.0).
- **Reference implementation (compiler, runtime, standard library):** Apache
  License 2.0.

---

## 2. The Three-Layer Architecture

Axiom programs exist in three representations. These are not separate languages —
they are views of the same semantic object, connected by deterministic, lossless
transformations.

### 2.1 Layer 1: Binary IR (Canonical Form)

The binary IR is the ground truth. It is a compact, versioned, binary encoding
of the fully elaborated abstract syntax tree. Every program is stored and
transmitted in this form. The binary IR is not a file format in the traditional
sense — it is a content-addressed graph of fragments that can be assembled,
queried, and transformed without ever materializing as a single flat file.

**Properties:**

- Fully type-annotated (all inference results are materialized).
- Every node carries a unique content-addressed identifier (Merkle hash).
- Effects are fully resolved — every `perform` is linked to its declared effect.
- Deterministic serialization — the same program always produces the same bytes.
- Designed for tooling: diffing, merging, refactoring, and analysis operate on
  the binary IR directly.

**Content-addressing and Merkle structure:** Each node's identifier is a
cryptographic hash of its own data plus the hashes of its children (Merkle
tree). This means:

- Structurally identical subtrees share the same hash and are automatically
  deduplicated.
- Any change to a node propagates new hashes upward to the root, making
  tampering or corruption detectable.
- Nodes are independently addressable fragments — a function, a type
  definition, or even a single pattern match arm can be referenced, fetched,
  and cached by its hash alone.
- Garbage collection reclaims unreachable fragments. When a refactor replaces
  a subtree, the old nodes become unreferenced and are eligible for collection.

**Header node:** Every complete program or library has a well-known header node
that serves as the root of the graph. For executable programs, the header node
references the main entry function plus metadata: target platform, required
top-level effect handlers, compiler version, and any other program-level
configuration. The header node is the starting point for all traversals — the
equivalent of an ELF entry point, but richer.

**Design rationale:** A binary canonical form means the working form and review
form can evolve independently. Syntax experiments, new keyword schemes, different
visual layouts — none of these require migrating stored programs. The IR is the
stable foundation. Content-addressing makes the IR inherently versionable without
external version control — every historical state of the program is a different
root hash.

**Encoding strategy:** The IR is a tagged, tree-structured binary format.
Each node is prefixed with a type tag (1 byte), followed by child references
(content-addressed hashes or inline data for small values). The format is
designed for streaming reads and random access via an index table.

### 2.2 Layer 2: Working Form (LLM-Optimized)

The working form is the syntax that LLMs read and write during development. It
is designed for attention efficiency: keyword-rich, explicitly typed at
boundaries, with visual structure that aids scope recognition.

**Design goals:**

- Keywords as attention anchors (`fn`, `let`, `match`, `handle`, `effect`,
  `perform`, `module`, `type`).
- Explicit type and effect annotations at function boundaries.
- Visual chunk boundaries via indentation and keyword-delimited blocks.
- Redundant semantic markers where they aid reasoning (e.g., `-> ReturnType`
  even when inferrable).
- Named parameters at function boundaries, positional shorthand (`$0`, `$1`)
  available within small closures.

**The working form is not fixed.** Different LLMs, different tasks, and future
research may reveal that alternative syntaxes are more effective. The three-layer
architecture allows the working form to evolve without breaking programs. Multiple
working forms can coexist — the binary IR is the source of truth.

**Grain of authoring.** The typical unit of authoring is a function or module,
not individual AST nodes. When an LLM writes a new feature, it often produces
an entire module as working form text that is parsed and elaborated into the IR.
When editing, the grain is usually a function: the LLM rewrites a function body,
and the system replaces the corresponding subtree in the IR graph. More granular
edits (e.g., changing a single parameter type) are possible but less common in
direct authoring — they are more typical of system-initiated cascading transforms
(see Section 11).

The working form remains critical even though the canonical representation is a
binary graph. It surfaces language semantics in a way that is far easier to
reason about than raw binary AST structure, and it is the form that LLMs are
most effective at producing and understanding.

### 2.3 Layer 3: Review Form (Human-Optimized)

The review form is a human-readable rendering in a familiar syntax. It is
generated mechanically from the binary IR (or equivalently, from the working
form). Humans read it for auditing and review. They do not edit it directly —
feedback is incorporated via the LLM, which modifies the working form.

**Translation targets:**

- **OCaml-like:** Best for reviewers comfortable with ML-family languages.
- **Rust-like:** Maps well to Axiom's strict evaluation and algebraic types.
- **TypeScript-like:** Most accessible for a wide audience.
- **Python-like:** Maximum readability, some loss of type precision in rendering.

The choice of review form syntax is a per-project or per-reviewer configuration.

#### 2.3.1 Review Form Examples

The following examples show the `get_key` function from Section 8.3 rendered
in each target syntax. Effect annotations are rendered as comments, since target
languages lack native effect types. The review form is read-only output — humans
do not edit it directly.

**OCaml-like:**

```ocaml
(* effects: State<Map<string, string>>, Throw<kv_error>, Log *)
let get_key (key : string) : string =
  let () = Log.log `Debug ("get: " ^ key) in
  match Map.lookup key (State.get ()) with
  | Some v -> v
  | None   -> raise (KeyNotFound key)
```

**Rust-like:**

```rust
// effects: State<Map<String, String>>, Throw<KvError>, Log
fn get_key(key: String) -> String {
    Log::log(Level::Debug, format!("get: {}", key));
    match State::get().lookup(&key) {
        Some(v) => v,
        None    => throw(KvError::KeyNotFound(key)),
    }
}
```

**TypeScript-like:**

```typescript
// effects: State<Map<string, string>>, Throw<KvError>, Log
function getKey(key: string): string {
    Log.log("debug", `get: ${key}`);
    const val = State.get().lookup(key);
    if (val !== undefined) {
        return val;
    } else {
        throw new KeyNotFoundError(key);
    }
}
```

**Python-like:**

```python
# effects: State[Map[str, str]], Throw[KvError], Log
def get_key(key: str) -> str:
    log(DEBUG, f"get: {key}")
    match lookup(key, state_get()):
        case Some(v): return v
        case None:    raise KeyNotFound(key)
```

In all four renderings the semantic content is identical — same control flow,
same data dependencies, same effect invocations. Only surface syntax and
naming conventions differ. A reviewer familiar with any of these languages can
audit Axiom code without learning Axiom's working form.

### 2.4 Transformation Pipeline

```
Working Form ──parse──▶ AST ──elaborate──▶ Binary IR ──emit──▶ Working Form
                                               │
                                               ├──emit──▶ Review Form (OCaml-like)
                                               ├──emit──▶ Review Form (Rust-like)
                                               ├──emit──▶ Review Form (TypeScript-like)
                                               │
                                               ├──compile──▶ WebAssembly (deployment artifact)
                                               │
                                               └──snapshot──▶ Image (development artifact)
```

All transformations are deterministic and lossless (between IR and any text form).
The binary IR preserves all information needed to reconstruct any surface syntax.

### 2.5 The Image (Development-Time Representation)

While the binary IR defines the canonical form of a program's semantics, the
**image** is the canonical form of a development workspace. An image is an
archive that bundles the IR fragments together with derived data structures,
indexes, and operational history that make the system usable for interactive
development.

**Image contents:**

```
program.axm-image/
  manifest.json              # image version, root hash, node count, index inventory
  nodes/                     # content-addressed IR fragments (the source of truth)
  indexes/
    graph.db                 # dependency graph, caller/callee relationships
    fulltext.idx             # full-text search index over working form text
    vectors.idx              # vector embeddings for semantic search
    types.idx                # type and effect index for structural queries
  history/
    operations.log           # ordered log of all edits, transforms, and verifications
    snapshots/               # periodic full snapshots (root hash + index state)
  cache/
    compiled.wasm            # pre-compiled deployment artifact
    merkle.db                # cached Merkle tree structure for fast traversal
```

**Key design decisions:**

- **The IR nodes are the source of truth.** Everything else in the image is
  derived and can be regenerated from the nodes alone — at the cost of time.
  The indexes and caches exist to make interactive development fast.
- **Graph indexes are not part of the IR.** The dependency graph, caller maps,
  and other structural indexes are computed from the IR and cached in the image.
  They accelerate graph-structured queries (see Section 11) but are not
  themselves content-addressed or versioned.
- **Operation history replaces version control.** The image records an ordered
  log of every edit, transform, and verification. This serves the role that
  git history serves for text-based projects, but at the semantic level — "changed
  parameter type of `process_request` from `String` to `RequestBody`" rather than
  "modified lines 47-52 of server.axm". Because the IR is content-addressed,
  any historical state can be reconstructed from the operation log and the
  (garbage-collected) node store.
- **Operations are undoable.** Each operation in the log records enough
  information to reverse it — the previous root hash, the replaced node
  hashes, and the prior index state. Undo is a first-class operation, not
  a convention. Additionally, the image periodically records **snapshots**
  (a root hash plus index state) so that navigating to an arbitrary point
  in history does not require replaying the entire operation log from the
  beginning. This is especially important for long-lived projects where
  the operation log may contain thousands of entries.
- **Images are portable.** An image is a self-contained archive (e.g., a
  tarball) that can be shared, inspected with standard tools (`tar tf`),
  and reconstituted on any machine with an Axiom toolchain.

**Image vs. deployment artifact:** The image is the development-time
representation. It contains everything needed to continue working on a program:
source IR, indexes, history, caches. The deployment artifact is a compiled
binary — initially WebAssembly — that contains only what is needed to run.
The two serve fundamentally different purposes and have different lifecycles.

**Future direction: live images.** In testing and staging scenarios, it may be
valuable to deploy a live image rather than a compiled binary. A live image
would retain the full IR graph and indexes at runtime, enabling runtime
introspection, hot-patching, and interactive debugging at the semantic level —
similar to Smalltalk images or Common Lisp cores. This is a future initiative,
not part of the initial design.

---

## 3. Core Calculus

Axiom's core is a polymorphic lambda calculus with algebraic data types, pattern
matching, and linear algebraic effects.

### 3.1 Abstract Syntax (Notation-Independent)

The abstract syntax defines the semantic structure. Both the working form and
review form are projections of this structure.

**Terms:**

```
e ::=
  | x                                        -- variable reference
  | λ x : τ . e                              -- abstraction
  | e₁ e₂                                   -- application
  | let x = e₁ in e₂                        -- let binding
  | letrec { x₁ = e₁ , ... , xₙ = eₙ } in e -- mutual recursion group
  | match e with { p₁ => e₁ | ... | pₙ => eₙ } -- pattern match
  | C e₁ ... eₙ                             -- constructor application
  | perform E.op e₁ ... eₙ                  -- effect operation invocation
  | handle e with H                          -- effect handler (linear)
  | literal                                  -- integer, float, string
  | { l₁ = e₁ , ... , lₙ = eₙ }            -- record construction
  | e.l                                      -- record projection
  | do { s₁ ; ... ; sₙ }                    -- sequencing (sugar)
```

**Patterns:**

```
p ::=
  | x                          -- variable binding
  | _                          -- wildcard
  | C p₁ ... pₙ               -- constructor pattern
  | literal                    -- literal pattern
  | { l₁ = p₁ , ... }         -- record pattern (open matching)
  | p₁ | p₂                   -- or-pattern
```

**Node-attached comments:**

Every expression, pattern, and declaration node in the AST carries an optional
comment annotation. This is a deliberate design choice: because LLMs are the
primary authors and consumers of Axiom code, textual context that explains
intent, records reasoning, or captures design rationale is semantically
meaningful — not mere decoration. Comments are preserved through the parse,
elaboration, and binary IR round-trip.

```
annotation ::= '@#' text '#@'
```

A `@#...#@` comment is a **postfix** annotation: it attaches to the
immediately *preceding* expression, pattern, or declaration node. This means
the comment is parsed as part of the node it follows, not the node that comes
after it. Parentheses can be used to control the attachment point:

```
x + 1 @# I attach to `1` #@
(x + 1) @# I attach to `x + 1` #@
```

Comments survive in the AST and binary IR, enabling tooling to display,
search, and reason about annotated code.

### 3.2 Mutual Recursion

Mutually recursive definitions are grouped explicitly with `letrec`:

```
letrec {
  is_even = fn (n: Nat) -> Bool {
    match n with {
    | Zero => true
    | Succ m => is_odd(m)
    }
  },
  is_odd = fn (n: Nat) -> Bool {
    match n with {
    | Zero => false
    | Succ m => is_even(m)
    }
  }
} in is_even(42)
```

This makes the recursion group explicit, aiding LLM reasoning about termination
and call structure. The compiler uses this grouping for more precise type and
effect inference.

### 3.3 Evaluation Order

Axiom uses **strict (eager) evaluation**. This is a deliberate choice:

- Strict evaluation makes effect ordering syntactically apparent.
- LLMs reason more reliably about eager evaluation — no hidden thunks.
- Compilation to strict targets (Zig, C, WASM) is direct.

Lazy evaluation is available explicitly via `Lazy<T>` with `delay` and `force`
primitives when needed.

---

## 4. Type System

### 4.1 Types

```
τ ::=
  | α                                        -- type variable
  | τ₁ -> τ₂ ! ε                             -- function type with effect
  | T<τ₁, ..., τₙ>                          -- type constructor application
  | { l₁: τ₁, ..., lₙ: τₙ | ρ }            -- extensible record (row poly)
  | forall α . τ                             -- universal quantification
  | rec α . τ                                -- recursive type
```

### 4.2 Effect Types

```
ε ::=
  | pure                                     -- no effects
  | { E₁, ..., Eₙ | ε' }                   -- effect set with row variable
  | ε'                                       -- effect variable (polymorphism)
```

Every function type carries an effect annotation. A function `A -> B ! pure` is
pure. A function `A -> B ! {FileSystem, Throw<ParseError>}` may perform file
operations and throw parse errors.

**Effect polymorphism** allows generic code to propagate effects:

```
fn map<A, B, E>(f: A -> B ! E, xs: List<A>) -> List<B> ! E
```

This means `map` propagates whatever effects `f` has — critical for writing
framework-level generic code.

### 4.3 Type Inference Strategy

**At function boundaries:** Full type and effect annotations are required. This
serves three purposes:

1. It is the contract that callers and other modules depend on.
2. It provides the LLM with a summary of function behavior without reading
   the implementation.
3. It serves as redundant semantic markers that anchor LLM attention.

**Within function bodies:** Types are fully inferred via bidirectional type
inference based on Hindley-Milner, extended with row polymorphism for records
and effects. No annotations are required within a function body, though they
are permitted for clarity.

**Design rationale for mandatory boundary annotations:** We initially considered
full inference (annotation-free code), but concluded that explicit annotations
at boundaries are an *attention optimization* — they give the LLM checkpoints
for validating its understanding as it reads code. The token cost is more than
repaid in reasoning reliability.

### 4.4 Algebraic Data Types

```
type List<A> =
  | Nil
  | Cons(A, List<A>)

type Result<A, E> =
  | Ok(A)
  | Err(E)

type Tree<A> =
  | Leaf
  | Node(Tree<A>, A, Tree<A>)
```

Types are nominal for algebraic data types and structural for records and
effect rows.

---

## 5. Effect System

The effect system is the heart of Axiom. It draws from algebraic effects (Eff,
Koka), Haskell's monadic IO (for explicit effect tracking), and Common Lisp's
condition system (for resumable handlers). Axiom's effect system is designed
specifically for LLM reasoning about system boundaries.

### 5.1 Effect Declarations

An effect is a named interface consisting of typed operations:

```
effect State<S> {
  get: () -> S
  put: (S) -> Unit
}

effect Throw<E> {
  throw: (E) -> Nothing
}

effect FileSystem {
  read_file:  (Path) -> Result<Bytes, IOError>
  write_file: (Path, Bytes) -> Result<Unit, IOError>
  list_dir:   (Path) -> Result<List<Path>, IOError>
}

effect Console {
  print:     (String) -> Unit
  read_line: () -> String
}

effect Log {
  log: (Level, String) -> Unit
}
```

Effect declarations define the *interface* — what operations are available.
They say nothing about implementation. This separation is the foundation of
testability, composability, and framework design.

### 5.2 Performing Effects

Code that needs an effect invokes it with `perform`:

```
fn read_config(path: Path) -> Config ! {FileSystem, Throw<ParseError>} {
  match perform FileSystem.read_file(path) with {
  | Ok(bytes) => parse_config(bytes)
  | Err(e)    => perform Throw.throw(wrap_io_error(e))
  }
}
```

**The effect appears in the function's type signature.** The LLM (and the
compiler) can see at a glance exactly what side effects this function may have.

### 5.3 Handling Effects (Linear Handlers)

Handlers provide implementations for effects, defining the boundary where
abstract operations meet concrete behavior.

**Axiom v1 uses linear (single-shot) handlers only.** The continuation may
be invoked at most once. This is a deliberate restriction that dramatically
simplifies reasoning, compilation, and runtime implementation.

**Handler syntax:**

```
handle read_config("/etc/app.conf") with {
  FileSystem {
    read_file(path) => resume os_read_file(path)
    write_file(path, bytes) => resume os_write_file(path, bytes)
  }
}
```

**Handler semantics:**

1. When `perform E.op(args)` is evaluated inside a `handle` block, control
   transfers to the matching clause in the handler.
2. The handler clause receives the operation's arguments.
3. `resume value` continues the handled computation as if `perform` returned
   `value`. This is the linear continuation — it may be called at most once.
4. Not calling `resume` aborts the computation. This is how `Throw` works —
   the throw handler simply never resumes.
5. The handler may perform its own effects, transform arguments or results,
   or substitute entirely different behavior.

**Why linear only:**

- Multi-shot continuations (calling `resume` multiple times) enable powerful
  patterns like backtracking and nondeterminism, but they require cloning the
  entire continuation — expensive at runtime and difficult for LLMs to reason
  about.
- Single-shot handlers cover the vast majority of practical patterns: error
  handling, dependency injection, resource management, logging, state, and
  async I/O.
- Nondeterminism and backtracking can be modeled explicitly with data structures
  (e.g., `List` for nondeterminism, `Tree` for search) which makes the
  branching visible in the program structure.
- Single-shot compiles directly to efficient code on WASM and native targets
  with no trampolining or continuation cloning.

### 5.4 Handler Composition

Handlers compose by nesting. Inner handlers shadow outer ones for the same effect:

```
handle
  handle my_computation() with {
    State<Int> {
      get()    => resume current_state
      put(s)   => { set current_state = s; resume () }
    }
  }
with {
  Throw<String> {
    return x      => Ok(x)
    throw(msg)    => Err(msg)        @# no resume: abort on throw #@
  }
}
```

### 5.5 Effects and Module Boundaries

**Effects are NOT required to be fully handled at module boundaries.** This is
a deliberate design choice to support framework-style and plugin-style
architectures.

A module *declares* what effects its public functions may perform. Callers see
this in the type signature and decide where and how to handle them:

```
module http_client {
  require effect Net { ... }
  require effect Throw<HttpError>
  require effect Log

  pub fn get(url: Url) -> HttpResponse ! {Net, Throw<HttpError>, Log} {
    ...
  }
}
```

**The rule is:** effects must be handled *before program termination*, not at
any particular syntactic boundary. This enables:

- **Frameworks** that define effects and let user code handle them.
- **Plugins** that perform effects defined by the host.
- **Middleware** that intercepts and transforms effects flowing through a stack.
- **Test harnesses** that install mock handlers around production code.

The compiler warns (but does not error) if a module exposes a large number of
unhandled effects, as this may indicate a design issue. The program entry point
must handle all effects — this is the only hard boundary.

### 5.6 Common Effect Patterns

**Error handling** — `Throw` handler aborts by not resuming:

```
handle risky_operation() with {
  Throw<AppError> {
    return x    => Ok(x)
    throw(err)  => Err(err)
  }
}
```

**Dependency injection** — handler provides an implementation:

```
handle app_logic() with {
  Database {
    query(sql)      => resume real_db.execute(sql)
    transaction(f)  => resume real_db.with_transaction(f)
  }
}

@# In tests: #@
handle app_logic() with {
  Database {
    query(sql)      => resume mock_db.record_and_return(sql)
    transaction(f)  => resume f()
  }
}
```

**Resource management** — handler ensures cleanup:

```
handle file_processing() with {
  FileSystem {
    read_file(path) => {
      let handle = os_open(path)
      let result = os_read(handle)
      os_close(handle)        @# cleanup always runs #@
      resume result
    }
  }
}
```

**Middleware** — handler transforms behavior:

```
fn with_caching<A>(computation: () -> A ! {FileSystem, E}) -> A ! {FileSystem, E} {
  let cache = Map.empty()
  handle computation() with {
    FileSystem {
      read_file(path) =>
        match Map.lookup(path, cache) with {
        | Some(cached) => resume(Ok(cached))
        | None => {
            let result = perform FileSystem.read_file(path)
            match result with {
            | Ok(bytes) => { Map.insert(path, bytes, cache); resume(result) }
            | err       => resume(err)
            }
          }
        }
    }
  }
}
```

### 5.7 Standard Effects

The following effects are provided by the language and runtime as built-in
interfaces. They form the foundation for frameworks and applications:

```
-- Error handling
effect Throw<E> {
  throw: (E) -> Nothing
}

-- Mutable state
effect State<S> {
  get: () -> S
  put: (S) -> Unit
}

-- Console I/O
effect Console {
  print:     (String) -> Unit
  read_line: () -> String
}

-- File system
effect FileSystem {
  read_file:  (Path) -> Result<Bytes, IOError>
  write_file: (Path, Bytes) -> Result<Unit, IOError>
  delete_file: (Path) -> Result<Unit, IOError>
  list_dir:   (Path) -> Result<List<Path>, IOError>
  file_exists: (Path) -> Bool
}

-- Logging
effect Log {
  log: (Level, String) -> Unit
}

-- Time
effect Clock {
  now: () -> Timestamp
  sleep: (Duration) -> Unit
}

-- Async (structured concurrency)
effect Async {
  spawn: (() -> A ! E) -> Task<A>
  await: (Task<A>) -> A
  yield: () -> Unit
}

-- Random
effect Random {
  random_int: (Int, Int) -> Int
  random_float: () -> Float64
}
```

---

## 6. Tail Calls and Iteration

### 6.1 Guaranteed Tail Call Optimization

Axiom guarantees that all calls in tail position are optimized into jumps. This
is a **semantic guarantee**, not merely a compiler optimization. Programs may
rely on tail calls for unbounded iteration.

### 6.2 Tail Position Definition

A call is in **tail position** if it is the last computation before the
enclosing function returns. Specifically:

- The body of a function `fn (...) { ... }`.
- The last expression in a `do { ... ; e }` block.
- Each branch of a `match` expression in tail position.
- The `e₂` in `let x = e₁ in e₂` when the `let` is in tail position.
- A `resume value` in a handler clause (since linear handlers resume exactly
  once, this is effectively a tail call to the continuation).

### 6.3 Idiomatic Iteration

The standard iteration pattern is tail-recursive functions with accumulators:

```
fn sum(xs: List<Int>) -> Int ! pure {
  fn go(acc: Int, rest: List<Int>) -> Int ! pure {
    match rest with {
    | Nil        => acc
    | Cons(h, t) => go(acc + h, t)    @# tail call, compiles to jump #@
    }
  }
  go(0, xs)
}
```

The compiler transforms this into a simple loop in the WASM output. No stack
growth, no allocation.

### 6.4 Tail Calls and Effect Handlers

Linear handlers interact cleanly with tail calls. Because `resume` is called
at most once, it can be compiled as a direct jump back to the call site:

```
handle count_loop(1000000) with {
  State<Int> {
    get()  => resume current
    put(s) => { set current = s; resume () }
  }
}

fn count_loop(n: Int) -> Int ! {State<Int>} {
  match n with {
  | 0 => perform State.get()
  | _ => {
      perform State.put(perform State.get() + 1)
      count_loop(n - 1)    @# tail call, bounded stack even through handler #@
    }
  }
}
```

### 6.5 The `do` Block

Sequential effects use `do` blocks. The last expression is in tail position:

```
do {
  perform Log.log(Info, "starting");
  let data = perform FileSystem.read_file(path);
  process(data)    @# tail position #@
}
```

`do` blocks are sugar for nested `let` bindings with discarded results:

```
let _ = perform Log.log(Info, "starting") in
let data = perform FileSystem.read_file(path) in
process(data)
```

---

## 7. Module System

### 7.1 Module Structure

A module is an organizational unit and a public API boundary. It declares
its required effects, its type definitions, and its exported functions:

```
module json_parser {
  require effect FileSystem
  require effect Throw<ParseError>

  type JsonValue =
    | JsonNull
    | JsonBool(Bool)
    | JsonNumber(Float64)
    | JsonString(String)
    | JsonArray(List<JsonValue>)
    | JsonObject(Map<String, JsonValue>)

  type ParseError =
    | UnexpectedToken(String, Int)
    | UnexpectedEof
    | InvalidEscape(String)

  pub fn parse_string(input: String) -> JsonValue ! {Throw<ParseError>} {
    ...
  }

  pub fn parse_file(path: Path) -> JsonValue ! {FileSystem, Throw<ParseError>} {
    match perform FileSystem.read_file(path) with {
    | Ok(bytes)  => parse_string(bytes_to_string(bytes))
    | Err(e)     => perform Throw.throw(io_to_parse_error(e))
    }
  }

  @# Private helper, not exported #@
  fn parse_value(tokens: List<Token>, pos: Int) -> (JsonValue, Int) ! {Throw<ParseError>} {
    ...
  }
}
```

### 7.2 Module as LLM Reasoning Boundary

The `pub` interface of a module is designed so that an LLM can reason about
the module's behavior by reading **only** the public signatures:

```
@# An LLM sees this and knows:
   json_parser needs file access and may throw ParseError.
   It provides parse_string (pure except for errors) and
   parse_file (needs filesystem, may error). #@
```

This is the fundamental unit of abstraction. The LLM can compose modules
without reading their implementations.

### 7.3 Module Imports and Composition

```
module app {
  import json_parser
  import http_client

  require effect Console
  require effect Net
  require effect FileSystem
  require effect Log

  pub fn main() -> Unit ! {Console, Net, FileSystem, Log} {
    let response = http_client.get("https://api.example.com/data")
    let data = json_parser.parse_string(response.body)
    perform Console.print(format_data(data))
  }
}
```

### 7.4 Effect Re-Export and Frameworks

Modules can define new effects and export them, enabling framework patterns:

```
module web_framework {
  @# Framework-defined effects that user code performs #@
  effect Route {
    get:  (String, Handler) -> Unit
    post: (String, Handler) -> Unit
  }

  effect Request {
    param:  (String) -> Option<String>
    header: (String) -> Option<String>
    body:   () -> Bytes
  }

  effect Response {
    status:  (Int) -> Unit
    send:    (String) -> Unit
    json:    (JsonValue) -> Unit
  }

  type Handler = () -> Unit ! {Request, Response, Throw<HttpError>}

  @# Framework provides the handler that wires effects to the runtime #@
  pub fn serve(port: Int, setup: () -> Unit ! {Route}) -> Unit ! {Net, Log} {
    ...
  }
}
```

User code then performs framework effects without knowing the implementation:

```
module my_app {
  import web_framework as web

  pub fn routes() -> Unit ! {web.Route} {
    perform web.Route.get("/hello", fn () -> Unit ! {web.Request, web.Response} {
      let name = perform web.Request.param("name")
      perform web.Response.send("Hello, " ++ name.unwrap_or("world"))
    })
  }
}
```

---

## 8. Working Form Syntax (v1)

The initial working form is a keyword-rich syntax drawing from Rust and OCaml,
optimized for LLM attention patterns.

### 8.1 Design Rationale

The syntax prioritizes:

1. **Keyword anchors.** `fn`, `let`, `match`, `handle`, `perform`, `effect`,
   `type`, `module`, `pub`, `with` — each keyword immediately signals the
   kind of construct, reducing ambiguity for attention.

2. **Braces for scope.** `{ }` delimit blocks. Unlike parentheses (which are
   visually identical), braces are typographically distinct and signal "scope
   boundary" to the LLM.

3. **Explicit types at boundaries.** Every `fn` and `pub` declaration has a
   full signature. Within function bodies, types are inferred.

4. **Effect annotations with `!`.** The `!` separator in function types
   (`-> ReturnType ! {Effects}`) is a compact, visually distinct marker.

5. **Pattern matching with `|`.** Each branch is visually distinct and
   scannable.

6. **`resume` keyword for handler continuations.** Makes handler flow explicit
   without exposing continuation variables.

### 8.2 Grammar Summary

```
-- Top-level structure

program     ::= decl*

decl        ::= require_decl | type_decl | effect_decl | fn_decl | module_decl

module_decl ::= 'pub'? 'module' IDENT '{' decl* '}'

require_decl ::= 'require' 'effect' type_expr

type_decl   ::= 'type' type_head '=' variant ('|' variant)*
type_head   ::= IDENT type_params?
variant     ::= CTOR_IDENT | CTOR_IDENT '(' type_expr (',' type_expr)* ')'

effect_decl ::= 'effect' type_head '{' op_decl* '}'
op_decl     ::= IDENT ':' '(' type_expr (',' type_expr)* ')' '->' type_expr
             -- Operations are comma-separated within the effect body.

fn_decl     ::= 'pub'? 'fn' IDENT type_params? '(' params ')' ('->' type_expr ('!' effect_set)?)? '{' expr '}'
             -- Return type and effect annotations are optional in the implementation.

-- Type-level

type_params ::= '<' IDENT (',' IDENT)* '>'
             -- Type parameter list on declarations, e.g. <A, B>

effect_set  ::= 'pure' | '{' type_expr (',' type_expr)* '}'
             -- 'pure' means no effects; braces list concrete effect types

type_expr   ::= IDENT                                    -- type variable or simple type
              | IDENT '<' type_expr (',' type_expr)* '>' -- parameterized type
              | '(' type_expr (',' type_expr)* ')'       -- tuple type
              | '(' params ')' '->' type_expr '!' effect_set  -- function type

-- Value-level expressions

expr        ::= let_expr
              | letrec_expr
              | match_expr
              | handle_expr
              | do_expr
              | perform_expr
              | fn_expr
              | if_expr
              | app_expr
              | record_expr
              | expr '.' IDENT     -- field projection
              | literal
              | IDENT

app_expr    ::= expr '(' args ')'  -- function application

let_expr    ::= 'let' pattern (':' type_expr)? '=' expr 'in' expr
              | 'let' pattern (':' type_expr)? '=' expr
              -- Second form is used inside 'do' blocks (see note below)

letrec_expr ::= 'letrec' '{' letrec_bind (',' letrec_bind)* '}' 'in' expr
letrec_bind ::= IDENT '(' params ')' ':' type_expr '=' expr
             -- Each bind names a mutually recursive function with a full signature

match_expr  ::= 'match' expr 'with' '{' ('|' pattern '=>' expr)+ '}'

handle_expr ::= 'handle' expr 'with' '{' handler_clause+ '}'
handler_clause ::= CTOR_IDENT '{' op_handler+ '}'
             -- Effect names are uppercase-start (CtorIdent), e.g. State, Log.
op_handler  ::= IDENT '(' params ')' '=>' expr
              | 'return' IDENT '=>' expr
              -- 'return' branch handles the normal completion value (see note below)

perform_expr ::= 'perform' IDENT '.' IDENT '(' args ')'
              -- 'perform EffectName.operation(arg1, arg2)'

do_expr     ::= 'do' '{' (stmt ';')* expr '}'
stmt        ::= 'let' pattern '=' expr | expr

fn_expr     ::= 'fn' '(' params ')' ('->' type_expr '!' effect_set)? '{' expr '}'
             -- Anonymous function; type annotation optional in body position

if_expr     ::= 'if' expr '{' expr '}' 'else' '{' expr '}'
             -- Both branches are required; if_expr is always an expression.
             -- Sugar for: match expr with { | true => e1 | false => e2 }

record_expr ::= '{' (field_assign (',' field_assign)*)? '}'
              | '{' expr 'with' field_assign (',' field_assign)* '}'
              -- First form constructs a record; second form copies with updates.
field_assign ::= IDENT ':' expr

-- Parameters and arguments

params      ::= (param (',' param)*)?
param       ::= IDENT ':' type_expr

args        ::= (expr (',' expr)*)?

-- Literals

literal     ::= INT_LIT | FLOAT_LIT | STRING_LIT | BOOL_LIT | '()'
             -- INT_LIT:    decimal integer (e.g. 42) or 0x hex (e.g. 0xFF)
             -- FLOAT_LIT:  decimal float with required dot (e.g. 3.14, 1.0e-5)
             -- STRING_LIT: double-quoted UTF-8 with escapes (\n \t \\ \" \uXXXX)
             -- BOOL_LIT:   'true' | 'false'
             -- '()':       Unit value

-- Patterns

pattern     ::= '_'                                              -- wildcard
              | IDENT                                            -- variable binding
              | CTOR_IDENT ('(' pattern (',' pattern)* ')')?    -- constructor
              | literal                                          -- literal match
              | '{' (field_pat (',' field_pat)*)? ('..')? '}'  -- record pattern
              | pattern '|' pattern                             -- or-pattern
              | '(' pattern ')'                                  -- grouping

field_pat   ::= IDENT '=' pattern   -- explicit field binding
              | IDENT               -- shorthand: field name bound as variable
             -- '..' in record patterns means "open" — remaining fields ignored.
             -- Without '..', the pattern must name every field (closed match).

-- Node-attached comments (postfix)

comment     ::= '@#' TEXT '#@'
             -- Postfix annotation: attaches to the immediately preceding
             -- expression, pattern, or declaration. Use parentheses to control
             -- attachment. Preserved in the AST and binary IR. See Section 3.1.
```

**Note — `let` in `do` blocks.** Inside a `do` block, `let` bindings omit the
`in` keyword. Their scope extends to the end of the enclosing block. The parser
distinguishes the two forms by position: a `let` that is not the final item in
the block is a statement binding; the final item must be a plain expression
(not `let`).

```
do {
  let x = foo();       @# statement binding: no 'in' #@
  let y = bar(x);      @# statement binding: no 'in' #@
  baz(x, y)            @# final expression: value of the block #@
}
```

**Note — handler `return` branch.** The optional `return` branch in a handler
clause runs when the handled computation completes normally — that is, when it
produces a value without invoking any operation handled by this handler. The
branch names that result value and may transform it. If omitted, the default is
the identity (`return v => v`).

```
handle compute() with {
  State {
    get()  => resume(current_state, current_state)
    put(s) => resume((), s)
    return v => v    @# computation finished; v is its result value #@
  }
}
```

**Note — or-patterns.** Or-patterns bind the same set of variables in both
branches. All bound names must have the same type in each branch.

```
match shape with {
| Circle(r) | Ellipse(r, r) => area_approx(r)
| _                         => 0.0
}
```

### 8.3 Complete Example: Key-Value Store

```
module kv_store {
  require effect State<Map<String, String>>
  require effect Throw<KVError>
  require effect Log

  type KVError =
    | KeyNotFound(String)
    | InvalidKey(String)

  pub fn get_key(key: String) -> String ! {State<Map<String, String>>, Throw<KVError>, Log} {
    do {
      perform Log.log(Debug, "get: " ++ key);
      match Map.lookup(key, perform State.get()) with {
      | Some(v) => v
      | None    => perform Throw.throw(KeyNotFound(key))
      }
    }
  }

  pub fn set_key(key: String, value: String) -> Unit ! {State<Map<String, String>>, Log} {
    do {
      perform Log.log(Debug, "set: " ++ key ++ " = " ++ value);
      perform State.put(Map.insert(key, value, perform State.get()))
    }
  }

  pub fn delete_key(key: String) -> Unit ! {State<Map<String, String>>, Throw<KVError>, Log} {
    do {
      perform Log.log(Debug, "del: " ++ key);
      match Map.lookup(key, perform State.get()) with {
      | Some(_) => perform State.put(Map.remove(key, perform State.get()))
      | None    => perform Throw.throw(KeyNotFound(key))
      }
    }
  }
}
```

---

## 9. Compilation to WebAssembly

### 9.1 Strategy

The compiler targets WebAssembly directly, without intermediate LLVM or C
emission. This keeps the compiler simple and the output inspectable.

**Compilation pipeline:**

```
Working Form → AST → Type Check + Effect Check → Binary IR → WASM Codegen → .wasm
```

### 9.2 Effect Compilation

Linear (single-shot) effects compile efficiently to WASM:

1. **Direct style with evidence passing.** Each effect in scope is compiled
   as an implicit parameter — a struct of function pointers (the handler
   implementations). `perform E.op(args)` becomes an indirect call through
   the evidence struct.

2. **Handler inlining.** When the handler is statically known (the common case),
   the indirect call is inlined to a direct call, eliminating all overhead.

3. **Abort effects (Throw).** Effects where the handler never resumes are
   compiled as WASM exceptions or as early-return control flow.

4. **State effects.** `State<S>` compiles to a mutable local or a heap
   allocation, depending on escape analysis.

### 9.3 Tail Calls

WASM has a tail call proposal (now widely supported). Axiom uses `return_call`
and `return_call_indirect` instructions for guaranteed TCO.

For WASM engines without tail call support, the compiler falls back to a
trampoline: tail calls return a thunk, and a top-level loop repeatedly
evaluates thunks until a final value is produced.

### 9.4 Memory Management

The Zig runtime provides garbage collection for WASM linear memory:

- **Primary strategy:** Incremental mark-and-sweep with a generational nursery.
- **Optimization:** Region inference and escape analysis move short-lived
  values to the stack or handler-scoped arenas, reducing GC pressure.
- **Handler arenas:** Values allocated within a handler scope that don't
  escape are freed in bulk when the handler returns, avoiding GC entirely.

### 9.5 Future Backend Targets

The binary IR is designed to support additional backends:

- **ARM64 / AMD64:** Direct machine code generation. The IR's SSA-like
  structure maps well to register allocation.
- **LLVM IR:** When LLVM's optimization passes are worth the compilation
  time overhead, particularly for numerically intensive code.

---

## 10. Primitive Types and Operations

### 10.1 Built-in Types

```
Int         -- arbitrary precision integer
Float64     -- IEEE 754 double
Bytes       -- immutable byte sequence
String      -- UTF-8 string (backed by Bytes)
Bool        -- True | False
Unit        -- single-valued type ()
Nothing     -- empty type (uninhabited, used for divergence)
Char        -- Unicode scalar value
```

### 10.2 Built-in Type Constructors

```
List<A>          -- singly-linked persistent list
Map<K, V>        -- persistent ordered map
Set<A>           -- persistent ordered set
Option<A>        -- None | Some(A)
Result<A, E>     -- Ok(A) | Err(E)
Tuple<A, B>      -- (A, B)  -- extends to arbitrary arity
Task<A>          -- async task handle
Lazy<A>          -- explicitly delayed computation
```

### 10.3 Primitive Operations

All primitives are pure unless noted. Division performs `Throw<DivByZero>`:

```
-- Arithmetic
(+)  : (Int, Int) -> Int ! pure
(-)  : (Int, Int) -> Int ! pure
(*)  : (Int, Int) -> Int ! pure
(/)  : (Int, Int) -> Int ! {Throw<DivByZero>}
(%)  : (Int, Int) -> Int ! {Throw<DivByZero>}

-- Comparison (for ordered types)
(==) : forall A. (A, A) -> Bool ! pure
(!=) : forall A. (A, A) -> Bool ! pure
(<)  : forall A [Ord]. (A, A) -> Bool ! pure
(>)  : forall A [Ord]. (A, A) -> Bool ! pure

-- String
(++) : (String, String) -> String ! pure

-- Collections
List.map    : forall A B E. (A -> B ! E, List<A>) -> List<B> ! E
List.filter : forall A E. (A -> Bool ! E, List<A>) -> List<A> ! E
List.fold   : forall A B E. (B, (B, A) -> B ! E, List<A>) -> B ! E
Map.lookup  : forall K V. (K, Map<K, V>) -> Option<V> ! pure
Map.insert  : forall K V. (K, V, Map<K, V>) -> Map<K, V> ! pure
Map.remove  : forall K V. (K, Map<K, V>) -> Map<K, V> ! pure
```

---

## 11. System Interaction: The MCP Server

Axiom is a programming *system*, not just a language with a compiler. The
primary interface for LLM interaction is an **MCP (Model Context Protocol)
server** that exposes the image's full capabilities as structured tool calls.
While the LLM writes code primarily as text in the working form, reading,
reasoning about, and refactoring code operates through the MCP server, which
provides targeted access to the IR graph and its indexes without dumping
entire modules into context.

The MCP server encourages a **graph-shaped workflow**: rather than treating
a program as a collection of text files to be read and written sequentially,
the LLM interacts with a live graph of content-addressed fragments. It can
query upstream dependencies, trace effect propagation, apply cascading
transforms, and validate invariants — all through structured tool calls that
operate on the IR graph and leverage the image's cached indexes.

The server provides three categories of tools: **query** (extract targeted
information from the IR graph), **transform** (apply structured modifications
that cascade through the graph), and **verify** (check invariants and
properties). These tools operate on the binary IR within the image and return
compact, semantically rich responses.

### 11.1 Design Rationale

The problem these tools solve is context pollution. When an LLM needs
to answer "what effects does this module require?" the naive approach is
to read the entire module source. For a large module, this consumes
thousands of tokens — most of which are irrelevant function bodies. A
structured query returns just the answer: a list of effect signatures.

The principle: **write code as text, inspect and refactor code via graph
operations.** The working form is optimized for authoring. The MCP server
is optimized for targeted retrieval, graph traversal, and mechanical
transformation.

### 11.2 Query Commands

Query commands extract information from the binary IR without emitting
full source. Responses are compact and structured.

**`query effects <module>`** — List all effects a module requires and
which functions perform each one:

```
> query effects kv_store

module kv_store requires:
  State<Map<String, String>>
    performed by: get_key, set_key, delete_key
  Throw<KVError>
    performed by: get_key, delete_key
  Log
    performed by: get_key, set_key, delete_key
```

**`query signature <function>`** — Return the full type and effect
signature of a function without its body:

```
> query signature kv_store.get_key

pub fn get_key(key: String) -> String ! {State<Map<String, String>>, Throw<KVError>, Log}
```

**`query callers <function>`** — List all functions that call a given
function, with their effect signatures:

```
> query callers json_parser.parse_string

  json_parser.parse_file : (Path) -> JsonValue ! {FileSystem, Throw<ParseError>}
  app.load_config        : (Path) -> Config ! {FileSystem, Throw<AppError>, Log}
```

**`query dependents <type|effect>`** — List all modules and functions that
reference a given type or effect:

```
> query dependents KVError

type KVError referenced by:
  kv_store         : defines KVError
  kv_store.get_key : Throw<KVError>
  kv_store.delete_key : Throw<KVError>
  app.main         : handles Throw<KVError>
  kv_store_test    : handles Throw<KVError>
```

**`query effect-flow <entry-point>`** — Trace the complete effect
propagation from an entry point to all handlers, showing where each
effect is ultimately handled:

```
> query effect-flow app.main

app.main performs:
  Console         → handled at: app.main (line 42, runtime handler)
  Net             → handled at: app.main (line 42, runtime handler)
  FileSystem      → handled at: app.main (line 42, runtime handler)
  Log             → handled at: app.with_logging (line 38)

  via http_client.get:
    Net           → (see above)
    Throw<HttpError> → handled at: app.main (line 30)
    Log           → (see above)

  via json_parser.parse_string:
    Throw<ParseError> → handled at: app.main (line 33)

  ✓ All effects handled before program termination.
```

**`query unhandled`** — Find all effect `perform` sites that are not
covered by any handler in the current call graph from the entry point:

```
> query unhandled

No unhandled effects. All paths from app.main handle all performed effects.
```

or:

```
> query unhandled

WARNING: 2 unhandled effect sites:
  monitoring.emit_metric performs Metrics.record
    not handled on path: app.main → process_request → emit_metric
  cache.evict performs Log.log
    not handled on path: app.main → handle_request → evict (Log handler only installed on admin path)
```

**`query pattern-coverage <function>`** — Check exhaustiveness of all
match expressions in a function:

```
> query pattern-coverage json_parser.parse_value

match at line 45: EXHAUSTIVE (JsonNull, JsonBool, JsonNumber, JsonString, JsonArray, JsonObject)
match at line 72: NON-EXHAUSTIVE — missing: JsonObject with empty map
```

**`query interface <module>`** — Return only the public API of a module:
types, effects, and pub function signatures. No implementations. This is
the primary command for understanding a module's role in the system
without reading its code:

```
> query interface http_client

module http_client
  requires: Net, Throw<HttpError>, Log

  type HttpResponse = HttpResponse {
    status:  Int,
    headers: Map<String, String>,
    body:    Bytes
  }

  type HttpError =
    | ConnectionFailed(String)
    | Timeout(Duration)
    | StatusError(Int, String)

  pub fn get(url: Url) -> HttpResponse ! {Net, Throw<HttpError>, Log}
  pub fn post(url: Url, body: Bytes) -> HttpResponse ! {Net, Throw<HttpError>, Log}
  pub fn head(url: Url) -> HttpResponse ! {Net, Throw<HttpError>, Log}
```

### 11.2.1 Graph-Structured Queries

Beyond the targeted queries above, the MCP server supports general
**graph-structured queries** that traverse the IR's dependency graph and
return subgraphs of content-addressed nodes. These queries leverage the
image's cached graph indexes for performance.

The key insight is that reads in Axiom are graph-shaped, not file-shaped.
Instead of "show me the file containing function F," the natural question
is "show me the transitive closure of all nodes affected by changing
parameter P of function F." The MCP server does not interpret natural
language — queries are structured graph traversals with explicit anchors,
edge types, directions, and collection points.

**Query structure:** A graph query specifies:

- An **anchor** node (identified by label and properties).
- A sequence of **traversals**, each specifying an edge type, direction,
  optional filter, and a collection name for matched nodes.
- Traversals can be **nested** (`follow`) to walk multiple edge types
  in a single query.
- A `depth` field controls whether traversal is single-hop or transitive.
- A `terminal_when` field specifies stopping conditions for transitive
  traversals.

**Edge types in the IR graph include:**

- `HAS_PARAM` — function → parameter definition
- `HAS_ARGUMENT` — call site → argument expression
- `CALLS` — function → function (or call site → callee)
- `DATA_FLOW` — expression → expression (value flows from source to sink)
- `HAS_TYPE` — node → type annotation
- `PERFORMS` — function → effect operation
- `HANDLES` — handler → effect operation
- `REFERENCES` — node → type, effect, or module

**Worked example: analyzing a parameter type change.** Suppose the user
asks the LLM to change the `loc` parameter of `get_location` from
`Vec<Float64>` to `Point2D`. The LLM needs the complete transitive closure
of affected nodes — the parameter definition, every call site, every
argument passed as `loc`, and every origin of those arguments recursively
until it hits literals or external boundaries. This should be a single
query, not an iterative exploration:

```json
{
  "tool": "query_codebase_graph",
  "arguments": {
    "anchor": {
      "label": "Function",
      "properties": { "name": "get_location" }
    },
    "traverse": [
      {
        "edge": "HAS_PARAM",
        "direction": "out",
        "filter": { "name": "loc" },
        "collect": "param"
      },
      {
        "edge": "CALLS",
        "direction": "in",
        "collect": "call_sites",
        "follow": {
          "edge": "HAS_ARGUMENT",
          "direction": "out",
          "filter": { "param": "loc" },
          "collect": "arguments",
          "follow": {
            "edge": "DATA_FLOW",
            "direction": "in",
            "depth": "transitive",
            "collect": "origins",
            "terminal_when": "no_incoming_data_flow"
          }
        }
      }
    ]
  }
}
```

The response returns all collected nodes across the full transitive
closure, each identified by its content-addressed hash. The traversal
follows data flow across function boundaries — when an argument traces
back to a caller's parameter, the query continues through that caller's
call sites and their origins, recursively, until every path terminates
at a node with no incoming data flow:

```json
{
  "param": [{ "id": "a3f8...", "name": "loc", "type": "Vec<Float64>" }],
  "call_sites": [
    { "id": "b7c2...", "function": "render_map", "line": 34 },
    { "id": "d1e5...", "function": "find_nearest", "line": 12 },
    { "id": "f9a0...", "function": "test_get_location", "line": 7 }
  ],
  "arguments": [
    { "id": "c4d1...", "call_site": "b7c2...", "expr": "user_pos" },
    { "id": "e6f3...", "call_site": "d1e5...", "expr": "coords" },
    { "id": "a2b8...", "call_site": "f9a0...", "expr": "[1.0, 2.0]" }
  ],
  "origins": [
    { "id": "1a2b...", "kind": "parameter", "name": "user_pos",
      "function": "render_map",
      "upstream": {
        "call_sites": [
          { "id": "aa01...", "function": "handle_click", "line": 18 },
          { "id": "aa02...", "function": "pan_viewport", "line": 7 }
        ],
        "arguments": [
          { "id": "ab01...", "call_site": "aa01...", "expr": "click_pos" },
          { "id": "ab02...", "call_site": "aa02...", "expr": "center" }
        ],
        "origins": [
          { "id": "ac01...", "kind": "external_boundary",
            "source": "event_handler", "type": "deserialize(ClickEvent).pos",
            "function": "handle_click" },
          { "id": "ac02...", "kind": "field_access",
            "expr": "viewport.center", "function": "pan_viewport",
            "upstream": {
              "origins": [
                { "id": "ac03...", "kind": "parameter", "name": "viewport",
                  "function": "pan_viewport",
                  "upstream": { "...": "continues transitively" }
                }
              ]
            }
          }
        ]
      }
    },
    { "id": "3c4d...", "kind": "let_binding", "name": "coords",
      "expr": "List.map(points, fn(p) -> [p.x, p.y])",
      "function": "find_nearest" },
    { "id": "5e6f...", "kind": "literal", "value": "[1.0, 2.0]",
      "function": "test_get_location" }
  ]
}
```

From this single response, the LLM has the complete picture and can
categorize each terminal node to plan the refactor:

- **Literals** (like `[1.0, 2.0]`): replace with a `Point2D` constructor
  directly.
- **Let bindings with intermediate expressions** (like `coords` computed
  via `List.map`): the binding's type changes, and the expression may
  need syntactic adjustment (e.g., `[p.x, p.y]` becomes `Point2D(p.x,
  p.y)`).
- **External boundaries** (like `deserialize(ClickEvent).pos`): indicate
  that the type change has reached an API or serialization boundary. The
  LLM must decide whether to change the external format (updating the
  `ClickEvent` schema) or insert a conversion at the boundary.
- **Intermediate parameters** that appear along the path (like `user_pos`
  in `render_map`) are not terminal — the query has already traversed
  through them. Their type annotations will need updating, but the LLM
  does not need to issue follow-up queries to discover their callers.

Node IDs serve as stable handles for all subsequent write operations.
The LLM uses these IDs to target specific nodes when submitting working
form replacements via the MCP server.

**Initial discovery.** For cases where the LLM does not already know
which node to anchor on — e.g., "find all functions related to
authentication" — the image's full-text and vector indexes support
keyword and semantic search as a discovery step. These searches return
node IDs that can then be used as anchors for structured graph queries.
The MCP server itself performs no AI inference; the indexes are
pre-computed and cached in the image.

### 11.3 Transform Commands

Transform commands apply structured modifications to the IR. They are
mechanical, deterministic operations — not code generation. The LLM
invokes a transform and then reviews the result in working form.

**`transform add-effect-logging <module> <effect>`** — Insert
`perform Log.log(...)` calls before every `perform` of the specified
effect in the module. Automatically adds `Log` to the function's
effect set if not already present:

```
> transform add-effect-logging kv_store FileSystem

Modified 3 functions:
  get_key:    added Log.log before FileSystem.read_file (1 site)
  set_key:    added Log.log before FileSystem.write_file (1 site)
  delete_key: added Log.log before FileSystem.delete_file (1 site)
Log effect added to: set_key (already present on get_key, delete_key)
```

**`transform mock-effects <module> <target-module>`** — Generate a test
module that wraps every pub function from `target-module` with handlers
that stub out all required effects:

```
> transform mock-effects kv_store_test kv_store

Generated kv_store_test with mock handlers for:
  State<Map<String, String>> → in-memory Map
  Throw<KVError>             → captures error in Result
  Log                        → discards all log messages

3 test stubs generated: test_get_key, test_set_key, test_delete_key
```

The generated stubs are working form text that the LLM then edits to
add actual test logic.

**`transform rename <old-name> <new-name>`** — Rename a function, type,
effect, or module across the entire project. Updates all references in
the IR:

```
> transform rename kv_store.get_key kv_store.lookup

Renamed kv_store.get_key → kv_store.lookup
Updated 4 call sites:
  app.main (line 15)
  app.handle_request (line 42)
  kv_store_test.test_get_key (line 8)
  cache.warm (line 23)
```

**`transform extract-function <module> <function> <line-range> <new-name>`** —
Extract a section of a function body into a new private function. The
system infers the parameters and return type from the extracted code's
free variables and produces:

```
> transform extract-function kv_store get_key 5-12 validate_and_lookup

Extracted kv_store.validate_and_lookup:
  fn validate_and_lookup(key: String, store: Map<String, String>)
    -> String ! {Throw<KVError>}
  
  get_key now calls validate_and_lookup at line 5.
```

**`transform inline-handler <module> <function> <effect>`** — When a
handler is trivial (e.g., a `Log` handler that just discards), inline
the handler's behavior directly into the call sites, removing the
`handle` block and simplifying the code.

### 11.4 Verify Commands

Verify commands check properties of the codebase and return pass/fail
results with targeted diagnostics.

**`verify effects`** — Check that all effects are handled on every
path from the program entry point:

```
> verify effects

✓ All effects handled. 14 perform sites across 6 modules, all covered.
```

**`verify exhaustive`** — Check that all pattern matches in the project
are exhaustive:

```
> verify exhaustive

✓ 23 match expressions checked, all exhaustive.
```

or:

```
> verify exhaustive

✗ 2 non-exhaustive matches:
  json_parser.parse_value line 72: missing JsonObject({})
  router.dispatch line 15: missing Method.Patch
```

**`verify types`** — Full type check of the project. Returns only
errors, not the full elaboration:

```
> verify types

✗ 1 type error:
  app.main line 22: expected String, got Int
    in argument 1 of kv_store.set_key
    set_key expects (String, String), got (String, Int)
```

**`verify tail-calls <function>`** — Confirm that a recursive function's
recursive calls are all in tail position:

```
> verify tail-calls kv_store.sum

✓ sum.go: recursive call at line 4 is in tail position.
  Will compile to loop.
```

or:

```
> verify tail-calls tree.map_values

✗ tree.map_values: recursive call at line 8 is NOT in tail position.
  The call `map_values(right)` is inside `Node(mapped_left, f(v), ...)`.
  Suggestion: use CPS or accumulator-passing style.
```

**`verify unused`** — Find unused functions, types, and imports:

```
> verify unused

2 unused items:
  kv_store.InvalidKey — type variant never constructed or matched
  http_client.head    — pub function never called from any module
```

### 11.5 Interaction Model

The MCP server defines the expected workflow:

1. **Author:** The LLM writes code in the working form text and submits it
   to the MCP server, which parses and elaborates it into the IR graph.
   The typical grain is a function or module. For new programs or large
   features, the LLM may produce an entire module as text in a single
   operation.

2. **Inspect:** When the LLM needs to understand existing code for
   composition, refactoring, or debugging, it uses query tools to
   retrieve precisely the information it needs. `query interface` to
   understand a module's API. `query effect-flow` to trace side effects.
   `query upstream` and `query surface` to understand the blast radius
   of a change. This avoids reading entire modules into context.

3. **Transform:** For mechanical refactoring operations (rename, extract,
   mock generation, cascading type changes), the LLM invokes transform
   tools rather than manually rewriting code. The system applies the
   change consistently across all references in the IR graph. Cascading
   transforms — such as changing a parameter type and propagating the
   change to all callers — are a first-class capability enabled by the
   graph structure.

4. **Validate:** After writing or transforming code, the LLM runs verify
   tools to confirm correctness. `verify types` and `verify effects`
   are the most common. The compact error output gives the LLM just
   enough information to locate and fix problems without re-reading the
   entire codebase.

All tools take structured input (node hashes, module names, type
expressions) and return structured output (node sets, diagnostic lists,
diff summaries). The MCP protocol ensures that the LLM never needs to
parse natural language responses from the system.

**Text files as import/export.** While it is possible to write an Axiom
program in a text file and compile it into the IR, this is not the
typical workflow. The typical workflow is interactive: modules, functions,
and other nodes are created and modified via the MCP server, which
maintains the image. Text files serve as an import path (for bootstrapping
or migration) and an export path (for review forms or archival).

### 11.6 Testing via Effects

Testing in Axiom is natural because effects make all external dependencies
injectable. The `transform mock-effects` command scaffolds test modules
automatically, but tests can also be written directly:

```
module kv_store_test {
  import kv_store

  pub fn test_get_missing_key() -> TestResult ! {Test} {
    handle kv_store.get_key("nonexistent") with {
      State<Map<String, String>> {
        get() => resume Map.empty()
        put(s) => resume ()
      }
      Throw<KVError> {
        throw(KeyNotFound(key)) => assert_eq(key, "nonexistent")
        throw(other)            => fail("unexpected error")
      }
      Log {
        log(_, _) => resume ()    @# discard logs in tests #@
      }
    }
  }
}
```

### 11.7 Incremental Compilation

The binary IR is content-addressed and dependency-tracked. When the LLM
modifies a module, the system determines the minimal set of modules that
need recompilation using the image's cached dependency graph and reports
the result:

```
> verify types

Recompiling: kv_store (modified), app (depends on kv_store)
Skipping: http_client, json_parser (unchanged, no transitive dependency)

✓ All types check.
```

Because the image maintains the Merkle tree and graph indexes, incremental
compilation is a natural consequence of the architecture rather than a
separate optimization. Changed nodes produce new hashes, and the dependency
graph identifies exactly which downstream nodes need re-elaboration.

---

## 12. Open Questions and Future Work

### 12.1 Concurrency Model

The `Async` effect provides structured concurrency primitives, but the
detailed semantics need specification:

- **Structured concurrency (Trio/Kotlin style):** Tasks are scoped to their
  parent. A parent cannot exit while children are running.
- **Effect propagation across task boundaries:** If a spawned task performs
  effects, how are they routed to handlers? The likely answer is that
  spawned tasks must handle their own effects (or explicitly receive
  handler evidence from the parent).
- **Cancellation:** Should `Async` include a cancel operation?

### 12.2 Foreign Function Interface

The FFI bridges Axiom and the Zig runtime (and through Zig's C interop,
any C library):

- Foreign calls are wrapped in an `FFI` effect, making them visible in types.
- The Zig runtime provides typed stubs that Axiom's codegen calls into.
- Memory ownership at the boundary follows a copy-in/copy-out discipline
  for safety, with an escape hatch for performance-critical paths.

### 12.3 Working Form Experimentation

The three-layer architecture explicitly supports experimenting with
alternative working forms:

- **A more Python-like form** with significant whitespace.
- **A concatenative/point-free form** for pipeline-heavy code.
- **A diagram-based form** where effects are visualized as wiring diagrams
  (for human review, not LLM authoring).

Each form is a pure syntactic transformation to/from the binary IR.
Empirical testing with LLMs will determine which forms produce the
highest code quality.

### 12.4 Metaprogramming

Compile-time metaprogramming is deferred to v2. The binary IR's
tree structure makes it a natural target for programmatic transformation,
but the design of a macro system that works well with effects and types
requires careful thought.

### 12.5 Token Efficiency Analysis

A rigorous comparison of Axiom working form vs. equivalent Rust, Python,
and OCaml programs should be performed to validate the attention
efficiency hypothesis. Key metrics:

- Token count for equivalent programs.
- LLM error rate when generating code in each language.
- LLM ability to correctly predict effect signatures.
- Round-trip fidelity: LLM reads working form, modifies it, and the
  result compiles without type errors.

### 12.6 Version Control and Collaboration

The image model represents a deliberate departure from text-file-based version
control. Traditional tools like git operate on line diffs of text files — a
model that is increasingly strained by AI-driven development where changes
are semantic, not textual.

In Axiom, version control is an intrinsic property of the system:

- **The IR is already content-addressed.** Every historical state of the
  program is a different root hash. "Rolling back" means pointing to a
  previous root, not running `git revert`.
- **Operation history is semantic.** The image's operation log records
  edits at the level of "renamed `get_key` to `lookup`" or "added parameter
  `timeout: Duration` to `http_client.get`" — not "changed lines 47-52."
  This makes history meaningful to both humans and LLMs.
- **Branching and merging operate on graphs.** Because nodes are content-
  addressed, merging two branches of development is a graph operation:
  nodes that share the same hash are identical and need no reconciliation.
  Conflicts are structural (two branches modified the same node) rather
  than textual (two branches modified the same line).
- **Distribution is image sharing.** Collaborators exchange images (or
  deltas between images) rather than text patches. The content-addressed
  node store means delta computation is efficient — only nodes with new
  hashes need to be transmitted.

This model represents a fundamentally different approach to version control
and collaboration. Text-based tools assume that the canonical representation
of a program is a collection of text files. Axiom's canonical representation
is a content-addressed graph, and its version control, collaboration, and
distribution mechanisms follow from that foundation rather than being bolted
on after the fact. Axiom will need its own collaboration infrastructure
designed around image exchange and graph-level operations.

### 12.7 Bootstrapping Path

The long-term goal is for Axiom to be self-hosting: the compiler is
written in Axiom, compiled to WASM, and runs on the Zig runtime.

**Proposed bootstrapping stages:**

1. **Stage 0:** OCaml compiler + Zig runtime. Targets WASM. (Current plan.)
2. **Stage 1:** Rewrite the compiler frontend in Axiom, compiled by the
   OCaml compiler. Zig runtime remains.
3. **Stage 2:** Rewrite the WASM codegen in Axiom. The OCaml compiler
   compiles the Axiom compiler, which then compiles itself. (Bootstrap.)
4. **Stage 3:** The Axiom compiler is fully self-hosting. The OCaml
   implementation becomes a historical artifact.

---

*This specification is a living document. It defines the semantic foundation
and architectural vision of Axiom. The working form syntax, standard library,
and compilation strategies will evolve as the language is tested in practice
through LLM code generation experiments.*
