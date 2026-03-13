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
transmitted in this form.

**Properties:**

- Fully type-annotated (all inference results are materialized).
- Every node carries a unique content-addressed identifier.
- Effects are fully resolved — every `perform` is linked to its declared effect.
- Deterministic serialization — the same program always produces the same bytes.
- Designed for tooling: diffing, merging, refactoring, and analysis operate on
  the binary IR directly.

**Design rationale:** A binary canonical form means the working form and review
form can evolve independently. Syntax experiments, new keyword schemes, different
visual layouts — none of these require migrating stored programs. The IR is the
stable foundation.

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

### 2.4 Transformation Pipeline

```
Working Form ──parse──▶ AST ──elaborate──▶ Binary IR ──emit──▶ Working Form
                                               │
                                               ├──emit──▶ Review Form (OCaml-like)
                                               ├──emit──▶ Review Form (Rust-like)
                                               ├──emit──▶ Review Form (TypeScript-like)
                                               │
                                               └──compile──▶ WebAssembly
```

All transformations are deterministic and lossless (between IR and any text form).
The binary IR preserves all information needed to reconstruct any surface syntax.

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
  | literal                                  -- integer, float, string, byte
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

### 3.2 Mutual Recursion

Mutually recursive definitions are grouped explicitly with `letrec`:

```
letrec {
  is_even = fn (n: Nat) -> Bool {
    match n with
    | Zero => true
    | Succ m => is_odd(m)
  },
  is_odd = fn (n: Nat) -> Bool {
    match n with
    | Zero => false
    | Succ m => is_even(m)
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
  match perform FileSystem.read_file(path) with
  | Ok(bytes) => parse_config(bytes)
  | Err(e)    => perform Throw.throw(wrap_io_error(e))
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
    throw(msg)    => Err(msg)        -- no resume: abort on throw
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

-- In tests:
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
      os_close(handle)        -- cleanup always runs
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
        match Map.lookup(path, cache) with
        | Some(cached) => resume Ok(cached)
        | None => {
            let result = perform FileSystem.read_file(path)
            match result with
            | Ok(bytes) => { Map.insert(path, bytes, cache); resume result }
            | err       => resume err
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
    match rest with
    | Nil        => acc
    | Cons(h, t) => go(acc + h, t)    -- tail call, compiles to jump
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
  match n with
  | 0 => perform State.get()
  | _ => {
      perform State.put(perform State.get() + 1)
      count_loop(n - 1)    -- tail call, bounded stack even through handler
    }
}
```

### 6.5 The `do` Block

Sequential effects use `do` blocks. The last expression is in tail position:

```
do {
  perform Log.log(Info, "starting");
  let data = perform FileSystem.read_file(path);
  process(data)    -- tail position
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
    match perform FileSystem.read_file(path) with
    | Ok(bytes)  => parse_string(bytes_to_string(bytes))
    | Err(e)     => perform Throw.throw(io_to_parse_error(e))
  }

  -- Private helper, not exported
  fn parse_value(tokens: List<Token>, pos: Int) -> (JsonValue, Int) ! {Throw<ParseError>} {
    ...
  }
}
```

### 7.2 Module as LLM Reasoning Boundary

The `pub` interface of a module is designed so that an LLM can reason about
the module's behavior by reading **only** the public signatures:

```
-- An LLM sees this and knows:
-- "json_parser needs file access and may throw ParseError.
--  It provides parse_string (pure except for errors) and
--  parse_file (needs filesystem, may error)."
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
  -- Framework-defined effects that user code performs
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

  -- Framework provides the handler that wires effects to the runtime
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
program     ::= module_decl*

module_decl ::= 'module' IDENT '{' module_item* '}'

module_item ::= require_decl | type_decl | effect_decl | fn_decl

require_decl ::= 'require' 'effect' type_expr

type_decl   ::= 'type' type_head '=' variant ('|' variant)*
variant     ::= CTOR_IDENT | CTOR_IDENT '(' type_expr (',' type_expr)* ')'

effect_decl ::= 'effect' type_head '{' op_decl* '}'
op_decl     ::= IDENT ':' '(' type_expr* ')' '->' type_expr

fn_decl     ::= 'pub'? 'fn' IDENT type_params? '(' params ')' '->' type_expr '!' effect_set '{' expr '}'

effect_set  ::= 'pure' | '{' effect (',' effect)* '}'

expr        ::= let_expr | match_expr | handle_expr | do_expr | perform_expr
              | fn_expr | if_expr | app_expr | literal | IDENT | record_expr
              | expr '.' IDENT

let_expr    ::= 'let' IDENT (':' type_expr)? '=' expr 'in' expr
              | 'let' IDENT (':' type_expr)? '=' expr  -- in do blocks

match_expr  ::= 'match' expr 'with' '{' ('|' pattern '=>' expr)+ '}'

handle_expr ::= 'handle' expr 'with' '{' handler_clause+ '}'
handler_clause ::= IDENT '{' (op_handler)+ '}'
op_handler  ::= IDENT '(' params ')' '=>' expr
              | 'return' IDENT '=>' expr

perform_expr ::= 'perform' IDENT '.' IDENT '(' args ')'

do_expr     ::= 'do' '{' (stmt ';')* expr '}'
stmt        ::= 'let' IDENT '=' expr | expr

fn_expr     ::= 'fn' '(' params ')' ('->' type_expr '!' effect_set)? '{' expr '}'
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
      match Map.lookup(key, perform State.get()) with
      | Some(v) => v
      | None    => perform Throw.throw(KeyNotFound(key))
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
      match Map.lookup(key, perform State.get()) with
      | Some(_) => perform State.put(Map.remove(key, perform State.get()))
      | None    => perform Throw.throw(KeyNotFound(key))
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

## 11. System Interaction Commands

Axiom is a programming *system*, not just a language with a compiler. While
the LLM writes code primarily as text in the working form, reading,
reasoning about, and refactoring code often benefits from structured
commands that return precisely the information needed without dumping
entire modules into context.

The system provides three categories of built-in commands: **query**
(extract targeted information from the codebase), **transform** (apply
structured modifications), and **verify** (check invariants and
properties). These commands operate on the binary IR and return compact,
semantically rich responses.

### 11.1 Design Rationale

The problem these commands solve is context pollution. When an LLM needs
to answer "what effects does this module require?" the naive approach is
to read the entire module source. For a large module, this consumes
thousands of tokens — most of which are irrelevant function bodies. A
structured query returns just the answer: a list of effect signatures.

The principle: **write code as text, inspect code via commands.** The
working form is optimized for authoring. The command interface is optimized
for targeted retrieval and mechanical transformation.

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

The expected workflow is:

1. **Author:** The LLM writes code in the working form text. This is the
   primary mode — most code is written and edited as text, just as it
   would be in any language.

2. **Inspect:** When the LLM needs to understand existing code for
   composition, refactoring, or debugging, it uses query commands to
   retrieve precisely the information it needs. `query interface` to
   understand a module's API. `query effect-flow` to trace side effects.
   `query callers` to find impact of a change. This avoids reading entire
   modules into context.

3. **Transform:** For mechanical refactoring operations (rename, extract,
   mock generation), the LLM invokes transform commands rather than
   manually rewriting code. The system applies the change consistently
   across all references.

4. **Validate:** After writing or transforming code, the LLM runs verify
   commands to confirm correctness. `verify types` and `verify effects`
   are the most common. The compact error output gives the LLM just
   enough information to locate and fix problems without re-reading the
   entire codebase.

These commands are designed to be invokable as tool calls (in the MCP
or similar agentic protocol sense) — each takes structured input and
returns structured output. The LLM does not need to parse natural
language responses from the system.

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
        log(_, _) => resume ()    -- discard logs in tests
      }
    }
  }
}
```

### 11.7 Incremental Compilation

The binary IR is content-addressed and dependency-tracked. When the LLM
modifies a module, the system determines the minimal set of modules that
need recompilation and reports the result:

```
> verify types

Recompiling: kv_store (modified), app (depends on kv_store)
Skipping: http_client, json_parser (unchanged, no transitive dependency)

✓ All types check.

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

### 12.6 Bootstrapping Path

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
