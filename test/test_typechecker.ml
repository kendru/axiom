open Axiom_lib.Typechecker

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let ty_testable = Alcotest.testable pp_ty equal_ty

(* Normalize a type so that every [TyFun] row becomes [RPure]. Tests that
   only care about the value shape can compare modulo rows by running
   both sides through this first. *)
let rec strip_rows = function
  | TyCon _ | TyVar _ as t -> t
  | TyFun (a, b, _) -> TyFun (strip_rows a, strip_rows b, RPure)
  | TyForall (v, t) -> TyForall (v, strip_rows t)
  | TyMeta { inst = Some t; _ } -> strip_rows t
  | TyMeta _ as m -> m
  | TyRecord fields -> TyRecord (List.map (fun (n, t) -> (n, strip_rows t)) fields)
  | TyApp (name, args) -> TyApp (name, List.map strip_rows args)

let infer_of src =
  let tokens = Axiom_lib.Lexer.tokenize src in
  let expr   = Axiom_lib.Parser.parse_expr tokens in
  infer_expr empty_env expr

let check_ty label src expected =
  Alcotest.(check ty_testable) label expected (infer_of src)

(* Check a function-returning expression ignoring effect rows on arrows. *)
let check_ty_shape label src expected =
  Alcotest.(check ty_testable) label (strip_rows expected)
    (strip_rows (infer_of src))

let check_ty_error label src =
  match infer_of src with
  | exception _ -> ()
  | ty ->
    Alcotest.failf "%s: expected type error but got %a" label pp_ty ty

(* ------------------------------------------------------------------ *)
(* Literal types                                                        *)
(* ------------------------------------------------------------------ *)

let test_int_lit ()    = check_ty "42 : Int"    "42"    (TyCon "Int")
let test_float_lit ()  = check_ty "3.14 : Float64" "3.14" (TyCon "Float64")
let test_string_lit () = check_ty {|"hi" : String|} {|"hi"|} (TyCon "String")
let test_bool_true ()  = check_ty "true : Bool"  "true"  (TyCon "Bool")
let test_bool_false () = check_ty "false : Bool" "false" (TyCon "Bool")
let test_unit_lit ()   = check_ty "() : Unit"   "()"    (TyCon "Unit")

(* ------------------------------------------------------------------ *)
(* Let bindings                                                         *)
(* ------------------------------------------------------------------ *)

(* let x = 42 in x  =>  Int *)
let test_let_int () =
  check_ty "let x = 42 in x" "let x = 42 in x" (TyCon "Int")

(* let x = true in x  =>  Bool *)
let test_let_bool () =
  check_ty "let x = true in x" "let x = true in x" (TyCon "Bool")

(* let x = 42 in true  =>  Bool  (x unused, body determines type) *)
let test_let_body_type () =
  check_ty "body determines type" "let x = 42 in true" (TyCon "Bool")

(* let-generalization: id can be used at Int *)
let test_let_generalize_int () =
  check_ty "id applied to Int"
    "let id = fn (x: a) { x } in id(42)"
    (TyCon "Int")

(* let-generalization: same id function, used at Bool *)
let test_let_generalize_bool () =
  check_ty "id applied to Bool"
    "let id = fn (x: a) { x } in id(true)"
    (TyCon "Bool")

(* ------------------------------------------------------------------ *)
(* Function types                                                       *)
(* ------------------------------------------------------------------ *)

(* fn (x: Int) -> Int ! pure { x }  =>  Int -> Int ! pure *)
let test_fn_annotated () =
  check_ty "fn Int->Int"
    "fn (x: Int) -> Int ! pure { x }"
    (TyFun (TyCon "Int", TyCon "Int", RPure))

(* fn (x: Int) { x }  =>  Int -> Int  (inferred return type; row ignored) *)
let test_fn_inferred_return () =
  check_ty_shape "fn inferred return"
    "fn (x: Int) { x }"
    (TyFun (TyCon "Int", TyCon "Int", RPure))

(* fn (x: Int, y: Bool) -> Bool ! pure { y }
   =>  Int -> Bool -> Bool  (outer arrow row ignored; innermost is pure) *)
let test_fn_two_params () =
  check_ty_shape "fn two params"
    "fn (x: Int, y: Bool) -> Bool ! pure { y }"
    (TyFun (TyCon "Int",
            TyFun (TyCon "Bool", TyCon "Bool", RPure),
            RPure))

(* ------------------------------------------------------------------ *)
(* Match expressions                                                    *)
(* ------------------------------------------------------------------ *)

(* match true with { | true => 1 | false => 0 }  =>  Int *)
let test_match_arms_same_type () =
  check_ty "match bool -> int"
    "match true with { | true => 1 | false => 0 }"
    (TyCon "Int")

(* Arms with different types should fail *)
let test_match_arms_type_mismatch () =
  check_ty_error "match arm type mismatch"
    {|match true with { | true => 1 | false => "oops" }|}

(* ------------------------------------------------------------------ *)
(* letrec                                                               *)
(* ------------------------------------------------------------------ *)

(* letrec { f(x: Int): Int = x } in f(42)  =>  Int *)
let test_letrec_simple () =
  check_ty "letrec simple"
    "letrec { f(x: Int): Int = x } in f(42)"
    (TyCon "Int")

(* letrec { f(x: Int): Bool = true } in f  =>  Int -> Bool  (row ignored) *)
let test_letrec_fn_type () =
  check_ty_shape "letrec fn type"
    "letrec { f(x: Int): Bool = true } in f"
    (TyFun (TyCon "Int", TyCon "Bool", RPure))

(* ------------------------------------------------------------------ *)
(* Type errors                                                          *)
(* ------------------------------------------------------------------ *)

(* Unbound variable *)
let test_unbound_var () =
  check_ty_error "unbound var" "x"

(* ------------------------------------------------------------------ *)
(* Effect operations and program-level checking                         *)
(* ------------------------------------------------------------------ *)

let parse_program src =
  Axiom_lib.Parser.parse_program (Axiom_lib.Lexer.tokenize src)

let check_program_of src = check_program (parse_program src)

let check_program_ok label src =
  match check_program_of src with
  | _ -> ()
  | exception Failure msg ->
    Alcotest.failf "%s: unexpected type error: %s" label msg

let check_program_error label src =
  match check_program_of src with
  | exception _ -> ()
  | _ ->
    Alcotest.failf "%s: expected type error but program checked cleanly" label

(* A function that performs a mono-typed effect op correctly. *)
let test_perform_console_ok () =
  check_program_ok "Console.print"
    {|
      effect Console {
        print: (String) -> Unit,
        read_line: () -> String
      }

      fn greet() -> Unit ! {Console} {
        perform Console.print("hi")
      }
    |}

(* A function that reads via a generic State effect. *)
let test_perform_state_get_ok () =
  check_program_ok "State.get"
    {|
      effect State<s> {
        get: () -> s,
        put: (s) -> Unit
      }

      fn use_state() -> Int ! {State<Int>} {
        do {
          perform State.put(42);
          perform State.get()
        }
      }
    |}

(* Unknown effect name *)
let test_perform_unknown_effect () =
  check_program_error "unknown effect"
    {|
      fn go() -> Unit ! pure {
        perform Nope.boom()
      }
    |}

(* Unknown operation on a known effect *)
let test_perform_unknown_op () =
  check_program_error "unknown operation"
    {|
      effect Console {
        print: (String) -> Unit
      }

      fn go() -> Unit ! {Console} {
        perform Console.froodle()
      }
    |}

(* Wrong arity *)
let test_perform_wrong_arity () =
  check_program_error "wrong arity"
    {|
      effect Console {
        print: (String) -> Unit
      }

      fn go() -> Unit ! {Console} {
        perform Console.print("a", "b")
      }
    |}

(* Wrong argument type *)
let test_perform_wrong_arg_type () =
  check_program_error "wrong arg type"
    {|
      effect Console {
        print: (String) -> Unit
      }

      fn go() -> Unit ! {Console} {
        perform Console.print(42)
      }
    |}

(* Body's inferred type disagrees with the declared return type *)
let test_body_return_mismatch () =
  check_program_error "body vs return"
    {|
      fn bad() -> Int ! pure {
        true
      }
    |}

(* ------------------------------------------------------------------ *)
(* Handler clauses                                                      *)
(* ------------------------------------------------------------------ *)

(* A Console handler whose op returns Unit. With no return clause the
   handled expression's type (Unit) coincides with the handle result. *)
let test_handle_console_ok () =
  check_program_ok "handle Console.print"
    {|
      effect Console {
        print: (String) -> Unit
      }

      fn silence(body: (u: Unit) -> Unit ! {Console}) -> Unit ! pure {
        handle body(()) with {
          Console {
            print(msg) => resume(())
          }
        }
      }
    |}

(* A State<s> handler with a return clause that transforms the handled
   computation's result. The effect type parameter s is instantiated once
   per handler clause so get and put share it. *)
let test_handle_state_with_return () =
  check_program_ok "handle State with return"
    {|
      effect State<s> {
        get: () -> s,
        put: (s) -> Unit
      }

      fn run_state(body: (u: Unit) -> Int ! {State<Int>}) -> Int ! pure {
        handle body(()) with {
          State {
            get() => resume(0)
            put(s) => resume(())
            return v => v
          }
        }
      }
    |}

(* Handler references an unknown effect. *)
let test_handle_unknown_effect () =
  check_program_error "unknown effect in handler"
    {|
      fn go() -> Unit ! pure {
        handle () with {
          Nope {
            boom() => resume(())
          }
        }
      }
    |}

(* Handler clause references an unknown operation on a known effect. *)
let test_handle_unknown_op () =
  check_program_error "unknown op in handler"
    {|
      effect Console {
        print: (String) -> Unit
      }

      fn go(body: (u: Unit) -> Unit ! {Console}) -> Unit ! pure {
        handle body(()) with {
          Console {
            froodle() => resume(())
          }
        }
      }
    |}

(* Handler clause binds the wrong number of parameter names. *)
let test_handle_wrong_arity () =
  check_program_error "handler arity mismatch"
    {|
      effect Console {
        print: (String) -> Unit
      }

      fn go(body: (u: Unit) -> Unit ! {Console}) -> Unit ! pure {
        handle body(()) with {
          Console {
            print() => resume(())
          }
        }
      }
    |}

(* resume is called with a value of the wrong type for the op.
   `print : (String) -> Unit`, so `resume` has type `Unit -> result`.
   Calling `resume(42)` forces Int = Unit and fails. *)
let test_handle_resume_wrong_type () =
  check_program_error "resume wrong type"
    {|
      effect Console {
        print: (String) -> Unit,
        read_line: () -> String
      }

      fn bad(body: (u: Unit) -> String ! {Console}) -> String ! pure {
        handle body(()) with {
          Console {
            print(msg) => resume(42)
            read_line() => resume("hi")
          }
        }
      }
    |}

(* Op handlers in the same handler must agree on the result type.
   `get()` returns 1 (Int) — becomes the result type — but `put` returns
   "oops" (String). *)
let test_handle_ops_disagree () =
  check_program_error "op handlers disagree"
    {|
      effect State<s> {
        get: () -> s,
        put: (s) -> Unit
      }

      fn bad(body: (u: Unit) -> Int ! {State<Int>}) -> Int ! pure {
        handle body(()) with {
          State {
            get() => 1
            put(s) => "oops"
            return v => v
          }
        }
      }
    |}

(* Return-clause body must have the handle's result type; here return
   contradicts the declared Int return of the enclosing function. *)
let test_handle_return_clause_mismatch () =
  check_program_error "return clause body mismatch"
    {|
      effect State<s> {
        get: () -> s,
        put: (s) -> Unit
      }

      fn bad(body: (u: Unit) -> Int ! {State<Int>}) -> Int ! pure {
        handle body(()) with {
          State {
            get() => resume(0)
            put(s) => resume(())
            return v => "not-an-int"
          }
        }
      }
    |}

(* Mutual recursion across two top-level functions resolves via pass-1
   signatures registered before bodies are checked. *)
let test_program_mutual_recursion () =
  check_program_ok "mutual recursion"
    {|
      fn even(n: Int) -> Bool ! pure {
        if eq(n, 0) { true } else { odd(sub(n, 1)) }
      }

      fn odd(n: Int) -> Bool ! pure {
        if eq(n, 0) { false } else { even(sub(n, 1)) }
      }

      fn eq(a: Int, b: Int) -> Bool ! pure { true }
      fn sub(a: Int, b: Int) -> Int ! pure { a }
    |}

(* ------------------------------------------------------------------ *)
(* Effect rows                                                          *)
(* ------------------------------------------------------------------ *)

(* Performing an effect declared in the function's effect row is fine. *)
let test_row_perform_in_declared_row () =
  check_program_ok "perform in declared row"
    {|
      effect Console {
        print: (String) -> Unit,
        read_line: () -> String
      }

      fn greet() -> Unit ! {Console} {
        perform Console.print("hi")
      }
    |}

(* Performing an effect NOT in the declared row is a type error. *)
let test_row_perform_outside_row () =
  check_program_error "perform outside declared row"
    {|
      effect Console {
        print: (String) -> Unit,
        read_line: () -> String
      }

      fn greet() -> Unit ! pure {
        perform Console.print("hi")
      }
    |}

(* Calling an effectful function from a function whose row doesn't admit
   that effect is a type error. *)
let test_row_call_effectful_from_pure () =
  check_program_error "call effectful from pure"
    {|
      effect Console {
        print: (String) -> Unit,
        read_line: () -> String
      }

      fn shout(s: String) -> Unit ! {Console} {
        perform Console.print(s)
      }

      fn greet() -> Unit ! pure {
        shout("hi")
      }
    |}

(* Calling a pure function from an effectful context is fine. *)
let test_row_call_pure_from_effectful () =
  check_program_ok "call pure from effectful"
    {|
      effect Console {
        print: (String) -> Unit,
        read_line: () -> String
      }

      fn id_int(x: Int) -> Int ! pure { x }

      fn greet() -> Unit ! {Console} {
        do {
          let _x = id_int(42);
          perform Console.print("hi")
        }
      }
    |}

(* handle discharges the effect: a function with a {State<Int>} body wrapped
   in a State handler can be pure overall. *)
let test_row_handler_discharges_effect () =
  check_program_ok "handler discharges effect"
    {|
      effect State<s> {
        get: () -> s,
        put: (s) -> Unit
      }

      fn run(init: Int, body: (u: Unit) -> Int ! {State<Int>}) -> Int ! pure {
        handle body(()) with {
          State {
            get() => resume(init)
            put(s) => resume(())
          }
        }
      }
    |}

(* Two performs of a State<s> operation in the same function must agree
   on the state type. Here both get and put are tied to the function's
   declared {State<Int>}, so their s type is unified to Int and calling
   put(42) succeeds. *)
let test_row_state_shared_param () =
  check_program_ok "State param shared across performs"
    {|
      effect State<s> {
        get: () -> s,
        put: (s) -> Unit
      }

      fn bump() -> Unit ! {State<Int>} {
        do {
          let _current = perform State.get();
          perform State.put(42)
        }
      }
    |}

(* Handler operation bodies run in the OUTER ambient. A Log handler that
   translates Log to Console can perform Console.print from its op body. *)
let test_row_handler_translates_effect () =
  check_program_ok "handler translates Log to Console"
    {|
      effect Console {
        print: (String) -> Unit,
        read_line: () -> String
      }

      effect Log {
        log: (String) -> Unit
      }

      fn log_to_console(body: (u: Unit) -> Unit ! {Log}) -> Unit ! {Console} {
        handle body(()) with {
          Log {
            log(msg) => do {
              perform Console.print(msg);
              resume(())
            }
          }
        }
      }
    |}

(* ------------------------------------------------------------------ *)
(* Constructor expressions                                              *)
(* ------------------------------------------------------------------ *)

(* A nullary constructor used as an expression has the declared type. *)
let test_ctor_expr_nullary () =
  check_program_ok "nullary ctor in expression"
    {|
      type Color = | Red | Green | Blue

      fn make_red() -> Color ! pure { Red }
    |}

(* A unary constructor applied to an argument. *)
let test_ctor_expr_applied () =
  check_program_ok "ctor applied to arg"
    {|
      type Wrapper = | Wrap(Int)

      fn wrap_it(n: Int) -> Wrapper ! pure { Wrap(n) }
    |}

(* Passing the wrong type to a constructor is a type error. *)
let test_ctor_expr_wrong_arg_type () =
  check_program_error "ctor applied to wrong type"
    {|
      type Wrapper = | Wrap(Int)

      fn bad() -> Wrapper ! pure { Wrap("hello") }
    |}

(* Construct-then-match round-trip: result type flows through correctly. *)
let test_ctor_expr_round_trip () =
  check_program_ok "ctor construct-then-match"
    {|
      type Option<a> = | Some(a) | None

      fn identity_option(x: Int) -> Int ! pure {
        match Some(x) with {
          | Some(n) => n
          | None    => 0
        }
      }
    |}

(* Return type mismatch: ctor produces the wrong type for the function. *)
let test_ctor_expr_return_mismatch () =
  check_program_error "ctor return type mismatch"
    {|
      type Color = | Red | Green | Blue

      fn bad() -> Int ! pure { Red }
    |}

(* ------------------------------------------------------------------ *)
(* DeclType — constructor registration                                  *)
(* ------------------------------------------------------------------ *)

(* Nullary constructors from a type declaration are in scope as patterns. *)
let test_decl_type_nullary_ctors () =
  check_program_ok "nullary constructors in scope"
    {|
      type Color = | Red | Green | Blue

      fn is_red(c: Color) -> Bool ! pure {
        match c with {
          | Red   => true
          | Green => false
          | Blue  => false
        }
      }
    |}

(* Constructor with a parameter: the bound variable gets the declared arg type. *)
let test_decl_type_ctor_param_type () =
  check_program_ok "ctor pattern binds correct type"
    {|
      type Wrapper = | Wrap(Int)

      fn unwrap(w: Wrapper) -> Int ! pure {
        match w with {
          | Wrap(n) => n
        }
      }
    |}

(* Bound variable from a ctor pattern has the wrong type for the arm body. *)
let test_decl_type_ctor_wrong_arg () =
  check_program_error "ctor pattern body type mismatch"
    {|
      type Wrapper = | Wrap(Int)

      fn bad(w: Wrapper) -> String ! pure {
        match w with {
          | Wrap(n) => n
        }
      }
    |}

(* Parametric type: sub-pattern bindings get consistent inferred types. *)
let test_decl_type_parametric_ctor () =
  check_program_ok "parametric ctor pattern"
    {|
      type Option<a> = | Some(a) | None

      fn get_or_zero(opt: Option<Int>) -> Int ! pure {
        match opt with {
          | Some(x) => x
          | None    => 0
        }
      }
    |}

(* Arity mismatch in a constructor pattern is a type error. *)
let test_decl_type_ctor_pattern_arity () =
  check_program_error "ctor pattern arity mismatch"
    {|
      type Option<a> = | Some(a) | None

      fn bad(opt: Option<Int>) -> Int ! pure {
        match opt with {
          | Some(x, y) => 0
          | None       => 1
        }
      }
    |}

(* ------------------------------------------------------------------ *)
(* DeclModule — body collected into flat env                            *)
(* ------------------------------------------------------------------ *)

(* Functions declared inside a module are accessible from outside. *)
let test_module_body_collected () =
  check_program_ok "module body declarations collected"
    {|
      module math {
        fn add(a: Int, b: Int) -> Int ! pure { a }
      }

      fn use_add() -> Int ! pure {
        add(1, 2)
      }
    |}

(* Type errors in module body functions are caught. *)
let test_module_body_type_error () =
  check_program_error "module body type error"
    {|
      module bad {
        fn wrong() -> Int ! pure { true }
      }
    |}

(* ------------------------------------------------------------------ *)
(* Record type inference                                                *)
(* ------------------------------------------------------------------ *)

(* { x: 42, y: true } infers as {x: Int, y: Bool} *)
let test_record_literal_type () =
  check_ty "record literal type"
    "{ x: 42, y: true }"
    (TyRecord [("x", TyCon "Int"); ("y", TyCon "Bool")])

(* Single-field record *)
let test_record_single_field () =
  check_ty "single-field record"
    "{ msg: \"hello\" }"
    (TyRecord [("msg", TyCon "String")])

(* let r = { x: 42 } in r.x  =>  Int *)
let test_project_field_type () =
  check_ty "project field type"
    "let r = { x: 42 } in r.x"
    (TyCon "Int")

(* let r = { x: 42, y: true } in r.y  =>  Bool *)
let test_project_second_field () =
  check_ty "project second field"
    "let r = { x: 42, y: true } in r.y"
    (TyCon "Bool")

(* Projecting a nonexistent field is a type error *)
let test_project_unknown_field () =
  check_ty_error "project unknown field"
    "let r = { x: 42 } in r.z"

(* Record update preserves type: { r with x: 99 } still has the same record type *)
let test_record_update_type () =
  check_ty "record update type"
    "let r = { x: 42 } in { r with x: 99 }"
    (TyRecord [("x", TyCon "Int")])

(* Record update with wrong field type is a type error *)
let test_record_update_type_mismatch () =
  check_ty_error "record update type mismatch"
    {|let r = { x: 42 } in { r with x: "oops" }|}

(* Record update targeting a nonexistent field is a type error *)
let test_record_update_unknown_field () =
  check_ty_error "record update unknown field"
    "let r = { x: 42 } in { r with z: 99 }"

(* Projecting a non-record type is a type error *)
let test_project_non_record () =
  check_ty_error "project on non-record"
    "let n = 42 in n.x"

(* Record unification: two records with different fields don't unify *)
let test_record_field_set_mismatch () =
  check_ty_error "record field set mismatch"
    {|if true { { x: 1 } } else { { y: 1 } }|}

(* Records with same fields but different value types don't unify *)
let test_record_field_type_mismatch () =
  check_ty_error "record field type mismatch"
    {|if true { { x: 42 } } else { { x: "hi" } }|}

(* Records with same fields and same types do unify *)
let test_record_unify_same () =
  check_ty "record same-type unify"
    {|if true { { x: 1, y: 2 } } else { { x: 3, y: 4 } }|}
    (TyRecord [("x", TyCon "Int"); ("y", TyCon "Int")])

(* Program-level: function that builds and returns a record *)
let test_program_record_return () =
  check_program_ok "function returns record"
    {|
      fn make_pair() -> Unit ! pure {
        let r = { x: 1, y: 2 } in
        let _x = r.x in
        ()
      }
    |}

(* Program-level: record update in a function body *)
let test_program_record_update () =
  check_program_ok "record update in function"
    {|
      fn bump() -> Unit ! pure {
        let r = { count: 0 } in
        let _r2 = { r with count: 1 } in
        ()
      }
    |}

(* Program-level: record pattern in match *)
let test_program_record_pattern () =
  check_program_ok "record pattern match"
    {|
      fn get_x() -> Int ! pure {
        let r = { x: 42, y: true } in
        match r with {
          | { x = n, y = _ } => n
        }
      }
    |}

(* Record pattern binds variables with the correct field types *)
let test_program_record_pattern_types () =
  check_program_ok "record pattern field types"
    {|
      fn check() -> Int ! pure {
        let r = { x: 42, y: true } in
        match r with {
          | { x = n, y = _ } => n
        }
      }
    |}

(* Record pattern mentioning an unknown field is a type error *)
let test_program_record_pattern_unknown_field () =
  check_program_error "record pattern unknown field"
    {|
      fn bad() -> Int ! pure {
        let r = { x: 42 } in
        match r with {
          | { z = n } => n
        }
      }
    |}

(* ------------------------------------------------------------------ *)
(* Module imports                                                       *)
(* ------------------------------------------------------------------ *)

(* Functions declared in a module are callable via module.fn syntax. *)
let test_import_qualified_access () =
  check_program_ok "module qualified access"
    {|
      module math {
        fn add(a: Int, b: Int) -> Int ! pure { a }
      }

      fn use_it() -> Int ! pure {
        math.add(1, 2)
      }
    |}

(* import M makes M.fn available under M.fn. *)
let test_import_bare () =
  check_program_ok "import bare"
    {|
      module math {
        fn add(a: Int, b: Int) -> Int ! pure { a }
      }

      import math

      fn use_it() -> Int ! pure {
        math.add(1, 2)
      }
    |}

(* import M as alias makes M's functions available as alias.fn. *)
let test_import_alias () =
  check_program_ok "import with alias"
    {|
      module math {
        fn add(a: Int, b: Int) -> Int ! pure { a }
      }

      import math as m

      fn use_it() -> Int ! pure {
        m.add(1, 2)
      }
    |}

(* Importing an unknown module is a type error. *)
let test_import_unknown_module () =
  check_program_error "import unknown module"
    {|
      import no_such_module

      fn use_it() -> Int ! pure { 42 }
    |}

(* A module's function called via qualified name with wrong arg type fails. *)
let test_import_qualified_wrong_type () =
  check_program_error "qualified call wrong arg type"
    {|
      module math {
        fn add(a: Int, b: Int) -> Int ! pure { a }
      }

      fn bad() -> Int ! pure {
        math.add(true, 2)
      }
    |}

(* import before module definition (order-independent via pre-pass). *)
let test_import_before_definition () =
  check_program_ok "import before definition"
    {|
      import math

      fn use_it() -> Int ! pure {
        math.add(1, 2)
      }

      module math {
        fn add(a: Int, b: Int) -> Int ! pure { a }
      }
    |}

(* ------------------------------------------------------------------ *)
(* Parametric type application                                          *)
(* ------------------------------------------------------------------ *)

(* Option<Int> and Option<Bool> are distinct types. *)
let test_tyapp_distinct_params () =
  check_program_error "Option<Int> ≠ Option<Bool>"
    {|
      type Option<a> = | Some(a) | None

      fn bad(x: Option<Int>) -> Option<Bool> ! pure { x }
    |}

(* Constructing Some(42) where Option<Bool> is expected is a type error. *)
let test_tyapp_ctor_wrong_param () =
  check_program_error "Some(42) where Option<Bool> expected"
    {|
      type Option<a> = | Some(a) | None

      fn bad() -> Option<Bool> ! pure { Some(42) }
    |}

(* Round-trip through a parameterized return type. *)
let test_tyapp_return_type () =
  check_program_ok "Option<Int> return type"
    {|
      type Option<a> = | Some(a) | None

      fn wrap(n: Int) -> Option<Int> ! pure { Some(n) }
    |}

(* Nested type application: Option<Option<Int>>. *)
let test_tyapp_nested () =
  check_program_ok "Option<Option<Int>>"
    {|
      type Option<a> = | Some(a) | None

      fn double_wrap(n: Int) -> Option<Option<Int>> ! pure {
        Some(Some(n))
      }
    |}

(* Two-parameter type: Result<a, e>. *)
let test_tyapp_two_params () =
  check_program_ok "Result<Int, String>"
    {|
      type Result<a, e> = | Ok(a) | Err(e)

      fn make_ok(n: Int) -> Result<Int, String> ! pure { Ok(n) }
      fn make_err(s: String) -> Result<Int, String> ! pure { Err(s) }
    |}

(* Mixing Ok and Err branches in a match must agree on the result type. *)
let test_tyapp_result_match () =
  check_program_ok "match Result arms"
    {|
      type Result<a, e> = | Ok(a) | Err(e)

      fn unwrap_or(r: Result<Int, String>, default: Int) -> Int ! pure {
        match r with {
          | Ok(x)  => x
          | Err(_) => default
        }
      }
    |}

(* Using the wrong type in an Ok arm is a type error. *)
let test_tyapp_result_arm_mismatch () =
  check_program_error "Result arm type mismatch"
    {|
      type Result<a, e> = | Ok(a) | Err(e)

      fn bad(r: Result<Int, String>) -> Int ! pure {
        match r with {
          | Ok(x)  => x
          | Err(s) => s
        }
      }
    |}

(* ------------------------------------------------------------------ *)
(* Exhaustiveness checking                                              *)
(* ------------------------------------------------------------------ *)

(* ADT: missing None arm is a type error. *)
let test_exhaustive_adt_missing_ctor () =
  check_program_error "non-exhaustive ADT match"
    {|
      type Option<a> = | Some(a) | None

      fn bad(opt: Option<Int>) -> Int ! pure {
        match opt with {
          | Some(x) => x
        }
      }
    |}

(* ADT: all constructors covered, no error. *)
let test_exhaustive_adt_full () =
  check_program_ok "exhaustive ADT match"
    {|
      type Option<a> = | Some(a) | None

      fn unwrap(opt: Option<Int>) -> Int ! pure {
        match opt with {
          | Some(x) => x
          | None    => 0
        }
      }
    |}

(* Bool: missing false arm is a type error. *)
let test_exhaustive_bool_missing_false () =
  check_program_error "non-exhaustive Bool match (missing false)"
    {|
      fn bad(b: Bool) -> Int ! pure {
        match b with {
          | true => 1
        }
      }
    |}

(* Bool: missing true arm is a type error. *)
let test_exhaustive_bool_missing_true () =
  check_program_error "non-exhaustive Bool match (missing true)"
    {|
      fn bad(b: Bool) -> Int ! pure {
        match b with {
          | false => 0
        }
      }
    |}

(* Bool: both arms present, ok. *)
let test_exhaustive_bool_full () =
  check_program_ok "exhaustive Bool match"
    {|
      fn to_int(b: Bool) -> Int ! pure {
        match b with {
          | true  => 1
          | false => 0
        }
      }
    |}

(* Int: literal-only match without wildcard is a type error. *)
let test_exhaustive_int_no_wildcard () =
  check_program_error "non-exhaustive Int match (no wildcard)"
    {|
      fn bad(n: Int) -> Int ! pure {
        match n with {
          | 0 => 0
          | 1 => 1
        }
      }
    |}

(* String: literal-only match without wildcard is a type error. *)
let test_exhaustive_string_no_wildcard () =
  check_program_error "non-exhaustive String match (no wildcard)"
    {|
      fn bad(s: String) -> Int ! pure {
        match s with {
          | "a" => 0
        }
      }
    |}

(* Wildcard makes any match exhaustive. *)
let test_exhaustive_wildcard_covers_all () =
  check_program_ok "wildcard covers Int"
    {|
      fn describe(n: Int) -> Int ! pure {
        match n with {
          | 0 => 0
          | _ => 1
        }
      }
    |}

(* Variable pattern makes any match exhaustive. *)
let test_exhaustive_var_covers_all () =
  check_program_ok "variable pattern covers ADT"
    {|
      type Option<a> = | Some(a) | None

      fn to_zero(opt: Option<Int>) -> Int ! pure {
        match opt with {
          | x => 0
        }
      }
    |}

(* Or-pattern covering all Bool constructors is exhaustive. *)
let test_exhaustive_or_pattern_bool () =
  check_program_ok "or-pattern covers Bool"
    {|
      fn to_int(b: Bool) -> Int ! pure {
        match b with {
          | true | false => 0
        }
      }
    |}

(* Or-pattern covering all ADT constructors is exhaustive. *)
let test_exhaustive_or_pattern_adt () =
  check_program_ok "or-pattern covers Option"
    {|
      type Option<a> = | Some(a) | None

      fn is_some(opt: Option<Int>) -> Int ! pure {
        match opt with {
          | Some(_) | None => 0
        }
      }
    |}

(* Record: wildcard/var sub-patterns make the match exhaustive. *)
let test_exhaustive_record_wildcard_fields () =
  check_program_ok "record with wildcard fields is exhaustive"
    {|
      fn get_x() -> Int ! pure {
        let r = { x: 42, y: true } in
        match r with {
          | { x = n, y = _ } => n
        }
      }
    |}

(* Record: a literal sub-pattern does not cover the whole field domain. *)
let test_exhaustive_record_literal_field () =
  check_program_error "record with literal field is non-exhaustive"
    {|
      fn bad() -> Int ! pure {
        let r = { x: 42 } in
        match r with {
          | { x = 0 } => 0
        }
      }
    |}

(* Record: two patterns together can cover the field domain. *)
let test_exhaustive_record_multi_pattern () =
  check_program_ok "two record patterns covering all field values"
    {|
      fn classify() -> Int ! pure {
        let r = { x: 42 } in
        match r with {
          | { x = 0 } => 0
          | { x = _ } => 1
        }
      }
    |}

(* Record: open pattern omitting a field implicitly wildcards that field. *)
let test_exhaustive_record_open_pattern () =
  check_program_ok "open record pattern covers omitted fields"
    {|
      fn get_x() -> Int ! pure {
        let r = { x: 42, y: true } in
        match r with {
          | { x = n, .. } => n
        }
      }
    |}

(* ------------------------------------------------------------------ *)
(* Test runner                                                          *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "Typechecker"
    [ ( "literals",
        [ Alcotest.test_case "int"    `Quick test_int_lit
        ; Alcotest.test_case "float"  `Quick test_float_lit
        ; Alcotest.test_case "string" `Quick test_string_lit
        ; Alcotest.test_case "true"   `Quick test_bool_true
        ; Alcotest.test_case "false"  `Quick test_bool_false
        ; Alcotest.test_case "unit"   `Quick test_unit_lit
        ] )
    ; ( "let-binding",
        [ Alcotest.test_case "let x = 42 in x"     `Quick test_let_int
        ; Alcotest.test_case "let x = true in x"   `Quick test_let_bool
        ; Alcotest.test_case "body type"            `Quick test_let_body_type
        ; Alcotest.test_case "id applied to Int"     `Quick test_let_generalize_int
        ; Alcotest.test_case "id applied to Bool"   `Quick test_let_generalize_bool
        ] )
    ; ( "functions",
        [ Alcotest.test_case "annotated fn"      `Quick test_fn_annotated
        ; Alcotest.test_case "inferred return"   `Quick test_fn_inferred_return
        ; Alcotest.test_case "two params"        `Quick test_fn_two_params
        ] )
    ; ( "match",
        [ Alcotest.test_case "arms same type"    `Quick test_match_arms_same_type
        ; Alcotest.test_case "arm type mismatch" `Quick test_match_arms_type_mismatch
        ] )
    ; ( "letrec",
        [ Alcotest.test_case "simple"            `Quick test_letrec_simple
        ; Alcotest.test_case "fn type"           `Quick test_letrec_fn_type
        ] )
    ; ( "errors",
        [ Alcotest.test_case "unbound var"       `Quick test_unbound_var
        ] )
    ; ( "effects",
        [ Alcotest.test_case "perform Console.print"     `Quick test_perform_console_ok
        ; Alcotest.test_case "perform State.get"         `Quick test_perform_state_get_ok
        ; Alcotest.test_case "unknown effect"            `Quick test_perform_unknown_effect
        ; Alcotest.test_case "unknown operation"         `Quick test_perform_unknown_op
        ; Alcotest.test_case "wrong arity"               `Quick test_perform_wrong_arity
        ; Alcotest.test_case "wrong arg type"            `Quick test_perform_wrong_arg_type
        ] )
    ; ( "handlers",
        [ Alcotest.test_case "Console handler"           `Quick test_handle_console_ok
        ; Alcotest.test_case "State with return clause"  `Quick test_handle_state_with_return
        ; Alcotest.test_case "unknown effect"            `Quick test_handle_unknown_effect
        ; Alcotest.test_case "unknown op"                `Quick test_handle_unknown_op
        ; Alcotest.test_case "wrong arity"               `Quick test_handle_wrong_arity
        ; Alcotest.test_case "resume wrong type"         `Quick test_handle_resume_wrong_type
        ; Alcotest.test_case "op handlers disagree"      `Quick test_handle_ops_disagree
        ; Alcotest.test_case "return clause mismatch"    `Quick test_handle_return_clause_mismatch
        ] )
    ; ( "program",
        [ Alcotest.test_case "body vs return mismatch"   `Quick test_body_return_mismatch
        ; Alcotest.test_case "mutual recursion"          `Quick test_program_mutual_recursion
        ] )
    ; ( "decl-type",
        [ Alcotest.test_case "nullary ctors"             `Quick test_decl_type_nullary_ctors
        ; Alcotest.test_case "ctor param type"           `Quick test_decl_type_ctor_param_type
        ; Alcotest.test_case "ctor wrong arg"            `Quick test_decl_type_ctor_wrong_arg
        ; Alcotest.test_case "parametric ctor"           `Quick test_decl_type_parametric_ctor
        ; Alcotest.test_case "ctor pattern arity"        `Quick test_decl_type_ctor_pattern_arity
        ] )
    ; ( "modules",
        [ Alcotest.test_case "body collected"            `Quick test_module_body_collected
        ; Alcotest.test_case "body type error"           `Quick test_module_body_type_error
        ] )
    ; ( "ctor-expressions",
        [ Alcotest.test_case "nullary ctor"              `Quick test_ctor_expr_nullary
        ; Alcotest.test_case "applied ctor"              `Quick test_ctor_expr_applied
        ; Alcotest.test_case "wrong arg type"            `Quick test_ctor_expr_wrong_arg_type
        ; Alcotest.test_case "construct-then-match"      `Quick test_ctor_expr_round_trip
        ; Alcotest.test_case "return mismatch"           `Quick test_ctor_expr_return_mismatch
        ] )
    ; ( "effect rows",
        [ Alcotest.test_case "perform in declared row"      `Quick test_row_perform_in_declared_row
        ; Alcotest.test_case "perform outside declared row" `Quick test_row_perform_outside_row
        ; Alcotest.test_case "call effectful from pure"     `Quick test_row_call_effectful_from_pure
        ; Alcotest.test_case "call pure from effectful"     `Quick test_row_call_pure_from_effectful
        ; Alcotest.test_case "handler discharges effect"    `Quick test_row_handler_discharges_effect
        ; Alcotest.test_case "State param shared"           `Quick test_row_state_shared_param
        ; Alcotest.test_case "handler translates effect"    `Quick test_row_handler_translates_effect
        ] )
    ; ( "records",
        [ Alcotest.test_case "literal type"              `Quick test_record_literal_type
        ; Alcotest.test_case "single field"              `Quick test_record_single_field
        ; Alcotest.test_case "project field"             `Quick test_project_field_type
        ; Alcotest.test_case "project second field"      `Quick test_project_second_field
        ; Alcotest.test_case "project unknown field"     `Quick test_project_unknown_field
        ; Alcotest.test_case "update type"               `Quick test_record_update_type
        ; Alcotest.test_case "update type mismatch"      `Quick test_record_update_type_mismatch
        ; Alcotest.test_case "update unknown field"      `Quick test_record_update_unknown_field
        ; Alcotest.test_case "project non-record"        `Quick test_project_non_record
        ; Alcotest.test_case "field set mismatch"        `Quick test_record_field_set_mismatch
        ; Alcotest.test_case "field type mismatch"       `Quick test_record_field_type_mismatch
        ; Alcotest.test_case "unify same fields"         `Quick test_record_unify_same
        ; Alcotest.test_case "program record return"     `Quick test_program_record_return
        ; Alcotest.test_case "program record update"     `Quick test_program_record_update
        ; Alcotest.test_case "program record pattern"    `Quick test_program_record_pattern
        ; Alcotest.test_case "program record pattern types" `Quick test_program_record_pattern_types
        ; Alcotest.test_case "program pattern unknown field" `Quick test_program_record_pattern_unknown_field
        ] )
    ; ( "imports",
        [ Alcotest.test_case "qualified access"          `Quick test_import_qualified_access
        ; Alcotest.test_case "import bare"               `Quick test_import_bare
        ; Alcotest.test_case "import alias"              `Quick test_import_alias
        ; Alcotest.test_case "unknown module"            `Quick test_import_unknown_module
        ; Alcotest.test_case "qualified wrong type"      `Quick test_import_qualified_wrong_type
        ; Alcotest.test_case "import before definition"  `Quick test_import_before_definition
        ] )
    ; ( "parametric types",
        [ Alcotest.test_case "distinct params"           `Quick test_tyapp_distinct_params
        ; Alcotest.test_case "ctor wrong param"          `Quick test_tyapp_ctor_wrong_param
        ; Alcotest.test_case "Option<Int> return"        `Quick test_tyapp_return_type
        ; Alcotest.test_case "nested TyApp"              `Quick test_tyapp_nested
        ; Alcotest.test_case "two-param Result"          `Quick test_tyapp_two_params
        ; Alcotest.test_case "Result match arms"         `Quick test_tyapp_result_match
        ; Alcotest.test_case "Result arm mismatch"       `Quick test_tyapp_result_arm_mismatch
        ] )
    ; ( "exhaustiveness",
        [ Alcotest.test_case "ADT missing ctor"          `Quick test_exhaustive_adt_missing_ctor
        ; Alcotest.test_case "ADT full coverage"         `Quick test_exhaustive_adt_full
        ; Alcotest.test_case "Bool missing false"        `Quick test_exhaustive_bool_missing_false
        ; Alcotest.test_case "Bool missing true"         `Quick test_exhaustive_bool_missing_true
        ; Alcotest.test_case "Bool full coverage"        `Quick test_exhaustive_bool_full
        ; Alcotest.test_case "Int no wildcard"           `Quick test_exhaustive_int_no_wildcard
        ; Alcotest.test_case "String no wildcard"        `Quick test_exhaustive_string_no_wildcard
        ; Alcotest.test_case "wildcard covers all"       `Quick test_exhaustive_wildcard_covers_all
        ; Alcotest.test_case "var covers all"            `Quick test_exhaustive_var_covers_all
        ; Alcotest.test_case "or-pattern Bool"           `Quick test_exhaustive_or_pattern_bool
        ; Alcotest.test_case "or-pattern ADT"            `Quick test_exhaustive_or_pattern_adt
        ; Alcotest.test_case "record wildcard fields"    `Quick test_exhaustive_record_wildcard_fields
        ; Alcotest.test_case "record literal field"      `Quick test_exhaustive_record_literal_field
        ; Alcotest.test_case "record multi-pattern"      `Quick test_exhaustive_record_multi_pattern
        ; Alcotest.test_case "record open pattern"       `Quick test_exhaustive_record_open_pattern
        ] ) ]
