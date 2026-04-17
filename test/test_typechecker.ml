open Axiom_lib.Typechecker

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let ty_testable = Alcotest.testable pp_ty equal_ty

let infer_of src =
  let tokens = Axiom_lib.Lexer.tokenize src in
  let expr   = Axiom_lib.Parser.parse_expr tokens in
  infer_expr empty_env expr

let check_ty label src expected =
  Alcotest.(check ty_testable) label expected (infer_of src)

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

(* fn (x: Int) -> Int ! pure { x }  =>  Int -> Int *)
let test_fn_annotated () =
  check_ty "fn Int->Int"
    "fn (x: Int) -> Int ! pure { x }"
    (TyFun (TyCon "Int", TyCon "Int"))

(* fn (x: Int) { x }  =>  Int -> Int  (inferred return type) *)
let test_fn_inferred_return () =
  check_ty "fn inferred return"
    "fn (x: Int) { x }"
    (TyFun (TyCon "Int", TyCon "Int"))

(* fn (x: Int, y: Bool) -> Bool ! pure { y }  =>  Int -> Bool -> Bool *)
let test_fn_two_params () =
  check_ty "fn two params"
    "fn (x: Int, y: Bool) -> Bool ! pure { y }"
    (TyFun (TyCon "Int", TyFun (TyCon "Bool", TyCon "Bool")))

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

(* letrec { f(x: Int): Bool = true } in f  =>  Int -> Bool *)
let test_letrec_fn_type () =
  check_ty "letrec fn type"
    "letrec { f(x: Int): Bool = true } in f"
    (TyFun (TyCon "Int", TyCon "Bool"))

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
        ] ) ]
