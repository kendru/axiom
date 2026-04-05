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
        ] ) ]
