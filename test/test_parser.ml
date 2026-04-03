open Axiom_lib.Ast
open Axiom_lib.Parser

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let expr_testable = Alcotest.testable pp_expr equal_expr

let parse_expr_of src =
  parse_expr (Axiom_lib.Lexer.tokenize src)

let check_expr label src expected =
  Alcotest.(check expr_testable) label expected (parse_expr_of src)

(* ------------------------------------------------------------------ *)
(* Let expression tests                                                 *)
(* ------------------------------------------------------------------ *)

let test_let_int_body_var () =
  check_expr "let x = 42 in x"
    "let x = 42 in x"
    (Let { name = "x"; value = IntLit 42; body = Var "x" })

let test_let_bool () =
  check_expr "let x = true in x"
    "let x = true in x"
    (Let { name = "x"; value = BoolLit true; body = Var "x" })

let test_let_string () =
  check_expr {|let x = "hello" in x|}
    {|let x = "hello" in x|}
    (Let { name = "x"; value = StringLit "hello"; body = Var "x" })

let test_let_unit () =
  check_expr "let x = () in x"
    "let x = () in x"
    (Let { name = "x"; value = UnitLit; body = Var "x" })

let test_let_nested () =
  check_expr "nested let"
    "let x = 1 in let y = 2 in x"
    (Let { name = "x"; value = IntLit 1
         ; body = Let { name = "y"; value = IntLit 2; body = Var "x" } })

let test_let_var_value () =
  check_expr "let y = x in y"
    "let y = x in y"
    (Let { name = "y"; value = Var "x"; body = Var "y" })

let test_let_chain_var () =
  check_expr "chained var binding"
    "let a = 1 in let b = a in b"
    (Let { name = "a"; value = IntLit 1
         ; body = Let { name = "b"; value = Var "a"; body = Var "b" } })

(* ------------------------------------------------------------------ *)
(* Function application                                                 *)
(* ------------------------------------------------------------------ *)

let test_app_single_arg () =
  check_expr "f(42)"
    "f(42)"
    (App (Var "f", [IntLit 42]))

let test_app_multi_arg () =
  check_expr "f(x, y)"
    "f(x, y)"
    (App (Var "f", [Var "x"; Var "y"]))

let test_app_no_args () =
  check_expr "f()"
    "f()"
    (App (Var "f", []))

let test_app_nested () =
  check_expr "f(g(x))"
    "f(g(x))"
    (App (Var "f", [App (Var "g", [Var "x"])]))

let test_app_in_let () =
  check_expr "let x = f(42) in x"
    "let x = f(42) in x"
    (Let { name = "x"; value = App (Var "f", [IntLit 42]); body = Var "x" })

(* ------------------------------------------------------------------ *)
(* fn expressions                                                       *)
(* ------------------------------------------------------------------ *)

(* fn (x: Int) -> Int ! pure { x } *)
let test_fn_identity_annotated () =
  check_expr "fn (x: Int) -> Int ! pure { x }"
    "fn (x: Int) -> Int ! pure { x }"
    (Fn { params      = [{ param_name = "x"; param_type = TyName "Int" }]
        ; return_type = Some (TyName "Int")
        ; effects     = Some Pure
        ; fn_body     =Var "x" })

(* fn (x: Int, y: Int) -> Int ! pure { x } *)
let test_fn_two_params () =
  check_expr "fn with two params"
    "fn (x: Int, y: Int) -> Int ! pure { x }"
    (Fn { params      = [ { param_name = "x"; param_type = TyName "Int" }
                         ; { param_name = "y"; param_type = TyName "Int" } ]
        ; return_type = Some (TyName "Int")
        ; effects     = Some Pure
        ; fn_body     =Var "x" })

(* fn () -> Unit ! pure { () }  — zero params *)
let test_fn_no_params () =
  check_expr "fn no params"
    "fn () -> Unit ! pure { () }"
    (Fn { params      = []
        ; return_type = Some (TyName "Unit")
        ; effects     = Some Pure
        ; fn_body     =UnitLit })

(* fn (x: Int) -> Int ! pure { x }  body is an application *)
let test_fn_body_app () =
  check_expr "fn body is application"
    "fn (x: Int) -> Int ! pure { f(x) }"
    (Fn { params      = [{ param_name = "x"; param_type = TyName "Int" }]
        ; return_type = Some (TyName "Int")
        ; effects     = Some Pure
        ; fn_body     =App (Var "f", [Var "x"]) })

(* Without return type annotation — optional in body position *)
let test_fn_no_annotation () =
  check_expr "fn without annotation"
    "fn (x: Int) { x }"
    (Fn { params      = [{ param_name = "x"; param_type = TyName "Int" }]
        ; return_type = None
        ; effects     = None
        ; fn_body     =Var "x" })

(* Parameterised type: fn (xs: List<Int>) -> Int ! pure { 0 } *)
let test_fn_generic_param () =
  check_expr "fn with generic param type"
    "fn (xs: List<Int>) -> Int ! pure { 0 }"
    (Fn { params      = [{ param_name = "xs"
                         ; param_type = TyApp ("List", [TyName "Int"]) }]
        ; return_type = Some (TyName "Int")
        ; effects     = Some Pure
        ; fn_body     =IntLit 0 })

(* Effect set with named effects: fn (x: Int) -> Int ! {Log, Throw<E>} { x } *)
let test_fn_effect_set () =
  check_expr "fn with effect set"
    "fn (x: Int) -> Int ! {Log} { x }"
    (Fn { params      = [{ param_name = "x"; param_type = TyName "Int" }]
        ; return_type = Some (TyName "Int")
        ; effects     = Some (Effects [TyName "Log"])
        ; fn_body     =Var "x" })

(* ------------------------------------------------------------------ *)
(* match expressions                                                    *)
(* ------------------------------------------------------------------ *)

(* match x with { | true => 1 | false => 0 } *)
let test_match_bool_arms () =
  check_expr "match bool"
    "match x with { | true => 1 | false => 0 }"
    (Match { scrutinee = Var "x"
           ; arms = [ { pattern = PLit (LBool true);  arm_body = IntLit 1 }
                    ; { pattern = PLit (LBool false); arm_body = IntLit 0 } ] })

(* match n with { | 0 => true | _ => false } *)
let test_match_wildcard () =
  check_expr "match with wildcard"
    "match n with { | 0 => true | _ => false }"
    (Match { scrutinee = Var "n"
           ; arms = [ { pattern = PLit (LInt 0); arm_body = BoolLit true }
                    ; { pattern = PWild;          arm_body = BoolLit false } ] })

(* match opt with { | Some(x) => x | None => 0 } *)
let test_match_constructor () =
  check_expr "match constructor"
    "match opt with { | Some(x) => x | None => 0 }"
    (Match { scrutinee = Var "opt"
           ; arms = [ { pattern = PCtor ("Some", [PVar "x"]); arm_body = Var "x" }
                    ; { pattern = PCtor ("None", []);          arm_body = IntLit 0 } ] })

(* match x with { | y => y }  -- variable binding pattern *)
let test_match_var_pattern () =
  check_expr "match var pattern"
    "match x with { | y => y }"
    (Match { scrutinee = Var "x"
           ; arms = [ { pattern = PVar "y"; arm_body = Var "y" } ] })

(* ------------------------------------------------------------------ *)
(* Test runner                                                          *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "Parser"
    [ ( "let-expression",
        [ Alcotest.test_case "let x = 42 in x"        `Quick test_let_int_body_var
        ; Alcotest.test_case "let x = true in x"      `Quick test_let_bool
        ; Alcotest.test_case "let x = \"hello\" in x" `Quick test_let_string
        ; Alcotest.test_case "let x = () in x"        `Quick test_let_unit
        ; Alcotest.test_case "nested let"              `Quick test_let_nested
        ; Alcotest.test_case "var as let value"        `Quick test_let_var_value
        ; Alcotest.test_case "chained var binding"     `Quick test_let_chain_var
        ] )
    ; ( "application",
        [ Alcotest.test_case "f(42)"              `Quick test_app_single_arg
        ; Alcotest.test_case "f(x, y)"            `Quick test_app_multi_arg
        ; Alcotest.test_case "f()"                `Quick test_app_no_args
        ; Alcotest.test_case "f(g(x))"            `Quick test_app_nested
        ; Alcotest.test_case "let x = f(42) in x" `Quick test_app_in_let
        ] )
    ; ( "fn-expression",
        [ Alcotest.test_case "identity annotated"  `Quick test_fn_identity_annotated
        ; Alcotest.test_case "two params"          `Quick test_fn_two_params
        ; Alcotest.test_case "no params"           `Quick test_fn_no_params
        ; Alcotest.test_case "body is application" `Quick test_fn_body_app
        ; Alcotest.test_case "no annotation"       `Quick test_fn_no_annotation
        ; Alcotest.test_case "generic param type"  `Quick test_fn_generic_param
        ; Alcotest.test_case "effect set"          `Quick test_fn_effect_set
        ] )
    ; ( "match-expression",
        [ Alcotest.test_case "bool arms"         `Quick test_match_bool_arms
        ; Alcotest.test_case "wildcard arm"      `Quick test_match_wildcard
        ; Alcotest.test_case "constructor arms"  `Quick test_match_constructor
        ; Alcotest.test_case "var pattern"       `Quick test_match_var_pattern
        ] ) ]
