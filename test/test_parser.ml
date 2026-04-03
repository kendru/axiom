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

(* ------------------------------------------------------------------ *)
(* Variable reference as let value                                      *)
(* ------------------------------------------------------------------ *)

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
        ] ) ]
