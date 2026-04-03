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

(* let x = 42 in x  →  Let { name="x"; value=IntLit 42; body=Var "x" } *)
let test_let_int_body_var () =
  check_expr "let x = 42 in x"
    "let x = 42 in x"
    (Let { name = "x"; value = IntLit 42; body = Var "x" })

(* let x = true in x *)
let test_let_bool () =
  check_expr "let x = true in x"
    "let x = true in x"
    (Let { name = "x"; value = BoolLit true; body = Var "x" })

(* let x = "hello" in x *)
let test_let_string () =
  check_expr {|let x = "hello" in x|}
    {|let x = "hello" in x|}
    (Let { name = "x"; value = StringLit "hello"; body = Var "x" })

(* let x = () in x *)
let test_let_unit () =
  check_expr "let x = () in x"
    "let x = () in x"
    (Let { name = "x"; value = UnitLit; body = Var "x" })

(* Nested: let x = 1 in let y = 2 in x *)
let test_let_nested () =
  check_expr "nested let"
    "let x = 1 in let y = 2 in x"
    (Let { name = "x"; value = IntLit 1
         ; body = Let { name = "y"; value = IntLit 2; body = Var "x" } })

(* ------------------------------------------------------------------ *)
(* Test runner                                                          *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "Parser"
    [ ( "let-expression",
        [ Alcotest.test_case "let x = 42 in x"      `Quick test_let_int_body_var
        ; Alcotest.test_case "let x = true in x"    `Quick test_let_bool
        ; Alcotest.test_case "let x = \"hello\" in x" `Quick test_let_string
        ; Alcotest.test_case "let x = () in x"      `Quick test_let_unit
        ; Alcotest.test_case "nested let"            `Quick test_let_nested
        ] ) ]
