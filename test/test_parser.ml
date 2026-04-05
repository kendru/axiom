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
    (Let { pat = PVar "x"; value = IntLit 42; body = Var "x" })

let test_let_bool () =
  check_expr "let x = true in x"
    "let x = true in x"
    (Let { pat = PVar "x"; value = BoolLit true; body = Var "x" })

let test_let_string () =
  check_expr {|let x = "hello" in x|}
    {|let x = "hello" in x|}
    (Let { pat = PVar "x"; value = StringLit "hello"; body = Var "x" })

let test_let_unit () =
  check_expr "let x = () in x"
    "let x = () in x"
    (Let { pat = PVar "x"; value = UnitLit; body = Var "x" })

let test_let_nested () =
  check_expr "nested let"
    "let x = 1 in let y = 2 in x"
    (Let { pat = PVar "x"; value = IntLit 1
         ; body = Let { pat = PVar "y"; value = IntLit 2; body = Var "x" } })

let test_let_var_value () =
  check_expr "let y = x in y"
    "let y = x in y"
    (Let { pat = PVar "y"; value = Var "x"; body = Var "y" })

let test_let_chain_var () =
  check_expr "chained var binding"
    "let a = 1 in let b = a in b"
    (Let { pat = PVar "a"; value = IntLit 1
         ; body = Let { pat = PVar "b"; value = Var "a"; body = Var "b" } })

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
    (Let { pat = PVar "x"; value = App (Var "f", [IntLit 42]); body = Var "x" })

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

(* ------------------------------------------------------------------ *)
(* if / else                                                            *)
(* ------------------------------------------------------------------ *)

(* if b { 1 } else { 0 } *)
let test_if_basic () =
  check_expr "if basic"
    "if b { 1 } else { 0 }"
    (If { cond = Var "b"; then_ = IntLit 1; else_ = IntLit 0 })

(* if cond { f(x) } else { g(x) } *)
let test_if_app_body () =
  check_expr "if with app body"
    "if cond { f(x) } else { g(x) }"
    (If { cond = Var "cond"
        ; then_ = App (Var "f", [Var "x"])
        ; else_ = App (Var "g", [Var "x"]) })

(* if a { if b { 1 } else { 2 } } else { 3 } *)
let test_if_nested () =
  check_expr "nested if"
    "if a { if b { 1 } else { 2 } } else { 3 }"
    (If { cond  = Var "a"
        ; then_ = If { cond = Var "b"; then_ = IntLit 1; else_ = IntLit 2 }
        ; else_ = IntLit 3 })

(* ------------------------------------------------------------------ *)
(* handle expressions                                                   *)
(* ------------------------------------------------------------------ *)

(*
  handle computation() with {
    State {
      get()    => resume(current)
      put(s)   => resume(())
    }
  }
*)
let test_handle_state () =
  check_expr "handle State"
    "handle computation() with { State { get() => resume(s) \n put(v) => resume(()) } }"
    (Handle
       { handled   = App (Var "computation", [])
       ; handlers  =
           [ { effect_handler = "State"
             ; op_handlers    =
                 [ { op_handler_name   = "get"
                   ; op_handler_params = []
                   ; op_handler_body   = App (Var "resume", [Var "s"]) }
                 ; { op_handler_name   = "put"
                   ; op_handler_params = ["v"]
                   ; op_handler_body   = App (Var "resume", [UnitLit]) } ]
             ; return_handler = None } ] })

(* handle with a return branch *)
let test_handle_return_branch () =
  check_expr "handle with return"
    "handle f() with { Throw { throw(e) => e \n return v => v } }"
    (Handle
       { handled  = App (Var "f", [])
       ; handlers =
           [ { effect_handler = "Throw"
             ; op_handlers    =
                 [ { op_handler_name   = "throw"
                   ; op_handler_params = ["e"]
                   ; op_handler_body   = Var "e" } ]
             ; return_handler = Some { return_var = "v"; return_body = Var "v" } } ] })

(* ------------------------------------------------------------------ *)
(* perform                                                              *)
(* ------------------------------------------------------------------ *)

(* perform Console.print("hi") *)
let test_perform_basic () =
  check_expr "perform basic"
    {|perform Console.print("hi")|}
    (Perform { effect_name = "Console"; op_name = "print"
             ; args = [StringLit "hi"] })

(* perform State.get() -- zero args *)
let test_perform_no_args () =
  check_expr "perform no args"
    "perform State.get()"
    (Perform { effect_name = "State"; op_name = "get"; args = [] })

(* perform Throw.throw(KeyNotFound(key)) -- ctor arg *)
let test_perform_ctor_arg () =
  check_expr "perform ctor arg"
    "perform Throw.throw(err)"
    (Perform { effect_name = "Throw"; op_name = "throw"; args = [Var "err"] })

(* inside a do block *)
let test_perform_in_do () =
  check_expr "perform in do"
    {|do { perform Log.log(msg); x }|}
    (Do [ StmtExpr (Perform { effect_name = "Log"; op_name = "log"
                             ; args = [Var "msg"] })
        ; StmtExpr (Var "x") ])

(* ------------------------------------------------------------------ *)
(* do blocks                                                            *)
(* ------------------------------------------------------------------ *)

(* do { x }  -- trivial single-expression block *)
let test_do_single () =
  check_expr "do single expr"
    "do { x }"
    (Do [StmtExpr (Var "x")])

(* do { f(); x }  -- effect stmt then result *)
let test_do_effect_then_result () =
  check_expr "do effect then result"
    "do { f(); x }"
    (Do [StmtExpr (App (Var "f", [])); StmtExpr (Var "x")])

(* do { let x = 42; x }  -- let stmt (no 'in') *)
let test_do_let_stmt () =
  check_expr "do let stmt"
    "do { let x = 42; x }"
    (Do [StmtLet { pat = PVar "x"; value = IntLit 42 }; StmtExpr (Var "x")])

(* do { let a = 1; let b = 2; a }  -- multiple let stmts *)
let test_do_multi_let () =
  check_expr "do multi let"
    "do { let a = 1; let b = 2; a }"
    (Do [ StmtLet { pat = PVar "a"; value = IntLit 1 }
        ; StmtLet { pat = PVar "b"; value = IntLit 2 }
        ; StmtExpr (Var "a") ])

(* ------------------------------------------------------------------ *)
(* letrec expressions                                                   *)
(* ------------------------------------------------------------------ *)

(* letrec { f(x: Int): Int = x } in f(42) *)
let test_letrec_single () =
  check_expr "letrec single"
    "letrec { f(x: Int): Int = x } in f(42)"
    (Letrec
       ( [ { letrec_name = "f"
           ; letrec_params = [{ param_name = "x"; param_type = TyName "Int" }]
           ; letrec_return_type = TyName "Int"
           ; letrec_body = Var "x" } ]
       , App (Var "f", [IntLit 42]) ))

(* letrec { even(n: Int): Bool = n, odd(n: Int): Bool = n } in even(0) *)
let test_letrec_mutual () =
  check_expr "letrec mutual"
    "letrec { even(n: Int): Bool = n, odd(n: Int): Bool = n } in even(0)"
    (Letrec
       ( [ { letrec_name = "even"
           ; letrec_params = [{ param_name = "n"; param_type = TyName "Int" }]
           ; letrec_return_type = TyName "Bool"
           ; letrec_body = Var "n" }
         ; { letrec_name = "odd"
           ; letrec_params = [{ param_name = "n"; param_type = TyName "Int" }]
           ; letrec_return_type = TyName "Bool"
           ; letrec_body = Var "n" } ]
       , App (Var "even", [IntLit 0]) ))

(* ------------------------------------------------------------------ *)
(* Record literals, updates, and projection                            *)
(* ------------------------------------------------------------------ *)

(* {} -- empty record *)
let test_record_empty () =
  check_expr "empty record"
    "{}"
    (Record [])

(* { x: 1, y: 2 } *)
let test_record_literal () =
  check_expr "record literal"
    "{ x: 1, y: 2 }"
    (Record [("x", IntLit 1); ("y", IntLit 2)])

(* { p with x: 3 } -- record update *)
let test_record_update () =
  check_expr "record update"
    "{ p with x: 3 }"
    (RecordUpdate (Var "p", [("x", IntLit 3)]))

(* p.x -- field projection *)
let test_project_field () =
  check_expr "project field"
    "p.x"
    (Project (Var "p", "x"))

(* p.x.y -- chained projection *)
let test_project_chain () =
  check_expr "chained projection"
    "p.x.y"
    (Project (Project (Var "p", "x"), "y"))

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
        ] )
    ; ( "if-else",
        [ Alcotest.test_case "if true/false"     `Quick test_if_basic
        ; Alcotest.test_case "if with app body"  `Quick test_if_app_body
        ; Alcotest.test_case "nested if"         `Quick test_if_nested
        ] )
    ; ( "do-block",
        [ Alcotest.test_case "single expr"           `Quick test_do_single
        ; Alcotest.test_case "effect then result"    `Quick test_do_effect_then_result
        ; Alcotest.test_case "let stmt"              `Quick test_do_let_stmt
        ; Alcotest.test_case "multi let"             `Quick test_do_multi_let
        ] )
    ; ( "perform",
        [ Alcotest.test_case "basic"        `Quick test_perform_basic
        ; Alcotest.test_case "no args"      `Quick test_perform_no_args
        ; Alcotest.test_case "ctor arg"     `Quick test_perform_ctor_arg
        ; Alcotest.test_case "in do block"  `Quick test_perform_in_do
        ] )
    ; ( "handle",
        [ Alcotest.test_case "State handler"    `Quick test_handle_state
        ; Alcotest.test_case "return branch"    `Quick test_handle_return_branch
        ] )
    ; ( "letrec",
        [ Alcotest.test_case "single binding"   `Quick test_letrec_single
        ; Alcotest.test_case "mutual bindings"  `Quick test_letrec_mutual
        ] )
    ; ( "record",
        [ Alcotest.test_case "empty"             `Quick test_record_empty
        ; Alcotest.test_case "literal"           `Quick test_record_literal
        ; Alcotest.test_case "update"            `Quick test_record_update
        ; Alcotest.test_case "project field"     `Quick test_project_field
        ; Alcotest.test_case "chained project"   `Quick test_project_chain
        ] ) ]
