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

(** Shorthand: build an expression node with no comment. *)
let e k = expr k

(** Shorthand: build a pattern node with no comment. *)
let p k = pat k

(* ------------------------------------------------------------------ *)
(* Let expression tests                                                 *)
(* ------------------------------------------------------------------ *)

let test_let_int_body_var () =
  check_expr "let x = 42 in x"
    "let x = 42 in x"
    (e (Let { pat = p (PVar "x"); value = e (IntLit 42); body = e (Var "x") }))

let test_let_bool () =
  check_expr "let x = true in x"
    "let x = true in x"
    (e (Let { pat = p (PVar "x"); value = e (BoolLit true); body = e (Var "x") }))

let test_let_string () =
  check_expr {|let x = "hello" in x|}
    {|let x = "hello" in x|}
    (e (Let { pat = p (PVar "x"); value = e (StringLit "hello"); body = e (Var "x") }))

let test_let_unit () =
  check_expr "let x = () in x"
    "let x = () in x"
    (e (Let { pat = p (PVar "x"); value = e UnitLit; body = e (Var "x") }))

let test_let_nested () =
  check_expr "nested let"
    "let x = 1 in let y = 2 in x"
    (e (Let { pat = p (PVar "x"); value = e (IntLit 1)
            ; body = e (Let { pat = p (PVar "y"); value = e (IntLit 2)
                            ; body = e (Var "x") }) }))

let test_let_var_value () =
  check_expr "let y = x in y"
    "let y = x in y"
    (e (Let { pat = p (PVar "y"); value = e (Var "x"); body = e (Var "y") }))

let test_let_chain_var () =
  check_expr "chained var binding"
    "let a = 1 in let b = a in b"
    (e (Let { pat = p (PVar "a"); value = e (IntLit 1)
            ; body = e (Let { pat = p (PVar "b"); value = e (Var "a")
                            ; body = e (Var "b") }) }))

(* ------------------------------------------------------------------ *)
(* Function application                                                 *)
(* ------------------------------------------------------------------ *)

let test_app_single_arg () =
  check_expr "f(42)"
    "f(42)"
    (e (App (e (Var "f"), [e (IntLit 42)])))

let test_app_multi_arg () =
  check_expr "f(x, y)"
    "f(x, y)"
    (e (App (e (Var "f"), [e (Var "x"); e (Var "y")])))

let test_app_no_args () =
  check_expr "f()"
    "f()"
    (e (App (e (Var "f"), [])))

let test_app_nested () =
  check_expr "f(g(x))"
    "f(g(x))"
    (e (App (e (Var "f"), [e (App (e (Var "g"), [e (Var "x")]))])))

let test_app_in_let () =
  check_expr "let x = f(42) in x"
    "let x = f(42) in x"
    (e (Let { pat = p (PVar "x"); value = e (App (e (Var "f"), [e (IntLit 42)]))
            ; body = e (Var "x") }))

(* ------------------------------------------------------------------ *)
(* fn expressions                                                       *)
(* ------------------------------------------------------------------ *)

(* fn (x: Int) -> Int ! pure { x } *)
let test_fn_identity_annotated () =
  check_expr "fn (x: Int) -> Int ! pure { x }"
    "fn (x: Int) -> Int ! pure { x }"
    (e (Fn { params      = [{ param_name = "x"; param_type = TyName "Int" }]
           ; return_type = Some (TyName "Int")
           ; effects     = Some Pure
           ; fn_body     = e (Var "x") }))

(* fn (x: Int, y: Int) -> Int ! pure { x } *)
let test_fn_two_params () =
  check_expr "fn with two params"
    "fn (x: Int, y: Int) -> Int ! pure { x }"
    (e (Fn { params      = [ { param_name = "x"; param_type = TyName "Int" }
                            ; { param_name = "y"; param_type = TyName "Int" } ]
           ; return_type = Some (TyName "Int")
           ; effects     = Some Pure
           ; fn_body     = e (Var "x") }))

(* fn () -> Unit ! pure { () }  — zero params *)
let test_fn_no_params () =
  check_expr "fn no params"
    "fn () -> Unit ! pure { () }"
    (e (Fn { params      = []
           ; return_type = Some (TyName "Unit")
           ; effects     = Some Pure
           ; fn_body     = e UnitLit }))

(* fn (x: Int) -> Int ! pure { x }  body is an application *)
let test_fn_body_app () =
  check_expr "fn body is application"
    "fn (x: Int) -> Int ! pure { f(x) }"
    (e (Fn { params      = [{ param_name = "x"; param_type = TyName "Int" }]
           ; return_type = Some (TyName "Int")
           ; effects     = Some Pure
           ; fn_body     = e (App (e (Var "f"), [e (Var "x")])) }))

(* Without return type annotation — optional in body position *)
let test_fn_no_annotation () =
  check_expr "fn without annotation"
    "fn (x: Int) { x }"
    (e (Fn { params      = [{ param_name = "x"; param_type = TyName "Int" }]
           ; return_type = None
           ; effects     = None
           ; fn_body     = e (Var "x") }))

(* Parameterised type: fn (xs: List<Int>) -> Int ! pure { 0 } *)
let test_fn_generic_param () =
  check_expr "fn with generic param type"
    "fn (xs: List<Int>) -> Int ! pure { 0 }"
    (e (Fn { params      = [{ param_name = "xs"
                             ; param_type = TyApp ("List", [TyName "Int"]) }]
           ; return_type = Some (TyName "Int")
           ; effects     = Some Pure
           ; fn_body     = e (IntLit 0) }))

(* Effect set with named effects: fn (x: Int) -> Int ! {Log, Throw<E>} { x } *)
let test_fn_effect_set () =
  check_expr "fn with effect set"
    "fn (x: Int) -> Int ! {Log} { x }"
    (e (Fn { params      = [{ param_name = "x"; param_type = TyName "Int" }]
           ; return_type = Some (TyName "Int")
           ; effects     = Some (Effects [TyName "Log"])
           ; fn_body     = e (Var "x") }))

(* ------------------------------------------------------------------ *)
(* match expressions                                                    *)
(* ------------------------------------------------------------------ *)

(* match x with { | true => 1 | false => 0 } *)
let test_match_bool_arms () =
  check_expr "match bool"
    "match x with { | true => 1 | false => 0 }"
    (e (Match { scrutinee = e (Var "x")
              ; arms = [ { pattern = p (PLit (LBool true));  arm_body = e (IntLit 1) }
                       ; { pattern = p (PLit (LBool false)); arm_body = e (IntLit 0) } ] }))

(* match n with { | 0 => true | _ => false } *)
let test_match_wildcard () =
  check_expr "match with wildcard"
    "match n with { | 0 => true | _ => false }"
    (e (Match { scrutinee = e (Var "n")
              ; arms = [ { pattern = p (PLit (LInt 0)); arm_body = e (BoolLit true) }
                       ; { pattern = p PWild;            arm_body = e (BoolLit false) } ] }))

(* match opt with { | Some(x) => x | None => 0 } *)
let test_match_constructor () =
  check_expr "match constructor"
    "match opt with { | Some(x) => x | None => 0 }"
    (e (Match { scrutinee = e (Var "opt")
              ; arms = [ { pattern = p (PCtor ("Some", [p (PVar "x")])); arm_body = e (Var "x") }
                       ; { pattern = p (PCtor ("None", []));              arm_body = e (IntLit 0) } ] }))

(* match x with { | y => y }  -- variable binding pattern *)
let test_match_var_pattern () =
  check_expr "match var pattern"
    "match x with { | y => y }"
    (e (Match { scrutinee = e (Var "x")
              ; arms = [ { pattern = p (PVar "y"); arm_body = e (Var "y") } ] }))

(* ------------------------------------------------------------------ *)
(* if / else                                                            *)
(* ------------------------------------------------------------------ *)

(* if b { 1 } else { 0 } *)
let test_if_basic () =
  check_expr "if basic"
    "if b { 1 } else { 0 }"
    (e (If { cond = e (Var "b"); then_ = e (IntLit 1); else_ = e (IntLit 0) }))

(* if cond { f(x) } else { g(x) } *)
let test_if_app_body () =
  check_expr "if with app body"
    "if cond { f(x) } else { g(x) }"
    (e (If { cond = e (Var "cond")
           ; then_ = e (App (e (Var "f"), [e (Var "x")]))
           ; else_ = e (App (e (Var "g"), [e (Var "x")])) }))

(* if a { if b { 1 } else { 2 } } else { 3 } *)
let test_if_nested () =
  check_expr "nested if"
    "if a { if b { 1 } else { 2 } } else { 3 }"
    (e (If { cond  = e (Var "a")
           ; then_ = e (If { cond = e (Var "b"); then_ = e (IntLit 1); else_ = e (IntLit 2) })
           ; else_ = e (IntLit 3) }))

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
    (e (Handle
       { handled   = e (App (e (Var "computation"), []))
       ; handlers  =
           [ { effect_handler = "State"
             ; op_handlers    =
                 [ { op_handler_name   = "get"
                   ; op_handler_params = []
                   ; op_handler_body   = e (App (e (Var "resume"), [e (Var "s")])) }
                 ; { op_handler_name   = "put"
                   ; op_handler_params = ["v"]
                   ; op_handler_body   = e (App (e (Var "resume"), [e UnitLit])) } ]
             ; return_handler = None } ] }))

(* handle with a return branch *)
let test_handle_return_branch () =
  check_expr "handle with return"
    "handle f() with { Throw { throw(e) => e \n return v => v } }"
    (e (Handle
       { handled  = e (App (e (Var "f"), []))
       ; handlers =
           [ { effect_handler = "Throw"
             ; op_handlers    =
                 [ { op_handler_name   = "throw"
                   ; op_handler_params = ["e"]
                   ; op_handler_body   = e (Var "e") } ]
             ; return_handler = Some { return_var = "v"; return_body = e (Var "v") } } ] }))

(* ------------------------------------------------------------------ *)
(* perform                                                              *)
(* ------------------------------------------------------------------ *)

(* perform Console.print("hi") *)
let test_perform_basic () =
  check_expr "perform basic"
    {|perform Console.print("hi")|}
    (e (Perform { effect_name = "Console"; op_name = "print"
                ; args = [e (StringLit "hi")] }))

(* perform State.get() -- zero args *)
let test_perform_no_args () =
  check_expr "perform no args"
    "perform State.get()"
    (e (Perform { effect_name = "State"; op_name = "get"; args = [] }))

(* perform Throw.throw(KeyNotFound(key)) -- ctor arg *)
let test_perform_ctor_arg () =
  check_expr "perform ctor arg"
    "perform Throw.throw(err)"
    (e (Perform { effect_name = "Throw"; op_name = "throw"; args = [e (Var "err")] }))

(* inside a do block *)
let test_perform_in_do () =
  check_expr "perform in do"
    {|do { perform Log.log(msg); x }|}
    (e (Do [ StmtExpr (e (Perform { effect_name = "Log"; op_name = "log"
                                   ; args = [e (Var "msg")] }))
           ; StmtExpr (e (Var "x")) ]))

(* ------------------------------------------------------------------ *)
(* do blocks                                                            *)
(* ------------------------------------------------------------------ *)

(* do { x }  -- trivial single-expression block *)
let test_do_single () =
  check_expr "do single expr"
    "do { x }"
    (e (Do [StmtExpr (e (Var "x"))]))

(* do { f(); x }  -- effect stmt then result *)
let test_do_effect_then_result () =
  check_expr "do effect then result"
    "do { f(); x }"
    (e (Do [StmtExpr (e (App (e (Var "f"), []))); StmtExpr (e (Var "x"))]))

(* do { let x = 42; x }  -- let stmt (no 'in') *)
let test_do_let_stmt () =
  check_expr "do let stmt"
    "do { let x = 42; x }"
    (e (Do [StmtLet { pat = p (PVar "x"); value = e (IntLit 42) }; StmtExpr (e (Var "x"))]))

(* do { let a = 1; let b = 2; a }  -- multiple let stmts *)
let test_do_multi_let () =
  check_expr "do multi let"
    "do { let a = 1; let b = 2; a }"
    (e (Do [ StmtLet { pat = p (PVar "a"); value = e (IntLit 1) }
           ; StmtLet { pat = p (PVar "b"); value = e (IntLit 2) }
           ; StmtExpr (e (Var "a")) ]))

(* ------------------------------------------------------------------ *)
(* letrec expressions                                                   *)
(* ------------------------------------------------------------------ *)

(* letrec { f(x: Int): Int = x } in f(42) *)
let test_letrec_single () =
  check_expr "letrec single"
    "letrec { f(x: Int): Int = x } in f(42)"
    (e (Letrec
       ( [ { letrec_name = "f"
           ; letrec_params = [{ param_name = "x"; param_type = TyName "Int" }]
           ; letrec_return_type = TyName "Int"
           ; letrec_body = e (Var "x") } ]
       , e (App (e (Var "f"), [e (IntLit 42)])) )))

(* letrec { even(n: Int): Bool = n, odd(n: Int): Bool = n } in even(0) *)
let test_letrec_mutual () =
  check_expr "letrec mutual"
    "letrec { even(n: Int): Bool = n, odd(n: Int): Bool = n } in even(0)"
    (e (Letrec
       ( [ { letrec_name = "even"
           ; letrec_params = [{ param_name = "n"; param_type = TyName "Int" }]
           ; letrec_return_type = TyName "Bool"
           ; letrec_body = e (Var "n") }
         ; { letrec_name = "odd"
           ; letrec_params = [{ param_name = "n"; param_type = TyName "Int" }]
           ; letrec_return_type = TyName "Bool"
           ; letrec_body = e (Var "n") } ]
       , e (App (e (Var "even"), [e (IntLit 0)])) )))

(* ------------------------------------------------------------------ *)
(* Record literals, updates, and projection                            *)
(* ------------------------------------------------------------------ *)

(* {} -- empty record *)
let test_record_empty () =
  check_expr "empty record"
    "{}"
    (e (Record []))

(* { x: 1, y: 2 } *)
let test_record_literal () =
  check_expr "record literal"
    "{ x: 1, y: 2 }"
    (e (Record [("x", e (IntLit 1)); ("y", e (IntLit 2))]))

(* { p with x: 3 } -- record update *)
let test_record_update () =
  check_expr "record update"
    "{ p with x: 3 }"
    (e (RecordUpdate (e (Var "p"), [("x", e (IntLit 3))])))

(* p.x -- field projection *)
let test_project_field () =
  check_expr "project field"
    "p.x"
    (e (Project (e (Var "p"), "x")))

(* p.x.y -- chained projection *)
let test_project_chain () =
  check_expr "chained projection"
    "p.x.y"
    (e (Project (e (Project (e (Var "p"), "x")), "y")))

(* ------------------------------------------------------------------ *)
(* Parenthesised expressions                                            *)
(* ------------------------------------------------------------------ *)

let test_paren_grouping () =
  check_expr "parenthesised expression"
    "(42)"
    (e (IntLit 42))

let test_paren_var () =
  check_expr "parenthesised var"
    "(x)"
    (e (Var "x"))

(* ------------------------------------------------------------------ *)
(* Comment attachment on expressions                                    *)
(* ------------------------------------------------------------------ *)

(* 42 @# the answer #@ *)
let test_comment_on_atom () =
  check_expr "comment on literal"
    "42 @# the answer #@"
    { kind = IntLit 42; comment = Some "the answer" }

(* let x = 42 @# the answer #@ in x *)
let test_comment_on_value () =
  check_expr "comment on let value"
    "let x = 42 @# the answer #@ in x"
    (e (Let { pat = p (PVar "x")
            ; value = { kind = IntLit 42; comment = Some "the answer" }
            ; body = e (Var "x") }))

(* f(42) @# function call #@ *)
let test_comment_on_app () =
  check_expr "comment on application"
    "f(42) @# function call #@"
    { kind = App (e (Var "f"), [e (IntLit 42)]); comment = Some "function call" }

(* (x) @# grouped #@ *)
let test_comment_on_paren () =
  check_expr "comment on parenthesised"
    "(x) @# grouped #@"
    { kind = Var "x"; comment = Some "grouped" }

(* f(42 @# the arg #@) *)
let test_comment_on_arg () =
  check_expr "comment on arg"
    "f(42 @# the arg #@)"
    (e (App (e (Var "f"), [{ kind = IntLit 42; comment = Some "the arg" }])))

(* p.x @# field access #@ *)
let test_comment_on_project () =
  check_expr "comment on projection"
    "p.x @# field access #@"
    { kind = Project (e (Var "p"), "x"); comment = Some "field access" }

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
        ] )
    ; ( "parenthesised",
        [ Alcotest.test_case "grouping"          `Quick test_paren_grouping
        ; Alcotest.test_case "var"               `Quick test_paren_var
        ] )
    ; ( "comments",
        [ Alcotest.test_case "on atom"           `Quick test_comment_on_atom
        ; Alcotest.test_case "on let value"      `Quick test_comment_on_value
        ; Alcotest.test_case "on app"            `Quick test_comment_on_app
        ; Alcotest.test_case "on paren"          `Quick test_comment_on_paren
        ; Alcotest.test_case "on arg"            `Quick test_comment_on_arg
        ; Alcotest.test_case "on projection"     `Quick test_comment_on_project
        ] ) ]
