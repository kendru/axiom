open Axiom_lib.Ast
open Axiom_lib.Lexer
open Axiom_lib.Parser
open Axiom_lib.Printer

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let expr_testable =
  Alcotest.testable pp_expr equal_expr

let prog_testable =
  Alcotest.testable
    (fun fmt prog ->
       Format.pp_print_list
         ~pp_sep:(fun f () -> Format.pp_print_string f "\n")
         pp_decl fmt prog)
    equal_program

let rt_expr label e =
  let src    = print_expr e in
  let parsed = parse_expr (tokenize src) in
  Alcotest.(check expr_testable) label e parsed

let rt_prog label ds =
  let src    = print_program ds in
  let parsed = parse_program (tokenize src) in
  Alcotest.(check prog_testable) label ds parsed

(* ------------------------------------------------------------------ *)
(* Literals and atoms                                                   *)
(* ------------------------------------------------------------------ *)

let test_var ()       = rt_expr "var"       (expr (Var "foo"))
let test_int ()       = rt_expr "int"       (expr (IntLit 42L))
let test_int_zero ()  = rt_expr "int zero"  (expr (IntLit 0L))
let test_float ()     = rt_expr "float"     (expr (FloatLit 3.14))
let test_float_one () = rt_expr "float 1.0" (expr (FloatLit 1.0))
let test_string ()    = rt_expr "string"    (expr (StringLit "hello"))
let test_string_esc () =
  rt_expr "string escapes"
    (expr (StringLit "say \"hi\"\nbye\\done"))
let test_bool_t ()    = rt_expr "true"      (expr (BoolLit true))
let test_bool_f ()    = rt_expr "false"     (expr (BoolLit false))
let test_unit ()      = rt_expr "unit"      (expr UnitLit)

(* ------------------------------------------------------------------ *)
(* Let                                                                  *)
(* ------------------------------------------------------------------ *)

let test_let () =
  rt_expr "let"
    (expr (Let { pat   = pat (PVar "x")
               ; value = expr (IntLit 1L)
               ; body  = expr (Var "x") }))

let test_let_nested () =
  rt_expr "let nested"
    (expr (Let { pat   = pat (PVar "a")
               ; value = expr (IntLit 1L)
               ; body  =
                   expr (Let { pat   = pat (PVar "b")
                              ; value = expr (Var "a")
                              ; body  = expr (Var "b") }) }))

(* ------------------------------------------------------------------ *)
(* Function application                                                 *)
(* ------------------------------------------------------------------ *)

let test_app_noargs () =
  rt_expr "app no args" (expr (App (expr (Var "f"), [])))

let test_app_args () =
  rt_expr "app args"
    (expr (App (expr (Var "f"),
                [expr (IntLit 1L); expr (Var "x")])))

let test_app_chained () =
  rt_expr "app chained"
    (expr (App (expr (App (expr (Var "f"), [expr (Var "x")])),
                [expr (Var "y")])))

let test_app_nested_arg () =
  rt_expr "app nested arg"
    (expr (App (expr (Var "f"),
                [expr (App (expr (Var "g"), [expr (Var "x")]))])))

(* ------------------------------------------------------------------ *)
(* Fn expression                                                        *)
(* ------------------------------------------------------------------ *)

let test_fn_annotated () =
  rt_expr "fn annotated"
    (expr (Fn { params      = [{ param_name = "x"; param_type = TyName "Int" }]
               ; return_type = Some (TyName "Int")
               ; effects     = Some Pure
               ; fn_body     = expr (Var "x") }))

let test_fn_no_annot () =
  rt_expr "fn no annotation"
    (expr (Fn { params      = [{ param_name = "x"; param_type = TyName "Int" }]
               ; return_type = None
               ; effects     = None
               ; fn_body     = expr (Var "x") }))

let test_fn_no_params () =
  rt_expr "fn no params"
    (expr (Fn { params      = []
               ; return_type = Some (TyName "Unit")
               ; effects     = Some Pure
               ; fn_body     = expr UnitLit }))

let test_fn_effect_set () =
  rt_expr "fn effect set"
    (expr (Fn { params      = [{ param_name = "x"; param_type = TyName "Int" }]
               ; return_type = Some (TyName "Int")
               ; effects     = Some (Effects [TyName "Log"])
               ; fn_body     = expr (Var "x") }))

let test_fn_generic_param () =
  rt_expr "fn generic param"
    (expr (Fn { params      = [{ param_name = "xs"
                                ; param_type = TyApp ("List", [TyName "Int"]) }]
               ; return_type = Some (TyName "Int")
               ; effects     = Some Pure
               ; fn_body     = expr (IntLit 0L) }))

(* ------------------------------------------------------------------ *)
(* Match                                                                *)
(* ------------------------------------------------------------------ *)

let test_match_basic () =
  rt_expr "match basic"
    (expr (Match
       { scrutinee = expr (Var "x")
       ; arms = [ { pattern  = pat PLitTrue;  arm_body = expr (IntLit 1L) }
                ; { pattern  = pat PLitFalse; arm_body = expr (IntLit 0L) } ] }))

let test_match_ctor () =
  rt_expr "match constructor"
    (expr (Match
       { scrutinee = expr (Var "opt")
       ; arms = [ { pattern  = pat (PCtor ("Some", [pat (PVar "x")]));
                    arm_body = expr (Var "x") }
                ; { pattern  = pat (PCtor ("None", []));
                    arm_body = expr (IntLit 0L) } ] }))

let test_match_or_pat () =
  rt_expr "match or-pattern"
    (expr (Match
       { scrutinee = expr (Var "n")
       ; arms = [ { pattern  = pat (POr (pat (PLitInt 0L), pat (PLitInt 1L)));
                    arm_body = expr (BoolLit true) }
                ; { pattern  = pat PWild;
                    arm_body = expr (BoolLit false) } ] }))

(* ------------------------------------------------------------------ *)
(* If                                                                   *)
(* ------------------------------------------------------------------ *)

let test_if () =
  rt_expr "if"
    (expr (If { cond  = expr (Var "b")
               ; then_ = expr (IntLit 1L)
               ; else_ = expr (IntLit 0L) }))

let test_if_nested () =
  rt_expr "if nested"
    (expr (If { cond  = expr (Var "a")
               ; then_ = expr (If { cond  = expr (Var "b")
                                   ; then_ = expr (IntLit 1L)
                                   ; else_ = expr (IntLit 2L) })
               ; else_ = expr (IntLit 3L) }))

(* ------------------------------------------------------------------ *)
(* Do                                                                   *)
(* ------------------------------------------------------------------ *)

let test_do_single () =
  rt_expr "do single"
    (expr (Do [StmtExpr (expr (Var "x"))]))

let test_do_seq () =
  rt_expr "do sequence"
    (expr (Do [ StmtExpr (expr (App (expr (Var "f"), [])))
              ; StmtExpr (expr (Var "x")) ]))

let test_do_let () =
  rt_expr "do let"
    (expr (Do [ StmtLet { pat = pat (PVar "x"); value = expr (IntLit 42L) }
              ; StmtExpr (expr (Var "x")) ]))

let test_do_multi_let () =
  rt_expr "do multi-let"
    (expr (Do [ StmtLet { pat = pat (PVar "a"); value = expr (IntLit 1L) }
              ; StmtLet { pat = pat (PVar "b"); value = expr (IntLit 2L) }
              ; StmtExpr (expr (Var "a")) ]))

(* ------------------------------------------------------------------ *)
(* Letrec                                                               *)
(* ------------------------------------------------------------------ *)

let test_letrec_single () =
  rt_expr "letrec single"
    (expr (Letrec
       ( [ { letrec_name        = "f"
           ; letrec_params      = [{ param_name = "x"; param_type = TyName "Int" }]
           ; letrec_return_type = TyName "Int"
           ; letrec_body        = expr (Var "x") } ]
       , expr (App (expr (Var "f"), [expr (IntLit 42L)])) )))

let test_letrec_mutual () =
  rt_expr "letrec mutual"
    (expr (Letrec
       ( [ { letrec_name        = "even"
           ; letrec_params      = [{ param_name = "n"; param_type = TyName "Int" }]
           ; letrec_return_type = TyName "Bool"
           ; letrec_body        = expr (Var "n") }
         ; { letrec_name        = "odd"
           ; letrec_params      = [{ param_name = "n"; param_type = TyName "Int" }]
           ; letrec_return_type = TyName "Bool"
           ; letrec_body        = expr (Var "n") } ]
       , expr (App (expr (Var "even"), [expr (IntLit 0L)])) )))

(* ------------------------------------------------------------------ *)
(* Records and projection                                               *)
(* ------------------------------------------------------------------ *)

let test_record_empty () =
  rt_expr "record empty" (expr (Record []))

let test_record_literal () =
  rt_expr "record literal"
    (expr (Record [ ("x", expr (IntLit 1L))
                  ; ("y", expr (IntLit 2L)) ]))

let test_record_update () =
  rt_expr "record update"
    (expr (RecordUpdate (expr (Var "p"), [("x", expr (IntLit 3L))])))

let test_project () =
  rt_expr "project"
    (expr (Project (expr (Var "p"), "x")))

let test_project_chain () =
  rt_expr "project chain"
    (expr (Project (expr (Project (expr (Var "p"), "x")), "y")))

let test_project_after_app () =
  rt_expr "project after app"
    (expr (Project (expr (App (expr (Var "f"), [expr (Var "x")])), "field")))

(* ------------------------------------------------------------------ *)
(* Perform and Handle                                                   *)
(* ------------------------------------------------------------------ *)

let test_perform_noargs () =
  rt_expr "perform no args"
    (expr (Perform { effect_name = "State"; op_name = "get"; args = [] }))

let test_perform_args () =
  rt_expr "perform with args"
    (expr (Perform { effect_name = "Console"; op_name = "print"
                   ; args = [expr (StringLit "hi")] }))

let test_handle_basic () =
  rt_expr "handle basic"
    (expr (Handle
       { handled  = expr (App (expr (Var "computation"), []))
       ; handlers =
           [ { effect_handler = "State"
             ; op_handlers    =
                 [ { op_handler_name   = "get"
                   ; op_handler_params = []
                   ; op_handler_body   =
                       expr (App (expr (Var "resume"), [expr (Var "s")])) }
                 ; { op_handler_name   = "put"
                   ; op_handler_params = ["v"]
                   ; op_handler_body   =
                       expr (App (expr (Var "resume"), [expr UnitLit])) } ]
             ; return_handler = None } ] }))

let test_handle_return () =
  rt_expr "handle with return"
    (expr (Handle
       { handled  = expr (App (expr (Var "f"), []))
       ; handlers =
           [ { effect_handler = "Throw"
             ; op_handlers    =
                 [ { op_handler_name   = "throw"
                   ; op_handler_params = ["e"]
                   ; op_handler_body   = expr (Var "e") } ]
             ; return_handler =
                 Some { return_var = "v"; return_body = expr (Var "v") } } ] }))

let test_handle_return_only () =
  rt_expr "handle return-only"
    (expr (Handle
       { handled  = expr (App (expr (Var "f"), []))
       ; handlers =
           [ { effect_handler = "Eff"
             ; op_handlers    = []
             ; return_handler =
                 Some { return_var = "x"; return_body = expr (Var "x") } } ] }))

(* ------------------------------------------------------------------ *)
(* Patterns (standalone)                                               *)
(* ------------------------------------------------------------------ *)

(* Exercise patterns via a match expression so the parser can see them. *)
let match_of p body =
  expr (Match { scrutinee = expr (Var "v")
               ; arms = [{ pattern = p; arm_body = body }] })

let test_pat_wild () =
  rt_expr "pat wild"     (match_of (pat PWild) (expr (IntLit 0L)))

let test_pat_var () =
  rt_expr "pat var"      (match_of (pat (PVar "x")) (expr (Var "x")))

let test_pat_int () =
  rt_expr "pat int"      (match_of (pat (PLitInt 7L)) (expr (IntLit 7L)))

let test_pat_float () =
  rt_expr "pat float"    (match_of (pat (PLitFloat 3.14)) (expr (IntLit 0L)))

let test_pat_string () =
  rt_expr "pat string"
    (match_of (pat (PLitString "hi")) (expr (IntLit 0L)))

let test_pat_true () =
  rt_expr "pat true"     (match_of (pat PLitTrue)  (expr (BoolLit true)))

let test_pat_false () =
  rt_expr "pat false"    (match_of (pat PLitFalse) (expr (BoolLit false)))

let test_pat_unit () =
  rt_expr "pat unit"     (match_of (pat PLitUnit)  (expr UnitLit))

let test_pat_ctor_noargs () =
  rt_expr "pat ctor no args"
    (match_of (pat (PCtor ("None", []))) (expr (IntLit 0L)))

let test_pat_ctor_args () =
  rt_expr "pat ctor args"
    (match_of (pat (PCtor ("Some", [pat (PVar "x")]))) (expr (Var "x")))

let test_pat_record_closed () =
  rt_expr "pat record closed"
    (match_of
       (pat (PRecord ([("x", pat (PVar "xv")); ("y", pat (PVar "yv"))], false)))
       (expr (Var "xv")))

let test_pat_record_open () =
  rt_expr "pat record open"
    (match_of
       (pat (PRecord ([("x", pat (PVar "xv"))], true)))
       (expr (Var "xv")))

let test_pat_record_empty () =
  rt_expr "pat record empty closed"
    (match_of (pat (PRecord ([], false))) (expr UnitLit))

let test_pat_or () =
  rt_expr "pat or"
    (match_of
       (pat (POr (pat (PLitInt 0L), pat (PLitInt 1L))))
       (expr (BoolLit true)))

let test_pat_or_right_assoc () =
  (* Parser is right-associative; AST must match that shape *)
  rt_expr "pat or right-assoc"
    (match_of
       (pat (POr (pat (PLitInt 0L),
                  pat (POr (pat (PLitInt 1L), pat (PLitInt 2L))))))
       (expr (BoolLit true)))

(* ------------------------------------------------------------------ *)
(* Type expressions (via fn parameter annotations)                     *)
(* ------------------------------------------------------------------ *)

let fn_with_ty ty =
  expr (Fn { params      = [{ param_name = "x"; param_type = ty }]
            ; return_type = Some ty
            ; effects     = Some Pure
            ; fn_body     = expr (Var "x") })

let test_ty_name () =
  rt_expr "type name"    (fn_with_ty (TyName "Int"))

let test_ty_app () =
  rt_expr "type app"
    (fn_with_ty (TyApp ("List", [TyName "Int"])))

let test_ty_app2 () =
  rt_expr "type app 2"
    (fn_with_ty (TyApp ("Map", [TyName "String"; TyName "Int"])))

let test_ty_tuple () =
  rt_expr "type tuple"
    (fn_with_ty (TyTuple [TyName "Int"; TyName "Bool"]))

let test_ty_fun_zero () =
  rt_expr "type fun zero params"
    (fn_with_ty (TyFun ([], TyName "Unit", Some Pure)))

let test_ty_fun_one () =
  rt_expr "type fun one param"
    (fn_with_ty (TyFun ([TyName "Int"], TyName "Bool", Some Pure)))

let test_ty_fun_effects () =
  rt_expr "type fun with effects"
    (fn_with_ty
       (TyFun ([TyName "Int"], TyName "Int",
               Some (Effects [TyName "Log"; TyName "State"]))))

(* ------------------------------------------------------------------ *)
(* Comments                                                             *)
(* ------------------------------------------------------------------ *)

let test_comment_on_int () =
  rt_expr "comment on int"
    { desc = IntLit 42L; comment = Some "the answer" }

let test_comment_on_app () =
  rt_expr "comment on app"
    { desc    = App (expr (Var "f"), [expr (IntLit 1L)])
    ; comment = Some "a call" }

let test_comment_on_project () =
  rt_expr "comment on project"
    { desc = Project (expr (Var "p"), "x"); comment = Some "field" }

let test_comment_on_pat () =
  rt_expr "comment on pat"
    (match_of
       { pat_desc = PVar "x"; pat_comment = Some "bound" }
       (expr (Var "x")))

(* ------------------------------------------------------------------ *)
(* Declarations                                                         *)
(* ------------------------------------------------------------------ *)

let decl_prog d = [d]

let test_decl_fn_simple () =
  rt_prog "decl fn simple"
    (decl_prog (decl (DeclFn
       { pub         = false
       ; fn_name     = "identity"
       ; type_params = []
       ; params      = [{ param_name = "x"; param_type = TyName "Int" }]
       ; return_type = Some (TyName "Int")
       ; effects     = Some Pure
       ; decl_body   = expr (Var "x") })))

let test_decl_fn_pub () =
  rt_prog "decl fn pub"
    (decl_prog (decl (DeclFn
       { pub         = true
       ; fn_name     = "add"
       ; type_params = []
       ; params      = [ { param_name = "x"; param_type = TyName "Int" }
                        ; { param_name = "y"; param_type = TyName "Int" } ]
       ; return_type = Some (TyName "Int")
       ; effects     = Some Pure
       ; decl_body   = expr (Var "x") })))

let test_decl_fn_type_params () =
  rt_prog "decl fn type params"
    (decl_prog (decl (DeclFn
       { pub         = false
       ; fn_name     = "id"
       ; type_params = ["a"]
       ; params      = [{ param_name = "x"; param_type = TyName "a" }]
       ; return_type = Some (TyName "a")
       ; effects     = Some Pure
       ; decl_body   = expr (Var "x") })))

let test_decl_fn_no_annot () =
  rt_prog "decl fn no annotation"
    (decl_prog (decl (DeclFn
       { pub         = false
       ; fn_name     = "noop"
       ; type_params = []
       ; params      = []
       ; return_type = None
       ; effects     = None
       ; decl_body   = expr UnitLit })))

let test_decl_type () =
  rt_prog "decl type"
    (decl_prog (decl (DeclType
       { pub         = false
       ; type_name   = "Option"
       ; type_params = ["a"]
       ; ctors       = [ { ctor_name = "None"; ctor_params = [] }
                        ; { ctor_name = "Some"; ctor_params = [TyName "a"] } ] })))

let test_decl_type_pub () =
  rt_prog "decl type pub"
    (decl_prog (decl (DeclType
       { pub         = true
       ; type_name   = "Result"
       ; type_params = ["a"; "e"]
       ; ctors       = [ { ctor_name = "Ok";  ctor_params = [TyName "a"] }
                        ; { ctor_name = "Err"; ctor_params = [TyName "e"] } ] })))

let test_decl_effect () =
  rt_prog "decl effect"
    (decl_prog (decl (DeclEffect
       { pub         = false
       ; effect_name = "State"
       ; type_params = ["s"]
       ; ops = [ { effect_op_name   = "get"
                 ; effect_op_params  = []
                 ; effect_op_return  = TyName "s" }
               ; { effect_op_name   = "put"
                 ; effect_op_params  = [TyName "s"]
                 ; effect_op_return  = TyName "Unit" } ] })))

let test_decl_effect_empty () =
  rt_prog "decl effect empty"
    (decl_prog (decl (DeclEffect
       { pub         = false
       ; effect_name = "Marker"
       ; type_params = []
       ; ops         = [] })))

let test_decl_module () =
  rt_prog "decl module"
    (decl_prog (decl (DeclModule
       { pub         = false
       ; module_name = "math"
       ; body        =
           [ decl (DeclFn { pub = false; fn_name = "square"; type_params = []
                           ; params = [{ param_name = "x"; param_type = TyName "Int" }]
                           ; return_type = None; effects = None
                           ; decl_body = expr (Var "x") }) ] })))

let test_decl_require () =
  rt_prog "decl require"
    (decl_prog (decl (DeclRequire (TyName "Log"))))

let test_decl_import_bare () =
  rt_prog "decl import bare"
    (decl_prog (decl (DeclImport { module_path = "json_parser"; alias = None })))

let test_decl_import_alias () =
  rt_prog "decl import alias"
    (decl_prog (decl (DeclImport { module_path = "http_client"; alias = Some "http" })))

let test_decl_comment () =
  rt_prog "decl with comment"
    (decl_prog
       { decl_desc =
           DeclFn { pub = false; fn_name = "foo"; type_params = []
                   ; params = [{ param_name = "x"; param_type = TyName "Int" }]
                   ; return_type = None; effects = None
                   ; decl_body = expr (Var "x") }
       ; decl_comment = Some "entry point" })

(* ------------------------------------------------------------------ *)
(* Multi-declaration program                                            *)
(* ------------------------------------------------------------------ *)

let test_program_two_fns () =
  rt_prog "two fns"
    [ decl (DeclFn { pub = false; fn_name = "foo"; type_params = []
                    ; params = [{ param_name = "x"; param_type = TyName "Int" }]
                    ; return_type = None; effects = None
                    ; decl_body = expr (Var "x") })
    ; decl (DeclFn { pub = false; fn_name = "bar"; type_params = []
                    ; params = [{ param_name = "y"; param_type = TyName "Bool" }]
                    ; return_type = None; effects = None
                    ; decl_body = expr (Var "y") }) ]

(* ------------------------------------------------------------------ *)
(* Test runner                                                          *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "Printer"
    [ ( "literals",
        [ Alcotest.test_case "var"            `Quick test_var
        ; Alcotest.test_case "int"            `Quick test_int
        ; Alcotest.test_case "int zero"       `Quick test_int_zero
        ; Alcotest.test_case "float"          `Quick test_float
        ; Alcotest.test_case "float 1.0"      `Quick test_float_one
        ; Alcotest.test_case "string"         `Quick test_string
        ; Alcotest.test_case "string escapes" `Quick test_string_esc
        ; Alcotest.test_case "true"           `Quick test_bool_t
        ; Alcotest.test_case "false"          `Quick test_bool_f
        ; Alcotest.test_case "unit"           `Quick test_unit
        ] )
    ; ( "let",
        [ Alcotest.test_case "simple"         `Quick test_let
        ; Alcotest.test_case "nested"         `Quick test_let_nested
        ] )
    ; ( "application",
        [ Alcotest.test_case "no args"        `Quick test_app_noargs
        ; Alcotest.test_case "with args"      `Quick test_app_args
        ; Alcotest.test_case "chained"        `Quick test_app_chained
        ; Alcotest.test_case "nested arg"     `Quick test_app_nested_arg
        ] )
    ; ( "fn-expression",
        [ Alcotest.test_case "annotated"      `Quick test_fn_annotated
        ; Alcotest.test_case "no annotation"  `Quick test_fn_no_annot
        ; Alcotest.test_case "no params"      `Quick test_fn_no_params
        ; Alcotest.test_case "effect set"     `Quick test_fn_effect_set
        ; Alcotest.test_case "generic param"  `Quick test_fn_generic_param
        ] )
    ; ( "match",
        [ Alcotest.test_case "basic"          `Quick test_match_basic
        ; Alcotest.test_case "constructor"    `Quick test_match_ctor
        ; Alcotest.test_case "or-pattern"     `Quick test_match_or_pat
        ] )
    ; ( "if",
        [ Alcotest.test_case "basic"          `Quick test_if
        ; Alcotest.test_case "nested"         `Quick test_if_nested
        ] )
    ; ( "do",
        [ Alcotest.test_case "single"         `Quick test_do_single
        ; Alcotest.test_case "sequence"       `Quick test_do_seq
        ; Alcotest.test_case "let stmt"       `Quick test_do_let
        ; Alcotest.test_case "multi-let"      `Quick test_do_multi_let
        ] )
    ; ( "letrec",
        [ Alcotest.test_case "single"         `Quick test_letrec_single
        ; Alcotest.test_case "mutual"         `Quick test_letrec_mutual
        ] )
    ; ( "record",
        [ Alcotest.test_case "empty"          `Quick test_record_empty
        ; Alcotest.test_case "literal"        `Quick test_record_literal
        ; Alcotest.test_case "update"         `Quick test_record_update
        ; Alcotest.test_case "project"        `Quick test_project
        ; Alcotest.test_case "project chain"  `Quick test_project_chain
        ; Alcotest.test_case "project app"    `Quick test_project_after_app
        ] )
    ; ( "perform-handle",
        [ Alcotest.test_case "perform no args"    `Quick test_perform_noargs
        ; Alcotest.test_case "perform with args"  `Quick test_perform_args
        ; Alcotest.test_case "handle basic"       `Quick test_handle_basic
        ; Alcotest.test_case "handle return"      `Quick test_handle_return
        ; Alcotest.test_case "handle return-only" `Quick test_handle_return_only
        ] )
    ; ( "patterns",
        [ Alcotest.test_case "wild"             `Quick test_pat_wild
        ; Alcotest.test_case "var"              `Quick test_pat_var
        ; Alcotest.test_case "int"              `Quick test_pat_int
        ; Alcotest.test_case "float"            `Quick test_pat_float
        ; Alcotest.test_case "string"           `Quick test_pat_string
        ; Alcotest.test_case "true"             `Quick test_pat_true
        ; Alcotest.test_case "false"            `Quick test_pat_false
        ; Alcotest.test_case "unit"             `Quick test_pat_unit
        ; Alcotest.test_case "ctor no args"     `Quick test_pat_ctor_noargs
        ; Alcotest.test_case "ctor args"        `Quick test_pat_ctor_args
        ; Alcotest.test_case "record closed"    `Quick test_pat_record_closed
        ; Alcotest.test_case "record open"      `Quick test_pat_record_open
        ; Alcotest.test_case "record empty"     `Quick test_pat_record_empty
        ; Alcotest.test_case "or"               `Quick test_pat_or
        ; Alcotest.test_case "or right-assoc"   `Quick test_pat_or_right_assoc
        ] )
    ; ( "types",
        [ Alcotest.test_case "TyName"           `Quick test_ty_name
        ; Alcotest.test_case "TyApp"            `Quick test_ty_app
        ; Alcotest.test_case "TyApp 2"          `Quick test_ty_app2
        ; Alcotest.test_case "TyTuple"          `Quick test_ty_tuple
        ; Alcotest.test_case "TyFun zero"       `Quick test_ty_fun_zero
        ; Alcotest.test_case "TyFun one"        `Quick test_ty_fun_one
        ; Alcotest.test_case "TyFun effects"    `Quick test_ty_fun_effects
        ] )
    ; ( "comments",
        [ Alcotest.test_case "on int"           `Quick test_comment_on_int
        ; Alcotest.test_case "on app"           `Quick test_comment_on_app
        ; Alcotest.test_case "on project"       `Quick test_comment_on_project
        ; Alcotest.test_case "on pat"           `Quick test_comment_on_pat
        ] )
    ; ( "declarations",
        [ Alcotest.test_case "fn simple"        `Quick test_decl_fn_simple
        ; Alcotest.test_case "fn pub"           `Quick test_decl_fn_pub
        ; Alcotest.test_case "fn type params"   `Quick test_decl_fn_type_params
        ; Alcotest.test_case "fn no annotation" `Quick test_decl_fn_no_annot
        ; Alcotest.test_case "type"             `Quick test_decl_type
        ; Alcotest.test_case "type pub"         `Quick test_decl_type_pub
        ; Alcotest.test_case "effect"           `Quick test_decl_effect
        ; Alcotest.test_case "effect empty"     `Quick test_decl_effect_empty
        ; Alcotest.test_case "module"           `Quick test_decl_module
        ; Alcotest.test_case "require"          `Quick test_decl_require
        ; Alcotest.test_case "import bare"      `Quick test_decl_import_bare
        ; Alcotest.test_case "import alias"     `Quick test_decl_import_alias
        ; Alcotest.test_case "with comment"     `Quick test_decl_comment
        ] )
    ; ( "program",
        [ Alcotest.test_case "two fns"          `Quick test_program_two_fns
        ] ) ]
