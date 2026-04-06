(** Round-trip tests: encode → decode → compare AST equality.

    Every test encodes an AST value to binary, decodes it back, and
    checks structural equality with the original. This validates that
    the encoder and decoder are exact inverses. *)

open Axiom_lib.Ast
open Axiom_lib.Node_encoding
open Axiom_lib.Node_decoding

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let expr_testable = Alcotest.testable pp_expr equal_expr
let decl_testable = Alcotest.testable pp_decl equal_decl
let program_testable = Alcotest.testable
  (fun fmt p -> Format.pp_print_list ~pp_sep:(fun f () -> Format.fprintf f "; ") pp_decl fmt p)
  equal_program

let roundtrip_expr (e : expr) =
  let store, tbl = make_mem_store () in
  let h = encode_expr store e in
  let lookup = lookup_of_hashtbl tbl in
  decode_expr lookup h

let roundtrip_decl (d : decl) =
  let store, tbl = make_mem_store () in
  let h = encode_decl store d in
  let lookup = lookup_of_hashtbl tbl in
  decode_decl lookup h

let roundtrip_program (p : program) =
  let store, tbl = make_mem_store () in
  let h = encode_program store p in
  let lookup = lookup_of_hashtbl tbl in
  decode_program lookup h

let check_expr label e =
  Alcotest.(check expr_testable) label e (roundtrip_expr e)

let check_decl label d =
  Alcotest.(check decl_testable) label d (roundtrip_decl d)

let check_program label p =
  Alcotest.(check program_testable) label p (roundtrip_program p)

(* ------------------------------------------------------------------ *)
(* Expression round-trips                                              *)
(* ------------------------------------------------------------------ *)

let test_var () = check_expr "Var" (expr (Var "x"))

let test_int_lit () = check_expr "IntLit" (expr (IntLit 42L))

let test_int_lit_neg () = check_expr "IntLit neg" (expr (IntLit (-999L)))

let test_int_lit_zero () = check_expr "IntLit 0" (expr (IntLit 0L))

let test_int_lit_max () = check_expr "IntLit max" (expr (IntLit Int64.max_int))

let test_int_lit_min () = check_expr "IntLit min" (expr (IntLit Int64.min_int))

let test_float_lit () = check_expr "FloatLit" (expr (FloatLit 3.14))

let test_float_lit_neg () = check_expr "FloatLit neg" (expr (FloatLit (-0.5)))

let test_float_lit_zero () = check_expr "FloatLit 0" (expr (FloatLit 0.0))

let test_float_lit_inf () = check_expr "FloatLit inf" (expr (FloatLit Float.infinity))

let test_float_lit_neg_inf () = check_expr "FloatLit -inf" (expr (FloatLit Float.neg_infinity))

let test_string_lit () = check_expr "StringLit" (expr (StringLit "hello"))

let test_string_lit_empty () = check_expr "StringLit empty" (expr (StringLit ""))

let test_string_lit_unicode () = check_expr "StringLit unicode" (expr (StringLit "hello \xC3\xA9\xC3\xA0"))

let test_bool_true () = check_expr "BoolTrue" (expr (BoolLit true))

let test_bool_false () = check_expr "BoolFalse" (expr (BoolLit false))

let test_unit_lit () = check_expr "UnitLit" (expr UnitLit)

let test_let_simple () =
  check_expr "Let simple"
    (expr (Let { pat = pat (PVar "x")
               ; value = expr (IntLit 42L)
               ; body = expr (Var "x") }))

let test_let_pattern_wild () =
  check_expr "Let PWild"
    (expr (Let { pat = pat PWild
               ; value = expr UnitLit
               ; body = expr (IntLit 1L) }))

let test_let_pattern_ctor () =
  check_expr "Let PCtor"
    (expr (Let { pat = pat (PCtor ("Some", [pat (PVar "x")]))
               ; value = expr (Var "opt")
               ; body = expr (Var "x") }))

let test_let_pattern_record () =
  check_expr "Let PRecord"
    (expr (Let { pat = pat (PRecord ([("x", pat (PVar "a")); ("y", pat (PVar "b"))], false))
               ; value = expr (Var "point")
               ; body = expr (Var "a") }))

let test_let_pattern_record_open () =
  check_expr "Let PRecord open"
    (expr (Let { pat = pat (PRecord ([("x", pat (PVar "a"))], true))
               ; value = expr (Var "point")
               ; body = expr (Var "a") }))

let test_let_pattern_or () =
  check_expr "Let POr"
    (expr (Let { pat = pat (POr (pat (PVar "x"), pat (PVar "y")))
               ; value = expr (Var "v")
               ; body = expr (Var "x") }))

let test_let_pattern_lit_int () =
  check_expr "Let PLitInt"
    (expr (Let { pat = pat (PLitInt 0L)
               ; value = expr (IntLit 0L)
               ; body = expr (BoolLit true) }))

let test_let_pattern_lit_float () =
  check_expr "Let PLitFloat"
    (expr (Let { pat = pat (PLitFloat 1.5)
               ; value = expr (FloatLit 1.5)
               ; body = expr (BoolLit true) }))

let test_let_pattern_lit_string () =
  check_expr "Let PLitString"
    (expr (Let { pat = pat (PLitString "hi")
               ; value = expr (StringLit "hi")
               ; body = expr (BoolLit true) }))

let test_let_pattern_lit_true () =
  check_expr "Let PLitTrue"
    (expr (Let { pat = pat PLitTrue
               ; value = expr (BoolLit true)
               ; body = expr (IntLit 1L) }))

let test_let_pattern_lit_false () =
  check_expr "Let PLitFalse"
    (expr (Let { pat = pat PLitFalse
               ; value = expr (BoolLit false)
               ; body = expr (IntLit 0L) }))

let test_let_pattern_lit_unit () =
  check_expr "Let PLitUnit"
    (expr (Let { pat = pat PLitUnit
               ; value = expr UnitLit
               ; body = expr UnitLit }))

let test_app_single () =
  check_expr "App single"
    (expr (App (expr (Var "f"), [expr (IntLit 1L)])))

let test_app_multi () =
  check_expr "App multi"
    (expr (App (expr (Var "f"), [expr (Var "x"); expr (Var "y"); expr (Var "z")])))

let test_app_zero () =
  check_expr "App zero"
    (expr (App (expr (Var "f"), [])))

let test_fn_annotated () =
  check_expr "Fn annotated"
    (expr (Fn { params = [{ param_name = "x"; param_type = TyName "Int" }
                          ;{ param_name = "y"; param_type = TyName "Bool" }]
              ; return_type = Some (TyName "Int")
              ; effects = Some Pure
              ; fn_body = expr (Var "x") }))

let test_fn_no_annotation () =
  check_expr "Fn no annotation"
    (expr (Fn { params = [{ param_name = "x"; param_type = TyName "Int" }]
              ; return_type = None
              ; effects = None
              ; fn_body = expr (Var "x") }))

let test_fn_effect_set () =
  check_expr "Fn effect set"
    (expr (Fn { params = [{ param_name = "x"; param_type = TyName "Int" }]
              ; return_type = Some (TyName "Unit")
              ; effects = Some (Effects [TyName "Log"; TyApp ("Throw", [TyName "E"])])
              ; fn_body = expr UnitLit }))

let test_fn_complex_types () =
  check_expr "Fn complex types"
    (expr (Fn { params = [{ param_name = "f"
                           ; param_type = TyFun ([TyName "Int"], TyName "Bool", None) }
                          ;{ param_name = "xs"
                           ; param_type = TyApp ("List", [TyName "Int"]) }]
              ; return_type = Some (TyTuple [TyName "Int"; TyName "Bool"])
              ; effects = None
              ; fn_body = expr UnitLit }))

let test_match () =
  check_expr "Match"
    (expr (Match { scrutinee = expr (Var "x")
                 ; arms = [ { pattern = pat PLitTrue;  arm_body = expr (IntLit 1L) }
                           ; { pattern = pat PLitFalse; arm_body = expr (IntLit 0L) } ] }))

let test_match_ctor () =
  check_expr "Match ctor"
    (expr (Match { scrutinee = expr (Var "opt")
                 ; arms = [ { pattern = pat (PCtor ("Some", [pat (PVar "x")])); arm_body = expr (Var "x") }
                           ; { pattern = pat (PCtor ("None", [])); arm_body = expr (IntLit 0L) } ] }))

let test_if () =
  check_expr "If"
    (expr (If { cond = expr (Var "b")
              ; then_ = expr (IntLit 1L)
              ; else_ = expr (IntLit 0L) }))

let test_do () =
  check_expr "Do"
    (expr (Do [ StmtLet { pat = pat (PVar "x"); value = expr (IntLit 1L) }
              ; StmtLet { pat = pat (PVar "y"); value = expr (IntLit 2L) }
              ; StmtExpr (expr (Var "x")) ]))

let test_letrec () =
  check_expr "Letrec"
    (expr (Letrec
      ( [ { letrec_name = "even"
          ; letrec_params = [{ param_name = "n"; param_type = TyName "Int" }]
          ; letrec_return_type = TyName "Bool"
          ; letrec_body = expr (Var "n") }
        ; { letrec_name = "odd"
          ; letrec_params = [{ param_name = "n"; param_type = TyName "Int" }]
          ; letrec_return_type = TyName "Bool"
          ; letrec_body = expr (Var "n") } ]
      , expr (App (expr (Var "even"), [expr (IntLit 0L)])) )))

let test_record () =
  check_expr "Record"
    (expr (Record [("x", expr (IntLit 1L)); ("y", expr (IntLit 2L))]))

let test_record_empty () =
  check_expr "Record empty"
    (expr (Record []))

let test_record_update () =
  check_expr "RecordUpdate"
    (expr (RecordUpdate (expr (Var "p"), [("x", expr (IntLit 3L))])))

let test_project () =
  check_expr "Project"
    (expr (Project (expr (Var "p"), "x")))

let test_perform () =
  check_expr "Perform"
    (expr (Perform { effect_name = "Console"; op_name = "print"
                   ; args = [expr (StringLit "hi")] }))

let test_perform_no_args () =
  check_expr "Perform no args"
    (expr (Perform { effect_name = "State"; op_name = "get"; args = [] }))

let test_handle () =
  check_expr "Handle"
    (expr (Handle
      { handled = expr (App (expr (Var "f"), []))
      ; handlers = [ { effect_handler = "State"
                      ; op_handlers = [ { op_handler_name = "get"
                                        ; op_handler_params = []
                                        ; op_handler_body = expr (Var "s") }
                                      ; { op_handler_name = "put"
                                        ; op_handler_params = ["v"]
                                        ; op_handler_body = expr UnitLit } ]
                      ; return_handler = None } ] }))

let test_handle_with_return () =
  check_expr "Handle with return"
    (expr (Handle
      { handled = expr (App (expr (Var "f"), []))
      ; handlers = [ { effect_handler = "Throw"
                      ; op_handlers = [ { op_handler_name = "throw"
                                        ; op_handler_params = ["e"]
                                        ; op_handler_body = expr (Var "e") } ]
                      ; return_handler = Some { return_var = "v"
                                              ; return_body = expr (Var "v") } } ] }))

let test_handle_multi_handler () =
  check_expr "Handle multi handler"
    (expr (Handle
      { handled = expr (Var "body")
      ; handlers = [ { effect_handler = "State"
                      ; op_handlers = [ { op_handler_name = "get"
                                        ; op_handler_params = []
                                        ; op_handler_body = expr (Var "s") } ]
                      ; return_handler = None }
                   ; { effect_handler = "Log"
                      ; op_handlers = [ { op_handler_name = "log"
                                        ; op_handler_params = ["msg"]
                                        ; op_handler_body = expr UnitLit } ]
                      ; return_handler = Some { return_var = "r"
                                              ; return_body = expr (Var "r") } } ] }))

(* ------------------------------------------------------------------ *)
(* Comment round-trips                                                 *)
(* ------------------------------------------------------------------ *)

let test_comment_on_expr () =
  check_expr "comment on expr"
    { desc = Var "x"; comment = Some "important variable" }

let test_comment_on_pattern () =
  check_expr "comment on pattern"
    (expr (Let { pat = { pat_desc = PVar "x"; pat_comment = Some "the binding" }
               ; value = expr (IntLit 1L)
               ; body = expr (Var "x") }))

let test_nested_comments () =
  check_expr "nested comments"
    { desc = Let { pat = { pat_desc = PVar "x"; pat_comment = Some "pat comment" }
                 ; value = { desc = IntLit 42L; comment = Some "value comment" }
                 ; body = { desc = Var "x"; comment = Some "body comment" } }
    ; comment = Some "let comment" }

(* ------------------------------------------------------------------ *)
(* Declaration round-trips                                             *)
(* ------------------------------------------------------------------ *)

let test_decl_fn () =
  check_decl "DeclFn"
    (decl (DeclFn { pub = true
                  ; fn_name = "double"
                  ; type_params = ["A"]
                  ; params = [{ param_name = "x"; param_type = TyName "Int" }]
                  ; return_type = Some (TyName "Int")
                  ; effects = Some Pure
                  ; decl_body = expr (Var "x") }))

let test_decl_fn_no_annotation () =
  check_decl "DeclFn no annotation"
    (decl (DeclFn { pub = false
                  ; fn_name = "id"
                  ; type_params = []
                  ; params = [{ param_name = "x"; param_type = TyName "a" }]
                  ; return_type = None
                  ; effects = None
                  ; decl_body = expr (Var "x") }))

let test_decl_type () =
  check_decl "DeclType"
    (decl (DeclType { pub = true
                    ; type_name = "Option"
                    ; type_params = ["A"]
                    ; ctors = [ { ctor_name = "Some"; ctor_params = [TyName "A"] }
                              ; { ctor_name = "None"; ctor_params = [] } ] }))

let test_decl_type_no_params () =
  check_decl "DeclType no params"
    (decl (DeclType { pub = false
                    ; type_name = "Bool"
                    ; type_params = []
                    ; ctors = [ { ctor_name = "True"; ctor_params = [] }
                              ; { ctor_name = "False"; ctor_params = [] } ] }))

let test_decl_effect () =
  check_decl "DeclEffect"
    (decl (DeclEffect { pub = true
                      ; effect_name = "State"
                      ; type_params = ["S"]
                      ; ops = [ { effect_op_name = "get"
                                ; effect_op_params = []
                                ; effect_op_return = TyName "S" }
                              ; { effect_op_name = "put"
                                ; effect_op_params = [TyName "S"]
                                ; effect_op_return = TyName "Unit" } ] }))

let test_decl_module () =
  check_decl "DeclModule"
    (decl (DeclModule { pub = false
                      ; module_name = "Math"
                      ; body = [ decl (DeclFn { pub = true
                                              ; fn_name = "add"
                                              ; type_params = []
                                              ; params = [{ param_name = "x"; param_type = TyName "Int" }
                                                         ;{ param_name = "y"; param_type = TyName "Int" }]
                                              ; return_type = Some (TyName "Int")
                                              ; effects = None
                                              ; decl_body = expr (Var "x") }) ] }))

let test_decl_require () =
  check_decl "DeclRequire"
    (decl (DeclRequire (TyName "Log")))

let test_decl_require_generic () =
  check_decl "DeclRequire generic"
    (decl (DeclRequire (TyApp ("Throw", [TyName "E"]))))

let test_decl_comment () =
  check_decl "DeclFn with comment"
    { decl_desc = DeclFn { pub = false; fn_name = "f"; type_params = []
                          ; params = []; return_type = None; effects = None
                          ; decl_body = expr UnitLit }
    ; decl_comment = Some "a function" }

(* ------------------------------------------------------------------ *)
(* Program round-trip                                                  *)
(* ------------------------------------------------------------------ *)

let test_program () =
  check_program "Program"
    [ decl (DeclType { pub = true; type_name = "Bool"; type_params = []
                     ; ctors = [ { ctor_name = "True"; ctor_params = [] }
                               ; { ctor_name = "False"; ctor_params = [] } ] })
    ; decl (DeclFn { pub = true; fn_name = "not"; type_params = []
                   ; params = [{ param_name = "b"; param_type = TyName "Bool" }]
                   ; return_type = Some (TyName "Bool")
                   ; effects = Some Pure
                   ; decl_body = expr (Match { scrutinee = expr (Var "b")
                                             ; arms = [ { pattern = pat PLitTrue
                                                        ; arm_body = expr (BoolLit false) }
                                                      ; { pattern = pat PLitFalse
                                                        ; arm_body = expr (BoolLit true) } ] }) }) ]

(* ------------------------------------------------------------------ *)
(* Nested / complex round-trips                                        *)
(* ------------------------------------------------------------------ *)

let test_nested_let () =
  check_expr "nested let"
    (expr (Let { pat = pat (PVar "x"); value = expr (IntLit 1L)
               ; body = expr (Let { pat = pat (PVar "y"); value = expr (IntLit 2L)
                                  ; body = expr (Var "x") }) }))

let test_nested_fn () =
  check_expr "nested fn"
    (expr (Fn { params = [{ param_name = "x"; param_type = TyName "Int" }]
              ; return_type = None; effects = None
              ; fn_body = expr (Fn { params = [{ param_name = "y"; param_type = TyName "Int" }]
                                   ; return_type = None; effects = None
                                   ; fn_body = expr (Var "x") }) }))

let test_complex_expression () =
  check_expr "complex expression"
    (expr (Let { pat = pat (PVar "result")
               ; value = expr (Handle
                   { handled = expr (Do [ StmtLet { pat = pat (PVar "x")
                                                  ; value = expr (Perform { effect_name = "State"
                                                                          ; op_name = "get"
                                                                          ; args = [] }) }
                                        ; StmtExpr (expr (Var "x")) ])
                   ; handlers = [ { effect_handler = "State"
                                  ; op_handlers = [ { op_handler_name = "get"
                                                    ; op_handler_params = []
                                                    ; op_handler_body = expr (IntLit 0L) } ]
                                  ; return_handler = Some { return_var = "v"
                                                          ; return_body = expr (Var "v") } } ] })
               ; body = expr (Var "result") }))

(* ------------------------------------------------------------------ *)
(* Test runner                                                         *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "Roundtrip"
    [ ( "literals",
        [ Alcotest.test_case "Var"               `Quick test_var
        ; Alcotest.test_case "IntLit"            `Quick test_int_lit
        ; Alcotest.test_case "IntLit neg"        `Quick test_int_lit_neg
        ; Alcotest.test_case "IntLit 0"          `Quick test_int_lit_zero
        ; Alcotest.test_case "IntLit max"        `Quick test_int_lit_max
        ; Alcotest.test_case "IntLit min"        `Quick test_int_lit_min
        ; Alcotest.test_case "FloatLit"          `Quick test_float_lit
        ; Alcotest.test_case "FloatLit neg"      `Quick test_float_lit_neg
        ; Alcotest.test_case "FloatLit 0"        `Quick test_float_lit_zero
        ; Alcotest.test_case "FloatLit inf"      `Quick test_float_lit_inf
        ; Alcotest.test_case "FloatLit -inf"     `Quick test_float_lit_neg_inf
        ; Alcotest.test_case "StringLit"         `Quick test_string_lit
        ; Alcotest.test_case "StringLit empty"   `Quick test_string_lit_empty
        ; Alcotest.test_case "StringLit unicode" `Quick test_string_lit_unicode
        ; Alcotest.test_case "BoolTrue"          `Quick test_bool_true
        ; Alcotest.test_case "BoolFalse"         `Quick test_bool_false
        ; Alcotest.test_case "UnitLit"           `Quick test_unit_lit
        ] )
    ; ( "let-patterns",
        [ Alcotest.test_case "simple"          `Quick test_let_simple
        ; Alcotest.test_case "PWild"           `Quick test_let_pattern_wild
        ; Alcotest.test_case "PCtor"           `Quick test_let_pattern_ctor
        ; Alcotest.test_case "PRecord"         `Quick test_let_pattern_record
        ; Alcotest.test_case "PRecord open"    `Quick test_let_pattern_record_open
        ; Alcotest.test_case "POr"             `Quick test_let_pattern_or
        ; Alcotest.test_case "PLitInt"         `Quick test_let_pattern_lit_int
        ; Alcotest.test_case "PLitFloat"       `Quick test_let_pattern_lit_float
        ; Alcotest.test_case "PLitString"      `Quick test_let_pattern_lit_string
        ; Alcotest.test_case "PLitTrue"        `Quick test_let_pattern_lit_true
        ; Alcotest.test_case "PLitFalse"       `Quick test_let_pattern_lit_false
        ; Alcotest.test_case "PLitUnit"        `Quick test_let_pattern_lit_unit
        ] )
    ; ( "expressions",
        [ Alcotest.test_case "App single"      `Quick test_app_single
        ; Alcotest.test_case "App multi"       `Quick test_app_multi
        ; Alcotest.test_case "App zero"        `Quick test_app_zero
        ; Alcotest.test_case "Fn annotated"    `Quick test_fn_annotated
        ; Alcotest.test_case "Fn no annot"     `Quick test_fn_no_annotation
        ; Alcotest.test_case "Fn effect set"   `Quick test_fn_effect_set
        ; Alcotest.test_case "Fn complex ty"   `Quick test_fn_complex_types
        ; Alcotest.test_case "Match"           `Quick test_match
        ; Alcotest.test_case "Match ctor"      `Quick test_match_ctor
        ; Alcotest.test_case "If"              `Quick test_if
        ; Alcotest.test_case "Do"              `Quick test_do
        ; Alcotest.test_case "Letrec"          `Quick test_letrec
        ; Alcotest.test_case "Record"          `Quick test_record
        ; Alcotest.test_case "Record empty"    `Quick test_record_empty
        ; Alcotest.test_case "RecordUpdate"    `Quick test_record_update
        ; Alcotest.test_case "Project"         `Quick test_project
        ; Alcotest.test_case "Perform"         `Quick test_perform
        ; Alcotest.test_case "Perform no args" `Quick test_perform_no_args
        ; Alcotest.test_case "Handle"          `Quick test_handle
        ; Alcotest.test_case "Handle return"   `Quick test_handle_with_return
        ; Alcotest.test_case "Handle multi"    `Quick test_handle_multi_handler
        ] )
    ; ( "comments",
        [ Alcotest.test_case "on expr"         `Quick test_comment_on_expr
        ; Alcotest.test_case "on pattern"      `Quick test_comment_on_pattern
        ; Alcotest.test_case "nested"          `Quick test_nested_comments
        ] )
    ; ( "declarations",
        [ Alcotest.test_case "DeclFn"          `Quick test_decl_fn
        ; Alcotest.test_case "DeclFn no annot" `Quick test_decl_fn_no_annotation
        ; Alcotest.test_case "DeclType"        `Quick test_decl_type
        ; Alcotest.test_case "DeclType no par" `Quick test_decl_type_no_params
        ; Alcotest.test_case "DeclEffect"      `Quick test_decl_effect
        ; Alcotest.test_case "DeclModule"      `Quick test_decl_module
        ; Alcotest.test_case "DeclRequire"     `Quick test_decl_require
        ; Alcotest.test_case "DeclRequire gen" `Quick test_decl_require_generic
        ; Alcotest.test_case "DeclFn comment"  `Quick test_decl_comment
        ] )
    ; ( "program",
        [ Alcotest.test_case "full program"    `Quick test_program
        ] )
    ; ( "complex",
        [ Alcotest.test_case "nested let"      `Quick test_nested_let
        ; Alcotest.test_case "nested fn"       `Quick test_nested_fn
        ; Alcotest.test_case "complex expr"    `Quick test_complex_expression
        ] ) ]
