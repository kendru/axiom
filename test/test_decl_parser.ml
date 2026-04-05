open Axiom_lib.Ast
open Axiom_lib.Parser

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let decl_testable = Alcotest.testable pp_decl equal_decl

let program_testable = Alcotest.testable
    (fun fmt prog ->
       Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f "\n") pp_decl fmt prog)
    equal_program

let parse_decl_of src =
  match parse_program (Axiom_lib.Lexer.tokenize src) with
  | [d] -> d
  | ds  -> failwith (Printf.sprintf "expected 1 decl, got %d" (List.length ds))

let parse_prog_of src =
  parse_program (Axiom_lib.Lexer.tokenize src)

let check_decl label src expected =
  Alcotest.(check decl_testable) label expected (parse_decl_of src)

let check_prog label src expected =
  Alcotest.(check program_testable) label expected (parse_prog_of src)

(** Shorthand: build an expression node with no comment. *)
let e k = expr k

(** Shorthand: build a declaration node with no comment. *)
let d k = decl k

(* ------------------------------------------------------------------ *)
(* fn declarations                                                      *)
(* ------------------------------------------------------------------ *)

(* fn identity(x: Int) -> Int ! pure { x } *)
let test_fn_decl_simple () =
  check_decl "fn simple"
    "fn identity(x: Int) -> Int ! pure { x }"
    (d (DeclFn { pub         = false
               ; fn_name     = "identity"
               ; type_params = []
               ; params      = [{ param_name = "x"; param_type = TyName "Int" }]
               ; return_type = Some (TyName "Int")
               ; effects     = Some Pure
               ; decl_body   = e (Var "x") }))

(* pub fn add(x: Int, y: Int) -> Int ! pure { x } *)
let test_fn_decl_pub () =
  check_decl "fn pub"
    "pub fn add(x: Int, y: Int) -> Int ! pure { x }"
    (d (DeclFn { pub         = true
               ; fn_name     = "add"
               ; type_params = []
               ; params      = [ { param_name = "x"; param_type = TyName "Int" }
                                ; { param_name = "y"; param_type = TyName "Int" } ]
               ; return_type = Some (TyName "Int")
               ; effects     = Some Pure
               ; decl_body   = e (Var "x") }))

(* fn id<a>(x: a) -> a ! pure { x } -- type params *)
let test_fn_decl_type_params () =
  check_decl "fn type params"
    "fn id<a>(x: a) -> a ! pure { x }"
    (d (DeclFn { pub         = false
               ; fn_name     = "id"
               ; type_params = ["a"]
               ; params      = [{ param_name = "x"; param_type = TyName "a" }]
               ; return_type = Some (TyName "a")
               ; effects     = Some Pure
               ; decl_body   = e (Var "x") }))

(* fn noop() { () } -- no return type annotation *)
let test_fn_decl_no_annotation () =
  check_decl "fn no annotation"
    "fn noop() { () }"
    (d (DeclFn { pub         = false
               ; fn_name     = "noop"
               ; type_params = []
               ; params      = []
               ; return_type = None
               ; effects     = None
               ; decl_body   = e UnitLit }))

(* ------------------------------------------------------------------ *)
(* type declarations                                                    *)
(* ------------------------------------------------------------------ *)

(* type Option<a> = | None | Some(a) *)
let test_type_decl_option () =
  check_decl "type Option"
    "type Option<a> = | None | Some(a)"
    (d (DeclType { pub         = false
                 ; type_name   = "Option"
                 ; type_params = ["a"]
                 ; ctors       = [ { ctor_name = "None"; ctor_params = [] }
                                  ; { ctor_name = "Some"; ctor_params = [TyName "a"] } ] }))

(* type Bool = | True | False *)
let test_type_decl_bool () =
  check_decl "type Bool"
    "type Bool = | True | False"
    (d (DeclType { pub         = false
                 ; type_name   = "Bool"
                 ; type_params = []
                 ; ctors       = [ { ctor_name = "True";  ctor_params = [] }
                                  ; { ctor_name = "False"; ctor_params = [] } ] }))

(* pub type Result<a, e> = | Ok(a) | Err(e) *)
let test_type_decl_result () =
  check_decl "type Result"
    "pub type Result<a, e> = | Ok(a) | Err(e)"
    (d (DeclType { pub         = true
                 ; type_name   = "Result"
                 ; type_params = ["a"; "e"]
                 ; ctors       = [ { ctor_name = "Ok";  ctor_params = [TyName "a"] }
                                  ; { ctor_name = "Err"; ctor_params = [TyName "e"] } ] }))

(* ------------------------------------------------------------------ *)
(* effect declarations                                                  *)
(* ------------------------------------------------------------------ *)

(* effect State<s> { get: () -> s, put: (s) -> Unit } *)
let test_effect_decl_state () =
  check_decl "effect State"
    "effect State<s> { get: () -> s, put: (s) -> Unit }"
    (d (DeclEffect { pub         = false
                   ; effect_name = "State"
                   ; type_params = ["s"]
                   ; ops         = [ { effect_op_name   = "get"
                                     ; effect_op_params  = []
                                     ; effect_op_return  = TyName "s" }
                                   ; { effect_op_name   = "put"
                                     ; effect_op_params  = [TyName "s"]
                                     ; effect_op_return  = TyName "Unit" } ] }))

(* effect Log { log: (String) -> Unit } *)
let test_effect_decl_log () =
  check_decl "effect Log"
    "effect Log { log: (String) -> Unit }"
    (d (DeclEffect { pub         = false
                   ; effect_name = "Log"
                   ; type_params = []
                   ; ops         = [ { effect_op_name   = "log"
                                     ; effect_op_params  = [TyName "String"]
                                     ; effect_op_return  = TyName "Unit" } ] }))

(* ------------------------------------------------------------------ *)
(* module declarations                                                  *)
(* ------------------------------------------------------------------ *)

(* module math { fn square(x: Int) { x } } *)
let test_module_decl () =
  check_decl "module"
    "module math { fn square(x: Int) { x } }"
    (d (DeclModule { pub         = false
                   ; module_name = "math"
                   ; body        =
                       [ d (DeclFn { pub         = false
                                   ; fn_name     = "square"
                                   ; type_params = []
                                   ; params      = [{ param_name = "x"; param_type = TyName "Int" }]
                                   ; return_type = None
                                   ; effects     = None
                                   ; decl_body   = e (Var "x") }) ] }))

(* ------------------------------------------------------------------ *)
(* require declarations                                                 *)
(* ------------------------------------------------------------------ *)

(* require effect Log *)
let test_require_decl () =
  check_decl "require"
    "require effect Log"
    (d (DeclRequire (TyName "Log")))

(* ------------------------------------------------------------------ *)
(* Multi-declaration programs                                           *)
(* ------------------------------------------------------------------ *)

(* Two top-level fn declarations *)
let test_program_two_fns () =
  check_prog "two fns"
    "fn foo(x: Int) { x }  fn bar(y: Bool) { y }"
    [ d (DeclFn { pub = false; fn_name = "foo"; type_params = []
                ; params = [{ param_name = "x"; param_type = TyName "Int" }]
                ; return_type = None; effects = None; decl_body = e (Var "x") })
    ; d (DeclFn { pub = false; fn_name = "bar"; type_params = []
                ; params = [{ param_name = "y"; param_type = TyName "Bool" }]
                ; return_type = None; effects = None; decl_body = e (Var "y") }) ]

(* ------------------------------------------------------------------ *)
(* Comment attachment on declarations                                   *)
(* ------------------------------------------------------------------ *)

(* fn foo(x: Int) { x } @# entry point #@ *)
let test_fn_decl_comment () =
  check_decl "fn with comment"
    "fn foo(x: Int) { x } @# entry point #@"
    { decl_desc = DeclFn { pub = false; fn_name = "foo"; type_params = []
                          ; params = [{ param_name = "x"; param_type = TyName "Int" }]
                          ; return_type = None; effects = None; decl_body = e (Var "x") }
    ; decl_comment = Some "entry point" }

(* type Bool = | True | False @# boolean type #@ *)
let test_type_decl_comment () =
  check_decl "type with comment"
    "type Bool = | True | False @# boolean type #@"
    { decl_desc = DeclType { pub = false; type_name = "Bool"; type_params = []
                            ; ctors = [ { ctor_name = "True"; ctor_params = [] }
                                       ; { ctor_name = "False"; ctor_params = [] } ] }
    ; decl_comment = Some "boolean type" }

(* ------------------------------------------------------------------ *)
(* Test runner                                                          *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "DeclParser"
    [ ( "fn",
        [ Alcotest.test_case "simple"          `Quick test_fn_decl_simple
        ; Alcotest.test_case "pub"             `Quick test_fn_decl_pub
        ; Alcotest.test_case "type params"     `Quick test_fn_decl_type_params
        ; Alcotest.test_case "no annotation"   `Quick test_fn_decl_no_annotation
        ] )
    ; ( "type",
        [ Alcotest.test_case "Option"          `Quick test_type_decl_option
        ; Alcotest.test_case "Bool"            `Quick test_type_decl_bool
        ; Alcotest.test_case "Result"          `Quick test_type_decl_result
        ] )
    ; ( "effect",
        [ Alcotest.test_case "State"           `Quick test_effect_decl_state
        ; Alcotest.test_case "Log"             `Quick test_effect_decl_log
        ] )
    ; ( "module",
        [ Alcotest.test_case "math"            `Quick test_module_decl
        ] )
    ; ( "require",
        [ Alcotest.test_case "Log"             `Quick test_require_decl
        ] )
    ; ( "program",
        [ Alcotest.test_case "two fns"         `Quick test_program_two_fns
        ] )
    ; ( "comments",
        [ Alcotest.test_case "fn comment"      `Quick test_fn_decl_comment
        ; Alcotest.test_case "type comment"    `Quick test_type_decl_comment
        ] ) ]
