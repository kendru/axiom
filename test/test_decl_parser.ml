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

(* ------------------------------------------------------------------ *)
(* fn declarations                                                      *)
(* ------------------------------------------------------------------ *)

(* fn identity(x: Int) -> Int ! pure { x } *)
let test_fn_decl_simple () =
  check_decl "fn simple"
    "fn identity(x: Int) -> Int ! pure { x }"
    (decl (DeclFn { pub         = false
               ; fn_name     = "identity"
               ; type_params = []
               ; params      = [{ param_name = "x"; param_type = TyName "Int" }]
               ; return_type = Some (TyName "Int")
               ; effects     = Some Pure
               ; decl_body   = expr (Var "x") }))

(* pub fn add(x: Int, y: Int) -> Int ! pure { x } *)
let test_fn_decl_pub () =
  check_decl "fn pub"
    "pub fn add(x: Int, y: Int) -> Int ! pure { x }"
    (decl (DeclFn { pub         = true
               ; fn_name     = "add"
               ; type_params = []
               ; params      = [ { param_name = "x"; param_type = TyName "Int" }
                                ; { param_name = "y"; param_type = TyName "Int" } ]
               ; return_type = Some (TyName "Int")
               ; effects     = Some Pure
               ; decl_body   = expr (Var "x") }))

(* fn id<a>(x: a) -> a ! pure { x } -- type params *)
let test_fn_decl_type_params () =
  check_decl "fn type params"
    "fn id<a>(x: a) -> a ! pure { x }"
    (decl (DeclFn { pub         = false
               ; fn_name     = "id"
               ; type_params = ["a"]
               ; params      = [{ param_name = "x"; param_type = TyName "a" }]
               ; return_type = Some (TyName "a")
               ; effects     = Some Pure
               ; decl_body   = expr (Var "x") }))

(* fn noop() { () } -- no return type annotation *)
let test_fn_decl_no_annotation () =
  check_decl "fn no annotation"
    "fn noop() { () }"
    (decl (DeclFn { pub         = false
               ; fn_name     = "noop"
               ; type_params = []
               ; params      = []
               ; return_type = None
               ; effects     = None
               ; decl_body   = expr UnitLit }))

(* ------------------------------------------------------------------ *)
(* type declarations                                                    *)
(* ------------------------------------------------------------------ *)

(* type Option<a> = | None | Some(a) *)
let test_type_decl_option () =
  check_decl "type Option"
    "type Option<a> = | None | Some(a)"
    (decl (DeclType { pub         = false
                 ; type_name   = "Option"
                 ; type_params = ["a"]
                 ; ctors       = [ { ctor_name = "None"; ctor_params = [] }
                                  ; { ctor_name = "Some"; ctor_params = [TyName "a"] } ] }))

(* type Bool = | True | False *)
let test_type_decl_bool () =
  check_decl "type Bool"
    "type Bool = | True | False"
    (decl (DeclType { pub         = false
                 ; type_name   = "Bool"
                 ; type_params = []
                 ; ctors       = [ { ctor_name = "True";  ctor_params = [] }
                                  ; { ctor_name = "False"; ctor_params = [] } ] }))

(* pub type Result<a, e> = | Ok(a) | Err(e) *)
let test_type_decl_result () =
  check_decl "type Result"
    "pub type Result<a, e> = | Ok(a) | Err(e)"
    (decl (DeclType { pub         = true
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
    (decl (DeclEffect { pub         = false
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
    (decl (DeclEffect { pub         = false
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
    (decl (DeclModule { pub         = false
                   ; module_name = "math"
                   ; body        =
                       [ decl (DeclFn { pub         = false
                                   ; fn_name     = "square"
                                   ; type_params = []
                                   ; params      = [{ param_name = "x"; param_type = TyName "Int" }]
                                   ; return_type = None
                                   ; effects     = None
                                   ; decl_body   = expr (Var "x") }) ] }))

(* ------------------------------------------------------------------ *)
(* require declarations                                                 *)
(* ------------------------------------------------------------------ *)

(* require effect Log *)
let test_require_decl () =
  check_decl "require"
    "require effect Log"
    (decl (DeclRequire (TyName "Log")))

(* ------------------------------------------------------------------ *)
(* import declarations                                                  *)
(* ------------------------------------------------------------------ *)

(* import json_parser *)
let test_import_decl_bare () =
  check_decl "import bare"
    "import json_parser"
    (decl (DeclImport { module_path = "json_parser"; alias = None }))

(* import http_client as http *)
let test_import_decl_alias () =
  check_decl "import with alias"
    "import http_client as http"
    (decl (DeclImport { module_path = "http_client"; alias = Some "http" }))

(* ------------------------------------------------------------------ *)
(* Multi-declaration programs                                           *)
(* ------------------------------------------------------------------ *)

(* Two top-level fn declarations *)
let test_program_two_fns () =
  check_prog "two fns"
    "fn foo(x: Int) { x }  fn bar(y: Bool) { y }"
    [ decl (DeclFn { pub = false; fn_name = "foo"; type_params = []
                ; params = [{ param_name = "x"; param_type = TyName "Int" }]
                ; return_type = None; effects = None; decl_body = expr (Var "x") })
    ; decl (DeclFn { pub = false; fn_name = "bar"; type_params = []
                ; params = [{ param_name = "y"; param_type = TyName "Bool" }]
                ; return_type = None; effects = None; decl_body = expr (Var "y") }) ]

(* ------------------------------------------------------------------ *)
(* Open effect rows — { E | rho } syntax                               *)
(* ------------------------------------------------------------------ *)

(* fn f(x: Int) -> Int ! {Log | e} { x }  — effect with tail variable *)
let test_open_effect_row () =
  check_decl "open effect row"
    "fn f(x: Int) -> Int ! {Log | e} { x }"
    (decl (DeclFn { pub         = false
               ; fn_name     = "f"
               ; type_params = []
               ; params      = [{ param_name = "x"; param_type = TyName "Int" }]
               ; return_type = Some (TyName "Int")
               ; effects     = Some (Effects ([TyName "Log"], Some "e"))
               ; decl_body   = expr (Var "x") }))

(* fn f() -> Unit ! { | e} { () }  — just a tail variable, no concrete effects *)
let test_open_effect_row_tail_only () =
  check_decl "open effect row tail only"
    "fn f() -> Unit ! {| e} { () }"
    (decl (DeclFn { pub         = false
               ; fn_name     = "f"
               ; type_params = []
               ; params      = []
               ; return_type = Some (TyName "Unit")
               ; effects     = Some (Effects ([], Some "e"))
               ; decl_body   = expr UnitLit }))

(* fn f() -> Unit ! e { () }  — bare row variable *)
let test_bare_row_variable () =
  check_decl "bare row variable"
    "fn f() -> Unit ! e { () }"
    (decl (DeclFn { pub         = false
               ; fn_name     = "f"
               ; type_params = []
               ; params      = []
               ; return_type = Some (TyName "Unit")
               ; effects     = Some (Effects ([], Some "e"))
               ; decl_body   = expr UnitLit }))

(* fn f() -> Unit ! {Log, Console | e} { () }  — multiple effects with tail *)
let test_open_effect_row_multi () =
  check_decl "open effect row multiple effects"
    "fn f() -> Unit ! {Log, Console | e} { () }"
    (decl (DeclFn { pub         = false
               ; fn_name     = "f"
               ; type_params = []
               ; params      = []
               ; return_type = Some (TyName "Unit")
               ; effects     = Some (Effects ([TyName "Log"; TyName "Console"], Some "e"))
               ; decl_body   = expr UnitLit }))

(* ------------------------------------------------------------------ *)
(* Comment attachment on declarations                                   *)
(* ------------------------------------------------------------------ *)

(* fn foo(x: Int) { x } @# entry point #@ *)
let test_fn_decl_comment () =
  check_decl "fn with comment"
    "fn foo(x: Int) { x } @# entry point #@"
    { decl_desc = DeclFn { pub = false; fn_name = "foo"; type_params = []
                          ; params = [{ param_name = "x"; param_type = TyName "Int" }]
                          ; return_type = None; effects = None; decl_body = expr (Var "x") }
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
    ; ( "import",
        [ Alcotest.test_case "bare"            `Quick test_import_decl_bare
        ; Alcotest.test_case "with alias"      `Quick test_import_decl_alias
        ] )
    ; ( "program",
        [ Alcotest.test_case "two fns"         `Quick test_program_two_fns
        ] )
    ; ( "comments",
        [ Alcotest.test_case "fn comment"      `Quick test_fn_decl_comment
        ; Alcotest.test_case "type comment"    `Quick test_type_decl_comment
        ] )
    ; ( "open-effect-rows",
        [ Alcotest.test_case "effect with tail var"      `Quick test_open_effect_row
        ; Alcotest.test_case "tail variable only"        `Quick test_open_effect_row_tail_only
        ; Alcotest.test_case "bare row variable"         `Quick test_bare_row_variable
        ; Alcotest.test_case "multiple effects with tail" `Quick test_open_effect_row_multi
        ] ) ]
