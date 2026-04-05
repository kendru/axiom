open Axiom_lib.Lexer

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let token_testable = Alcotest.testable pp_token equal_token

let check_tokens label input expected =
  Alcotest.(check (list token_testable)) label expected (tokenize input)

(* ------------------------------------------------------------------ *)
(* Keyword tests                                                        *)
(* ------------------------------------------------------------------ *)

let test_lex_let () = check_tokens "let" "let" [ Let ]
let test_lex_in () = check_tokens "in" "in" [ In ]
let test_lex_fn () = check_tokens "fn" "fn" [ Fn ]
let test_lex_pub () = check_tokens "pub" "pub" [ Pub ]
let test_lex_type () = check_tokens "type" "type" [ Type ]
let test_lex_module () = check_tokens "module" "module" [ Module ]
let test_lex_require () = check_tokens "require" "require" [ Require ]
let test_lex_effect () = check_tokens "effect" "effect" [ Effect ]
let test_lex_perform () = check_tokens "perform" "perform" [ Perform ]
let test_lex_handle () = check_tokens "handle" "handle" [ Handle ]
let test_lex_with () = check_tokens "with" "with" [ With ]
let test_lex_match () = check_tokens "match" "match" [ Match ]
let test_lex_do () = check_tokens "do" "do" [ Do ]
let test_lex_resume () = check_tokens "resume" "resume" [ Resume ]
let test_lex_letrec () = check_tokens "letrec" "letrec" [ Letrec ]
let test_lex_if () = check_tokens "if" "if" [ If ]
let test_lex_else () = check_tokens "else" "else" [ Else ]
let test_lex_return () = check_tokens "return" "return" [ Return ]
let test_lex_pure () = check_tokens "pure" "pure" [ Pure ]
let test_lex_true () = check_tokens "true" "true" [ True ]
let test_lex_false () = check_tokens "false" "false" [ False ]

(* ------------------------------------------------------------------ *)
(* Identifier tests                                                     *)
(* ------------------------------------------------------------------ *)

(* Lowercase-start → Ident *)
let test_lex_lowercase_ident () =
  check_tokens "lowercase ident" "foo" [ Ident "foo" ]

let test_lex_ident_with_digits () =
  check_tokens "ident with digits" "x1" [ Ident "x1" ]

let test_lex_ident_with_underscore () =
  check_tokens "ident with underscore" "my_var" [ Ident "my_var" ]

(* Uppercase-start → CtorIdent *)
let test_lex_ctor_ident () =
  check_tokens "constructor ident" "Foo" [ CtorIdent "Foo" ]

let test_lex_ctor_with_digits () =
  check_tokens "ctor with digits" "Option2" [ CtorIdent "Option2" ]

(* ------------------------------------------------------------------ *)
(* Integer literal tests                                                *)
(* ------------------------------------------------------------------ *)

let test_lex_int_zero () = check_tokens "int 0" "0" [ IntLit 0L ]
let test_lex_int_pos () = check_tokens "int 42" "42" [ IntLit 42L ]

let test_lex_int_hex () =
  check_tokens "hex 0xFF" "0xFF" [ IntLit 255L ]

(* ------------------------------------------------------------------ *)
(* Float literal tests                                                  *)
(* ------------------------------------------------------------------ *)

let test_lex_float_simple () =
  check_tokens "float 3.14" "3.14" [ FloatLit 3.14 ]

let test_lex_float_exp () =
  check_tokens "float 1.0e-5" "1.0e-5" [ FloatLit 1.0e-5 ]

(* ------------------------------------------------------------------ *)
(* String literal tests                                                 *)
(* ------------------------------------------------------------------ *)

let test_lex_string_simple () =
  check_tokens {|string "hello"|} {|"hello"|} [ StringLit "hello" ]

let test_lex_string_escape_n () =
  check_tokens "string with \\n" {|"a\nb"|} [ StringLit "a\nb" ]

let test_lex_string_escape_t () =
  check_tokens "string with \\t" {|"a\tb"|} [ StringLit "a\tb" ]

let test_lex_string_escape_quote () =
  check_tokens {|string with \"|} {|"a\"b"|} [ StringLit "a\"b" ]

let test_lex_string_escape_backslash () =
  check_tokens {|string with \\|} {|"a\\b"|} [ StringLit "a\\b" ]

(* ------------------------------------------------------------------ *)
(* Unit: () lexes as LParen RParen; parser resolves the meaning        *)
(* ------------------------------------------------------------------ *)

let test_lex_unit () =
  check_tokens "unit ()" "()" [ LParen; RParen ]

(* ------------------------------------------------------------------ *)
(* Punctuation / operator tests                                         *)
(* ------------------------------------------------------------------ *)

let test_lex_arrow () = check_tokens "->" "->" [ Arrow ]
let test_lex_fat_arrow () = check_tokens "=>" "=>" [ FatArrow ]
let test_lex_bang () = check_tokens "!" "!" [ Bang ]
let test_lex_pipe () = check_tokens "|" "|" [ Pipe ]
let test_lex_equal () = check_tokens "=" "=" [ Equal ]
let test_lex_dot () = check_tokens "." "." [ Dot ]
let test_lex_dot_dot () = check_tokens ".." ".." [ DotDot ]
let test_lex_comma () = check_tokens "," "," [ Comma ]
let test_lex_semi () = check_tokens ";" ";" [ Semi ]
let test_lex_colon () = check_tokens ":" ":" [ Colon ]
let test_lex_lparen () = check_tokens "(" "(" [ LParen ]
let test_lex_rparen () = check_tokens ")" ")" [ RParen ]
let test_lex_lbrace () = check_tokens "{" "{" [ LBrace ]
let test_lex_rbrace () = check_tokens "}" "}" [ RBrace ]
let test_lex_langle () = check_tokens "<" "<" [ LAngle ]
let test_lex_rangle () = check_tokens ">" ">" [ RAngle ]
let test_lex_plus_plus () = check_tokens "++" "++" [ PlusPlus ]
let test_lex_plus () = check_tokens "+" "+" [ Plus ]
let test_lex_minus () = check_tokens "-" "-" [ Minus ]
let test_lex_star () = check_tokens "*" "*" [ Star ]
let test_lex_slash () = check_tokens "/" "/" [ Slash ]
let test_lex_percent () = check_tokens "%" "%" [ Percent ]
let test_lex_eq_eq () = check_tokens "==" "==" [ EqEq ]
let test_lex_bang_eq () = check_tokens "!=" "!=" [ BangEq ]

(* ------------------------------------------------------------------ *)
(* Whitespace / multi-token tests                                       *)
(* ------------------------------------------------------------------ *)

let test_lex_whitespace_ignored () =
  check_tokens "whitespace ignored" "  let  " [ Let ]

let test_lex_newline_ignored () =
  check_tokens "newline ignored" "let\nin" [ Let; In ]

let test_lex_multi_token () =
  check_tokens "let x = 42 in x"
    "let x = 42 in x"
    [ Let; Ident "x"; Equal; IntLit 42L; In; Ident "x" ]

(* ------------------------------------------------------------------ *)
(* Comment tests (single-line -- style)                                 *)
(* ------------------------------------------------------------------ *)

let test_lex_line_comment () =
  check_tokens "line comment skipped"
    "let -- this is a comment\nin"
    [ Let; In ]

(* ------------------------------------------------------------------ *)
(* Node-attached comment tests (@# ... #@)                              *)
(* ------------------------------------------------------------------ *)

let test_lex_comment_simple () =
  check_tokens "simple comment"
    "42 @# the answer #@"
    [ IntLit 42L; Comment "the answer" ]

let test_lex_comment_multiword () =
  check_tokens "multiword comment"
    "x @# this is a longer comment #@"
    [ Ident "x"; Comment "this is a longer comment" ]

let test_lex_comment_whitespace_trimmed () =
  check_tokens "comment whitespace trimmed"
    "x @#   padded   #@"
    [ Ident "x"; Comment "padded" ]

let test_lex_comment_between_tokens () =
  check_tokens "comment between tokens"
    "let x = 42 @# the value #@ in x"
    [ Let; Ident "x"; Equal; IntLit 42L; Comment "the value"; In; Ident "x" ]

let test_lex_comment_multiline () =
  check_tokens "multiline comment"
    "x @# line one\nline two #@"
    [ Ident "x"; Comment "line one\nline two" ]

(* ------------------------------------------------------------------ *)
(* Test runner                                                          *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "Lexer"
    [ ( "keywords",
        [ Alcotest.test_case "let" `Quick test_lex_let
        ; Alcotest.test_case "in" `Quick test_lex_in
        ; Alcotest.test_case "fn" `Quick test_lex_fn
        ; Alcotest.test_case "pub" `Quick test_lex_pub
        ; Alcotest.test_case "type" `Quick test_lex_type
        ; Alcotest.test_case "module" `Quick test_lex_module
        ; Alcotest.test_case "require" `Quick test_lex_require
        ; Alcotest.test_case "effect" `Quick test_lex_effect
        ; Alcotest.test_case "perform" `Quick test_lex_perform
        ; Alcotest.test_case "handle" `Quick test_lex_handle
        ; Alcotest.test_case "with" `Quick test_lex_with
        ; Alcotest.test_case "match" `Quick test_lex_match
        ; Alcotest.test_case "do" `Quick test_lex_do
        ; Alcotest.test_case "resume" `Quick test_lex_resume
        ; Alcotest.test_case "letrec" `Quick test_lex_letrec
        ; Alcotest.test_case "if" `Quick test_lex_if
        ; Alcotest.test_case "else" `Quick test_lex_else
        ; Alcotest.test_case "return" `Quick test_lex_return
        ; Alcotest.test_case "pure" `Quick test_lex_pure
        ; Alcotest.test_case "true" `Quick test_lex_true
        ; Alcotest.test_case "false" `Quick test_lex_false
        ] )
    ; ( "identifiers",
        [ Alcotest.test_case "lowercase ident" `Quick test_lex_lowercase_ident
        ; Alcotest.test_case "ident with digits" `Quick test_lex_ident_with_digits
        ; Alcotest.test_case "ident with underscore" `Quick test_lex_ident_with_underscore
        ; Alcotest.test_case "constructor ident" `Quick test_lex_ctor_ident
        ; Alcotest.test_case "ctor with digits" `Quick test_lex_ctor_with_digits
        ] )
    ; ( "literals",
        [ Alcotest.test_case "int 0" `Quick test_lex_int_zero
        ; Alcotest.test_case "int 42" `Quick test_lex_int_pos
        ; Alcotest.test_case "hex 0xFF" `Quick test_lex_int_hex
        ; Alcotest.test_case "float 3.14" `Quick test_lex_float_simple
        ; Alcotest.test_case "float 1.0e-5" `Quick test_lex_float_exp
        ; Alcotest.test_case "string simple" `Quick test_lex_string_simple
        ; Alcotest.test_case "string \\n escape" `Quick test_lex_string_escape_n
        ; Alcotest.test_case "string \\t escape" `Quick test_lex_string_escape_t
        ; Alcotest.test_case "string \\\" escape" `Quick test_lex_string_escape_quote
        ; Alcotest.test_case "string \\\\ escape" `Quick test_lex_string_escape_backslash
        ; Alcotest.test_case "unit ()" `Quick test_lex_unit
        ] )
    ; ( "punctuation",
        [ Alcotest.test_case "->" `Quick test_lex_arrow
        ; Alcotest.test_case "=>" `Quick test_lex_fat_arrow
        ; Alcotest.test_case "!" `Quick test_lex_bang
        ; Alcotest.test_case "|" `Quick test_lex_pipe
        ; Alcotest.test_case "=" `Quick test_lex_equal
        ; Alcotest.test_case "." `Quick test_lex_dot
        ; Alcotest.test_case ".." `Quick test_lex_dot_dot
        ; Alcotest.test_case "," `Quick test_lex_comma
        ; Alcotest.test_case ";" `Quick test_lex_semi
        ; Alcotest.test_case ":" `Quick test_lex_colon
        ; Alcotest.test_case "(" `Quick test_lex_lparen
        ; Alcotest.test_case ")" `Quick test_lex_rparen
        ; Alcotest.test_case "{" `Quick test_lex_lbrace
        ; Alcotest.test_case "}" `Quick test_lex_rbrace
        ; Alcotest.test_case "<" `Quick test_lex_langle
        ; Alcotest.test_case ">" `Quick test_lex_rangle
        ; Alcotest.test_case "++" `Quick test_lex_plus_plus
        ; Alcotest.test_case "+" `Quick test_lex_plus
        ; Alcotest.test_case "-" `Quick test_lex_minus
        ; Alcotest.test_case "*" `Quick test_lex_star
        ; Alcotest.test_case "/" `Quick test_lex_slash
        ; Alcotest.test_case "%" `Quick test_lex_percent
        ; Alcotest.test_case "==" `Quick test_lex_eq_eq
        ; Alcotest.test_case "!=" `Quick test_lex_bang_eq
        ] )
    ; ( "whitespace-and-comments",
        [ Alcotest.test_case "whitespace ignored" `Quick test_lex_whitespace_ignored
        ; Alcotest.test_case "newline ignored" `Quick test_lex_newline_ignored
        ; Alcotest.test_case "multi-token" `Quick test_lex_multi_token
        ; Alcotest.test_case "line comment" `Quick test_lex_line_comment
        ] )
    ; ( "node-comments",
        [ Alcotest.test_case "simple"              `Quick test_lex_comment_simple
        ; Alcotest.test_case "multiword"           `Quick test_lex_comment_multiword
        ; Alcotest.test_case "whitespace trimmed"  `Quick test_lex_comment_whitespace_trimmed
        ; Alcotest.test_case "between tokens"      `Quick test_lex_comment_between_tokens
        ; Alcotest.test_case "multiline"           `Quick test_lex_comment_multiline
        ] )
    ]
