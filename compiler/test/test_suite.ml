(* Copyright 2026 Andrew Meredith
   SPDX-License-Identifier: Apache-2.0 *)

(** Axiom compiler test suite.
    Uses Alcotest: https://github.com/mirage/alcotest

    Add test modules here as each compiler pass is implemented:
      ("lexer",     Lexer_tests.suite)
      ("parser",    Parser_tests.suite)
      ("elaborate", Elaborate_tests.suite)
      ("codegen",   Codegen_tests.suite)
*)

let () =
  Alcotest.run "axiom_compiler" []
