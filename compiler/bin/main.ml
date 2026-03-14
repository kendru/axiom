(* Copyright 2026 Andrew Meredith
   SPDX-License-Identifier: Apache-2.0 *)

(** Axiom compiler CLI entry point.

    Subcommands (see spec/axiom-overview-draft.md §11):
      axiom check     <file>  -- type-check without compiling
      axiom compile   <file>  -- compile to WebAssembly
      axiom query     ...     -- query codebase information
      axiom transform ...     -- mechanical refactors
      axiom verify    ...     -- property verification
*)

let usage = {|
Usage: axiom <subcommand> [options] <file>

Subcommands:
  check     <file>   Type-check a source file
  compile   <file>   Compile to WebAssembly
  query     ...      Query codebase information
  transform ...      Apply mechanical refactors
  verify    ...      Verify program properties

Run `axiom <subcommand> --help` for subcommand-specific options.
|}

let () =
  if Array.length Sys.argv < 2 then (
    print_string usage;
    exit 1
  );
  match Sys.argv.(1) with
  | "check"     -> Printf.eprintf "axiom check: not yet implemented\n"; exit 1
  | "compile"   -> Printf.eprintf "axiom compile: not yet implemented\n"; exit 1
  | "query"     -> Printf.eprintf "axiom query: not yet implemented\n"; exit 1
  | "transform" -> Printf.eprintf "axiom transform: not yet implemented\n"; exit 1
  | "verify"    -> Printf.eprintf "axiom verify: not yet implemented\n"; exit 1
  | cmd ->
    Printf.eprintf "axiom: unknown subcommand '%s'\n%s" cmd usage;
    exit 1
