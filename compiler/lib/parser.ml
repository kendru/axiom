(* Copyright 2026 Andrew Meredith
   SPDX-License-Identifier: Apache-2.0 *)

(** Parser for the Axiom working form.
    Produces an unelaborated Ast.program.
    See spec/axiom-overview-draft.md §8.2 for the grammar. *)

(* TODO: implement parse : (Lexer.token * Ast.loc) list -> Ast.program

   Key parsing concerns:

   Expressions (expr) are left-recursive via app_expr and field projection.
   Use Pratt parsing or a precedence-climbing approach to handle:
     - Function application:   f(x, y)       high precedence, left-assoc
     - Field projection:       e.field        high precedence, left-assoc
     - Or-patterns:            p1 | p2        right-associative

   Disambiguation:
   - let-in vs do-block let: track whether we are inside a do { } block.
     Inside a do block, 'let x = e' without 'in' is a statement binding.
   - record_expr '{' vs block '{': lookahead for 'IDENT COLON' after '{'.
   - if_expr: both 'then' branch and 'else' branch are required.

   Handler clauses:
   - Each handler_clause is 'IDENT { op_handler* (return IDENT => expr)? }'
   - The 'return' branch is optional; if absent, defaults to identity.

   Or-patterns bind the same variables in both branches with the same types. *)
