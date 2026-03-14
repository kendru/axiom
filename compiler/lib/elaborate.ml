(* Copyright 2026 Andrew Meredith
   SPDX-License-Identifier: Apache-2.0 *)

(** Type checking and elaboration.
    Translates the surface AST into a fully-typed Binary IR.
    See spec/axiom-overview-draft.md §4 (type system) and §5 (effects).

    Strategy (§4.3):
    - Explicit annotations required at all fn and pub fn boundaries.
    - Bidirectional type inference within function bodies.
    - Effect inference: propagate effect sets upward through call graph;
      check inferred set against declared signature.
    - Row polymorphism for record types (§4.1) and effect sets (§4.2):
      unification generates row constraints, solved by the constraint solver.
*)

(* TODO: define Env.t — type environment mapping ident -> ty scheme *)
(* TODO: implement infer   : Env.t -> Ast.expr -> Ast.ty * Ast.eff *)
(* TODO: implement check   : Env.t -> Ast.expr -> Ast.ty -> Ast.eff -> unit *)
(* TODO: implement unify   : Ast.ty -> Ast.ty -> unit  (with occurs check) *)
(* TODO: implement unify_eff : Ast.eff -> Ast.eff -> unit *)
(* TODO: implement generalize : Env.t -> Ast.ty -> Ast.ty scheme *)
(* TODO: implement instantiate : Ast.ty scheme -> Ast.ty * Ast.ty list *)
(* TODO: implement elaborate : Ast.program -> Ir.ir_node list *)
