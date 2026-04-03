(** Axiom abstract syntax tree.

    This module defines the expression AST for the Axiom working form.
    Types grow incrementally as new language features are parsed. *)

(* ------------------------------------------------------------------ *)
(* Expression AST                                                       *)
(* ------------------------------------------------------------------ *)

type let_binding = {
  name  : string;
  value : expr;
  body  : expr;
}

and expr =
  | Var       of string
  | IntLit    of int
  | FloatLit  of float
  | StringLit of string
  | BoolLit   of bool
  | UnitLit
  | Let       of let_binding

(* ------------------------------------------------------------------ *)
(* Pretty-printer                                                       *)
(* ------------------------------------------------------------------ *)

let rec pp_expr fmt = function
  | Var s       -> Format.fprintf fmt "Var(%S)" s
  | IntLit n    -> Format.fprintf fmt "IntLit(%d)" n
  | FloatLit f  -> Format.fprintf fmt "FloatLit(%g)" f
  | StringLit s -> Format.fprintf fmt "StringLit(%S)" s
  | BoolLit b   -> Format.fprintf fmt "BoolLit(%b)" b
  | UnitLit     -> Format.pp_print_string fmt "UnitLit"
  | Let { name; value; body } ->
    Format.fprintf fmt "Let{name=%S; value=%a; body=%a}"
      name pp_expr value pp_expr body

(* ------------------------------------------------------------------ *)
(* Structural equality                                                  *)
(* ------------------------------------------------------------------ *)

let rec equal_expr a b = match a, b with
  | Var x,       Var y       -> x = y
  | IntLit m,    IntLit n    -> m = n
  | FloatLit f,  FloatLit g  -> f = g
  | StringLit s, StringLit t -> s = t
  | BoolLit p,   BoolLit q   -> p = q
  | UnitLit,     UnitLit     -> true
  | Let la,      Let lb      ->
    la.name = lb.name
    && equal_expr la.value lb.value
    && equal_expr la.body  lb.body
  | _, _ -> false
