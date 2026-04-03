(** Axiom abstract syntax tree.

    Types grow incrementally as new language features are parsed.
    All nodes are position-free for now; source locations are a future concern. *)

(* ------------------------------------------------------------------ *)
(* Type expressions                                                     *)
(* ------------------------------------------------------------------ *)

type type_expr =
  | TyName of string                      (** Int, String, Bool, Unit, ... *)
  | TyApp  of string * type_expr list     (** List<A>, Map<K,V>, Option<A> *)

(* ------------------------------------------------------------------ *)
(* Effect sets                                                          *)
(* ------------------------------------------------------------------ *)

type effect_set =
  | Pure                   (** no effects *)
  | Effects of type_expr list  (** { Log, Throw<E>, ... } *)

(* ------------------------------------------------------------------ *)
(* Function parameters                                                  *)
(* ------------------------------------------------------------------ *)

type param = {
  param_name : string;
  param_type : type_expr;
}

(* ------------------------------------------------------------------ *)
(* Literal values (used in patterns and expressions)                   *)
(* ------------------------------------------------------------------ *)

type literal =
  | LInt    of int
  | LFloat  of float
  | LString of string
  | LBool   of bool
  | LUnit

(* ------------------------------------------------------------------ *)
(* Patterns                                                             *)
(* ------------------------------------------------------------------ *)

type pattern =
  | PWild                             (** _ *)
  | PVar   of string                  (** x *)
  | PLit   of literal                 (** 42, true, "s", () *)
  | PCtor  of string * pattern list   (** Some(p), Cons(h, t), None *)

(* ------------------------------------------------------------------ *)
(* Expression AST                                                       *)
(* ------------------------------------------------------------------ *)

type fn_data = {
  params      : param list;
  return_type : type_expr option;
  effects     : effect_set option;
  fn_body     : expr;
}

and let_binding = {
  name  : string;
  value : expr;
  body  : expr;
}

and match_arm = {
  pattern  : pattern;
  arm_body : expr;
}

and match_data = {
  scrutinee : expr;
  arms      : match_arm list;
}

and expr =
  | Var       of string
  | IntLit    of int
  | FloatLit  of float
  | StringLit of string
  | BoolLit   of bool
  | UnitLit
  | Let       of let_binding
  | App       of expr * expr list   (** f(arg1, arg2, ...) *)
  | Fn        of fn_data            (** fn (params) -> T ! E { body } *)
  | Match     of match_data         (** match e with { | p => e ... } *)

(* ------------------------------------------------------------------ *)
(* Pretty-printers                                                      *)
(* ------------------------------------------------------------------ *)

let pp_literal fmt = function
  | LInt n    -> Format.fprintf fmt "LInt(%d)" n
  | LFloat f  -> Format.fprintf fmt "LFloat(%g)" f
  | LString s -> Format.fprintf fmt "LString(%S)" s
  | LBool b   -> Format.fprintf fmt "LBool(%b)" b
  | LUnit     -> Format.pp_print_string fmt "LUnit"

let rec pp_pattern fmt = function
  | PWild      -> Format.pp_print_string fmt "PWild"
  | PVar s     -> Format.fprintf fmt "PVar(%S)" s
  | PLit l     -> Format.fprintf fmt "PLit(%a)" pp_literal l
  | PCtor (s, ps) ->
    Format.fprintf fmt "PCtor(%S, [%a])" s
      (Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f "; ")
         pp_pattern) ps

let rec pp_type_expr fmt = function
  | TyName s -> Format.pp_print_string fmt s
  | TyApp (s, args) ->
    Format.fprintf fmt "%s<%a>" s
      (Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f ", ")
         pp_type_expr) args

let pp_effect_set fmt = function
  | Pure -> Format.pp_print_string fmt "pure"
  | Effects ts ->
    Format.fprintf fmt "{%a}"
      (Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f ", ")
         pp_type_expr) ts

let pp_param fmt { param_name; param_type } =
  Format.fprintf fmt "%s: %a" param_name pp_type_expr param_type

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
  | App (f, args) ->
    Format.fprintf fmt "App(%a, [%a])"
      pp_expr f
      (Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f "; ")
         pp_expr) args
  | Fn { params; return_type; effects; fn_body } ->
    Format.fprintf fmt "Fn{params=[%a]; ret=%a; eff=%a; body=%a}"
      (Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f ", ")
         pp_param) params
      (Format.pp_print_option pp_type_expr) return_type
      (Format.pp_print_option pp_effect_set) effects
      pp_expr fn_body
  | Match { scrutinee; arms } ->
    let pp_arm fmt { pattern; arm_body } =
      Format.fprintf fmt "| %a => %a" pp_pattern pattern pp_expr arm_body
    in
    Format.fprintf fmt "Match{scrut=%a; arms=[%a]}"
      pp_expr scrutinee
      (Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f "; ")
         pp_arm) arms

(* ------------------------------------------------------------------ *)
(* Structural equality                                                  *)
(* ------------------------------------------------------------------ *)

let equal_literal a b = match a, b with
  | LInt m,    LInt n    -> m = n
  | LFloat f,  LFloat g  -> f = g
  | LString s, LString t -> s = t
  | LBool p,   LBool q   -> p = q
  | LUnit,     LUnit     -> true
  | _,         _         -> false

let rec equal_pattern a b = match a, b with
  | PWild,        PWild        -> true
  | PVar x,       PVar y       -> x = y
  | PLit la,      PLit lb      -> equal_literal la lb
  | PCtor (a, pa), PCtor (b, pb) ->
    a = b && List.length pa = List.length pb
    && List.for_all2 equal_pattern pa pb
  | _, _ -> false

let rec equal_type_expr a b = match a, b with
  | TyName x,     TyName y     -> x = y
  | TyApp (x, a), TyApp (y, b) ->
    x = y && List.length a = List.length b
    && List.for_all2 equal_type_expr a b
  | _, _ -> false

let equal_effect_set a b = match a, b with
  | Pure,       Pure       -> true
  | Effects xs, Effects ys ->
    List.length xs = List.length ys
    && List.for_all2 equal_type_expr xs ys
  | _, _ -> false

let equal_param a b =
  a.param_name = b.param_name && equal_type_expr a.param_type b.param_type

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
  | App (f1, a1), App (f2, a2) ->
    equal_expr f1 f2
    && List.length a1 = List.length a2
    && List.for_all2 equal_expr a1 a2
  | Fn fa, Fn fb ->
    List.length fa.params = List.length fb.params
    && List.for_all2 equal_param fa.params fb.params
    && Option.equal equal_type_expr fa.return_type fb.return_type
    && Option.equal equal_effect_set fa.effects fb.effects
    && equal_expr fa.fn_body fb.fn_body
  | Match ma, Match mb ->
    equal_expr ma.scrutinee mb.scrutinee
    && List.length ma.arms = List.length mb.arms
    && List.for_all2
         (fun a b -> equal_pattern a.pattern b.pattern
                     && equal_expr a.arm_body b.arm_body)
         ma.arms mb.arms
  | _, _ -> false
