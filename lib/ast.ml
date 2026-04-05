(** Axiom abstract syntax tree. *)

(* ------------------------------------------------------------------ *)
(* Type expressions and effect sets (mutually recursive)               *)
(* ------------------------------------------------------------------ *)

type type_expr =
  | TyName  of string                       (** Int, Bool, a, ... *)
  | TyApp   of string * type_expr list      (** List<A>, Map<K,V> *)
  | TyTuple of type_expr list               (** (T1, T2, ...) *)
  | TyFun   of type_expr list * type_expr * effect_set option
                                            (** (T1, T2) -> T ! E *)

and effect_set =
  | Pure                        (** no effects *)
  | Effects of type_expr list   (** { Log, Throw<E>, ... } *)

(* ------------------------------------------------------------------ *)
(* Function parameters                                                  *)
(* ------------------------------------------------------------------ *)

type param = {
  param_name : string;
  param_type : type_expr;
}

(* ------------------------------------------------------------------ *)
(* Literal values                                                       *)
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

type pattern = {
  pat_desc : pat_desc;
  pat_comment : string option;
}

and pat_desc =
  | PWild                               (** _ *)
  | PVar    of string                   (** x *)
  | PLit    of literal                  (** 42, true, "s", () *)
  | PCtor   of string * pattern list    (** Some(p), Cons(h, t) *)
  | PRecord of (string * pattern) list * bool
                                        (** { f = p, .. }; bool = is_open *)
  | POr     of pattern * pattern        (** p1 | p2 *)

(** Build a pattern node with no comment. *)
let pat k = { pat_desc = k; pat_comment = None }

(* ------------------------------------------------------------------ *)
(* Expression AST (mutually recursive)                                  *)
(* ------------------------------------------------------------------ *)

type expr = {
  desc    : expr_desc;
  comment : string option;
}

and expr_desc =
  | Var       of string
  | IntLit    of int
  | FloatLit  of float
  | StringLit of string
  | BoolLit   of bool
  | UnitLit
  | Let       of let_binding
  | App       of expr * expr list
  | Fn        of fn_data
  | Match     of match_data
  | If        of if_data
  | Do        of do_stmt list
  | Letrec    of letrec_binding list * expr
  | Record    of (string * expr) list
  | RecordUpdate of expr * (string * expr) list
  | Project   of expr * string
  | Perform   of perform_data
  | Handle    of handle_data

and fn_data = {
  params      : param list;
  return_type : type_expr option;
  effects     : effect_set option;
  fn_body     : expr;
}

and let_binding = {
  pat   : pattern;    (** the bound pattern — PVar "x" for simple bindings *)
  value : expr;
  body  : expr;
}

and letrec_binding = {
  letrec_name        : string;
  letrec_params      : param list;
  letrec_return_type : type_expr;
  letrec_body        : expr;
}

and match_arm = {
  pattern  : pattern;
  arm_body : expr;
}

and match_data = {
  scrutinee : expr;
  arms      : match_arm list;
}

and if_data = {
  cond  : expr;
  then_ : expr;
  else_ : expr;
}

and do_stmt =
  | StmtLet  of { pat : pattern; value : expr }
  | StmtExpr of expr

and perform_data = {
  effect_name : string;
  op_name     : string;
  args        : expr list;
}

and op_handler = {
  op_handler_name   : string;
  op_handler_params : string list;
  op_handler_body   : expr;
}

and return_handler = {
  return_var  : string;
  return_body : expr;
}

and effect_handler = {
  effect_handler : string;
  op_handlers    : op_handler list;
  return_handler : return_handler option;
}

and handle_data = {
  handled  : expr;
  handlers : effect_handler list;
}

(** Build an expression node with no comment. *)
let expr k = { desc = k; comment = None }

(* ------------------------------------------------------------------ *)
(* Pretty-printers                                                      *)
(* ------------------------------------------------------------------ *)

let pp_literal fmt = function
  | LInt n    -> Format.fprintf fmt "LInt(%d)" n
  | LFloat f  -> Format.fprintf fmt "LFloat(%g)" f
  | LString s -> Format.fprintf fmt "LString(%S)" s
  | LBool b   -> Format.fprintf fmt "LBool(%b)" b
  | LUnit     -> Format.pp_print_string fmt "LUnit"

let rec pp_pattern fmt p =
  pp_pattern_desc fmt p.pat_desc;
  match p.pat_comment with
  | None   -> ()
  | Some c -> Format.fprintf fmt " @#%s#@" c

and pp_pattern_desc fmt = function
  | PWild        -> Format.pp_print_string fmt "PWild"
  | PVar s       -> Format.fprintf fmt "PVar(%S)" s
  | PLit l       -> Format.fprintf fmt "PLit(%a)" pp_literal l
  | PCtor (s, ps) ->
    Format.fprintf fmt "PCtor(%S, [%a])" s
      (Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f "; ")
         pp_pattern) ps
  | PRecord (fields, open_) ->
    let pp_f fmt (name, p) = Format.fprintf fmt "%s = %a" name pp_pattern p in
    Format.fprintf fmt "PRecord{%a%s}"
      (Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f ", ") pp_f)
      fields
      (if open_ then ", .." else "")
  | POr (a, b) ->
    Format.fprintf fmt "POr(%a, %a)" pp_pattern a pp_pattern b

let rec pp_type_expr fmt = function
  | TyName s -> Format.pp_print_string fmt s
  | TyApp (s, args) ->
    Format.fprintf fmt "%s<%a>" s
      (Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f ", ")
         pp_type_expr) args
  | TyTuple ts ->
    Format.fprintf fmt "(%a)"
      (Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f ", ")
         pp_type_expr) ts
  | TyFun (params, ret, eff) ->
    Format.fprintf fmt "(%a) -> %a%s"
      (Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f ", ")
         pp_type_expr) params
      pp_type_expr ret
      (match eff with None -> "" | Some e -> Format.asprintf " ! %a" pp_effect_set e)

and pp_effect_set fmt = function
  | Pure -> Format.pp_print_string fmt "pure"
  | Effects ts ->
    Format.fprintf fmt "{%a}"
      (Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f ", ")
         pp_type_expr) ts

let pp_param fmt { param_name; param_type } =
  Format.fprintf fmt "%s: %a" param_name pp_type_expr param_type

let rec pp_expr fmt e =
  pp_expr_desc fmt e.desc;
  match e.comment with
  | None   -> ()
  | Some c -> Format.fprintf fmt " @#%s#@" c

and pp_expr_desc fmt = function
  | Var s       -> Format.fprintf fmt "Var(%S)" s
  | IntLit n    -> Format.fprintf fmt "IntLit(%d)" n
  | FloatLit f  -> Format.fprintf fmt "FloatLit(%g)" f
  | StringLit s -> Format.fprintf fmt "StringLit(%S)" s
  | BoolLit b   -> Format.fprintf fmt "BoolLit(%b)" b
  | UnitLit     -> Format.pp_print_string fmt "UnitLit"
  | Let { pat; value; body } ->
    Format.fprintf fmt "Let{pat=%a; value=%a; body=%a}"
      pp_pattern pat pp_expr value pp_expr body
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
  | If { cond; then_; else_ } ->
    Format.fprintf fmt "If{cond=%a; then=%a; else=%a}"
      pp_expr cond pp_expr then_ pp_expr else_
  | Handle { handled; handlers } ->
    let pp_op fmt { op_handler_name; op_handler_params; op_handler_body } =
      Format.fprintf fmt "%s(%s) => %a"
        op_handler_name (String.concat ", " op_handler_params) pp_expr op_handler_body
    in
    let pp_ret fmt { return_var; return_body } =
      Format.fprintf fmt "return %s => %a" return_var pp_expr return_body
    in
    let pp_handler fmt { effect_handler; op_handlers; return_handler } =
      Format.fprintf fmt "%s{%a%s}"
        effect_handler
        (Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f "; ") pp_op)
        op_handlers
        (match return_handler with
         | None   -> ""
         | Some r -> Format.asprintf "; %a" pp_ret r)
    in
    Format.fprintf fmt "Handle{handled=%a; handlers=[%a]}"
      pp_expr handled
      (Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f "; ") pp_handler)
      handlers
  | Perform { effect_name; op_name; args } ->
    Format.fprintf fmt "Perform{%s.%s(%a)}" effect_name op_name
      (Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f ", ") pp_expr)
      args
  | Do stmts ->
    let pp_stmt fmt = function
      | StmtLet { pat; value } ->
        Format.fprintf fmt "StmtLet(%a, %a)" pp_pattern pat pp_expr value
      | StmtExpr e ->
        Format.fprintf fmt "StmtExpr(%a)" pp_expr e
    in
    Format.fprintf fmt "Do[%a]"
      (Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f "; ") pp_stmt)
      stmts
  | Letrec (bindings, body) ->
    let pp_binding fmt { letrec_name; letrec_params; letrec_return_type; letrec_body } =
      Format.fprintf fmt "%s([%a]): %a = %a"
        letrec_name
        (Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f ", ") pp_param)
        letrec_params
        pp_type_expr letrec_return_type
        pp_expr letrec_body
    in
    Format.fprintf fmt "Letrec{bindings=[%a]; body=%a}"
      (Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f "; ") pp_binding)
      bindings
      pp_expr body
  | Record fields ->
    let pp_field fmt (name, e) = Format.fprintf fmt "%s: %a" name pp_expr e in
    Format.fprintf fmt "Record{%a}"
      (Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f ", ") pp_field)
      fields
  | RecordUpdate (base, fields) ->
    let pp_field fmt (name, e) = Format.fprintf fmt "%s: %a" name pp_expr e in
    Format.fprintf fmt "RecordUpdate{%a with %a}"
      pp_expr base
      (Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f ", ") pp_field)
      fields
  | Project (e, field) ->
    Format.fprintf fmt "Project(%a, %S)" pp_expr e field
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

let rec equal_pattern a b =
  a.pat_comment = b.pat_comment &&
  equal_pattern_desc a.pat_desc b.pat_desc

and equal_pattern_desc a b = match a, b with
  | PWild,           PWild           -> true
  | PVar x,          PVar y          -> x = y
  | PLit la,         PLit lb         -> equal_literal la lb
  | PCtor (a, pa),   PCtor (b, pb)   ->
    a = b && List.length pa = List.length pb
    && List.for_all2 equal_pattern pa pb
  | PRecord (fa, oa), PRecord (fb, ob) ->
    oa = ob && List.length fa = List.length fb
    && List.for_all2 (fun (na, pa) (nb, pb) ->
        na = nb && equal_pattern pa pb) fa fb
  | POr (a1, a2),    POr (b1, b2)   ->
    equal_pattern a1 b1 && equal_pattern a2 b2
  | _,               _              -> false

let rec equal_type_expr a b = match a, b with
  | TyName x,       TyName y       -> x = y
  | TyApp (x, a),   TyApp (y, b)   ->
    x = y && List.length a = List.length b
    && List.for_all2 equal_type_expr a b
  | TyTuple ta,     TyTuple tb     ->
    List.length ta = List.length tb
    && List.for_all2 equal_type_expr ta tb
  | TyFun (pa, ra, ea), TyFun (pb, rb, eb) ->
    List.length pa = List.length pb
    && List.for_all2 equal_type_expr pa pb
    && equal_type_expr ra rb
    && Option.equal equal_effect_set ea eb
  | _,               _             -> false

and equal_effect_set a b = match a, b with
  | Pure,       Pure       -> true
  | Effects xs, Effects ys ->
    List.length xs = List.length ys
    && List.for_all2 equal_type_expr xs ys
  | _,          _          -> false

let equal_param a b =
  a.param_name = b.param_name && equal_type_expr a.param_type b.param_type

let rec equal_expr a b =
  a.comment = b.comment &&
  equal_expr_desc a.desc b.desc

and equal_expr_desc a b = match a, b with
  | Var x,       Var y       -> x = y
  | IntLit m,    IntLit n    -> m = n
  | FloatLit f,  FloatLit g  -> f = g
  | StringLit s, StringLit t -> s = t
  | BoolLit p,   BoolLit q   -> p = q
  | UnitLit,     UnitLit     -> true
  | Let la,      Let lb      ->
    equal_pattern la.pat lb.pat
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
  | If ia, If ib ->
    equal_expr ia.cond ib.cond
    && equal_expr ia.then_ ib.then_
    && equal_expr ia.else_ ib.else_
  | Handle ha, Handle hb ->
    equal_expr ha.handled hb.handled
    && List.length ha.handlers = List.length hb.handlers
    && List.for_all2 (fun a b ->
        a.effect_handler = b.effect_handler
        && List.length a.op_handlers = List.length b.op_handlers
        && List.for_all2 (fun x y ->
            x.op_handler_name = y.op_handler_name
            && x.op_handler_params = y.op_handler_params
            && equal_expr x.op_handler_body y.op_handler_body)
           a.op_handlers b.op_handlers
        && Option.equal (fun r s ->
            r.return_var = s.return_var
            && equal_expr r.return_body s.return_body)
           a.return_handler b.return_handler)
       ha.handlers hb.handlers
  | Perform pa, Perform pb ->
    pa.effect_name = pb.effect_name && pa.op_name = pb.op_name
    && List.length pa.args = List.length pb.args
    && List.for_all2 equal_expr pa.args pb.args
  | Do sa, Do sb ->
    List.length sa = List.length sb
    && List.for_all2 (fun a b -> match a, b with
        | StmtLet la, StmtLet lb ->
          equal_pattern la.pat lb.pat && equal_expr la.value lb.value
        | StmtExpr a, StmtExpr b -> equal_expr a b
        | _,          _          -> false) sa sb
  | Letrec (ba, bodya), Letrec (bb, bodyb) ->
    List.length ba = List.length bb
    && List.for_all2 (fun a b ->
        a.letrec_name = b.letrec_name
        && List.length a.letrec_params = List.length b.letrec_params
        && List.for_all2 equal_param a.letrec_params b.letrec_params
        && equal_type_expr a.letrec_return_type b.letrec_return_type
        && equal_expr a.letrec_body b.letrec_body) ba bb
    && equal_expr bodya bodyb
  | Record fa, Record fb ->
    List.length fa = List.length fb
    && List.for_all2 (fun (na, ea) (nb, eb) -> na = nb && equal_expr ea eb) fa fb
  | RecordUpdate (ba, fa), RecordUpdate (bb, fb) ->
    equal_expr ba bb
    && List.length fa = List.length fb
    && List.for_all2 (fun (na, ea) (nb, eb) -> na = nb && equal_expr ea eb) fa fb
  | Project (ea, na), Project (eb, nb) ->
    equal_expr ea eb && na = nb
  | Match ma, Match mb ->
    equal_expr ma.scrutinee mb.scrutinee
    && List.length ma.arms = List.length mb.arms
    && List.for_all2
         (fun a b -> equal_pattern a.pattern b.pattern
                     && equal_expr a.arm_body b.arm_body)
         ma.arms mb.arms
  | _, _ -> false

(* ------------------------------------------------------------------ *)
(* Top-level declarations                                               *)
(* ------------------------------------------------------------------ *)

type ctor_decl = {
  ctor_name   : string;
  ctor_params : type_expr list;
}

type effect_op = {
  effect_op_name   : string;
  effect_op_params : type_expr list;
  effect_op_return : type_expr;
}

type decl = {
  decl_desc    : decl_desc;
  decl_comment : string option;
}

and decl_desc =
  | DeclFn of {
      pub         : bool;
      fn_name     : string;
      type_params : string list;
      params      : param list;
      return_type : type_expr option;
      effects     : effect_set option;
      decl_body   : expr;
    }
  | DeclType of {
      pub         : bool;
      type_name   : string;
      type_params : string list;
      ctors       : ctor_decl list;
    }
  | DeclEffect of {
      pub         : bool;
      effect_name : string;
      type_params : string list;
      ops         : effect_op list;
    }
  | DeclModule of {
      pub         : bool;
      module_name : string;
      body        : decl list;
    }
  | DeclRequire of type_expr

(** Build a declaration node with no comment. *)
let decl k = { decl_desc = k; decl_comment = None }

type program = decl list

(* ------------------------------------------------------------------ *)
(* Pretty-printers for declarations                                     *)
(* ------------------------------------------------------------------ *)

let pp_ctor_decl fmt { ctor_name; ctor_params } =
  if ctor_params = [] then
    Format.pp_print_string fmt ctor_name
  else
    Format.fprintf fmt "%s(%a)" ctor_name
      (Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f ", ")
         pp_type_expr) ctor_params

let pp_effect_op fmt { effect_op_name; effect_op_params; effect_op_return } =
  Format.fprintf fmt "%s(%a): %a"
    effect_op_name
    (Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f ", ")
       pp_type_expr) effect_op_params
    pp_type_expr effect_op_return

let rec pp_decl fmt d =
  pp_decl_desc fmt d.decl_desc;
  match d.decl_comment with
  | None   -> ()
  | Some c -> Format.fprintf fmt " @#%s#@" c

and pp_decl_desc fmt = function
  | DeclFn { pub; fn_name; type_params; params; return_type; effects; decl_body } ->
    Format.fprintf fmt "%sFn %s%s(%a)%s%s { %a }"
      (if pub then "pub " else "")
      fn_name
      (if type_params = [] then "" else "<" ^ String.concat ", " type_params ^ ">")
      (Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f ", ")
         pp_param) params
      (match return_type with None -> "" | Some t ->
         Format.asprintf " -> %a" pp_type_expr t)
      (match effects with None -> "" | Some e ->
         Format.asprintf " ! %a" pp_effect_set e)
      pp_expr decl_body
  | DeclType { pub; type_name; type_params; ctors } ->
    Format.fprintf fmt "%sType %s%s = %a"
      (if pub then "pub " else "")
      type_name
      (if type_params = [] then "" else "<" ^ String.concat ", " type_params ^ ">")
      (Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f " | ")
         pp_ctor_decl) ctors
  | DeclEffect { pub; effect_name; type_params; ops } ->
    Format.fprintf fmt "%sEffect %s%s { %a }"
      (if pub then "pub " else "")
      effect_name
      (if type_params = [] then "" else "<" ^ String.concat ", " type_params ^ ">")
      (Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f "; ")
         pp_effect_op) ops
  | DeclModule { pub; module_name; body } ->
    Format.fprintf fmt "%sModule %s { %a }"
      (if pub then "pub " else "")
      module_name
      (Format.pp_print_list ~pp_sep:(fun f () -> Format.pp_print_string f "; ")
         pp_decl) body
  | DeclRequire t ->
    Format.fprintf fmt "Require(%a)" pp_type_expr t

(* ------------------------------------------------------------------ *)
(* Equality for declarations                                            *)
(* ------------------------------------------------------------------ *)

let equal_ctor_decl a b =
  a.ctor_name = b.ctor_name
  && List.length a.ctor_params = List.length b.ctor_params
  && List.for_all2 equal_type_expr a.ctor_params b.ctor_params

let equal_effect_op a b =
  a.effect_op_name = b.effect_op_name
  && List.length a.effect_op_params = List.length b.effect_op_params
  && List.for_all2 equal_type_expr a.effect_op_params b.effect_op_params
  && equal_type_expr a.effect_op_return b.effect_op_return

let rec equal_decl a b =
  a.decl_comment = b.decl_comment &&
  equal_decl_desc a.decl_desc b.decl_desc

and equal_decl_desc a b = match a, b with
  | DeclFn a, DeclFn b ->
    a.pub = b.pub && a.fn_name = b.fn_name
    && a.type_params = b.type_params
    && List.length a.params = List.length b.params
    && List.for_all2 equal_param a.params b.params
    && Option.equal equal_type_expr a.return_type b.return_type
    && Option.equal equal_effect_set a.effects b.effects
    && equal_expr a.decl_body b.decl_body
  | DeclType a, DeclType b ->
    a.pub = b.pub && a.type_name = b.type_name
    && a.type_params = b.type_params
    && List.length a.ctors = List.length b.ctors
    && List.for_all2 equal_ctor_decl a.ctors b.ctors
  | DeclEffect a, DeclEffect b ->
    a.pub = b.pub && a.effect_name = b.effect_name
    && a.type_params = b.type_params
    && List.length a.ops = List.length b.ops
    && List.for_all2 equal_effect_op a.ops b.ops
  | DeclModule a, DeclModule b ->
    a.pub = b.pub && a.module_name = b.module_name
    && List.length a.body = List.length b.body
    && List.for_all2 equal_decl a.body b.body
  | DeclRequire a, DeclRequire b -> equal_type_expr a b
  | _, _ -> false

let equal_program a b =
  List.length a = List.length b && List.for_all2 equal_decl a b
