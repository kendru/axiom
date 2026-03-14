(* Copyright 2026 Andrew Meredith
   SPDX-License-Identifier: Apache-2.0 *)

(** Abstract syntax tree for the Axiom working form.
    See spec/axiom-overview-draft.md §3 (core calculus) and §8.2 (grammar). *)

(** Source location for error reporting. *)
type loc = { file : string; line : int; col : int }

type ident = string
type ctor   = string

(** Type expressions — see §4.1 *)
type ty =
  | TVar    of ident                             (** type variable *)
  | TApp    of ident * ty list                   (** parameterized type, e.g. List<A> *)
  | TFun    of (ident * ty) list * ty * eff      (** (params) -> ret ! eff *)
  | TTuple  of ty list                           (** tuple type *)
  | TRecord of (ident * ty) list * ident option  (** record with optional row var *)

(** Effect sets — see §4.2 *)
and eff =
  | Pure               (** no effects *)
  | EffSet of ty list  (** concrete effect set *)
  | EffVar of ident    (** effect row variable *)

(** Literals — see §8.2 *)
type lit =
  | LInt    of int
  | LFloat  of float
  | LString of string
  | LBool   of bool
  | LUnit

(** Patterns — see §3.1, §8.2 *)
type pattern =
  | PWild                                    (** _ *)
  | PVar    of ident                         (** variable binding *)
  | PCtor   of ctor * pattern list           (** Ctor(p1, p2) *)
  | PLit    of lit                           (** literal match *)
  | PRecord of (ident * pattern) list * bool (** {f=p..} open=true if '..' present *)
  | POr     of pattern * pattern             (** p1 | p2 *)

(** Terms — see §3.1 *)
type expr =
  | Var     of loc * ident
  | App     of loc * expr * expr list
  | Let     of loc * pattern * ty option * expr * expr option
                                             (** None body = do-block binding *)
  | LetRec  of loc * letrec_bind list * expr
  | Match   of loc * expr * (pattern * expr) list
  | Handle  of loc * expr * handler_clause list
  | Perform of loc * ident * ident * expr list  (** Effect.op(args) *)
  | Do      of loc * stmt list * expr
  | If      of loc * expr * expr * expr
  | Record  of loc * (ident * expr) list * expr option  (** with-update if Some *)
  | Field   of loc * expr * ident
  | Lit     of loc * lit
  | Ctor    of loc * ctor * expr list

and stmt =
  | SLet  of pattern * expr
  | SExpr of expr

and letrec_bind =
  { rb_name   : ident
  ; rb_params : (ident * ty) list
  ; rb_ret    : ty
  ; rb_body   : expr
  }

and handler_clause =
  { hc_effect : ident
  ; hc_ops    : op_handler list
  ; hc_return : (ident * expr) option  (** None = identity return *)
  }

and op_handler =
  { oh_op     : ident
  ; oh_params : (ident * ty) list
  ; oh_body   : expr
  }

(** Module-level items — see §7.1 *)
type module_item =
  | MIRequire of ty
  | MIType    of ident * ident list * (ctor * ty list) list
  | MIEffect  of ident * ident list * (ident * ty list * ty) list
  | MIFn      of bool * ident * ident list * (ident * ty) list * ty * eff * expr
                 (** pub? name type_params params ret eff body *)

type module_decl =
  { md_name  : ident
  ; md_items : module_item list
  }

type program = module_decl list
