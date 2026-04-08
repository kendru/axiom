(** Binary IR node tag constants.

    Single source of truth for the tag byte used in node payloads and
    inline sub-structures. Both {!Node_encoding} and {!Node_decoding}
    reference these constants so the two sides cannot drift apart.

    See docs/implementation/node-encoding.md for the wire format. *)

(* ================================================================== *)
(* Expression tags                                                     *)
(* ================================================================== *)

let tag_var           = 0x01
let tag_int_lit       = 0x02
let tag_float_lit     = 0x03
let tag_string_lit    = 0x04
let tag_bool_true     = 0x05
let tag_bool_false    = 0x06
let tag_unit_lit      = 0x07
let tag_let           = 0x08
let tag_app           = 0x09
let tag_fn            = 0x0A
let tag_match         = 0x0B
let tag_if            = 0x0C
let tag_do            = 0x0D
let tag_letrec        = 0x0E
let tag_record        = 0x0F
let tag_record_update = 0x10
let tag_project       = 0x11
let tag_perform       = 0x12
let tag_handle        = 0x13

(* ================================================================== *)
(* Declaration tags                                                    *)
(* ================================================================== *)

let tag_decl_fn       = 0x50
let tag_decl_type     = 0x51
let tag_decl_effect   = 0x52
let tag_decl_module   = 0x53
let tag_decl_require  = 0x54
let tag_program       = 0x55

(* ================================================================== *)
(* Pattern tags (inline)                                               *)
(* ================================================================== *)

let ptag_wild         = 0x00
let ptag_var          = 0x01
let ptag_lit_int      = 0x02
let ptag_lit_float    = 0x03
let ptag_lit_string   = 0x04
let ptag_lit_true     = 0x05
let ptag_lit_false    = 0x06
let ptag_lit_unit     = 0x07
let ptag_ctor         = 0x08
let ptag_record       = 0x09
let ptag_or           = 0x0A

(* ================================================================== *)
(* Type expression tags (inline)                                       *)
(* ================================================================== *)

let ttag_name         = 0x00
let ttag_app          = 0x01
let ttag_tuple        = 0x02
let ttag_fun          = 0x03

(* ================================================================== *)
(* Effect set tags (inline)                                            *)
(* ================================================================== *)

let etag_pure         = 0x00
let etag_effects      = 0x01

(* ================================================================== *)
(* Do-statement tags (inline)                                          *)
(* ================================================================== *)

let stmt_tag_expr     = 0x00
let stmt_tag_let      = 0x01
