(** Tests for the binary IR node encoding.

    Tests verify:
    1. Byte-level encoding matches the spec (worked example, primitive values)
    2. Structural sharing: identical subtrees produce identical hashes
    3. Comment changes produce different hashes
    4. Round-trip properties: different ASTs produce different hashes
    5. Payload structure: tag, n_children, len_inline are correct *)

open Axiom_lib.Ast
open Axiom_lib.Node_encoding

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let hash_testable = Alcotest.testable
  (fun fmt h ->
    let s = Bytes.to_string h in
    for i = 0 to String.length s - 1 do
      Format.fprintf fmt "%02x" (Char.code s.[i])
    done)
  Bytes.equal

let encode_expr_fresh e =
  let store, _tbl = make_mem_store () in
  encode_expr store e

let _encode_decl_fresh d =
  let store, _tbl = make_mem_store () in
  encode_decl store d

let _encode_program_fresh p =
  let store, _tbl = make_mem_store () in
  encode_program store p

(** Read a u8 from bytes at offset. *)
let get_u8 b off = Char.code (Bytes.get b off)

(** Read a u16 LE from bytes at offset. *)
let get_u16 b off =
  (Char.code (Bytes.get b off))
  lor ((Char.code (Bytes.get b (off + 1))) lsl 8)

(** Read a u32 LE from bytes at offset. *)
let get_u32 b off =
  (Char.code (Bytes.get b off))
  lor ((Char.code (Bytes.get b (off + 1))) lsl 8)
  lor ((Char.code (Bytes.get b (off + 2))) lsl 16)
  lor ((Char.code (Bytes.get b (off + 3))) lsl 24)

(** Get the raw payload for a hash from a mem store. *)
let get_payload store_tbl hash =
  Hashtbl.find store_tbl hash

(* ------------------------------------------------------------------ *)
(* Byte-level encoding tests                                           *)
(* ------------------------------------------------------------------ *)

(** Test the worked example from the spec: Var "x" *)
let test_var_x_payload () =
  let store, tbl = make_mem_store () in
  let h = encode_expr store (expr (Var "x")) in
  let payload = get_payload tbl h in
  (* tag=0x01, n_children=0, len_inline=4 (str "x" = 3 bytes + comment None = 1 byte) *)
  Alcotest.(check int) "tag" 0x01 (get_u8 payload 0);
  Alcotest.(check int) "n_children" 0 (get_u16 payload 1);
  Alcotest.(check int) "len_inline" 4 (get_u32 payload 3);
  (* inline: str len=1, 'x', comment=None *)
  Alcotest.(check int) "str len lo" 1 (get_u8 payload 7);
  Alcotest.(check int) "str len hi" 0 (get_u8 payload 8);
  Alcotest.(check int) "char x" (Char.code 'x') (get_u8 payload 9);
  Alcotest.(check int) "comment none" 0 (get_u8 payload 10);
  Alcotest.(check int) "payload length" 11 (Bytes.length payload)

(** Test IntLit encoding: 8-byte LE i64 *)
let test_int_lit_42 () =
  let store, tbl = make_mem_store () in
  let h = encode_expr store (expr (IntLit 42L)) in
  let payload = get_payload tbl h in
  Alcotest.(check int) "tag" 0x02 (get_u8 payload 0);
  Alcotest.(check int) "n_children" 0 (get_u16 payload 1);
  Alcotest.(check int) "len_inline" 9 (get_u32 payload 3);
  (* inline: i64 42 LE = 0x2A 0x00 ... 0x00, then comment None *)
  Alcotest.(check int) "i64 byte 0" 42 (get_u8 payload 7);
  Alcotest.(check int) "i64 byte 1" 0 (get_u8 payload 8);
  Alcotest.(check int) "comment none" 0 (get_u8 payload 15)

(** Test negative integer: -1 = 0xFF FF FF FF FF FF FF FF *)
let test_int_lit_neg1 () =
  let store, tbl = make_mem_store () in
  let h = encode_expr store (expr (IntLit (-1L))) in
  let payload = get_payload tbl h in
  for i = 0 to 7 do
    Alcotest.(check int) (Printf.sprintf "i64 byte %d" i) 0xFF (get_u8 payload (7 + i))
  done

(** Test BoolTrue: tag=0x05, no children, inline is just comment *)
let test_bool_true () =
  let store, tbl = make_mem_store () in
  let h = encode_expr store (expr (BoolLit true)) in
  let payload = get_payload tbl h in
  Alcotest.(check int) "tag" 0x05 (get_u8 payload 0);
  Alcotest.(check int) "n_children" 0 (get_u16 payload 1);
  Alcotest.(check int) "len_inline" 1 (get_u32 payload 3);
  Alcotest.(check int) "comment none" 0 (get_u8 payload 7)

(** Test BoolFalse: tag=0x06 *)
let test_bool_false () =
  let store, tbl = make_mem_store () in
  let h = encode_expr store (expr (BoolLit false)) in
  let payload = get_payload tbl h in
  Alcotest.(check int) "tag" 0x06 (get_u8 payload 0)

(** Test UnitLit: tag=0x07 *)
let test_unit_lit () =
  let store, tbl = make_mem_store () in
  let h = encode_expr store (expr UnitLit) in
  let payload = get_payload tbl h in
  Alcotest.(check int) "tag" 0x07 (get_u8 payload 0);
  Alcotest.(check int) "len_inline" 1 (get_u32 payload 3)

(** Test StringLit: lstr encoding (u32 length prefix) *)
let test_string_lit () =
  let store, tbl = make_mem_store () in
  let h = encode_expr store (expr (StringLit "hi")) in
  let payload = get_payload tbl h in
  Alcotest.(check int) "tag" 0x04 (get_u8 payload 0);
  (* inline: lstr len=2 (u32 LE), 'h', 'i', comment None *)
  Alcotest.(check int) "lstr len" 2 (get_u32 payload 7);
  Alcotest.(check int) "char h" (Char.code 'h') (get_u8 payload 11);
  Alcotest.(check int) "char i" (Char.code 'i') (get_u8 payload 12);
  Alcotest.(check int) "comment none" 0 (get_u8 payload 13)

(** Test FloatLit: f64 with NaN canonicalization *)
let test_float_nan_canonical () =
  let store, _tbl = make_mem_store () in
  let h1 = encode_expr store (expr (FloatLit Float.nan)) in
  let h2 = encode_expr store (expr (FloatLit (Float.nan *. 2.0))) in
  (* All NaNs should produce the same hash *)
  Alcotest.(check hash_testable) "NaN canonical" h1 h2

(* ------------------------------------------------------------------ *)
(* If expression: 3 children, no inline except comment                 *)
(* ------------------------------------------------------------------ *)

let test_if_structure () =
  let store, tbl = make_mem_store () in
  let e = expr (If { cond = expr (BoolLit true)
                    ; then_ = expr (IntLit 1L)
                    ; else_ = expr (IntLit 0L) }) in
  let h = encode_expr store e in
  let payload = get_payload tbl h in
  Alcotest.(check int) "tag" 0x0C (get_u8 payload 0);
  Alcotest.(check int) "n_children" 3 (get_u16 payload 1);
  Alcotest.(check int) "len_inline" 1 (get_u32 payload 3);
  (* Children: 3 × 32 bytes starting at offset 7 *)
  let child_end = 7 + 3 * 32 in
  (* Comment None at end of inline *)
  Alcotest.(check int) "comment none" 0 (get_u8 payload child_end)

(* ------------------------------------------------------------------ *)
(* Let expression: 2 children, pattern inline                          *)
(* ------------------------------------------------------------------ *)

let test_let_structure () =
  let store, tbl = make_mem_store () in
  let e = expr (Let { pat = pat (PVar "x")
                     ; value = expr (IntLit 42L)
                     ; body = expr (Var "x") }) in
  let h = encode_expr store e in
  let payload = get_payload tbl h in
  Alcotest.(check int) "tag" 0x08 (get_u8 payload 0);
  Alcotest.(check int) "n_children" 2 (get_u16 payload 1);
  (* Inline: PVar "x" = ptag(1) + str(01 00 78) + pat_comment(00) + expr_comment(00) = 6 bytes *)
  Alcotest.(check int) "len_inline" 6 (get_u32 payload 3);
  let inline_start = 7 + 2 * 32 in
  (* pat tag = PVar = 0x01 *)
  Alcotest.(check int) "pat tag" 0x01 (get_u8 payload inline_start);
  (* pat str len = 1 *)
  Alcotest.(check int) "pat str len" 1 (get_u16 payload (inline_start + 1));
  (* pat str data = 'x' *)
  Alcotest.(check int) "pat str char" (Char.code 'x') (get_u8 payload (inline_start + 3))

(* ------------------------------------------------------------------ *)
(* App expression: variable number of children                         *)
(* ------------------------------------------------------------------ *)

let test_app_children_count () =
  let store, tbl = make_mem_store () in
  let e = expr (App (expr (Var "f"), [expr (Var "x"); expr (Var "y")])) in
  let h = encode_expr store e in
  let payload = get_payload tbl h in
  Alcotest.(check int) "tag" 0x09 (get_u8 payload 0);
  Alcotest.(check int) "n_children" 3 (get_u16 payload 1)  (* fn + 2 args *)

(* ------------------------------------------------------------------ *)
(* Structural sharing tests                                            *)
(* ------------------------------------------------------------------ *)

(** Identical expressions must produce the same hash *)
let test_structural_sharing () =
  let h1 = encode_expr_fresh (expr (Var "x")) in
  let h2 = encode_expr_fresh (expr (Var "x")) in
  Alcotest.(check hash_testable) "same hash" h1 h2

(** Different variable names must produce different hashes *)
let test_different_vars () =
  let h1 = encode_expr_fresh (expr (Var "x")) in
  let h2 = encode_expr_fresh (expr (Var "y")) in
  Alcotest.(check (neg hash_testable)) "different hash" h1 h2

(** Within one store, identical subtrees are stored once *)
let test_dedup_in_store () =
  let store, tbl = make_mem_store () in
  (* Encode App(f, [x, x]) — the two Var "x" children should dedup *)
  let _ = encode_expr store (expr (App (expr (Var "f"), [expr (Var "x"); expr (Var "x")]))) in
  (* Count distinct entries: should have Var "f", Var "x", and App — 3 total *)
  Alcotest.(check int) "store entries" 3 (Hashtbl.length tbl)

(* ------------------------------------------------------------------ *)
(* Comment tests                                                       *)
(* ------------------------------------------------------------------ *)

(** Adding a comment changes the hash *)
let test_comment_changes_hash () =
  let h1 = encode_expr_fresh (expr (Var "x")) in
  let h2 = encode_expr_fresh { desc = Var "x"; comment = Some "a note" } in
  Alcotest.(check (neg hash_testable)) "comment changes hash" h1 h2

(** Same comment produces same hash *)
let test_same_comment_same_hash () =
  let h1 = encode_expr_fresh { desc = Var "x"; comment = Some "a note" } in
  let h2 = encode_expr_fresh { desc = Var "x"; comment = Some "a note" } in
  Alcotest.(check hash_testable) "same comment same hash" h1 h2

(** Different comments produce different hashes *)
let test_different_comments_different_hash () =
  let h1 = encode_expr_fresh { desc = Var "x"; comment = Some "note A" } in
  let h2 = encode_expr_fresh { desc = Var "x"; comment = Some "note B" } in
  Alcotest.(check (neg hash_testable)) "different comments" h1 h2

(** Pattern comment changes parent hash *)
let test_pattern_comment_changes_hash () =
  let e1 = expr (Let { pat = pat (PVar "x"); value = expr (IntLit 1L); body = expr (Var "x") }) in
  let e2 = expr (Let { pat = { pat_desc = PVar "x"; pat_comment = Some "binding" }
                      ; value = expr (IntLit 1L); body = expr (Var "x") }) in
  let h1 = encode_expr_fresh e1 in
  let h2 = encode_expr_fresh e2 in
  Alcotest.(check (neg hash_testable)) "pattern comment changes hash" h1 h2

(* ------------------------------------------------------------------ *)
(* Declaration encoding tests                                          *)
(* ------------------------------------------------------------------ *)

(** DeclFn: tag=0x50, 1 child *)
let test_decl_fn_structure () =
  let store, tbl = make_mem_store () in
  let d = decl (DeclFn { pub = false
                        ; fn_name = "id"
                        ; type_params = []
                        ; params = [{ param_name = "x"; param_type = TyName "Int" }]
                        ; return_type = Some (TyName "Int")
                        ; effects = None
                        ; decl_body = expr (Var "x") }) in
  let h = encode_decl store d in
  let payload = get_payload tbl h in
  Alcotest.(check int) "tag" 0x50 (get_u8 payload 0);
  Alcotest.(check int) "n_children" 1 (get_u16 payload 1);
  (* First inline byte: pub=false=0x00 *)
  let inline_start = 7 + 32 in
  Alcotest.(check int) "pub" 0 (get_u8 payload inline_start)

(** DeclType: tag=0x51, 0 children *)
let test_decl_type_structure () =
  let store, tbl = make_mem_store () in
  let d = decl (DeclType { pub = true
                          ; type_name = "Bool"
                          ; type_params = []
                          ; ctors = [ { ctor_name = "True"; ctor_params = [] }
                                    ; { ctor_name = "False"; ctor_params = [] } ] }) in
  let h = encode_decl store d in
  let payload = get_payload tbl h in
  Alcotest.(check int) "tag" 0x51 (get_u8 payload 0);
  Alcotest.(check int) "n_children" 0 (get_u16 payload 1);
  (* First inline byte: pub=true=0x01 *)
  Alcotest.(check int) "pub" 1 (get_u8 payload 7)

(** Program: tag=0x55, children = decl count *)
let test_program_structure () =
  let store, tbl = make_mem_store () in
  let prog = [ decl (DeclFn { pub = false; fn_name = "f"; type_params = []
                             ; params = []; return_type = None; effects = None
                             ; decl_body = expr UnitLit })
             ; decl (DeclFn { pub = false; fn_name = "g"; type_params = []
                             ; params = []; return_type = None; effects = None
                             ; decl_body = expr UnitLit }) ] in
  let h = encode_program store prog in
  let payload = get_payload tbl h in
  Alcotest.(check int) "tag" 0x55 (get_u8 payload 0);
  Alcotest.(check int) "n_children" 2 (get_u16 payload 1)

(* ------------------------------------------------------------------ *)
(* Match encoding test                                                 *)
(* ------------------------------------------------------------------ *)

let test_match_encoding () =
  let store, tbl = make_mem_store () in
  let e = expr (Match { scrutinee = expr (Var "x")
                       ; arms = [ { pattern = pat PLitTrue; arm_body = expr (IntLit 1L) }
                                ; { pattern = pat PLitFalse; arm_body = expr (IntLit 0L) } ] }) in
  let h = encode_expr store e in
  let payload = get_payload tbl h in
  Alcotest.(check int) "tag" 0x0B (get_u8 payload 0);
  Alcotest.(check int) "n_children" 3 (get_u16 payload 1)  (* scrutinee + 2 arm bodies *)

(* ------------------------------------------------------------------ *)
(* Do block encoding test                                              *)
(* ------------------------------------------------------------------ *)

let test_do_encoding () =
  let store, tbl = make_mem_store () in
  let e = expr (Do [ StmtLet { pat = pat (PVar "x"); value = expr (IntLit 1L) }
                   ; StmtExpr (expr (Var "x")) ]) in
  let h = encode_expr store e in
  let payload = get_payload tbl h in
  Alcotest.(check int) "tag" 0x0D (get_u8 payload 0);
  Alcotest.(check int) "n_children" 2 (get_u16 payload 1);
  (* Inline starts after children *)
  let inline_start = 7 + 2 * 32 in
  (* n_stmts = 2 *)
  Alcotest.(check int) "n_stmts" 2 (get_u16 payload inline_start);
  (* stmt 0: StmtLet = 0x01 *)
  Alcotest.(check int) "stmt 0 tag" 0x01 (get_u8 payload (inline_start + 2));
  (* After stmt 0's pattern data, stmt 1: StmtExpr = 0x00 *)
  (* PVar "x" = ptag(01) + str(01 00 78) + pat_comment(00) = 5 bytes *)
  Alcotest.(check int) "stmt 1 tag" 0x00 (get_u8 payload (inline_start + 2 + 1 + 5))

(* ------------------------------------------------------------------ *)
(* Fn encoding test                                                    *)
(* ------------------------------------------------------------------ *)

let test_fn_encoding () =
  let store, tbl = make_mem_store () in
  let e = expr (Fn { params = [{ param_name = "x"; param_type = TyName "Int" }]
                    ; return_type = Some (TyName "Int")
                    ; effects = Some Pure
                    ; fn_body = expr (Var "x") }) in
  let h = encode_expr store e in
  let payload = get_payload tbl h in
  Alcotest.(check int) "tag" 0x0A (get_u8 payload 0);
  Alcotest.(check int) "n_children" 1 (get_u16 payload 1)

(* ------------------------------------------------------------------ *)
(* Handle encoding test                                                *)
(* ------------------------------------------------------------------ *)

let test_handle_encoding () =
  let store, tbl = make_mem_store () in
  let e = expr (Handle
    { handled = expr (App (expr (Var "f"), []))
    ; handlers = [ { effect_handler = "State"
                   ; op_handlers = [ { op_handler_name = "get"
                                     ; op_handler_params = []
                                     ; op_handler_body = expr (Var "s") } ]
                   ; return_handler = Some { return_var = "v"
                                           ; return_body = expr (Var "v") } } ] }) in
  let h = encode_expr store e in
  let payload = get_payload tbl h in
  Alcotest.(check int) "tag" 0x13 (get_u8 payload 0);
  (* Children: handled + 1 op body + 1 return body = 3 *)
  Alcotest.(check int) "n_children" 3 (get_u16 payload 1)

(* ------------------------------------------------------------------ *)
(* Record encoding test                                                *)
(* ------------------------------------------------------------------ *)

let test_record_encoding () =
  let store, tbl = make_mem_store () in
  let e = expr (Record [("x", expr (IntLit 1L)); ("y", expr (IntLit 2L))]) in
  let h = encode_expr store e in
  let payload = get_payload tbl h in
  Alcotest.(check int) "tag" 0x0F (get_u8 payload 0);
  Alcotest.(check int) "n_children" 2 (get_u16 payload 1)

(* ------------------------------------------------------------------ *)
(* Letrec encoding test                                                *)
(* ------------------------------------------------------------------ *)

let test_letrec_encoding () =
  let store, tbl = make_mem_store () in
  let e = expr (Letrec
    ( [ { letrec_name = "f"
        ; letrec_params = [{ param_name = "x"; param_type = TyName "Int" }]
        ; letrec_return_type = TyName "Int"
        ; letrec_body = expr (Var "x") } ]
    , expr (App (expr (Var "f"), [expr (IntLit 0L)])) )) in
  let h = encode_expr store e in
  let payload = get_payload tbl h in
  Alcotest.(check int) "tag" 0x0E (get_u8 payload 0);
  (* Children: outer_body + 1 binding body = 2 *)
  Alcotest.(check int) "n_children" 2 (get_u16 payload 1)

(* ------------------------------------------------------------------ *)
(* Perform and Project encoding tests                                  *)
(* ------------------------------------------------------------------ *)

let test_perform_encoding () =
  let store, tbl = make_mem_store () in
  let e = expr (Perform { effect_name = "Log"; op_name = "log"
                         ; args = [expr (StringLit "hi")] }) in
  let h = encode_expr store e in
  let payload = get_payload tbl h in
  Alcotest.(check int) "tag" 0x12 (get_u8 payload 0);
  Alcotest.(check int) "n_children" 1 (get_u16 payload 1)

let test_project_encoding () =
  let store, tbl = make_mem_store () in
  let e = expr (Project (expr (Var "p"), "x")) in
  let h = encode_expr store e in
  let payload = get_payload tbl h in
  Alcotest.(check int) "tag" 0x11 (get_u8 payload 0);
  Alcotest.(check int) "n_children" 1 (get_u16 payload 1)

(* ------------------------------------------------------------------ *)
(* Test runner                                                         *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "Node_encoding"
    [ ( "byte-level",
        [ Alcotest.test_case "Var x payload"      `Quick test_var_x_payload
        ; Alcotest.test_case "IntLit 42"           `Quick test_int_lit_42
        ; Alcotest.test_case "IntLit -1"           `Quick test_int_lit_neg1
        ; Alcotest.test_case "BoolTrue"            `Quick test_bool_true
        ; Alcotest.test_case "BoolFalse"           `Quick test_bool_false
        ; Alcotest.test_case "UnitLit"             `Quick test_unit_lit
        ; Alcotest.test_case "StringLit"           `Quick test_string_lit
        ; Alcotest.test_case "FloatLit NaN canon"  `Quick test_float_nan_canonical
        ] )
    ; ( "structure",
        [ Alcotest.test_case "If 3 children"       `Quick test_if_structure
        ; Alcotest.test_case "Let 2 children"       `Quick test_let_structure
        ; Alcotest.test_case "App child count"      `Quick test_app_children_count
        ; Alcotest.test_case "Match encoding"       `Quick test_match_encoding
        ; Alcotest.test_case "Do encoding"          `Quick test_do_encoding
        ; Alcotest.test_case "Fn encoding"          `Quick test_fn_encoding
        ; Alcotest.test_case "Handle encoding"      `Quick test_handle_encoding
        ; Alcotest.test_case "Record encoding"      `Quick test_record_encoding
        ; Alcotest.test_case "Letrec encoding"      `Quick test_letrec_encoding
        ; Alcotest.test_case "Perform encoding"     `Quick test_perform_encoding
        ; Alcotest.test_case "Project encoding"     `Quick test_project_encoding
        ] )
    ; ( "sharing",
        [ Alcotest.test_case "identical = same hash"  `Quick test_structural_sharing
        ; Alcotest.test_case "different = diff hash"  `Quick test_different_vars
        ; Alcotest.test_case "dedup in store"         `Quick test_dedup_in_store
        ] )
    ; ( "comments",
        [ Alcotest.test_case "comment changes hash"   `Quick test_comment_changes_hash
        ; Alcotest.test_case "same comment same hash" `Quick test_same_comment_same_hash
        ; Alcotest.test_case "diff comment diff hash" `Quick test_different_comments_different_hash
        ; Alcotest.test_case "pattern comment"        `Quick test_pattern_comment_changes_hash
        ] )
    ; ( "declarations",
        [ Alcotest.test_case "DeclFn structure"   `Quick test_decl_fn_structure
        ; Alcotest.test_case "DeclType structure"  `Quick test_decl_type_structure
        ; Alcotest.test_case "Program structure"   `Quick test_program_structure
        ] ) ]
