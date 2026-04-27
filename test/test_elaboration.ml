open Axiom_lib.Elaboration

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let elaborate_src src =
  let tokens = Axiom_lib.Lexer.tokenize src in
  let prog   = Axiom_lib.Parser.parse_program tokens in
  ignore (Axiom_lib.Typechecker.check_program prog);
  elaborate_program prog

(** Render an elaborated program to a string for structural assertions. *)
let dump src = dump_program (elaborate_src src)

(** Find the first [EDeclFn] matching [name] in an elaborated program. *)
let find_fn name prog =
  let rec search ds =
    List.find_map (fun d ->
        match d.edecl_desc with
        | EDeclFn f when f.fn_name = name -> Some f
        | EDeclModule { body; _ } -> search body
        | _ -> None) ds
  in
  match search prog with
  | Some f -> f
  | None   -> Alcotest.failf "function '%s' not found in elaborated program" name

(** Collect [EPerform] nodes in an [eexpr] (depth-first). *)
let rec collect_performs acc e =
  match e.edesc with
  | EPerform p -> p :: acc
  | EApp (f, args) ->
    List.fold_left collect_performs (collect_performs acc f) args
  | EFn { fn_body; _ } -> collect_performs acc fn_body
  | ELet { value; body; _ } ->
    collect_performs (collect_performs acc value) body
  | EIf { cond; then_; else_ } ->
    collect_performs
      (collect_performs (collect_performs acc cond) then_) else_
  | EDo stmts ->
    List.fold_left (fun a s -> match s with
        | EStmtExpr e | EStmtLet { value = e; _ } -> collect_performs a e)
      acc stmts
  | EHandle { handled; handlers } ->
    let a = collect_performs acc handled in
    List.fold_left (fun a h ->
        List.fold_left (fun a oh -> collect_performs a oh.op_handler_body)
          a h.op_handlers)
      a handlers
  | EMatch { scrutinee; arms } ->
    List.fold_left (fun a arm -> collect_performs a arm.arm_body)
      (collect_performs acc scrutinee) arms
  | ELetrec (bs, body) ->
    List.fold_left (fun a b -> collect_performs a b.letrec_body)
      (collect_performs acc body) bs
  | ERecord fields ->
    List.fold_left (fun a (_, v) -> collect_performs a v) acc fields
  | ERecordUpdate (e, fields) ->
    List.fold_left (fun a (_, v) -> collect_performs a v)
      (collect_performs acc e) fields
  | EProject (e, _) -> collect_performs acc e
  | _ -> acc

(* ------------------------------------------------------------------ *)
(* Pure programs: no evidence threading                                 *)
(* ------------------------------------------------------------------ *)

let test_pure_fn_no_ev_params () =
  let prog = elaborate_src {|
    fn add(x: Int, y: Int) -> Int ! pure { add(x, y) }
  |} in
  let f = find_fn "add" prog in
  Alcotest.(check int) "pure fn has no evidence params" 0 (List.length f.ev_params)

let test_pure_fn_body_unchanged () =
  (* A pure let binding: elaboration should not inject any __ev_ variables. *)
  let prog = elaborate_src {|
    fn identity(x: Int) -> Int ! pure { x }
  |} in
  let f = find_fn "identity" prog in
  (match f.decl_body.edesc with
   | EVar "x" -> ()
   | _ -> Alcotest.fail "expected body to be EVar \"x\"")

let test_pure_call_no_ev_args () =
  (* Calling a pure function should not prepend evidence arguments. *)
  let prog = elaborate_src {|
    fn double(x: Int) -> Int ! pure { add(x, x) }
  |} in
  let f = find_fn "double" prog in
  (match f.decl_body.edesc with
   | EApp (_, args) ->
     let ev_args = List.filter (fun a -> match a.edesc with
         | EVar v -> String.length v >= 4 && String.sub v 0 4 = "__ev"
         | _ -> false) args in
     Alcotest.(check int) "no evidence args for pure call" 0 (List.length ev_args)
   | _ -> Alcotest.fail "expected App node")

(* ------------------------------------------------------------------ *)
(* Effectful functions: evidence parameters added                       *)
(* ------------------------------------------------------------------ *)

let console_effect = {|
  effect Console {
    print: (String) -> Unit,
    read_line: () -> String
  }
|}

let test_effectful_fn_gets_ev_param () =
  let prog = elaborate_src (console_effect ^ {|
    fn greet() -> Unit ! {Console} {
      perform Console.print("hi")
    }
  |}) in
  let f = find_fn "greet" prog in
  Alcotest.(check int) "one evidence param" 1 (List.length f.ev_params);
  let ep = List.hd f.ev_params in
  Alcotest.(check string) "ev_effect is Console" "Console" ep.ev_effect;
  Alcotest.(check string) "ev_name is __ev_Console" "__ev_Console" ep.ev_name

let test_multiple_effects_ev_params () =
  let prog = elaborate_src ({|
    effect Log   { log:    (String) -> Unit }
    effect Audit { record: (String) -> Unit }
  |} ^ {|
    fn log_and_audit(msg: String) -> Unit ! {Log, Audit} {
      do {
        perform Log.log(msg);
        perform Audit.record(msg)
      }
    }
  |}) in
  let f = find_fn "log_and_audit" prog in
  Alcotest.(check int) "two evidence params" 2 (List.length f.ev_params);
  let names = List.map (fun ep -> ep.ev_effect) f.ev_params in
  Alcotest.(check bool) "Log present"   true (List.mem "Log"   names);
  Alcotest.(check bool) "Audit present" true (List.mem "Audit" names)

(* ------------------------------------------------------------------ *)
(* Perform nodes: annotated with evidence variable                      *)
(* ------------------------------------------------------------------ *)

let test_perform_annotated_with_ev_var () =
  let prog = elaborate_src (console_effect ^ {|
    fn say_hello() -> Unit ! {Console} {
      perform Console.print("hello")
    }
  |}) in
  let f = find_fn "say_hello" prog in
  let performs = collect_performs [] f.decl_body in
  Alcotest.(check int) "one perform" 1 (List.length performs);
  let p = List.hd performs in
  Alcotest.(check string) "ev_var is __ev_Console" "__ev_Console" p.ev_var;
  Alcotest.(check string) "effect_name" "Console" p.effect_name;
  Alcotest.(check string) "op_name" "print" p.op_name

let test_perform_multiple_effects_correct_ev () =
  let prog = elaborate_src ({|
    effect Log { log: (String) -> Unit }
    effect State<s> { get: () -> s, put: (s) -> Unit }
  |} ^ {|
    fn log_and_put(msg: String, v: Int) -> Unit ! {Log, State<Int>} {
      do {
        perform Log.log(msg);
        perform State.put(v)
      }
    }
  |}) in
  let f = find_fn "log_and_put" prog in
  let performs = collect_performs [] f.decl_body in
  Alcotest.(check int) "two performs" 2 (List.length performs);
  let find_op op_name =
    List.find (fun p -> p.op_name = op_name) performs in
  let log_p   = find_op "log" in
  let state_p = find_op "put" in
  Alcotest.(check string) "log uses __ev_Log"     "__ev_Log"   log_p.ev_var;
  Alcotest.(check string) "put uses __ev_State"   "__ev_State" state_p.ev_var

(* ------------------------------------------------------------------ *)
(* Call sites: evidence threaded for effectful callees                  *)
(* ------------------------------------------------------------------ *)

let test_call_effectful_fn_gets_ev_arg () =
  let prog = elaborate_src (console_effect ^ {|
    fn greet() -> Unit ! {Console} {
      perform Console.print("hi")
    }
    fn run_greet() -> Unit ! {Console} {
      greet()
    }
  |}) in
  let f = find_fn "run_greet" prog in
  (* The body should be EApp(EVar "greet", [EVar "__ev_Console"]) *)
  (match f.decl_body.edesc with
   | EApp (callee, args) ->
     (match callee.edesc with
      | EVar "greet" -> ()
      | _ -> Alcotest.fail "expected callee EVar greet");
     Alcotest.(check int) "one arg (the evidence)" 1 (List.length args);
     (match (List.hd args).edesc with
      | EVar "__ev_Console" -> ()
      | EVar v -> Alcotest.failf "expected __ev_Console but got %s" v
      | _ -> Alcotest.fail "expected EVar for evidence arg")
   | _ -> Alcotest.fail "expected App node")

(* ------------------------------------------------------------------ *)
(* Handle: evidence created for handled effects                         *)
(* ------------------------------------------------------------------ *)

let test_handle_creates_ev_param () =
  let prog = elaborate_src (console_effect ^ {|
    fn run(k: (u: Unit) -> Unit ! {Console}) -> Unit ! pure {
      handle k(()) with {
        Console {
          print(msg) => resume(())
          read_line() => resume("")
        }
      }
    }
  |}) in
  let f = find_fn "run" prog in
  (match f.decl_body.edesc with
   | EHandle { handlers; _ } ->
     Alcotest.(check int) "one handler" 1 (List.length handlers);
     let h = List.hd handlers in
     Alcotest.(check string) "handler effect" "Console" h.effect_handler;
     Alcotest.(check string) "ev_param effect" "Console" h.ev_param.ev_effect;
     Alcotest.(check string) "ev_param name" "__ev_Console" h.ev_param.ev_name
   | _ -> Alcotest.fail "expected EHandle node")

let test_handle_handled_expr_sees_evidence () =
  (* Use a named top-level function (greet) as the handled expression so the
     elaborator can look up its effects in fn_env and thread evidence.
     greet() is elaborated to greet(__ev_Console) inside the handle, because
     the handle extends ev_env with __ev_Console for the handled scope. *)
  let prog = elaborate_src (console_effect ^ {|
    fn greet() -> Unit ! {Console} {
      perform Console.print("hello")
    }
    fn run_handle() -> Unit ! pure {
      handle greet() with {
        Console {
          print(msg) => resume(())
          read_line() => resume("")
        }
      }
    }
  |}) in
  let f = find_fn "run_handle" prog in
  (match f.decl_body.edesc with
   | EHandle { handled; _ } ->
     (* handled = greet(__ev_Console) *)
     (match handled.edesc with
      | EApp (callee, args) ->
        (match callee.edesc with
         | EVar "greet" -> ()
         | _ -> Alcotest.fail "expected callee to be greet");
        let ev_args = List.filter (fun a -> match a.edesc with
            | EVar v -> String.length v >= 5 && String.sub v 0 5 = "__ev_"
            | _ -> false) args in
        Alcotest.(check int) "evidence arg threaded into greet() inside handle" 1
          (List.length ev_args)
      | _ -> Alcotest.fail "expected App(greet, ...) as handled expression")
   | _ -> Alcotest.fail "expected EHandle node")

(* ------------------------------------------------------------------ *)
(* Dump / round-trip: string output contains expected markers          *)
(* ------------------------------------------------------------------ *)

let test_dump_shows_ev_params () =
  let out = dump (console_effect ^ {|
    fn greet() -> Unit ! {Console} {
      perform Console.print("hi")
    }
  |}) in
  Alcotest.(check bool) "dump contains __ev_Console param"
    true (let re = Str.regexp_string "__ev_Console" in
          try ignore (Str.search_forward re out 0); true
          with Not_found -> false)

let test_dump_shows_perform_evidence () =
  let out = dump (console_effect ^ {|
    fn greet() -> Unit ! {Console} {
      perform Console.print("hello")
    }
  |}) in
  (* The printer emits "perform[__ev_Console] Console.print(...)" *)
  Alcotest.(check bool) "dump contains perform[__ev_Console]"
    true (let re = Str.regexp_string "perform[__ev_Console]" in
          try ignore (Str.search_forward re out 0); true
          with Not_found -> false)

let test_dump_pure_fn_no_ev () =
  let out = dump {|
    fn square(x: Int) -> Int ! pure { mul(x, x) }
  |} in
  Alcotest.(check bool) "dump of pure fn has no __ev marker"
    true (not (let re = Str.regexp_string "__ev" in
               try ignore (Str.search_forward re out 0); true
               with Not_found -> false))

(* ------------------------------------------------------------------ *)
(* Test registration                                                    *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "elaboration" [
    "pure programs", [
      Alcotest.test_case "pure fn has no evidence params"  `Quick test_pure_fn_no_ev_params;
      Alcotest.test_case "pure fn body unchanged"          `Quick test_pure_fn_body_unchanged;
      Alcotest.test_case "pure call has no evidence args"  `Quick test_pure_call_no_ev_args;
    ];
    "effectful functions", [
      Alcotest.test_case "effectful fn gets ev param"      `Quick test_effectful_fn_gets_ev_param;
      Alcotest.test_case "multiple effects get ev params"  `Quick test_multiple_effects_ev_params;
    ];
    "perform nodes", [
      Alcotest.test_case "perform annotated with ev_var"   `Quick test_perform_annotated_with_ev_var;
      Alcotest.test_case "perform with multiple effects"   `Quick test_perform_multiple_effects_correct_ev;
    ];
    "call sites", [
      Alcotest.test_case "call effectful fn gets ev arg"   `Quick test_call_effectful_fn_gets_ev_arg;
    ];
    "handle nodes", [
      Alcotest.test_case "handle creates ev_param"         `Quick test_handle_creates_ev_param;
      Alcotest.test_case "handle: handled expr sees ev"    `Quick test_handle_handled_expr_sees_evidence;
    ];
    "dump / round-trip", [
      Alcotest.test_case "dump shows ev params"            `Quick test_dump_shows_ev_params;
      Alcotest.test_case "dump shows perform evidence"     `Quick test_dump_shows_perform_evidence;
      Alcotest.test_case "dump of pure fn has no ev"       `Quick test_dump_pure_fn_no_ev;
    ];
  ]
