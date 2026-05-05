let usage = "Usage: axiom build <input.axm> [-o <output.wasm>] [--no-tail-calls]"

(** Scan [prog] for [DeclImport] nodes and load each referenced module from
    [dir] as a [DeclModule] prepended to the program.  Modules already
    satisfied by a [DeclModule] declaration in the same file (or in [seen])
    are not loaded again.  Raises [Failure] on missing files or cycles. *)
let rec resolve_imports ~dir ~seen prog =
  let defined =
    List.filter_map (fun (d : Axiom_lib.Ast.decl) ->
      match d.Axiom_lib.Ast.decl_desc with
      | Axiom_lib.Ast.DeclModule { module_name; _ } -> Some module_name
      | _ -> None
    ) prog
  in
  let needed =
    List.filter_map (fun (d : Axiom_lib.Ast.decl) ->
      match d.Axiom_lib.Ast.decl_desc with
      | Axiom_lib.Ast.DeclImport { module_path; _ } ->
        if List.mem module_path defined || List.mem module_path seen
        then None
        else Some module_path
      | _ -> None
    ) prog
    |> List.sort_uniq String.compare
  in
  let module_decls =
    List.map (fun module_name ->
      let file_path = Filename.concat dir (module_name ^ ".axm") in
      let source =
        try
          let ic = open_in file_path in
          let n  = in_channel_length ic in
          let s  = Bytes.create n in
          really_input ic s 0 n;
          close_in ic;
          Bytes.to_string s
        with Sys_error msg ->
          failwith (Printf.sprintf
            "axiom build: cannot read imported module '%s': %s"
            module_name msg)
      in
      let tokens =
        try Axiom_lib.Lexer.tokenize source
        with Failure msg ->
          failwith (Printf.sprintf
            "axiom build: lex error in module '%s': %s" module_name msg)
      in
      let inner_prog =
        try Axiom_lib.Parser.parse_program tokens
        with Failure msg ->
          failwith (Printf.sprintf
            "axiom build: parse error in module '%s': %s" module_name msg)
      in
      let inner_prog' =
        resolve_imports ~dir ~seen:(module_name :: seen) inner_prog
      in
      Axiom_lib.Ast.(decl (DeclModule {
        pub         = true;
        module_name = module_name;
        body        = inner_prog';
      }))
    ) needed
  in
  module_decls @ prog

let () =
  let input_file   = ref "" in
  let output_file  = ref "" in
  let no_tail_calls = ref false in
  let specs =
    [ ("-o", Arg.Set_string output_file, "<file>  Output .wasm file")
    ; ("--no-tail-calls", Arg.Set no_tail_calls,
       "        Lower tail calls via a trampoline loop instead of return_call")
    ]
  in
  let anon s =
    if !input_file = "" then input_file := s
  in
  (match Array.to_list Sys.argv with
   | _ :: "build" :: rest ->
     Arg.parse_argv (Array.of_list ("axiom" :: rest)) specs anon usage
   | _ ->
     Printf.eprintf "%s\n" usage;
     exit 1);
  if !input_file = "" then (
    Printf.eprintf "axiom build: missing input file\n%s\n" usage;
    exit 1);
  if !output_file = "" then (
    Printf.eprintf "axiom build: missing -o <output>\n%s\n" usage;
    exit 1);
  let source =
    try
      let ic = open_in !input_file in
      let n = in_channel_length ic in
      let s = Bytes.create n in
      really_input ic s 0 n;
      close_in ic;
      Bytes.to_string s
    with Sys_error msg ->
      Printf.eprintf "axiom build: cannot read '%s': %s\n" !input_file msg;
      exit 1
  in
  let tokens =
    try Axiom_lib.Lexer.tokenize source
    with Failure msg ->
      Printf.eprintf "axiom build: lex error: %s\n" msg;
      exit 1
  in
  let prog =
    try Axiom_lib.Parser.parse_program tokens
    with Failure msg ->
      Printf.eprintf "axiom build: parse error: %s\n" msg;
      exit 1
  in
  let dir = Filename.dirname !input_file in
  let prog =
    try resolve_imports ~dir ~seen:[] prog
    with Failure msg ->
      Printf.eprintf "%s\n" msg;
      exit 1
  in
  (try ignore (Axiom_lib.Typechecker.check_program prog)
   with Failure msg ->
     Printf.eprintf "axiom build: type error: %s\n" msg;
     exit 1);
  let _elaborated = Axiom_lib.Elaboration.elaborate_program prog in
  let use_tail_calls = not !no_tail_calls in
  let wasm_bytes = Axiom_lib.Codegen.emit ~use_tail_calls prog in
  (try
     let oc = open_out_bin !output_file in
     output_bytes oc wasm_bytes;
     close_out oc
   with Sys_error msg ->
     Printf.eprintf "axiom build: cannot write '%s': %s\n" !output_file msg;
     exit 1)
