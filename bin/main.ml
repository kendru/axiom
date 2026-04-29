let usage = "Usage: axiom build <input.axm> [-o <output.wasm>] [--no-tail-calls]"

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
