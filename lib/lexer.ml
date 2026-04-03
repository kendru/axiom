(** Axiom working-form lexer.

    Converts source text into a flat list of {!token} values.
    Tokens are position-free; callers that need source locations should
    wrap this module or extend it later. *)

(* ------------------------------------------------------------------ *)
(* Token type                                                           *)
(* ------------------------------------------------------------------ *)

type token =
  (* Keywords *)
  | Let | In | Fn | Pub | Type | Module | Require | Effect
  | Perform | Handle | With | Match | Do | Resume | Letrec
  | If | Else | Return
  (* Built-in value keywords *)
  | Pure | True | False | UnitLit
  (* Identifiers *)
  | Ident of string       (** lowercase-start or underscore-start *)
  | CtorIdent of string   (** uppercase-start — constructor / type name *)
  (* Literals *)
  | IntLit of int
  | FloatLit of float
  | StringLit of string
  (* Punctuation *)
  | Arrow      (** -> *)
  | FatArrow   (** => *)
  | Bang       (** !  *)
  | Pipe       (** |  *)
  | Equal      (** =  *)
  | DotDot     (** .. *)
  | Dot        (** .  *)
  | Comma      (** ,  *)
  | Semi       (** ;  *)
  | Colon      (** :  *)
  | LParen | RParen
  | LBrace | RBrace
  | LAngle | RAngle
  (* Operators *)
  | PlusPlus   (** ++ *)
  | Plus | Minus | Star | Slash | Percent
  | EqEq       (** == *)
  | BangEq     (** != *)

(* ------------------------------------------------------------------ *)
(* Pretty-printer and equality (used by tests via Alcotest)            *)
(* ------------------------------------------------------------------ *)

let pp_token fmt = function
  | Let      -> Format.pp_print_string fmt "Let"
  | In       -> Format.pp_print_string fmt "In"
  | Fn       -> Format.pp_print_string fmt "Fn"
  | Pub      -> Format.pp_print_string fmt "Pub"
  | Type     -> Format.pp_print_string fmt "Type"
  | Module   -> Format.pp_print_string fmt "Module"
  | Require  -> Format.pp_print_string fmt "Require"
  | Effect   -> Format.pp_print_string fmt "Effect"
  | Perform  -> Format.pp_print_string fmt "Perform"
  | Handle   -> Format.pp_print_string fmt "Handle"
  | With     -> Format.pp_print_string fmt "With"
  | Match    -> Format.pp_print_string fmt "Match"
  | Do       -> Format.pp_print_string fmt "Do"
  | Resume   -> Format.pp_print_string fmt "Resume"
  | Letrec   -> Format.pp_print_string fmt "Letrec"
  | If       -> Format.pp_print_string fmt "If"
  | Else     -> Format.pp_print_string fmt "Else"
  | Return   -> Format.pp_print_string fmt "Return"
  | Pure     -> Format.pp_print_string fmt "Pure"
  | True     -> Format.pp_print_string fmt "True"
  | False    -> Format.pp_print_string fmt "False"
  | UnitLit  -> Format.pp_print_string fmt "UnitLit"
  | Ident s       -> Format.fprintf fmt "Ident(%S)" s
  | CtorIdent s   -> Format.fprintf fmt "CtorIdent(%S)" s
  | IntLit n      -> Format.fprintf fmt "IntLit(%d)" n
  | FloatLit f    -> Format.fprintf fmt "FloatLit(%g)" f
  | StringLit s   -> Format.fprintf fmt "StringLit(%S)" s
  | Arrow    -> Format.pp_print_string fmt "Arrow"
  | FatArrow -> Format.pp_print_string fmt "FatArrow"
  | Bang     -> Format.pp_print_string fmt "Bang"
  | Pipe     -> Format.pp_print_string fmt "Pipe"
  | Equal    -> Format.pp_print_string fmt "Equal"
  | DotDot   -> Format.pp_print_string fmt "DotDot"
  | Dot      -> Format.pp_print_string fmt "Dot"
  | Comma    -> Format.pp_print_string fmt "Comma"
  | Semi     -> Format.pp_print_string fmt "Semi"
  | Colon    -> Format.pp_print_string fmt "Colon"
  | LParen   -> Format.pp_print_string fmt "LParen"
  | RParen   -> Format.pp_print_string fmt "RParen"
  | LBrace   -> Format.pp_print_string fmt "LBrace"
  | RBrace   -> Format.pp_print_string fmt "RBrace"
  | LAngle   -> Format.pp_print_string fmt "LAngle"
  | RAngle   -> Format.pp_print_string fmt "RAngle"
  | PlusPlus -> Format.pp_print_string fmt "PlusPlus"
  | Plus     -> Format.pp_print_string fmt "Plus"
  | Minus    -> Format.pp_print_string fmt "Minus"
  | Star     -> Format.pp_print_string fmt "Star"
  | Slash    -> Format.pp_print_string fmt "Slash"
  | Percent  -> Format.pp_print_string fmt "Percent"
  | EqEq     -> Format.pp_print_string fmt "EqEq"
  | BangEq   -> Format.pp_print_string fmt "BangEq"

let equal_token a b = a = b

(* ------------------------------------------------------------------ *)
(* Keyword table                                                        *)
(* ------------------------------------------------------------------ *)

let keyword_of_string = function
  | "let"     -> Some Let
  | "in"      -> Some In
  | "fn"      -> Some Fn
  | "pub"     -> Some Pub
  | "type"    -> Some Type
  | "module"  -> Some Module
  | "require" -> Some Require
  | "effect"  -> Some Effect
  | "perform" -> Some Perform
  | "handle"  -> Some Handle
  | "with"    -> Some With
  | "match"   -> Some Match
  | "do"      -> Some Do
  | "resume"  -> Some Resume
  | "letrec"  -> Some Letrec
  | "if"      -> Some If
  | "else"    -> Some Else
  | "return"  -> Some Return
  | "pure"    -> Some Pure
  | "true"    -> Some True
  | "false"   -> Some False
  | _         -> None

(* ------------------------------------------------------------------ *)
(* Tokenizer state                                                      *)
(* ------------------------------------------------------------------ *)

type state = {
  src : string;
  len : int;
  mutable pos : int;
}

let make_state src = { src; len = String.length src; pos = 0 }

let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_upper c = c >= 'A' && c <= 'Z'
let is_digit c = c >= '0' && c <= '9'
let is_hex_digit c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
let is_ident_start c = is_alpha c || c = '_'
let is_ident_cont c = is_alpha c || is_digit c || c = '_'
let is_whitespace c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

let peek st = if st.pos < st.len then Some st.src.[st.pos] else None
let peek2 st = if st.pos + 1 < st.len then Some st.src.[st.pos + 1] else None
let advance st = st.pos <- st.pos + 1

(* ------------------------------------------------------------------ *)
(* Scanners                                                             *)
(* ------------------------------------------------------------------ *)

let scan_word st =
  let start = st.pos in
  while st.pos < st.len && is_ident_cont st.src.[st.pos] do advance st done;
  String.sub st.src start (st.pos - start)

(** Scan an integer starting after "0x" has been consumed. *)
let scan_hex_digits st =
  let start = st.pos in
  while st.pos < st.len && is_hex_digit st.src.[st.pos] do advance st done;
  String.sub st.src start (st.pos - start)

(** Scan decimal digits, returning the substring. *)
let scan_decimal_digits st =
  let start = st.pos in
  while st.pos < st.len && is_digit st.src.[st.pos] do advance st done;
  String.sub st.src start (st.pos - start)

(** Scan a numeric literal (int or float).
    The first digit has already been checked but NOT consumed. *)
let scan_number st =
  (* Check for 0x hex prefix *)
  if st.src.[st.pos] = '0' && (match peek2 st with Some 'x' | Some 'X' -> true | _ -> false) then begin
    advance st; (* '0' *)
    advance st; (* 'x' *)
    let digits = scan_hex_digits st in
    IntLit (int_of_string ("0x" ^ digits))
  end else begin
    let int_part = scan_decimal_digits st in
    (* Check for fractional or exponent part → float *)
    let is_float = ref false in
    let frac_part = ref "" in
    let exp_part = ref "" in
    (match peek st with
     | Some '.' when (match peek2 st with Some '.' -> false | _ -> true) ->
       is_float := true;
       advance st;
       frac_part := scan_decimal_digits st
     | _ -> ());
    (match peek st with
     | Some 'e' | Some 'E' ->
       is_float := true;
       advance st;
       let sign = match peek st with
         | Some '+' -> advance st; "+"
         | Some '-' -> advance st; "-"
         | _ -> ""
       in
       exp_part := sign ^ scan_decimal_digits st
     | _ -> ());
    if !is_float then
      FloatLit (float_of_string (int_part ^ "." ^ !frac_part ^
                                  (if !exp_part = "" then "" else "e" ^ !exp_part)))
    else
      IntLit (int_of_string int_part)
  end

(** Scan a string literal. The opening '"' has already been consumed. *)
let scan_string st =
  let buf = Buffer.create 32 in
  let rec loop () =
    match peek st with
    | None -> failwith "Unterminated string literal"
    | Some '"' -> advance st
    | Some '\\' ->
      advance st;
      (match peek st with
       | Some 'n'  -> advance st; Buffer.add_char buf '\n'; loop ()
       | Some 't'  -> advance st; Buffer.add_char buf '\t'; loop ()
       | Some '"'  -> advance st; Buffer.add_char buf '"';  loop ()
       | Some '\\' -> advance st; Buffer.add_char buf '\\'; loop ()
       | Some c    -> advance st; Buffer.add_char buf c;    loop ()
       | None      -> failwith "Unterminated escape sequence")
    | Some c ->
      advance st; Buffer.add_char buf c; loop ()
  in
  loop ();
  StringLit (Buffer.contents buf)

(** Skip a line comment started with '--'. *)
let skip_line_comment st =
  while st.pos < st.len && st.src.[st.pos] <> '\n' do advance st done

(* ------------------------------------------------------------------ *)
(* Main tokenizer                                                       *)
(* ------------------------------------------------------------------ *)

let tokenize (src : string) : token list =
  let st = make_state src in
  let acc = ref [] in
  let push t = acc := t :: !acc in
  let rec loop () =
    match peek st with
    | None -> ()

    (* Whitespace *)
    | Some ch when is_whitespace ch -> advance st; loop ()

    (* Line comments: -- *)
    | Some '-' when peek2 st = Some '-' ->
      advance st; advance st; skip_line_comment st; loop ()

    (* Identifiers / keywords *)
    | Some ch when is_ident_start ch ->
      let word = scan_word st in
      (match keyword_of_string word with
       | Some kw -> push kw
       | None ->
         if is_upper ch
         then push (CtorIdent word)
         else push (Ident word));
      loop ()

    (* Numeric literals *)
    | Some ch when is_digit ch ->
      push (scan_number st); loop ()

    (* String literals *)
    | Some '"' -> advance st; push (scan_string st); loop ()

    (* Unit literal "()" — must check before '(' *)
    | Some '(' when peek2 st = Some ')' ->
      advance st; advance st; push UnitLit; loop ()

    (* Two-character operators — check before single-char fallthrough *)
    | Some '-' when peek2 st = Some '>' ->
      advance st; advance st; push Arrow; loop ()
    | Some '=' when peek2 st = Some '>' ->
      advance st; advance st; push FatArrow; loop ()
    | Some '=' when peek2 st = Some '=' ->
      advance st; advance st; push EqEq; loop ()
    | Some '!' when peek2 st = Some '=' ->
      advance st; advance st; push BangEq; loop ()
    | Some '+' when peek2 st = Some '+' ->
      advance st; advance st; push PlusPlus; loop ()
    | Some '.' when peek2 st = Some '.' ->
      advance st; advance st; push DotDot; loop ()

    (* Single-character tokens *)
    | Some '!' -> advance st; push Bang;    loop ()
    | Some '|' -> advance st; push Pipe;    loop ()
    | Some '=' -> advance st; push Equal;   loop ()
    | Some '.' -> advance st; push Dot;     loop ()
    | Some ',' -> advance st; push Comma;   loop ()
    | Some ';' -> advance st; push Semi;    loop ()
    | Some ':' -> advance st; push Colon;   loop ()
    | Some '(' -> advance st; push LParen;  loop ()
    | Some ')' -> advance st; push RParen;  loop ()
    | Some '{' -> advance st; push LBrace;  loop ()
    | Some '}' -> advance st; push RBrace;  loop ()
    | Some '<' -> advance st; push LAngle;  loop ()
    | Some '>' -> advance st; push RAngle;  loop ()
    | Some '+' -> advance st; push Plus;    loop ()
    | Some '-' -> advance st; push Minus;   loop ()
    | Some '*' -> advance st; push Star;    loop ()
    | Some '/' -> advance st; push Slash;   loop ()
    | Some '%' -> advance st; push Percent; loop ()

    (* Skip unknown characters *)
    | Some _ -> advance st; loop ()
  in
  loop ();
  List.rev !acc
