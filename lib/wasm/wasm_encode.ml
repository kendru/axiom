(** WASM binary encoder for Milestone 1 codegen.

    Covers: LEB128 helpers, section emitters, a structured instruction type
    for the required opcode subset, function-body encoding, and a module
    builder with add_type / add_func / add_export helpers.

    Out of scope: Memory64, GC proposal, exception-handling, full instruction
    set.  Add encodings on demand in later sub-issues. *)

(* ------------------------------------------------------------------ *)
(* Types                                                               *)
(* ------------------------------------------------------------------ *)

type val_type = I32 | I64 | F32 | F64

(** Block type annotation for if/block/loop. *)
type block_type =
  | Void             (** No result (0x40) *)
  | ValType of val_type
  | TypeIdx of int   (** Multi-value: signed LEB128 index into type section *)

(** Instruction subset required for Milestone 1. *)
type instr =
  | I32Const of int
  | I32Add
  | I32Sub
  | I32Mul
  | I32Eq
  | I32Ne
  | I32LtS
  | I32GtS
  | I32LeS
  | I32GeS
  | LocalGet of int
  | LocalSet of int
  | LocalTee of int
  | Call of int
  | If of block_type * instr list * instr list option
  | Block of block_type * instr list
  | Loop of block_type * instr list
  | Br of int
  | BrIf of int
  | BrTable of int list * int  (** targets, default *)
  | Return
  | Drop
  | GlobalGet of int
  | GlobalSet of int
  | I32Load of int * int        (** align, offset *)
  | I32Store of int * int       (** align, offset *)
  | CallIndirect of int * int   (** type_idx, table_idx *)
  | ReturnCall of int           (** tail call by func index (opcode 0x12) *)
  | ReturnCallIndirect of int * int (** tail call indirect: type_idx, table_idx (opcode 0x13) *)

(** Run-length encoded local variable group in a function body. *)
type local_decl = {
  count : int;
  ty : val_type;
}

(** A function's type signature. *)
type func_type = {
  params : val_type list;
  results : val_type list;
}

(** Export descriptor. *)
type export_desc =
  | ExportFunc of int
  | ExportTable of int
  | ExportMem of int
  | ExportGlobal of int

(** Import descriptor (func imports only for now). *)
type import_desc =
  | ImportFunc of int  (** type index *)

(** Constant initialiser for globals. *)
type global_init =
  | GlobI32 of int
  | GlobI64 of int64
  | GlobF32 of float
  | GlobF64 of float

(** Memory / table size constraints. *)
type limits = {
  min : int;
  max : int option;
}

(** Active data segment targeting memory 0. *)
type data_segment = {
  offset : int;
  data : bytes;
}

(** Active element segment targeting table 0. *)
type elem_segment = {
  elem_offset : int;
  func_indices : int list;
}

(** Mutable accumulator used to build a WASM module incrementally. *)
type module_builder = {
  mutable types    : func_type list;
  mutable imports  : (string * string * import_desc) list;
  mutable funcs    : int list;
  mutable tables   : limits list;
  mutable memories : limits list;
  mutable globals  : (val_type * bool * global_init) list;
  mutable exports  : (string * export_desc) list;
  mutable start    : int option;
  mutable elements : elem_segment list;
  mutable bodies   : (local_decl list * instr list) list;
  mutable data     : data_segment list;
}

(* ------------------------------------------------------------------ *)
(* Low-level byte-emission primitives                                  *)
(* ------------------------------------------------------------------ *)

let put_byte buf b =
  Buffer.add_char buf (Char.chr (b land 0xFF))

let put_u32_le buf n =
  put_byte buf (n land 0xFF);
  put_byte buf ((n lsr 8)  land 0xFF);
  put_byte buf ((n lsr 16) land 0xFF);
  put_byte buf ((n lsr 24) land 0xFF)

(** Unsigned LEB128 (used for counts, section sizes, unsigned indices). *)
let put_uleb128 buf n =
  let n = ref n in
  let go = ref true in
  while !go do
    let byte = !n land 0x7F in
    n := !n lsr 7;
    if !n = 0 then (put_byte buf byte; go := false)
    else put_byte buf (byte lor 0x80)
  done

(** Signed LEB128 (used for i32.const immediates and block type indices). *)
let put_sleb128 buf n =
  let n = ref n in
  let go = ref true in
  while !go do
    let byte = !n land 0x7F in
    n := !n asr 7;
    if (!n = 0 && byte land 0x40 = 0) || (!n = -1 && byte land 0x40 <> 0)
    then (put_byte buf byte; go := false)
    else put_byte buf (byte lor 0x80)
  done

(** Length-prefixed UTF-8 string (uleb128 length then raw bytes). *)
let put_string buf s =
  put_uleb128 buf (String.length s);
  Buffer.add_string buf s

(* ------------------------------------------------------------------ *)
(* Value / block type encoding                                         *)
(* ------------------------------------------------------------------ *)

let byte_of_val_type = function
  | I32 -> 0x7F
  | I64 -> 0x7E
  | F32 -> 0x7D
  | F64 -> 0x7C

let put_val_type buf vt = put_byte buf (byte_of_val_type vt)

let put_block_type buf = function
  | Void       -> put_byte buf 0x40
  | ValType vt -> put_val_type buf vt
  | TypeIdx i  -> put_sleb128 buf i

(* ------------------------------------------------------------------ *)
(* Instruction encoding                                                *)
(* ------------------------------------------------------------------ *)

let rec put_instr buf instr =
  match instr with
  | I32Const n ->
    put_byte buf 0x41;
    put_sleb128 buf n
  | I32Add  -> put_byte buf 0x6A
  | I32Sub  -> put_byte buf 0x6B
  | I32Mul  -> put_byte buf 0x6C
  | I32Eq   -> put_byte buf 0x46
  | I32Ne   -> put_byte buf 0x47
  | I32LtS  -> put_byte buf 0x48
  | I32GtS  -> put_byte buf 0x4A
  | I32LeS  -> put_byte buf 0x4C
  | I32GeS  -> put_byte buf 0x4E
  | LocalGet i ->
    put_byte buf 0x20;
    put_uleb128 buf i
  | LocalSet i ->
    put_byte buf 0x21;
    put_uleb128 buf i
  | LocalTee i ->
    put_byte buf 0x22;
    put_uleb128 buf i
  | Call i ->
    put_byte buf 0x10;
    put_uleb128 buf i
  | If (bt, then_b, else_opt) ->
    put_byte buf 0x04;
    put_block_type buf bt;
    List.iter (put_instr buf) then_b;
    (match else_opt with
     | Some else_b ->
       put_byte buf 0x05;
       List.iter (put_instr buf) else_b
     | None -> ());
    put_byte buf 0x0B
  | Block (bt, body) ->
    put_byte buf 0x02;
    put_block_type buf bt;
    List.iter (put_instr buf) body;
    put_byte buf 0x0B
  | Loop (bt, body) ->
    put_byte buf 0x03;
    put_block_type buf bt;
    List.iter (put_instr buf) body;
    put_byte buf 0x0B
  | Br label ->
    put_byte buf 0x0C;
    put_uleb128 buf label
  | BrIf label ->
    put_byte buf 0x0D;
    put_uleb128 buf label
  | BrTable (targets, default) ->
    put_byte buf 0x0E;
    put_uleb128 buf (List.length targets);
    List.iter (put_uleb128 buf) targets;
    put_uleb128 buf default
  | Return -> put_byte buf 0x0F
  | Drop   -> put_byte buf 0x1A
  | GlobalGet i ->
    put_byte buf 0x23;
    put_uleb128 buf i
  | GlobalSet i ->
    put_byte buf 0x24;
    put_uleb128 buf i
  | I32Load (align, offset) ->
    put_byte buf 0x28;
    put_uleb128 buf align;
    put_uleb128 buf offset
  | I32Store (align, offset) ->
    put_byte buf 0x36;
    put_uleb128 buf align;
    put_uleb128 buf offset
  | CallIndirect (type_idx, table_idx) ->
    put_byte buf 0x11;
    put_uleb128 buf type_idx;
    put_uleb128 buf table_idx
  | ReturnCall i ->
    put_byte buf 0x12;
    put_uleb128 buf i
  | ReturnCallIndirect (type_idx, table_idx) ->
    put_byte buf 0x13;
    put_uleb128 buf type_idx;
    put_uleb128 buf table_idx

(* ------------------------------------------------------------------ *)
(* Section helpers                                                     *)
(* ------------------------------------------------------------------ *)

(** Write a section: emit the id byte, compute content into a temp buffer,
    then emit the uleb128 size followed by the content bytes. *)
let emit_section buf id encode_content data =
  put_byte buf id;
  let sec = Buffer.create 32 in
  encode_content sec data;
  let sec_bytes = Buffer.to_bytes sec in
  put_uleb128 buf (Bytes.length sec_bytes);
  Buffer.add_bytes buf sec_bytes

(* ------------------------------------------------------------------ *)
(* Per-section encoders                                                *)
(* ------------------------------------------------------------------ *)

let encode_type_section buf types =
  put_uleb128 buf (List.length types);
  List.iter (fun { params; results } ->
    put_byte buf 0x60;
    put_uleb128 buf (List.length params);
    List.iter (put_val_type buf) params;
    put_uleb128 buf (List.length results);
    List.iter (put_val_type buf) results
  ) types

let encode_import_section buf imports =
  put_uleb128 buf (List.length imports);
  List.iter (fun (modname, fieldname, desc) ->
    put_string buf modname;
    put_string buf fieldname;
    match desc with
    | ImportFunc type_idx ->
      put_byte buf 0x00;
      put_uleb128 buf type_idx
  ) imports

let encode_func_section buf funcs =
  put_uleb128 buf (List.length funcs);
  List.iter (put_uleb128 buf) funcs

let put_limits buf { min; max } =
  match max with
  | None   -> put_byte buf 0x00; put_uleb128 buf min
  | Some n -> put_byte buf 0x01; put_uleb128 buf min; put_uleb128 buf n

let encode_table_section buf tables =
  put_uleb128 buf (List.length tables);
  List.iter (fun lim ->
    put_byte buf 0x70;   (* funcref *)
    put_limits buf lim
  ) tables

let encode_memory_section buf memories =
  put_uleb128 buf (List.length memories);
  List.iter (put_limits buf) memories

let put_global_init buf = function
  | GlobI32 n ->
    put_byte buf 0x41; put_sleb128 buf n; put_byte buf 0x0B
  | GlobI64 n ->
    put_byte buf 0x42;
    let n = ref n in
    let go = ref true in
    while !go do
      let byte = Int64.to_int (Int64.logand !n 0x7FL) in
      n := Int64.shift_right !n 7;
      if (!n = 0L && byte land 0x40 = 0) || (!n = Int64.minus_one && byte land 0x40 <> 0)
      then (put_byte buf byte; go := false)
      else put_byte buf (byte lor 0x80)
    done;
    put_byte buf 0x0B
  | GlobF32 f ->
    put_byte buf 0x43;
    let bits = Int32.bits_of_float f in
    for i = 0 to 3 do
      put_byte buf (Int32.to_int (Int32.logand (Int32.shift_right_logical bits (i * 8)) 0xFFl))
    done;
    put_byte buf 0x0B
  | GlobF64 f ->
    put_byte buf 0x44;
    let bits = Int64.bits_of_float f in
    for i = 0 to 7 do
      put_byte buf (Int64.to_int (Int64.logand (Int64.shift_right_logical bits (i * 8)) 0xFFL))
    done;
    put_byte buf 0x0B

let encode_global_section buf globals =
  put_uleb128 buf (List.length globals);
  List.iter (fun (vt, mut_, init) ->
    put_val_type buf vt;
    put_byte buf (if mut_ then 1 else 0);
    put_global_init buf init
  ) globals

let encode_export_section buf exports =
  put_uleb128 buf (List.length exports);
  List.iter (fun (name, desc) ->
    put_string buf name;
    match desc with
    | ExportFunc  i -> put_byte buf 0x00; put_uleb128 buf i
    | ExportTable i -> put_byte buf 0x01; put_uleb128 buf i
    | ExportMem   i -> put_byte buf 0x02; put_uleb128 buf i
    | ExportGlobal i -> put_byte buf 0x03; put_uleb128 buf i
  ) exports

let encode_code_section buf bodies =
  put_uleb128 buf (List.length bodies);
  List.iter (fun (locals, instrs) ->
    let body = Buffer.create 32 in
    put_uleb128 body (List.length locals);
    List.iter (fun { count; ty } ->
      put_uleb128 body count;
      put_val_type body ty
    ) locals;
    List.iter (put_instr body) instrs;
    put_byte body 0x0B;   (* end of function body *)
    let body_bytes = Buffer.to_bytes body in
    put_uleb128 buf (Bytes.length body_bytes);
    Buffer.add_bytes buf body_bytes
  ) bodies

let encode_element_section buf elements =
  put_uleb128 buf (List.length elements);
  List.iter (fun { elem_offset; func_indices } ->
    put_byte buf 0x00;         (* kind 0: active, table 0, funcref *)
    put_byte buf 0x41; put_sleb128 buf elem_offset; put_byte buf 0x0B;
    put_uleb128 buf (List.length func_indices);
    List.iter (put_uleb128 buf) func_indices
  ) elements

let encode_data_section buf data =
  put_uleb128 buf (List.length data);
  List.iter (fun { offset; data = d } ->
    put_byte buf 0x00;         (* active, memory 0 *)
    put_byte buf 0x41; put_sleb128 buf offset; put_byte buf 0x0B;
    put_uleb128 buf (Bytes.length d);
    Buffer.add_bytes buf d
  ) data

(* ------------------------------------------------------------------ *)
(* Full module encoding                                                *)
(* ------------------------------------------------------------------ *)

let encode m =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "\x00asm";
  put_u32_le buf 1;
  if m.types    <> [] then emit_section buf  1 encode_type_section    m.types;
  if m.imports  <> [] then emit_section buf  2 encode_import_section  m.imports;
  if m.funcs    <> [] then emit_section buf  3 encode_func_section    m.funcs;
  if m.tables   <> [] then emit_section buf  4 encode_table_section   m.tables;
  if m.memories <> [] then emit_section buf  5 encode_memory_section  m.memories;
  if m.globals  <> [] then emit_section buf  6 encode_global_section  m.globals;
  if m.exports  <> [] then emit_section buf  7 encode_export_section  m.exports;
  (match m.start with
   | Some idx ->
     let sec = Buffer.create 4 in
     put_uleb128 sec idx;
     let sec_bytes = Buffer.to_bytes sec in
     put_byte buf 8;
     put_uleb128 buf (Bytes.length sec_bytes);
     Buffer.add_bytes buf sec_bytes
   | None -> ());
  if m.elements <> [] then emit_section buf  9 encode_element_section m.elements;
  if m.bodies   <> [] then emit_section buf 10 encode_code_section    m.bodies;
  if m.data     <> [] then emit_section buf 11 encode_data_section    m.data;
  Buffer.to_bytes buf

(* ------------------------------------------------------------------ *)
(* Module builder API                                                  *)
(* ------------------------------------------------------------------ *)

let create () = {
  types = []; imports = []; funcs = []; tables = []; memories = [];
  globals = []; exports = []; start = None; elements = []; bodies = [];
  data = [];
}

(** Add a function type; returns its index in the type section. *)
let add_type m ft =
  let idx = List.length m.types in
  m.types <- m.types @ [ft];
  idx

(** Add an import; does not affect the local function index space directly
    but the import count is included when computing function indices. *)
let add_import m modname fieldname desc =
  m.imports <- m.imports @ [(modname, fieldname, desc)]

(** Add a local function (type index + body); returns the function index
    (import count + position in local function list). *)
let add_func m type_idx locals instrs =
  let func_idx = List.length m.imports + List.length m.funcs in
  m.funcs  <- m.funcs  @ [type_idx];
  m.bodies <- m.bodies @ [(locals, instrs)];
  func_idx

(** Add a table with the given limits. *)
let add_table m lim =
  m.tables <- m.tables @ [lim]

(** Add a memory with the given limits. *)
let add_memory m lim =
  m.memories <- m.memories @ [lim]

(** Add a global (val_type, mutable, init expression). *)
let add_global m vt mut_ init =
  let idx = List.length m.globals in
  m.globals <- m.globals @ [(vt, mut_, init)];
  idx

(** Add a named export. *)
let add_export m name desc =
  m.exports <- m.exports @ [(name, desc)]

(** Set the start function (called on module instantiation). *)
let set_start m idx =
  m.start <- Some idx

(** Add an active element segment for table 0. *)
let add_element m elem_offset func_indices =
  m.elements <- m.elements @ [{ elem_offset; func_indices }]

(** Add an active data segment for memory 0. *)
let add_data m offset data =
  m.data <- m.data @ [{ offset; data }]

(** Convenience: define a complete function (type + body) with an export
    name; returns the function index. *)
let add_function m ?(export = "") params results locals instrs =
  let type_idx = add_type m { params; results } in
  let func_idx = add_func m type_idx locals instrs in
  if export <> "" then add_export m export (ExportFunc func_idx);
  func_idx

(** Reserve a function slot (type + func-section entry) without a body.
    Returns the function index. The caller must call [add_body] exactly once
    per reserved slot, in index order, before calling [encode]. *)
let reserve_function m params results =
  let type_idx = add_type m { params; results } in
  let func_idx = List.length m.imports + List.length m.funcs in
  m.funcs <- m.funcs @ [type_idx];
  func_idx

(** Append a function body.  Must be called once for each slot produced by
    [reserve_function] (or [add_func]), in the same index order. *)
let add_body m locals instrs =
  m.bodies <- m.bodies @ [(locals, instrs)]
