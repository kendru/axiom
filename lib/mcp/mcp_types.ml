(* Unified envelope types for the MCP batch protocol (issue #83).
   Every tool handler must return a batch_response; every diagnostic must be a
   diagnostic record.  Using these shared OCaml types makes it a compile-time
   error to diverge from the schema. *)

type severity = Sev_error | Sev_warning | Sev_info

(* Supplementary source location — content-addressed node hash is preferred. *)
type location = {
  loc_module : string option;
  loc_line   : int;
  loc_col    : int;
}

(* A secondary reference in a diagnostic (e.g. the "other" site in a conflict). *)
type related_ref = {
  ref_node    : string option;
  ref_message : string;
}

(* Single diagnostic shared by all four tools.
   [diag_node] is the primary content-addressed BLAKE3 anchor; it must be
   populated whenever the diagnostic refers to a specific program element.
   [diag_location] is supplementary and may be omitted. *)
type diagnostic = {
  diag_severity : severity;
  diag_code     : string;
  diag_message  : string;
  diag_node     : string option;
  diag_location : location option;
  diag_related  : related_ref list;
}

let make_diagnostic sev ?node ?location ?(related = []) code message =
  { diag_severity = sev;
    diag_code = code;
    diag_message = message;
    diag_node = node;
    diag_location = location;
    diag_related = related }

let make_error   = make_diagnostic Sev_error
let make_warning = make_diagnostic Sev_warning
let make_info    = make_diagnostic Sev_info
