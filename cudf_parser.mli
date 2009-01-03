
type cudf_parser
val from_in_channel : in_channel -> cudf_parser
val close : cudf_parser -> unit

(** Parse error. Arguments: line numnber and error message *)
exception Parse_error of int * string

(** {6 Full CUDF document parsing} *)

(** parse a CUDF document as a whole *)
val parse_cudf : cudf_parser -> Cudf.cudf

(** parse a CUDF document missing the request information item *)
val parse_packages : cudf_parser -> Cudf.package list

(** {6 Item-by-item CUDF parsing} *)

(** parse the next information item (either a package description or a
    user request) from the given input channel. *)
val parse_item :
  cudf_parser -> [ `Package of Cudf.package | `Request of Cudf.request ]

(** {6 Low-level parsing functions} *)

(** Parse a file stanza (i.e., a RFC822-like stanza, with the notable
    simplification that all field/value pairs are one-liners). Strip
    any heading blanks lines leading to the first available
    field/value pair.
    
    @return an associative list mapping field name to field values*)
val parse_stanza : cudf_parser -> (string * string) list

