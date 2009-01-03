
(** {6 Full CUDF document parsing} *)

(** parse a CUDF document as a whole *)
val parse_cudf : in_channel -> Cudf.cudf

(** parse a CUDF document missing the request information item *)
val parse_packages : in_channel -> Cudf.package list

(** {6 Item-by-item CUDF parsing} *)

(** parse the next information item (either a package description or a
    user request) from the given input channel. *)
val parse_item :
  in_channel -> [ `Package of Cudf.package | `Request of Cudf.request ]

(** {6 Low-level parsing functions} *)

(** Parse a file stanza (i.e., a RFC822-like stanza, with the notable
    simplification that all field/value pairs are one-liners). Strip
    any heading blanks lines leading to the first available
    field/value pair.
    
    @return an associative list mapping field name to field values*)
val parse_stanza : in_channel -> (string * string) list

