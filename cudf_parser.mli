(*****************************************************************************)
(*  libCUDF - CUDF (Common Upgrade Description Format) manipulation library  *)
(*  Copyright (C) 2009  Stefano Zacchiroli <zack@pps.jussieu.fr>             *)
(*                                                                           *)
(*  This library is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU Lesser General Public License as           *)
(*  published by the Free Software Foundation, either version 3 of the       *)
(*  License, or (at your option) any later version.  A special linking       *)
(*  exception to the GNU Lesser General Public License applies to this       *)
(*  library, see the COPYING file for more information.                      *)
(*****************************************************************************)

(** Parser for CUDF related documents *)

open Cudf

type cudf_parser
val from_in_channel : in_channel -> cudf_parser
val close : cudf_parser -> unit

(** Parse error. Arguments: line numnber and error message *)
exception Parse_error of int * string

(** {6 Full CUDF document parsing}

  "parse_*" functions offer plain syntax parsing, with no semantic
  interpretation of what is being parsed. "load_*" functions offer
  the latter, hence also checking for semantic constraints (such as
  the lack of key duplication).
*)

(** parse a CUDF document (or a universe) as a whole

  @return a pair [packages, Some req] if a complete CUDF (i.e., with
  a request part) is found, otherwise return a pair [package, None]
  if the request part is missing. Note that a document with no
  request part is not a valid CUDF document. *)
val parse : cudf_parser -> package list * request option

(** same as {!Cudf_parser.parse}, but additionally loads the package
  list as an abstract {!Cudf.universe} *)
val load : cudf_parser -> universe * request option

(** shorthand: parse a file given its name *)
val parse_from_file : string -> package list * request option

(** shorthand: load from a file given its name *)
val load_from_file : string -> universe * request option

(** {6 Item-by-item CUDF parsing} *)

(** parse the next information item (either a package description or a
  user request) from the given input channel. *)
val parse_item :
cudf_parser -> [ `Package of package | `Request of request ]

(** {6 Low-level parsing functions} *)

(** Parse a file stanza (i.e., a RFC822-like stanza, with the notable
  simplification that all field/value pairs are one-liners). Strip
  any heading blanks lines leading to the first available
  field/value pair.
  
  @return an associative list mapping field name to field values*)
val parse_stanza : cudf_parser -> (string * string) list

