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

(** a CUDF parser opened on some input source *)
type cudf_parser

(** create a CUDF parser reading data from an input channel *)
val from_in_channel : in_channel -> cudf_parser

(** Dispose a CUDF parser.

    Afterwards, the parser should not be used any longer *)
val close : cudf_parser -> unit

(** Parse error. Arguments: line number and error message *)
exception Parse_error of int * string

(** {6 Full CUDF document parsing}

    "parse_*" functions offer plain syntax parsing, with no semantic
    interpretation of what is being parsed. "load_*" functions offer
    the latter, hence also checking for semantic constraints (such as
    the lack of key duplication).
*)

(** parse a CUDF document (or a universe) as a whole

    @return a triple [preamble, packages, request] where preamble and request
    are returned only if actually met in the parsed document. Note that a
    document with no request part is not a valid CUDF document (but might still
    be used to represent solver solutions, for instance). *)
val parse : cudf_parser -> preamble option * package list * request option

(** same as {!Cudf_parser.parse}, but additionally loads the package
    list as an abstract {!Cudf.universe} *)
val load : cudf_parser -> preamble option * universe * request option

(** shorthand: parse a file given its name *)
val parse_from_file : string -> preamble option * package list * request option

(** shorthand: load from a file given its name *)
val load_from_file : string -> preamble option * universe * request option

(** {6 Item-by-item CUDF parsing} *)

(** parse the next information item (either a package description, a user
    request, or a preamble) from the given input channel. *)
val parse_item : cudf_parser -> cudf_item

(** {6 Low-level parsing functions} *)

(** Parse a file stanza (i.e., a RFC822-like stanza, with the notable
    simplification that all field/value pairs are one-liners). Strip
    any heading blanks lines leading to the first available
    field/value pair.

    @return an associative list mapping field name to field values
    @raise End_of_file if no other stanza is available due to reached EOF
*)
val parse_stanza : cudf_parser -> (string * string) list
