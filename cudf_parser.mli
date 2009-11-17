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
open Cudf_types

(** a CUDF parser opened on some input source *)
type cudf_parser

(** RFC-822-like stanza, i.e. an associative list mapping property names to
    property values.

    Values are typed according to the type variable ['ty]. Usually, libCUDF
    uses either [string stanza] (for untyped stanzas) or
    [Cudf_types.typed_value stanza] (for typed stanzas). *)
type 'ty stanza = (string * 'ty) list

(** create a CUDF parser reading data from an input channel

    @param typedecl (initial) per-stanza and per-property type declarations to
    be used while parsing. Default: {!Cudf_conf.stanza_typedecl}
*)
val from_in_channel : ?typedecl:Cudf_conf.stanza_typedecl ->
  in_channel -> cudf_parser

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

(** shorthand: parse a file given its name

    @param typedecl see {!Cudf_parser.from_in_channel} *)
val parse_from_file : ?typedecl:Cudf_conf.stanza_typedecl ->
  string -> preamble option * package list * request option

(** shorthand: load from a file given its name

    @param typedecl see {!Cudf_parser.from_in_channel} *)
val load_from_file : ?typedecl:Cudf_conf.stanza_typedecl ->
  string -> preamble option * universe * request option

(** {6 Item-by-item CUDF parsing} *)

(** Parse the next information item (either a package description, a user
    request, or a preamble) from the given input channel.

    Beware that parsing is stateful; in particular when the preamble is parsed,
    the list of allowed properties for future package stanzas is internally
    updated.
*)
val parse_item : cudf_parser -> cudf_item

(** {6 Low-level parsing functions} *)

(** Parse a file stanza (i.e., a RFC822-like stanza, with the notable
    simplification that all field/value pairs are one-liners). Strip
    any heading blanks lines leading to the first available
    field/value pair.

    @return an associative list mapping field name to field values
    @raise End_of_file if no other stanza is available due to reached EOF
*)
val parse_stanza : cudf_parser -> string stanza

(** Type check an untyped stanza according to a given set of type declarations.
    Also take care of default values, adding missing properties where needed;
    fail if a required property is missing.

    @raise Parse_error
*)
val type_check_stanza : string stanza -> typedecl -> typed_value stanza
