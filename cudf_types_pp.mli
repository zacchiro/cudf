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

(** CUDF type library: parsing and pretty printing

    Implement parsing and pretty printing of CUDF types {see CUDF spec. §2.2.2).

    For the actual CUDF type definition see {!Cudf_types}
*)

open Cudf_types

(** {5 Parsers} *)

(** {6 Public types} *)

val parse_int : string -> int
val parse_posint : string -> int
val parse_nat : string -> int
val parse_bool : string -> bool
val parse_string : string -> string
val parse_pkgname : string -> pkgname
val parse_ident : string -> string
val parse_enum : enums:string list -> string -> string
val parse_vpkg : string -> vpkg
val parse_vpkglist : string -> vpkglist
val parse_vpkgformula : string -> vpkgformula
val parse_veqpkg : string -> veqpkg
val parse_veqpkglist : string -> veqpkglist
val parse_typedecl : string -> typedecl

(** Parse a quoted string, enclosed by double quotes as it happens within the
    "property" property of preamble stanzas. The only place where such strings
    are allowed in CUDF are within type declarations; see
    {!Cudf_types_pp.parse_typedecl}.

    @return the parsed string after having resolved escaping and removed
    surrounding double quotes
*)
val parse_qstring : string -> string

(** Parse the enum value corresponding to the "keep" core property of package
    stanzas. Shorthand to avoid parsing the corresponding `Enum and then
    casting to {!Cudf_types.enum_keep} *)
val parse_keep : string -> enum_keep

(** generic, type-based parsing *)
val parse_value : typ -> string -> typed_value


(** {5 Pretty printers} *)

(** {6 Pretty print to abstract formatters} *)

val pp_int : Format.formatter -> int -> unit
val pp_posint : Format.formatter -> int -> unit
val pp_nat : Format.formatter -> int -> unit
val pp_bool : Format.formatter -> bool -> unit
val pp_string : Format.formatter -> string -> unit
val pp_keep : Format.formatter -> enum_keep -> unit
val pp_pkgname : Format.formatter -> pkgname -> unit
val pp_version : Format.formatter -> version -> unit
val pp_vpkg : Format.formatter -> vpkg -> unit
val pp_vpkglist : Format.formatter -> vpkglist -> unit
val pp_vpkgformula : Format.formatter -> vpkgformula -> unit
val pp_veqpkg : Format.formatter -> veqpkg -> unit
val pp_veqpkglist : Format.formatter -> veqpkglist -> unit
val pp_typedecl : Format.formatter -> typedecl -> unit

val pp_type : Format.formatter -> typ -> unit
val pp_value : Format.formatter -> typed_value -> unit


(** {6 Pretty print to string}

    Shorthand functions. *)

val string_of_int : int -> string
val string_of_posint : int -> string
val string_of_nat : int -> string
val string_of_bool : bool -> string
val string_of_keep : enum_keep -> string
val string_of_pkgname : pkgname -> string
val string_of_version : version -> string
val string_of_vpkg : vpkg -> string
val string_of_vpkglist : vpkglist -> string
val string_of_vpkgformula : vpkgformula -> string
val string_of_veqpkg : veqpkg -> string
val string_of_veqpkglist : veqpkglist -> string
val string_of_typedecl : typedecl -> string

val string_of_type : typ -> string
val string_of_value : typed_value -> string


(**/**)

(*
val encode : string -> string
val decode : string -> string
*)
