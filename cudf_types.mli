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

(** CUDF type library

    Implement core CUDF types (see CUDF spec. §3.2.2), together with
    parsing and pretty printing for them *)

(** {5 CUDF types} *)

type version = int	(* required to be non 0 *)
type relop = [`Eq|`Neq|`Geq|`Gt|`Leq|`Lt]
type constr = (relop * version) option

(** {6 CUDF spec. types} *)

type pkgname = string
type vpkg = pkgname * constr
type vpkglist = vpkg list
type vpkgformula = (* XXX does not enforce CNF, whereas the spec requires it *)
    FTrue
  | FPkg of vpkg
  | FOr of vpkgformula list
  | FAnd of vpkgformula list
type veqpkg = pkgname * ([`Eq] * version) option
type veqpkglist = veqpkg list
type enum_keep = [ `Keep_version | `Keep_package | `Keep_feature ]

(** {5 Parsers} *)

(** error while parsing the lexical representation of some type
    arguments:
    - type name
    - literal read *)
exception Parse_error of string * string

(** {6 Public types} *)

val parse_bool : string -> bool

val parse_pkgname : string -> pkgname
val parse_version : string -> version
val parse_vpkg : string -> vpkg
val parse_vpkglist : string -> vpkglist
val parse_vpkgformula : string -> vpkgformula
val parse_veqpkg : string -> veqpkg
val parse_veqpkglist : string -> veqpkglist

val parse_keep : string -> enum_keep

(** {6 Private (i.e., low-level) types} *)

val parse_relop : string -> relop

(** {5 Pretty printers} *)

(** {6 Pretty print to abstract formatters} *)

val pp_bool : Format.formatter -> bool -> unit
val pp_keep : Format.formatter -> enum_keep -> unit
val pp_pkgname : Format.formatter -> pkgname -> unit
val pp_version : Format.formatter -> version -> unit
val pp_vpkg : Format.formatter -> vpkg -> unit
val pp_vpkglist : Format.formatter -> vpkglist -> unit
val pp_vpkgformula : Format.formatter -> vpkgformula -> unit
val pp_veqpkg : Format.formatter -> veqpkg -> unit
val pp_veqpkglist : Format.formatter -> veqpkglist -> unit

(** {6 Pretty print to string}

    Shorthand functions. *)

val string_of_bool : bool -> string
val string_of_keep : enum_keep -> string
val string_of_pkgname : pkgname -> string
val string_of_version : version -> string
val string_of_vpkg : vpkg -> string
val string_of_vpkglist : vpkglist -> string
val string_of_vpkgformula : vpkgformula -> string
val string_of_veqpkg : veqpkg -> string
val string_of_veqpkglist : veqpkglist -> string
