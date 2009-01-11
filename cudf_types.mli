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

open Cudf

(** CUDF type library

    Implement parsing and pretty printing of core CUDF types (see CUDF
    spec. §3.2.2) *)

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

open Format

val pp_pkgname : formatter -> pkgname -> unit
val pp_version : formatter -> version -> unit
val pp_vpkg : formatter -> vpkg -> unit
val pp_vpkglist : formatter -> vpkglist -> unit
val pp_vpkgformula : formatter -> vpkgformula -> unit
val pp_veqpkg : formatter -> veqpkg -> unit
val pp_veqpkglist : formatter -> veqpkglist -> unit
