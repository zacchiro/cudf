(*****************************************************************************)
(*  libCUDF - CUDF (Common Upgrade Description Format) manipulation library  *)
(*  Copyright (C) 2009  Stefano Zacchiroli <zack@pps.jussieu.fr>             *)
(*                                                                           *)
(*  This program is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU General Public License as published by     *)
(*  the Free Software Foundation, either version 3 of the License, or (at    *)
(*  your option) any later version.                                          *)
(*                                                                           *)
(*  This program is distributed in the hope that it will be useful, but      *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of               *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU        *)
(*  General Public License for more details.                                 *)
(*                                                                           *)
(*  You should have received a copy of the GNU General Public License        *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
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

val parse_pkgname : string -> string
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

