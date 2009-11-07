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

    Implement core CUDF types (see CUDF spec. §2.2.2).

    For parsing and pretty printing of CUDF types see {!Cudf_types_pp}
*)

(** {5 CUDF types} *)

type version = int	(* required to be non 0 *)
type relop = [`Eq|`Neq|`Geq|`Gt|`Leq|`Lt]
type constr = (relop * version) option

(** {6 CUDF spec. types} *)

type pkgname = string
type vpkg = pkgname * constr
type vpkglist = vpkg list
type enum_keep = [`Keep_version | `Keep_package | `Keep_feature ]

(** CNF formula. Inner lists are OR-ed, outer AND-ed.
    E.g.:
    - "Depends: foo, baz | baz"		-->	[ [ foo ] ; [ bar ; baz ] ]
*)
type vpkgformula = vpkg list list

type veqpkg = pkgname * ([`Eq] * version) option
type veqpkglist = veqpkg list

(** CUDF types *)
type typ =
    [ `Int | `Posint | `Nat | `Bool | `String | `Enum of string list
    | `Pkgname | `Ident
    | `Vpkg | `Vpkgformula | `Vpkglist | `Veqpkg | `Veqpkglist ]

(** (Single) type declaration: each variant denotes a type, its argument the
    default value, None if missing *)
type typedecl1 =
    [ `Int of int option
    | `Posint of int option
    | `Nat of int option
    | `Bool of bool option
    | `String of string option
    | `Pkgname of string option
    | `Ident of string option
    | `Enum of (string list * string option)	(** enums, default enum *)
    | `Vpkg of vpkg option
    | `Vpkgformula of vpkgformula option
    | `Vpkglist of vpkglist option
    | `Veqpkg of veqpkg option
    | `Veqpkglist of veqpkglist option
    ]
type typed_value = typedecl1
type typedecl = (string * typedecl1) list

val type_of_typedecl : typedecl1 -> typ

(** {5 Various errors} *)

(** type error: mismatch between typed value and expected type

    arguments: expected type, found value *)
exception Type_error of typ * typed_value

(** Error while parsing RFC822-like syntax of CUDF documents.
    Arguments: start and end position of the error, respectively. *)
exception Parse_error_822 of Lexing.position * Lexing.position

exception Parse_error_typelib of string * Lexing.position * Lexing.position

