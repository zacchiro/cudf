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

open ExtLib
open Printf

type version = int
type relop = [`Eq|`Neq|`Geq|`Gt|`Leq|`Lt]
type constr = (relop * version) option

type pkgname = string
type vpkg = pkgname * constr
type vpkglist = vpkg list
type vpkgformula = vpkg list list
type veqpkg = pkgname * ([`Eq] * version) option
type veqpkglist = veqpkg list
type enum_keep = [`Keep_version | `Keep_package | `Keep_feature ]

type typ =
    [ `Int | `Posint | `Nat | `Bool | `String | `Enum of string list
    | `Pkgname | `Ident
    | `Vpkg | `Vpkgformula | `Vpkglist | `Veqpkg | `Veqpkglist ]
type typedecl1 =
    [ `Int of int option | `Posint of int option | `Nat of int option
    | `Bool of bool option | `String of string option
    | `Pkgname of string option | `Ident of string option
    | `Enum of (string list * string option) | `Vpkg of vpkg option
    | `Vpkgformula of vpkgformula option | `Vpkglist of vpkglist option
    | `Veqpkg of veqpkg option | `Veqpkglist of veqpkglist option ]
type typed_value = typedecl1
type typedecl = (string * typedecl1) list

let type_of_typedecl = function
  | `Int _ -> `Int
  | `Posint _ -> `Posint
  | `Nat _ -> `Nat
  | `Bool _ -> `Bool
  | `String _ -> `String
  | `Pkgname _ -> `Pkgname
  | `Ident _ -> `Ident
  | `Enum (enums, _) -> `Enum enums
  | `Vpkg _ -> `Vpkg
  | `Vpkgformula _ -> `Vpkgformula
  | `Vpkglist _ -> `Vpkglist
  | `Veqpkg _ -> `Veqpkg
  | `Veqpkglist _ -> `Veqpkglist

exception Type_error of typ * typed_value	(* <type, literal> *)
exception Parse_error_822 of Lexing.position * Lexing.position
exception Parse_error_typelib of string * Lexing.position * Lexing.position

