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

open Printf

(* <type, literal> *)
exception Parse_error of string * string

(** Regexps *)

let space_RE = Pcre.regexp " "
let pkgname_RE = Pcre.regexp "^[a-z][a-z0-9.+-]+$"
let vconstr_REs = "(=|!=|>=|>|<=|<)\\s+(\\d+)"
let vpkg_RE = Pcre.regexp (sprintf "^([a-z][a-z0-9.-]+)(\\s+%s)?$" vconstr_REs)
let and_sep_RE = Pcre.regexp "\\s*,\\s*"
let or_sep_RE = Pcre.regexp "\\s*\\|\\s*"

(** Higher-order parsers *)

(** Given a parser as input, return a parser for "list of" what it
    usually parses.
    @param sep list items separator, a regexp *)
let list_parser ?(sep = space_RE) p s = List.map p (Pcre.split ~rex:sep s)

(** Parsers *)

let parse_bool = function
  | "true" -> true
  | "false" -> false
  | s -> raise (Parse_error ("bool", s))

let parse_pkgname s =
  if not (Pcre.pmatch ~rex:pkgname_RE s) then
    raise (Parse_error ("pkgname", s));
  s

let parse_version s =
  try
    int_of_string s
  with Failure _ -> raise (Parse_error ("version", s))

let parse_keep = function
  | "version" -> `Keep_version
  | "package" -> `Keep_package
  | "feature" -> `Keep_feature
  | s -> raise (Parse_error ("enum('version,'package,'feature)", s))

let parse_relop = function
  | "=" -> `Eq
  | "!=" -> `Neq
  | ">=" -> `Geq
  | ">" -> `Gt
  | "<=" -> `Leq
  | "<" -> `Lt
  | s -> raise (Parse_error ("relop", s))

let parse_vpkg s =
  try
    let subs = Pcre.extract ~rex:vpkg_RE s in
    let vconstr =
      match subs.(2) with
	| "" -> None
	| _ -> Some (parse_relop subs.(3), parse_version subs.(4))
    in
      (subs.(1), vconstr)
  with
      Not_found
    | Parse_error _ -> raise (Parse_error ("vpkg", s))

let parse_vpkglist = list_parser ~sep:and_sep_RE parse_vpkg
  
let parse_veqpkg s =
  match parse_vpkg s with
    | (_, None) as veqpkg -> veqpkg
    | (_, Some (`Eq, _)) as veqpkg -> veqpkg
    | _ -> raise (Parse_error ("veqpkg", s))

let parse_vpkgformula s =
  let and_args = Pcre.split ~rex:and_sep_RE s in
    Cudf.FAnd
      (List.map
	 (fun and_arg ->
	    let or_args = Pcre.split ~rex:or_sep_RE and_arg in
	      Cudf.FOr (List.map (fun s -> Cudf.FPkg (parse_vpkg s)) or_args))
	 and_args)
      
let parse_veqpkglist = list_parser ~sep:and_sep_RE parse_veqpkg

