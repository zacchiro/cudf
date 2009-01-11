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

open Printf

open Cudf

(* <type, literal> *)
exception Parse_error of string * string

(** Regexps *)

let space_RE = Pcre.regexp " "
let pkgname_RE = Pcre.regexp "^[a-z][a-z0-9.+-]+$"
let vconstr_REs = "(=|!=|>=|>|<=|<)\\s+(\\d+)"
let vpkg_RE =
  Pcre.regexp (sprintf "^([a-z][a-z0-9.+-]+)(\\s+%s)?$" vconstr_REs)
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

(** Pretty printers *)

let pp_pkgname fmt name = Format.fprintf fmt "%s" name
let pp_version fmt ver = Format.fprintf fmt "%d" ver

let string_of_relop = function
    `Eq -> "="
  | `Neq -> "!="
  | `Geq -> ">="
  | `Gt -> ">"
  | `Leq -> "<="
  | `Lt -> "<"

let pp_vpkg fmt = function
    (name, None) -> pp_pkgname fmt name
  | (name, Some (relop, v)) ->
      Format.fprintf fmt "%a %s %a"
	pp_pkgname name (string_of_relop relop) pp_version v

let pp_list fmt ~pp_item ~sep l =
  let rec aux fmt = function
      [] -> assert false
    | [last] -> (* last item, no trailing sep *)
	Format.fprintf fmt "@,%a" pp_item last
    | vpkg :: tl -> (* at least one package in tl *)
	Format.fprintf fmt "@,%a%s" pp_item vpkg sep ;
	aux fmt tl
  in
    match l with
      | [] -> ()
      | [sole] -> pp_item fmt sole
      | _ -> Format.fprintf fmt "@[<hv>%a@]" aux l

let pp_vpkglist fmt = pp_list fmt ~pp_item:pp_vpkg ~sep:" , "

(** ASSUMPTION: formula is in CNF *)
let rec pp_vpkgformula fmt = function
    FTrue -> ()
  | FPkg vpkg -> pp_vpkg fmt vpkg
  | FOr []
  | FAnd [] -> assert false
  | FOr [vpkg] -> pp_vpkgformula fmt vpkg
  | FOr vpkgs -> pp_list fmt ~pp_item:pp_vpkgformula ~sep:" | " vpkgs
  | FAnd [fmla] -> pp_vpkgformula fmt fmla
  | FAnd fmlas -> pp_list fmt ~pp_item:pp_vpkgformula ~sep:" , " fmlas

let pp_veqpkglist = pp_vpkglist
let pp_veqpkg = pp_vpkg

let buf = Buffer.create 1024
let buf_formatter = Format.formatter_of_buffer buf

let string_of pp arg =
  Buffer.clear buf;
  pp buf_formatter arg;
  Format.pp_print_flush buf_formatter ();
  Buffer.contents buf

let string_of_pkgname = string_of pp_pkgname
let string_of_version = string_of pp_version
let string_of_vpkg = string_of pp_vpkg
let string_of_vpkglist = string_of pp_vpkglist
let string_of_vpkgformula = string_of pp_vpkgformula
let string_of_veqpkg = string_of pp_veqpkg
let string_of_veqpkglist = string_of pp_veqpkglist
