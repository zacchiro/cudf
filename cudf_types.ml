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

type version = int
type relop = [`Eq|`Neq|`Geq|`Gt|`Leq|`Lt]
type constr = (relop * version) option

type pkgname = string
type vpkg = pkgname * constr
type vpkglist = vpkg list
type vpkgformula = vpkg list list
type veqpkg = pkgname * ([`Eq] * version) option
type veqpkglist = veqpkg list
type enum_keep = [ `Keep_version | `Keep_package | `Keep_feature ]

type basetype = [
  |`Int of int
  |`PostInt of int
  |`Nat of int
  |`Bool of bool
  |`String of string
  |`Enum of string
  |`Vpkg of vpkg
  |`Vpkgformula of vpkgformula
  |`Vpkglist of vpkglist
  |`Veqpkg of veqpkg
  |`Veqpkglist of veqpkglist
]

(* <type, literal> *)
exception Parse_error of string * string

(* TODO : uniform lexical naming conventions for identifiers (package names,
 * property names, enumeration values) *)

(** Regexps *)
let pkgname_STR = "[a-z0-9%.+-]+"
let space_RE = Pcre.regexp "\\s+"
let pkgname_RE = Pcre.regexp (sprintf "^%s$" pkgname_STR)
let vconstr_REs = "(=|!=|>=|>|<=|<)\\s+(\\d+)"
let vpkg_RE = Pcre.regexp (sprintf "^(%s)(\\s+%s)?$" pkgname_STR vconstr_REs)
let and_sep_RE = Pcre.regexp "\\s*,\\s*"
let or_sep_RE = Pcre.regexp "\\s*\\|\\s*"
let semicol_sep_RE = Pcre.regexp "\\s*;\\s*"
let colon_sep_RE = Pcre.regexp "\\s*:\\s*"
let eq_sep_RE = Pcre.regexp "\\s*=\\s*"
let enum_RE =  Pcre.regexp "enum\\s*\\(\\s*([^)]+)\\s*\\)"
let quote_RE = Pcre.regexp "\"(.*)\""

(** Higher-order parsers *)

(** Given a parser as input, return a parser for "list of" what it
    usually parses.
    @param sep list items separator, a regexp *)
let list_parser ?(sep = space_RE) p s = List.map p (Pcre.split ~rex:sep s)

(** Parsers *)

let parse_int s =
  try int_of_string s
  with Failure("int_of_string") -> raise (Parse_error ("Error parsing type int", s))

let parse_posint s = match int_of_string s with
  |i when i > 0 -> i
  |i -> raise (Parse_error ("posint", s))

let parse_nat s = match int_of_string s with
  |i when i >= 0 -> i
  |i -> raise (Parse_error ("nat", s))

let parse_bool = function
  | "true" -> true
  | "false" -> false
  | s -> raise (Parse_error ("Error parsing type bool", s))

let parse_pkgname s =
  if not (Pcre.pmatch ~rex:pkgname_RE s) then
    raise (Parse_error ("Error parsing type package name", s));
  s

let parse_version s =
  try parse_posint s with
  |Failure _ -> raise (Parse_error ("Error parsing version", s))
  |Parse_error ("posint",_) -> raise (Parse_error ("version must be posint", s))

let parse_keep = function
  | "version" -> `Keep_version
  | "package" -> `Keep_package
  | "feature" -> `Keep_feature
  | s -> raise (Parse_error ("Error parsing keep", s))

let parse_relop = function
  | "=" -> `Eq
  | "!=" -> `Neq
  | ">=" -> `Geq
  | ">" -> `Gt
  | "<=" -> `Leq
  | "<" -> `Lt
  | s -> raise (Parse_error ("Error parsing version operator", s))

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
  | Not_found
  | Parse_error _ -> raise (Parse_error ("Error parsing type vpkg", s))

let parse_vpkglist = list_parser ~sep:and_sep_RE parse_vpkg
  
let parse_veqpkg s =
  match parse_vpkg s with
    | (_, None) as veqpkg -> veqpkg
    | (_, Some (`Eq, _)) as veqpkg -> veqpkg
    | _ -> raise (Parse_error ("Error parsing type veqpkg", s))

let parse_vpkgformula s =
  let and_args = Pcre.split ~rex:and_sep_RE s in
    List.map
      (fun and_arg ->
	 let or_args = Pcre.split ~rex:or_sep_RE and_arg in
	   List.map parse_vpkg or_args)
      and_args
      
let parse_veqpkglist = list_parser ~sep:and_sep_RE parse_veqpkg

let remove_quotes s = let subs = Pcre.extract ~rex:quote_RE s in subs.(1)

let parse_enum l s =
  try 
    if List.mem s (List.map remove_quotes l) then s 
    else raise (Parse_error ("Error parsing enum : Unknown Value in enum", s))
  with Not_found -> raise (Parse_error ("Error parsing enum : Quotes needed", s))

let parse_default s =
  try remove_quotes s
  with Not_found -> raise (Parse_error ("Error parsing default value : Quotes needed", s))

let parse_basetype t s = match t with
  |"int" -> `Int (parse_int s)
  |"posint" -> `PostInt (parse_posint s)
  |"nat" -> `Nat (parse_nat s)
  |"bool" -> `Bool (parse_bool s)
  |"string" -> `String s
  |"vpkg" -> `Vpkg (parse_vpkg s)
  |"vpkglist" -> `Vpkglist (parse_vpkglist s)
  |"vpkgformula" -> `Vpkgformula (parse_vpkgformula s)
  |"veqpkg" -> `Veqpkg (parse_veqpkg s)
  |"veqpkglist" -> `Veqpkglist (parse_veqpkglist s)
  |str when Pcre.pmatch ~rex:enum_RE str -> (* enum *)
        let subs = Pcre.extract ~rex:enum_RE str in
        let l = Pcre.split ~rex:semicol_sep_RE (subs.(1)) in
        `Enum (parse_enum l s)
  |str -> raise (Parse_error ("Error parsing base type : Unknown type", str))

let parse_type s =
  match Pcre.split ~rex:eq_sep_RE s with
  |[typeid;default] -> (typeid, parse_basetype typeid (parse_default default))
  |_ -> raise (Parse_error ("Error parsing base type : No default value", s))

let starts_with sw s =
  let sl = String.length s in
  let swl = String.length sw in
  sl >= swl && String.sub s 0 swl = sw

let reserved_properties s =
  starts_with "is-installed" s ||
  starts_with "was-installed" s

let parse_type_schema s =
  match Pcre.split ~rex:colon_sep_RE s with
  |[ident;str] when not(reserved_properties ident) -> (ident, parse_type str)
  |_ -> raise (Parse_error ("Error parsing property : Wrong separator", s))

let parse_typedecls = list_parser ~sep:and_sep_RE parse_type_schema

(** Pretty printers *)

let pp_int fmt i =  Format.fprintf fmt "%d" i
let pp_posint = pp_int
let pp_nat = pp_int

let pp_bool fmt = function
    true -> Format.fprintf fmt "true"
  | false -> Format.fprintf fmt "false"

let pp_string fmt = Format.fprintf fmt "%s"

let pp_keep fmt = function
    `Keep_version -> Format.fprintf fmt "version"
  | `Keep_package -> Format.fprintf fmt "package"
  | `Keep_feature -> Format.fprintf fmt "feature"

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
    | [] -> assert false
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
let rec pp_vpkgformula =
  let pp_or fmt = pp_list fmt ~pp_item:pp_vpkg ~sep:" | " in
  let pp_and fmt = pp_list fmt ~pp_item:pp_or ~sep:" , " in
  pp_and

let pp_veqpkglist = pp_vpkglist
let pp_veqpkg = pp_vpkg

let pp_basetype fmt = function
  |`Int i |`PostInt i |`Nat i -> pp_int fmt i
  |`Bool b -> pp_bool fmt b
  |`String s -> pp_string fmt s
  |`Enum s -> pp_string fmt s
  |`Vpkg x -> pp_vpkg fmt x
  |`Vpkgformula x -> pp_vpkgformula fmt x
  |`Vpkglist x -> pp_vpkglist fmt x
  |`Veqpkg x -> pp_veqpkg fmt x
  |`Veqpkglist x -> pp_veqpkglist fmt x

let buf = Buffer.create 1024
let buf_formatter =
  let fmt = Format.formatter_of_buffer buf in
    Format.pp_set_margin fmt max_int;
    fmt

let string_of pp arg =
  Buffer.clear buf;
  pp buf_formatter arg;
  Format.pp_print_flush buf_formatter ();
  Buffer.contents buf

let string_of_int = string_of pp_int
let string_of_posint = string_of pp_int
let string_of_nat = string_of pp_int
let string_of_bool = string_of pp_bool
let string_of_keep = string_of pp_keep
let string_of_pkgname = string_of pp_pkgname
let string_of_version = string_of pp_version
let string_of_vpkg = string_of pp_vpkg
let string_of_vpkglist = string_of pp_vpkglist
let string_of_vpkgformula = string_of pp_vpkgformula
let string_of_veqpkg = string_of pp_veqpkg
let string_of_veqpkglist = string_of pp_veqpkglist
let string_of_basetype = string_of pp_basetype
