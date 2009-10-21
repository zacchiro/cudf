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
type enum_keep = [ `Keep_version | `Keep_package | `Keep_feature ]

type basetype = [
  |`Int of int
  |`PosInt of int
  |`Nat of int
  |`Bool of bool
  |`String of string
  |`Enum of (string list * string)
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
let pkgname_STR = "[A-Za-z0-9%.+-\\@]"
let space_RE = Pcre.regexp "\\s+"
let pkgname_RE = Pcre.regexp (sprintf "^%s+$" pkgname_STR)
let vconstr_REs = "(=|!=|>=|>|<=|<)\\s+(\\d+)"
let vpkg_RE = Pcre.regexp (sprintf "^(%s+)(\\s+%s)?$" pkgname_STR vconstr_REs)
let and_sep_RE = Pcre.regexp "\\s*,\\s*"
let or_sep_RE = Pcre.regexp "\\s*\\|\\s*"
let semicol_sep_RE = Pcre.regexp "\\s*;\\s*"
let colon_sep_RE = Pcre.regexp "\\s*:\\s*"
let eq_sep_RE = Pcre.regexp "\\s*=\\s*"
let quote_RE = Pcre.regexp "\"(.*)\""

let encode s =
  let escape_string s =
    let make_hex chr = Printf.sprintf "%%%x" (Char.code chr) in
    let allowed_RE = Pcre.regexp pkgname_STR in
    let n = String.length s in
    let b = Buffer.create n in
    for i = 0 to n-1 do
      let s' = String.of_char s.[i] in
      if not(Pcre.pmatch ~rex:allowed_RE s') then
        Buffer.add_string b (make_hex s.[i])
      else
        Buffer.add_string b s'
    done;
    Buffer.contents b
  in
  if Pcre.pmatch ~rex:pkgname_RE s then s
  else escape_string s

let rec decode s =
  let hex_re = Pcre.regexp "%[0-9a-f][0-9a-f]" in
  let un s =
    let hex = String.sub s 1 2 in
    let n = int_of_string ("0x" ^ hex) in
    String.make 1 (Char.chr n)
  in
  Pcre.substitute ~rex:hex_re ~subst:un s

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

let parse_enum l s =
  try
    if List.mem s l then s
    else raise (Parse_error ("Error parsing enum : Unknown Value in enum", s))
  with Not_found -> raise (Parse_error ("Error parsing enum : Quotes needed", s))

let parse_basetype (t : basetype) s = match t with
  |`Int _ -> `Int (parse_int s)
  |`PosInt _ -> `PosInt (parse_posint s)
  |`Nat _ -> `Nat (parse_nat s)
  |`Bool _ -> `Bool (parse_bool s)
  |`String _ -> `String s
  |`Vpkg _ -> `Vpkg (parse_vpkg s)
  |`Vpkglist _ -> `Vpkglist (parse_vpkglist s)
  |`Vpkgformula _ -> `Vpkgformula (parse_vpkgformula s)
  |`Veqpkg _ -> `Veqpkg (parse_veqpkg s)
  |`Veqpkglist _ -> `Veqpkglist (parse_veqpkglist s)
  |`Enum (l,_) -> `Enum (l, parse_enum l s)

let reserved_properties s =
  String.starts_with s "is-installed" ||
  String.starts_with s "was-installed"

open Genlex

let rec parse_list ?(q=[]) p = 
  try parser
    | [< e = p ; nxt >] -> parse_list ~q:(e::q) p nxt
    | [<>] -> List.rev q
  with Stream.Error s ->
    raise (Parse_error ("Error parsing base type : Unknown type", s))
let rec parse_list_sep sep ?(q=[]) p = 
  try parser
    | [< e = p ; nxt >] -> opt_sep sep (e::q) p nxt
    | [<>] -> List.rev q
  with Stream.Error s ->
    raise (Parse_error ("Error parsing base type : Unknown type", s))
and opt_sep sep q p =
  try parser
    | [< 'Kwd s when s = sep ; nxt >] -> parse_list_sep sep ~q p nxt
    | [<>] -> List.rev q
  with Stream.Error s ->
    raise (Parse_error ("Error parsing base type : Unknown type", s))

let parse_typedecls s =
  let lex = Genlex.make_lexer [ 
    "(" ; ")"; "="; ","; ":"; 
    "enum"; "int"; "posint"; "nat"; "bool"; "string"; 
    "vpkg"; "vpkglist"; "vpkgformula"; "veqpkg"; "veqpkglist"
    ] 
  in
  let parse_string = 
    try parser [< 'String s >] -> s 
    with Stream.Error s ->
      raise (Parse_error ("Error parsing base type : Value problem", s))
  in
  let parse_default =
    try parser [< 'Kwd "="; s = parse_string >] -> s
    with Stream.Error s ->
      raise (Parse_error ("Error parsing base type : Default type problem", s))
  in
  let parse_type = 
    try parser
    | [< 'Kwd "int" ; s = parse_default >] -> `Int (parse_int s)
    | [< 'Kwd "posint" ; s = parse_default >] -> `PosInt (parse_posint s)
    | [< 'Kwd "nat" ; s = parse_default >] -> `Nat (parse_nat s)
    | [< 'Kwd "bool" ; s = parse_default >] -> `Bool (parse_bool s)
    | [< 'Kwd "string" ; s = parse_default >] -> `String (s)
    | [< 'Kwd "vpkg" ; s = parse_default >] -> `Vpkg (parse_vpkg s)
    | [< 'Kwd "vpkglist" ; s = parse_default >] -> `Vpkglist (parse_vpkglist s)
    | [< 'Kwd "vpkgformula" ; s = parse_default >] -> `Vpkgformula (parse_vpkgformula s)
    | [< 'Kwd "veqpkg" ; s = parse_default >] -> `Veqpkg (parse_veqpkg s)
    | [< 'Kwd "veqpkglist" ; s = parse_default >] -> `Veqpkglist (parse_veqpkglist s)
    | [< 'Kwd "enum" ; 'Kwd "(" ; l = parse_list_sep "," parse_string ; 'Kwd ")" ; 
          s = parse_default >] -> `Enum (l, parse_enum l s)
    | [< 'Kwd str >] -> raise (Stream.Error str)
    with Stream.Error s ->
      raise (Parse_error ("Error parsing base type : Type problem", s))
  in
  let parse_stm = 
    try parser [< 'Ident id ; 'Kwd ":"; stm = parse_type >] -> (id,stm) 
    with Stream.Error s ->
      raise (Parse_error ("Error parsing base type : Statement problem", s))
  in
  let top = 
    try parse_list_sep "," parse_stm
    with Stream.Error s ->
      raise (Parse_error ("Error parsing base type : List problem", s))
  in 
  try top (lex(Stream.of_string s))
  with Stream.Error s ->
    raise (Parse_error ("Error parsing base type : Lexing problem", s))

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

let dump_typedecl fmt = function
  |`Int i -> pp_int fmt i; "int"
  |`PosInt i -> pp_int fmt i; "posint"
  |`Nat i -> pp_int fmt i; "nat"
  |`Bool b -> pp_bool fmt b; "bool"
  |`String s -> pp_string fmt s; "string"
  |`Enum (_,s) -> pp_string fmt s; "enum"
  |`Vpkg x -> pp_vpkg fmt x; "vpkg"
  |`Vpkgformula x -> pp_vpkgformula fmt x; "vpkgformula"
  |`Vpkglist x -> pp_vpkglist fmt x ; "vpkglist"
  |`Veqpkg x -> pp_veqpkg fmt x ; "veqpkg"
  |`Veqpkglist x -> pp_veqpkglist fmt x ; "veqpkglist"

let pp_basetype fmt t = ignore(dump_typedecl fmt t)

let buf = Buffer.create 1024
let buf_formatter =
  let fmt = Format.formatter_of_buffer buf in
    Format.pp_set_margin fmt max_int;
    fmt

let string_of pp arg =
  Buffer.clear buf;
  ignore(pp buf_formatter arg);
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
(* XXX not really nice ... *)
let string_of_typedecl b =
  Buffer.clear buf;
  let t = dump_typedecl buf_formatter b in
  Format.pp_print_flush buf_formatter ();
  (t,Buffer.contents buf)
