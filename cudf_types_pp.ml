(*****************************************************************************)
(*  libCUDF - CUDF (Common Upgrade Description Format) manipulation library  *)
(*  Copyright (C) 2009-2010  Stefano Zacchiroli <zack@pps.jussieu.fr>        *)
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

open Cudf_types

(* note: Type_error <> Cudf_types.Type_error, this one is not located *)
exception Type_error of typ * typed_value

let lexbuf_wrapper type_parser typ =
  fun s ->
    try
      type_parser Cudf_type_lexer.token_cudf (Lexing.from_string s)
    with Cudf_types.Syntax_error (_msg, loc) ->
      raise (Type_error (typ, `String s))

let lexbuf_wrapper' type_parser =
  fun s ->
    type_parser Cudf_type_lexer.token_cudf (Lexing.from_string s)

let parse_int = lexbuf_wrapper Cudf_type_parser.int_top `Int
let parse_ident = lexbuf_wrapper Cudf_type_parser.ident_top `Ident
let parse_pkgname = lexbuf_wrapper Cudf_type_parser.pkgname_top `Pkgname
let parse_vpkg = lexbuf_wrapper Cudf_type_parser.vpkg_top `Vpkg
let parse_vpkglist = lexbuf_wrapper Cudf_type_parser.vpkglist_top `Vpkglist
let parse_vpkgformula =
  lexbuf_wrapper Cudf_type_parser.vpkgformula_top `Vpkgformula
let parse_typedecl = lexbuf_wrapper Cudf_type_parser.typedecl_top `Typedecl

let parse_qstring = lexbuf_wrapper' Cudf_type_parser.qstring_top
let parse_type = lexbuf_wrapper' Cudf_type_parser.type_top


(** DEFCON 4, use with care!

    Rationale: to avoid duplicating code we have the cast checks enclosed only
    in the [cast] function. After having used it however, we will have to
    extract the contained typed value. To avoid writing several functions
    extracting the appropriate value and [assert false] everywhere else we
    cheat with [Obj.magic].
*)
let unbox v = snd (Obj.magic v: 'a * 'b)

let cast' typ v =
  try
    cast typ v
  with Cudf_types.Type_error _ -> raise (Type_error (typ, v))

let parse_posint s: int = unbox (cast' `Posint (`Int (parse_int s)))
let parse_nat s: int = unbox (cast' `Nat (`Int (parse_int s)))
let parse_bool s: bool = unbox (cast' `Bool (`Ident (parse_ident s)))
let parse_veqpkg s: veqpkg = unbox (cast' `Veqpkg (`Vpkg (parse_vpkg s)))
let parse_veqpkglist s: veqpkglist =
  unbox (cast' `Veqpkglist (`Vpkglist (parse_vpkglist s)))

let parse_enum ~enums s =
  match cast' (`Enum enums) (`Ident (parse_ident s)) with
    | `Enum (_, i) -> i
    | _ -> assert false

let parse_keep = function
  | "version" -> `Keep_version
  | "feature" -> `Keep_feature
  | "package" -> `Keep_package
  | "none" -> `Keep_none
  | i -> raise (Type_error (Cudf_types.keep_type, `Ident i))

let parse_string s =
  let type_error () = raise (Type_error (`String, `String s)) in
  (try ignore (String.index s '\n') ; type_error () with Not_found -> ());
  (try ignore (String.index s '\r') ; type_error () with Not_found -> ());
  s

let parse_value ty s =
  match ty with
    | `Int -> `Int (parse_int s)
    | `Posint -> `Posint (parse_posint s)
    | `Nat -> `Nat (parse_nat s)
    | `Bool -> `Bool (parse_bool s)
    | `String -> `String (parse_string s)
    | `Enum l -> `Enum (l, parse_enum l s)
    | `Pkgname -> `Pkgname (parse_pkgname s)
    | `Ident -> `Ident (parse_ident s)
    | `Vpkg -> `Vpkg (parse_vpkg s)
    | `Vpkglist -> `Vpkglist (parse_vpkglist s)
    | `Vpkgformula -> `Vpkgformula (parse_vpkgformula s)
    | `Veqpkg -> `Veqpkg (parse_veqpkg s)
    | `Veqpkglist -> `Veqpkglist (parse_veqpkglist s)
    | `Typedecl -> `Typedecl (parse_typedecl s)

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
  | `Keep_none -> Format.fprintf fmt "none"

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
let rec pp_vpkgformula fmt = function
  | [] -> Format.fprintf fmt "true!"
  | [ [] ] -> Format.fprintf fmt "false!"
  | [] :: _ ->
      eprintf "malformed vpkgformula: `[] :: _' ; aborting\n%!";
      assert false
  | fmla ->
      let pp_or fmt = pp_list fmt ~pp_item:pp_vpkg ~sep:" | " in
      let pp_and fmt = pp_list fmt ~pp_item:pp_or ~sep:" , " in
      pp_and fmt fmla

let pp_veqpkglist = pp_vpkglist
let pp_veqpkg = pp_vpkg

let pp_type fmt = function
  | `Int -> Format.fprintf fmt "int"
  | `Posint -> Format.fprintf fmt "posint"
  | `Nat -> Format.fprintf fmt "nat"
  | `Bool -> Format.fprintf fmt "bool"
  | `String -> Format.fprintf fmt "string"
  | `Enum enums -> Format.fprintf fmt "enum(%s)" (String.concat "," enums)
  | `Pkgname -> Format.fprintf fmt "pkgname"
  | `Ident -> Format.fprintf fmt "ident"
  | `Vpkg -> Format.fprintf fmt "vpkg"
  | `Vpkgformula -> Format.fprintf fmt "vpkgformula"
  | `Vpkglist -> Format.fprintf fmt "vpkglist"
  | `Veqpkg -> Format.fprintf fmt "veqpkg"
  | `Veqpkglist -> Format.fprintf fmt "veqpkglist"
  | `Typedecl -> Format.fprintf fmt "typedecl"

let rec pp_typedecl' fmt (name, decl1) =
  let string_escape =
    String.replace_chars
      (function '"' -> "\\\"" | '\\' -> "\\\\" | c -> String.of_char c) in
  match value_of_typedecl decl1 with
    | None -> Format.fprintf fmt "%s: %a" name pp_type (type_of_typedecl decl1)
    | Some (`String s) ->
        Format.fprintf fmt "%s: %a = [\"%s\"]"
	  name pp_type `String (string_escape s)
    | Some v ->
        Format.fprintf fmt "%s: %a = [%a]"
	  name pp_type (type_of_typedecl decl1) pp_value v

and pp_value fmt (v: typed_value) = match v with
  | (`Int i | `Posint i | `Nat i) -> pp_int fmt i
  | `Bool b -> pp_bool fmt b
  | (`String s | `Pkgname s | `Ident s | `Enum (_, s)) -> pp_string fmt s
  | `Vpkg p -> pp_vpkg fmt p
  | `Veqpkg p -> pp_vpkg fmt p
  | `Vpkglist l -> pp_vpkglist fmt l
  | `Veqpkglist l -> pp_vpkglist fmt l
  | `Vpkgformula f -> pp_vpkgformula fmt f
  | `Typedecl d -> pp_typedecl fmt d

and pp_typedecl fmt = pp_list fmt ~pp_item:pp_typedecl' ~sep:", "

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
let string_of_type = string_of pp_type
let string_of_value = string_of pp_value
let string_of_typedecl = string_of pp_typedecl

(*
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
*)
