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

open Cudf_types

(* let parse_value ty s = *)
(*   match ty with *)
(*     | `Int -> `Int (parse_int s) *)
(*     | `Posint -> `Posint (parse_posint s) *)
(*     | `Nat -> `Nat (parse_nat s) *)
(*     | `Bool -> `Bool (parse_bool s) *)
(*     | `String -> `String s *)
(*     | `Enum l -> `Enum (l, parse_enum l s) *)
(*     | `Pkgname -> `Pkgname (parse_pkgname s) *)
(*     | `Ident -> `Ident (parse_ident s) *)
(*     | `Vpkg -> `Vpkg (parse_vpkg s) *)
(*     | `Vpkglist -> `Vpkglist (parse_vpkglist s) *)
(*     | `Vpkgformula -> `Vpkgformula (parse_vpkgformula s) *)
(*     | `Veqpkg -> `Veqpkg (parse_veqpkg s) *)
(*     | `Veqpkglist -> `Veqpkglist (parse_veqpkglist s) *)

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

let pp_typ fmt = function
  | `Int -> Format.fprintf fmt "int"
  | `PosInt -> Format.fprintf fmt "posint"
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

let dump_typedecl fmt = function
  |`Int i -> pp_int fmt i; "int"
  |`Posint i -> pp_int fmt i; "posint"
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
let string_of_typ = string_of pp_typ

(* TODO XXX not really nice ... *)
let string_of_typedecl b =
  Buffer.clear buf;
  let t = dump_typedecl buf_formatter b in
  Format.pp_print_flush buf_formatter ();
  (t,Buffer.contents buf)

(** Misc stuff *)

(* let encode s = *)
(*   let escape_string s = *)
(*     let make_hex chr = Printf.sprintf "%%%x" (Char.code chr) in *)
(*     let allowed_RE = Pcre.regexp pkgname_STR in *)
(*     let n = String.length s in *)
(*     let b = Buffer.create n in *)
(*     for i = 0 to n-1 do *)
(*       let s' = String.of_char s.[i] in *)
(*       if not(Pcre.pmatch ~rex:allowed_RE s') then *)
(*         Buffer.add_string b (make_hex s.[i]) *)
(*       else *)
(*         Buffer.add_string b s' *)
(*     done; *)
(*     Buffer.contents b *)
(*   in *)
(*   if Pcre.pmatch ~rex:pkgname_RE s then s *)
(*   else escape_string s *)

(* let rec decode s = *)
(*   let hex_re = Pcre.regexp "%[0-9a-f][0-9a-f]" in *)
(*   let un s = *)
(*     let hex = String.sub s 1 2 in *)
(*     let n = int_of_string ("0x" ^ hex) in *)
(*     String.make 1 (Char.chr n) *)
(*   in *)
(*   Pcre.substitute ~rex:hex_re ~subst:un s *)
