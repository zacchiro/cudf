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

open Cudf
open Cudf_types

type cudf_parser = {
  lexbuf: Lexing.lexbuf;
  mutable typedecl: Cudf_conf.stanza_typedecl;
}

type 'ty stanza = (string * 'ty) list

exception Parse_error of int * string

let parse_error i msg = 
  Printf.eprintf "Parse error at line %d: %s\n" i msg;
  raise (Parse_error (i, msg))

let parse_error_e msg = function
  | Parse_error_822 (startpos, endpos) ->
      parse_error startpos.Lexing.pos_lnum msg
  | _ -> assert false

let from_in_channel ?(typedecl=Cudf_conf.stanza_typedecl) ic =
  { lexbuf = Lexing.from_channel ic ;
    typedecl = typedecl ;
  }

let close p = ()

let parse_stanza p =
  try
    (match Cudf_822_parser.stanza_822 Cudf_822_lexer.token_822 p.lexbuf with
      | Some stanza -> stanza
      | None -> raise End_of_file)
  with Parse_error_822 _ as exn -> parse_error_e "" exn

let type_check_stanza stanza types =
  List.map
    (fun (k, v) ->
       try
	 let decl = List.assoc k types in
	 let typed_v = Cudf_types_pp.parse_value (type_of_typedecl decl) v in
	 k, typed_v
       with Not_found ->
	 parse_error ~-1 (sprintf "unexpected property \"%s\" in this stanza" k))
    stanza

(** cast a typed stanza starting with "preamble: " to a {!Cudf.preamble} *)
let bless_preamble stanza =
  let p = default_preamble in	(* assumption: should be completely overrode *)
  let rec aux p = function
    | ("preamble", `String v) :: tl -> aux { p with preamble_id = v } tl
    | ("property", `Typedecl v) :: tl -> aux { p with property = v } tl
    | ("univ-checksum", `String v) :: tl -> aux { p with univ_checksum = v } tl
    | ("status-checksum", `String v) :: tl -> aux { p with status_checksum = v } tl
    | ("req-checksum", `String v) :: tl -> aux { p with req_checksum = v } tl
    | [] -> p
    | _ -> assert false
  in
  aux p stanza

(** Cast a typed stanza starting with "package: " to a {!Cudf.package}.
    ASSUMPTION: type checking of the stanza has already happend, in particular
    all extra properties have already been checked for allowance. *)
let bless_package stanza =
  let p = default_package in	(* assumption: should be completely overrode *)
  let rec aux p = function
    | ("package", `Pkgname v) :: tl -> aux { p with package = v } tl
    | ("version", `Posint v) :: tl -> aux { p with version = v } tl
    | ("depends", `Vpkgformula v) :: tl -> aux { p with depends = v } tl
    | ("conflicts", `Vpkglist v) :: tl -> aux { p with conflicts = v } tl
    | ("provides", `Veqpkglist v) :: tl -> aux { p with provides = v } tl
    | ("installed", `Bool v) :: tl -> aux { p with installed = v } tl
    | ("was-installed", `Bool v) :: tl -> aux { p with was_installed = v } tl
    | ("keep", `Enum (_, v)) :: tl ->
	aux { p with keep = Cudf_types_pp.parse_keep v } tl
    | (k, (v: typed_value)) :: tl ->
	aux { p with pkg_extra = (k, v) :: p.pkg_extra } tl
    | [] -> p
  in
  let p' = aux p stanza in
  { p' with pkg_extra = List.rev p'.pkg_extra }

(** Cast a typed stanza starting with "request: " to a {!Cudf.request}.
    ASSUMPTION: as per {!Cudf_parser.bless_package} above. *)
let bless_request stanza =
  let r = default_request in	(* assumption: should be completely overrode *)
  let rec aux r = function
    | ("request", `String v) :: tl -> aux { r with request_id = v } tl
    | ("install", `Vpkglist v) :: tl -> aux { r with install = v } tl
    | ("remove", `Vpkglist v) :: tl -> aux { r with remove = v } tl
    | ("upgrade", `Vpkglist v) :: tl -> aux { r with upgrade = v } tl
    | (k, (v: typed_value)) :: tl ->
	aux { r with req_extra = (k, v) :: r.req_extra } tl
    | [] -> r
  in
  let r' = aux r stanza in
  { r' with req_extra = List.rev r'.req_extra }

let parse_item p =
  let stanza = parse_stanza p in
  let typed_stanza =
    match stanza with
      | [] -> parse_error ~-1 "empty stanza"
      | (postmark, _) :: _ ->
	  (try
	     type_check_stanza stanza (List.assoc postmark p.typedecl)
	   with Not_found ->
	     parse_error ~-1
	       (sprintf "Unknown stanza type, starting with \"%s\" postmark."
		  postmark)) in
  match typed_stanza with
    | [] -> assert false
    | ("preamble", _) :: _ ->
	let preamble = bless_preamble typed_stanza in
	p.typedecl <-	(* update type declaration for "package" stanza *)
	  (let pkg_typedecl =
	     (List.assoc "package" p.typedecl) @ preamble.property in
	   ("package", pkg_typedecl) :: List.remove_assoc "package" p.typedecl);
	`Preamble preamble
    | ("package", _) :: _ -> `Package (bless_package typed_stanza)
    | ("request", _) :: _ -> `Request (bless_request typed_stanza)
    | _ -> assert false

let parse p =
  let pre, pkgs, req = ref None, ref [], ref None in
  let rec aux_pkg () =
    match parse_item p with
      | `Package pkg -> pkgs := pkg :: !pkgs ; aux_pkg ()
      | `Request req' -> req := Some req'	(* stop recursion after req *)
      | `Preamble _ -> parse_error ~-1 "late preamble"
  in
  let parse () =
    try
      (match parse_item p with	(* parse first item *)
	 | `Preamble pre' ->
	     pre := Some pre' ;
	     (try aux_pkg () with End_of_file -> ())
	 | `Package pkg ->
	     pkgs := [pkg] ;
	     (try aux_pkg () with End_of_file -> ())
	 | `Request req' -> req := Some req')
    with End_of_file -> parse_error ~-1 "empty CUDF"
  in
  parse () ;
  (try	(* check for forbidden trailing content *)
     ignore (parse_item p);
     parse_error ~-1 "trailing stanzas after final request stanza"
   with End_of_file -> ());
  (!pre, !pkgs, !req)
  

let load p =
  let pre, pkgs, req = parse p in
  (pre, load_universe pkgs, req)

let parser_wrapper ?typedecl fname f =
  let ic = open_in fname in
  let p = from_in_channel ?typedecl ic in
  finally (fun () -> close_in ic ; close p) f p

let parse_from_file ?typedecl fname = parser_wrapper fname parse
let load_from_file ?typedecl fname = parser_wrapper fname load
