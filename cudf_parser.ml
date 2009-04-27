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

exception Parse_error of int * string

(* INVARIANT: lines are always kept lstrip-ed (see [lstrip] below) *)
type cudf_parser = {
  lines : string Enum.t ;	(* TODO: to be converted to a _real_ parser *)
  mutable pos: int ;	(** last line read; start with 0, 1st line read is 1 *)
}

let parse_error p msg = raise (Parse_error (p.pos, msg))

let blank_RE = Pcre.regexp "^\\s*$"
let prop_RE = Pcre.regexp "(^[a-zA-Z][a-zA-Z0-9-]*): (.*)$"

(* strip all lines up to the first non-blank line *)
let rec lstrip p =
  match Enum.get p.lines with
    | None -> ()	(* empty enum, nothing to strip *)
    | Some line when Pcre.pmatch ~rex:blank_RE line ->
        p.pos <- p.pos + 1;
        lstrip p
    | Some line -> Enum.push p.lines line	(* non-blank line, rollback *)

let from_in_channel ic =
  let p = { lines = input_lines ic ; pos = 0 } in
    lstrip p;
    p
let close p = ()

(* XXX: non tail-recursive *)
let parse_stanza p =
  let rec aux ?(start = false) p =
    match Enum.get p.lines with
      | Some line ->
	  (try
	     let subs = Pcre.extract ~rex:prop_RE line in
	     let prop = subs.(1), subs.(2) in
	       (match prop with
		 | "Package", _
		 | "Problem", _ when not start ->
		     (* beginning of next stanza, rollback *)
		     Enum.push p.lines line;
		     []
		 | _ ->
		     p.pos <- p.pos + 1;
		     prop :: aux p)
	   with Not_found ->	(* not a valid property line *)
	     if not (Pcre.pmatch ~rex:blank_RE line) then
	       parse_error p "invalid property line";
	     lstrip p;
	     [])
      | None -> []
  in
    match aux ~start:true p with
      | [] -> raise End_of_file
      | stanza -> stanza
	
let parse_item p =
  let stanza = parse_stanza p in
  let rec aux_package pkg = function
    | ("Version", s) :: tl ->
	aux_package { pkg with version = parse_version s } tl
    | ("Depends", s) :: tl ->
	aux_package { pkg with depends = parse_vpkgformula s } tl
    | ("Conflicts", s) :: tl ->
	aux_package { pkg with conflicts = parse_vpkglist s } tl
    | ("Provides", s) :: tl ->
	aux_package { pkg with provides = parse_veqpkglist s } tl
    | ("Installed" , s) :: tl ->
	aux_package { pkg with installed = parse_bool s } tl
    | ("Keep" , s) :: tl ->
	aux_package { pkg with keep = Some (parse_keep s) } tl
    | (k,v) :: tl ->
	aux_package { pkg with extra = (k,`Unparsed v) :: pkg.extra } tl
    | [] -> pkg
  in
  let rec aux_request req = function
    | ("Install", s) :: tl ->
	aux_request { req with install = parse_vpkglist s } tl
    | ("Remove", s) :: tl ->
	aux_request { req with remove = parse_vpkglist s } tl
    | ("Upgrade", s) :: tl ->
	aux_request { req with upgrade = parse_vpkglist s } tl
    | (name, _) :: tl ->
	parse_error p
	  (sprintf "unexpected property '%s' in problem description item" name);
    | [] -> req
   in
    try
      (match stanza with
	| [] -> parse_error p "empty file stanza"
	| ("Package", n) :: tl ->
	    `Package
	      (aux_package { default_package with package = parse_pkgname n }
		 tl)
	| ("Problem", id) :: tl ->
	    `Request (aux_request { default_request with problem_id = id } tl)
	| (prop_name, _) :: _ ->
	    parse_error p
	      (sprintf "unexpected stanza starting with postmark '%s'"
		 prop_name))
    with Cudf_types.Parse_error _ as exn ->
      parse_error p (sprintf "error while parsing a CUDF type: %s"
		       (Printexc.to_string exn))

let parse_items p =
  let items = ref [] in
    try
      while true do
	items := parse_item p :: !items
      done;
      assert false	(* unreachable *)
    with End_of_file -> List.rev !items

let parse p =
  let pkg_items, req_items =
    List.partition (function `Package _ -> true | _ -> false) (parse_items p)
  in
  let pkgs =
    List.map (function `Package pkg -> pkg | _ -> assert false) pkg_items
  in
    match req_items with
      | [`Request req] -> pkgs, Some req
      | [] -> pkgs, None
      | _ -> parse_error p "too many problem description items (1 expected)"

let load p =
  let pkgs, req = parse p in
    Cudf.load_universe pkgs, req

let parser_wrapper fname f =
  let ic = open_in fname in
  let p = from_in_channel ic in
    finally (fun () -> close_in ic ; close p) f p

let parse_from_file fname = parser_wrapper fname parse
let load_from_file fname = parser_wrapper fname load
