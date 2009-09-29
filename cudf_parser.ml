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

(* INVARIANT: lines are always kept lstrip-ed (see [lstrip] below) *)
type cudf_parser = {
  lines : string Enum.t ; (* TODO: to be converted to a _real_ parser *)
  mutable pos: int ;  (** last line read; start with 0, 1st line read is 1 *)
}

exception Parse_error of int * string

let parse_error i msg = 
  Printf.eprintf "Parse error at line %d : %s\n" i msg;
  raise (Parse_error (i, msg))

let blank_RE = Pcre.regexp "^\\s*$"
let prop_RE = Pcre.regexp "(^[a-zA-Z][a-zA-Z0-9-]*): (.*)$"

(* strip all lines up to the first non-blank line *)
let rec lstrip p =
  match Enum.get p.lines with
    | None -> ()  (* empty enum, nothing to strip *)
    | Some line when Pcre.pmatch ~rex:blank_RE line ->
        p.pos <- p.pos + 1;
        lstrip p
    | Some line -> Enum.push p.lines line (* non-blank line, rollback *)

let from_in_channel ic =
  let p = { lines = input_lines ic ; pos = 1 } in
    lstrip p;
    p
let close p = ()

let starts_with sw s =
  let sl = String.length s in
  let swl = String.length sw in
  sl >= swl && String.sub s 0 swl = sw

(* XXX problem TO be removed *)
let is_postmark s = 
  starts_with "package" s ||
  starts_with "problem" s ||
  starts_with "request" s

(* XXX: non tail-recursive *)
let parse_paragraph ch =
  let rec aux acc ?(start = false) p =
    match Enum.get ch.lines with
    (* RFC822-style line continuations *)
    |Some line when Pcre.pmatch ~rex:(Pcre.regexp "^\\s+(.+)$") line ->
        begin match acc with
        |(n,_,_)::_ ->
            begin
              ch.pos <- ch.pos + 1;
              let subs = Pcre.extract ~rex:(Pcre.regexp "^\\s+(.+)$") line in
              let prop = (n, subs.(1), ch.pos) in
              aux (prop::acc) ch
            end
        |_ -> parse_error ch.pos ("Error parsing paragraph : invalid property line :" ^ line)
        end
    |Some line ->
        (try
          let subs = Pcre.extract ~rex:prop_RE line in
          let name = String.lowercase subs.(1) in
          let prop = (name , subs.(2), ch.pos) in
          if (is_postmark name) && not(start) then begin
            (* beginning of next stanza, rollback *)
            Enum.push ch.lines line;
            acc
          end
          else begin
            ch.pos <- ch.pos + 1;
            aux (prop::acc) ch
          end
        with Not_found ->  (* not a valid property line *)
          if not (Pcre.pmatch ~rex:blank_RE line) then
            parse_error ch.pos "Error parsing paragraph : invalid property line";
          lstrip ch;
          acc
        )
    |None -> acc
  in
  match aux [] ~start:true ch with
  |[] -> None
  |stanza -> Some stanza

let parse_stanza_package preamble par = 
  let rec aux_package pkg = function
    |("package", s, _) :: tl ->
        aux_package { pkg with package = parse_pkgname s } tl
    |("version", s, _) :: tl ->
        aux_package { pkg with version = parse_version s } tl
    |("depends", s, _) :: tl ->
        aux_package { pkg with depends = parse_vpkgformula s } tl
    |("conflicts", s, _) :: tl ->
        aux_package { pkg with conflicts = parse_vpkglist s } tl
    |("provides", s, _) :: tl ->
        aux_package { pkg with provides = parse_veqpkglist s } tl
    |("installed" , s, _) :: tl ->
        aux_package { pkg with installed = parse_bool s } tl
    |("keep" , s, _) :: tl ->
        aux_package { pkg with keep = Some (parse_keep s) } tl
    |(name, s, i) :: tl ->
        begin try
          let (typeid, _) = List.assoc name preamble in
          let p = (name, Cudf_types.parse_basetype typeid s) in
          aux_package { pkg with extra = p :: pkg.extra } tl
        with Not_found ->
          parse_error i 
          (sprintf "Error parsing preamble: unexpected property '%s' in package description item" name)
        end
    |[] -> pkg
  in
  `Package (aux_package default_package par)

(* XXX problem TO be removed *)
let parse_stanza_request par =
  let rec aux_request req = function
    |(("problem" | "request"), s, _) :: tl ->
        aux_request { req with problem_id = s } tl
    |("install", s, _) :: tl ->
        aux_request { req with install = parse_vpkglist s } tl
    |("remove", s, _) :: tl ->
        aux_request { req with remove = parse_vpkglist s } tl
    |("upgrade", s, _) :: tl ->
        aux_request { req with upgrade = parse_vpkglist s } tl
    |(name, _, i) :: tl ->
        parse_error i
        (sprintf "Error parsing request : unexpected property '%s' in problem description item" name)
    |[] -> req
  in
	`Request (aux_request default_request par)

let parse_stanza_preample par =
  let rec aux_request acc = function
    |("property", s, i) :: tl ->
        (try
          let l = Cudf_types.parse_typedecls s in
          aux_request (acc @ l) tl
        with Cudf_types.Parse_error (msg,s) ->
          parse_error i (Printf.sprintf "%s : %s" msg s)
        )
    |[] -> acc
    | _ :: tl -> aux_request acc tl
  in
  aux_request [] par

let has_package = List.exists (fun (p,_,_) -> p = "package")
(* XXX problem TO be removed *)
let has_request = List.exists (fun (p,_,_) -> (p = "request") || (p = "problem") )
let has_property = List.exists (fun (p,_,_) -> p = "property")

let parse_stanza preamble par =
  if has_package par then
    parse_stanza_package preamble par
  else if has_request par then
    parse_stanza_request par
  else
    match par with
    |(name,v,i)::_ -> parse_error i ("Error parsing paragraph : invalid stanza line : "^ name)
    |[] -> assert false (* unreachable *) 

(* we read the first paragraph. if it has a property declaration, we 
 * give back a parser. If it is not a property stanza, then we give back
 * the first paragraph to be parsed *)
let parse ch =
  let preamble, firstpar =
    match parse_paragraph ch with
    |Some par when has_property par -> (parse_stanza_preample par, [])
    |Some par -> ([], par)
    |None -> parse_error 0 "Error parsing file : empty file"
  in
  let packages = ref [] in
  let request = ref None in
  let has_preable = ref (firstpar = []) in
  while
    match
      if !has_preable then parse_paragraph ch
      else (has_preable := true ; Some firstpar)
    with
    |None -> false
    |Some paragraph -> begin
        (match parse_stanza preamble paragraph with
        |(`Package e) -> packages := e :: !packages
        |(`Request e) -> request := Some (e));
        true end 
  do () done ;
  (preamble,!packages,!request)

let load cudf_parser =
  let pre, pkgs, req = parse cudf_parser in
  (pre, load_universe pkgs, req)

let parser_wrapper fname f =
  let ic = open_in fname in
  let p = from_in_channel ic in
  finally (fun () -> close_in ic ; close p) f p

let parse_from_file fname = parser_wrapper fname parse
let load_from_file fname = parser_wrapper fname load
