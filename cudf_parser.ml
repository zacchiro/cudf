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

type cudf_parser =
  { mutable ic : in_channel;
    mutable cur : string;
    mutable lineno : int;
    mutable eof : bool }

let eof i = i.eof

let cur i =
  assert (not i.eof);
  i.cur

exception Parse_error of int * string

let parse_error i msg = raise (Parse_error (i, msg))

let next i =
  assert (not i.eof);
  try
    i.cur <- input_line i.ic ;
    i.lineno <- i.lineno + 1
  with End_of_file ->
    i.eof <- true

let reset i =
  seek_in i.ic 0 ;
  i.lineno <- 0 ;
  i.eof <- false
  
let expect s v = assert (not (eof s) && cur s = v); next s

let is_blank i = not (eof i) && cur i = ""

let skip_blank_lines i =
  while is_blank i do next i done

let field_re = Str.regexp "^\\([^:]*\\)*:[ \t]*\\(.*\\)$"

let remove_ws s =
  let l = String.length s in
  let p = ref (l - 1) in
  while !p >= 0 && (s.[!p] = ' ' || s.[!p] = '\t') do decr p done;
  if !p + 1 = l then s else
  String.sub s 0 (!p + 1)

(* return None with EOF *)
let parse_paragraph i =
  skip_blank_lines i;
  if eof i then None else begin
    let fields = ref [] in
    while
      let l = cur i in
      if not (Str.string_match field_re l 0) then
        parse_error i.lineno ("Malformed field :"^l);
      let name = Str.matched_group 1 l in
      let data1 = remove_ws (Str.matched_group 2 l) in
      let data = ref [data1] in
      next i;
      while
        not (eof i || is_blank i) &&
        let l = cur i in l.[0] = ' ' || l.[0] = '\t'
      do
        data := remove_ws (cur i) :: !data;
        next i
      done;
      fields := (String.lowercase name, List.rev !data, i.lineno - 1) :: !fields;
      not (eof i || is_blank i)
    do () done;
    assert (!fields <> []) ;
    Some (List.rev !fields)
  end

let single_line = function
  |[s] -> s
  | _ as l ->
      failwith (
        Printf.sprintf "field '' should be a single line\n%s"
        (String.concat " " l)
      )

let token_re =
  Str.regexp
    ("[ \t]+\\|\\(" ^
     String.concat "\\|"
       [","; "|"; "("; ")"; "<<"; "<="; "="; ">="; ">>"; "<"; ">";
        "[A-Za-z0-9.:_+~-]+"] ^
     "\\)")

let rec next_token s p =
  if !p = String.length s then raise End_of_file else
  if Str.string_match token_re s !p then begin
    p := Str.match_end ();
    try
      Str.matched_group 1 s
    with Not_found ->
      next_token s p
  end else
    failwith (Format.sprintf "Bad token in '%s' at %d" s !p)

let from_in_channel ic =
  let res = { ic = ic; cur = ""; lineno = 0 ; eof = false }
  in next res ; res

let close p = ()

exception Eof

let parse_822_iter parse ch =
  let p = ref [] in
  let r = ref None in
  try
    while true do
      match parse_paragraph ch with
      |None -> raise Eof
      |Some par ->
          match parse par with
          |(`Package e) -> p := e :: !p
          |(`Request e) -> r := Some (e)
    done ;
    (!p,!r)
  with Eof -> (!p,!r)

let parse_stanza_package extra_parser par = 
  let rec aux_package pkg = function
    |("package", s, _) :: tl ->
        aux_package { pkg with package = parse_pkgname (single_line s) } tl
    |("version", s, _) :: tl ->
        aux_package { pkg with version = parse_version (single_line s) } tl
    |("depends", s, _) :: tl ->
        aux_package { pkg with depends = parse_vpkgformula (single_line s) } tl
    |("conflicts", s, _) :: tl ->
        aux_package { pkg with conflicts = parse_vpkglist (single_line s) } tl
    |("provides", s, _) :: tl ->
        aux_package { pkg with provides = parse_veqpkglist (single_line s) } tl
    |("installed" , s, _) :: tl ->
        aux_package { pkg with installed = parse_bool (single_line s) } tl
    |("keep" , s, _) :: tl ->
        aux_package { pkg with keep = Some (parse_keep (single_line s)) } tl
    |((name, s, i) as prop) :: tl ->
        let (pparser, default) = extra_parser prop in
        let p = (name, pparser (single_line s)) in
        aux_package { pkg with extra = p :: pkg.extra } tl
    |[] -> pkg
  in
  `Package (aux_package default_package par)

let parse_stanza_problem par =
  let rec aux_request req = function
    |("problem", s, _) :: tl ->
        aux_request { req with problem_id = single_line s } tl
    |("install", s, _) :: tl ->
        aux_request { req with install = parse_vpkglist (single_line s) } tl
    |("remove", s, _) :: tl ->
        aux_request { req with remove = parse_vpkglist (single_line s) } tl
    |("upgrade", s, _) :: tl ->
        aux_request { req with upgrade = parse_vpkglist (single_line s) } tl
    |(name, _, i) :: tl ->
        parse_error i
        (sprintf "unexpected property '%s' in problem description item" name)
    |[] -> req
  in
	`Request (aux_request default_request par)

let parse_stanza_preample par =
  let rec aux_request acc = function
    |("property", s, i) :: tl ->
        (try
          let l = Cudf_types.parse_typedecls (single_line s) in
          aux_request (acc @ l) tl
        with Cudf_types.Parse_error (msg,_) -> parse_error i msg)
    |[] -> acc
    | _ :: tl -> aux_request acc tl
  in
  let pl = aux_request [] par in
  function (name, _, i) ->
    try List.assoc name pl 
    with Not_found ->
      parse_error i 
      (sprintf "unexpected property '%s' in package description item" name)

let parse_stanza extra_parser par =
  if List.exists (fun (p,_,_) -> p = "package") par then
    parse_stanza_package extra_parser par
  else if List.exists (fun (p,_,_) -> p = "problem") par then
    parse_stanza_problem par
  else
    match par with
    |(name,v,i)::_ -> parse_error i "invalid stanza line"
    |[] -> parse_error 0 "invalid stanza line"

let parse cudf_parser =
  let extra_parser, r =
    let default_parser = 
      (fun (name, _, i) -> parse_error i ("unexpected property "^name))
    in
    match parse_paragraph cudf_parser with
    |Some par when List.exists (fun (p,_,_) -> p = "property") par -> 
        (parse_stanza_preample par, false)
    |_ -> (default_parser, true)
  in
  let parse_aux = parse_822_iter (parse_stanza extra_parser) in
  if r = true then reset cudf_parser ;
  parse_aux cudf_parser

let load cudf_parser =
  let pkgs, req = parse cudf_parser in
  (Cudf.load_universe pkgs, req)

let parser_wrapper fname f =
  let ic = open_in fname in
  let p = from_in_channel ic in
  finally (fun () -> close_in ic ; close p) f p

let parse_from_file fname = parser_wrapper fname parse
let load_from_file fname = parser_wrapper fname load
