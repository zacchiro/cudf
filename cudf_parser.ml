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
  mutable types: Cudf_conf.stanza_types;
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

let from_in_channel ic = { lexbuf = Lexing.from_channel ic; }
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
	 let ty = List.assoc k types in
	 
       with Not_found ->
	 parse_error ~-1 (sprintf "unexpected property \"%s\" in this stanza" k)
    )
    stanza

(* let parse_item p = *)
(*   let stanza = parse_stanza p in *)
(*   match stanza with *)
(*     | [] -> assert false *)
(*     | "preamble", _ :: tl -> *)
(*     | "request", _ :: tl -> *)
(*     | "package", _ :: tl -> *)

(*
(* we read the first paragraph. if it has a property declaration, we 
 * give back a parser. If it is not a property stanza, then we give back
 * the first paragraph to be parsed *)
let parse ch =
  let preamble, firstpar =
    match parse_paragraph ch with
    |Some par when has_preamble par -> (parse_stanza_preample par, [])
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
*)

(*
let load cudf_parser =
  let pre, pkgs, req = parse cudf_parser in
  (pre, load_universe pkgs, req)
*)

let parser_wrapper fname f =
  let ic = open_in fname in
  let p = from_in_channel ic in
  finally (fun () -> close_in ic ; close p) f p

(* let parse_from_file fname = parser_wrapper fname parse *)
(* let load_from_file fname = parser_wrapper fname load *)
