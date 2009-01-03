
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
let rec parse_stanza p =
  match Enum.get p.lines with
    | Some line ->
	(try
	  let subs = Pcre.extract ~rex:prop_RE line in
	    p.pos <- p.pos + 1;
	    (subs.(1), subs.(2)) :: parse_stanza p
	with Not_found ->	(* not a valid property line *)
	  if not (Pcre.pmatch ~rex:blank_RE line) then
	    parse_error p "invalid property line";
	  lstrip p;
	  [])
    | None -> []
	    
let dummy_package = {	(** implement package defaults *)
  package = "" ;
  version = 0 ;
  depends = FTrue ;
  conflicts = [] ;
  provides = [] ;
  installed = false ;
  keep = None ;
  extra = [] ;
}

let dummy_request = {	(** implement request defaults *)
  problem_id = "" ;
  install = [] ;
  remove = [] ;
  upgrade = [] ;
}

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
    | prop :: tl ->
	aux_package { pkg with extra = prop :: pkg.extra } tl
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
    match stanza with
      | [] -> parse_error p "empty file stanza"
      | ("Package", name) :: tl ->
	  `Package (aux_package { dummy_package with package = name } tl)
      | ("Problem", id) :: tl ->
	  `Request (aux_request { dummy_request with problem_id = id } tl)
      | (prop_name, _) :: _ ->
	  parse_error p
	    (sprintf "unexpected stanza starting with postmark '%s'" prop_name)

let parse_cudf p =
  failwith "Not implemented: Cudf_parser.parse_cudf"

let parse_packages p =
  failwith "Not implemented: Cudf_parser.parse_packages"

