
open ExtLib

(* INVARIANT: lines are always kept lstrip-ed (see [lstrip] below) *)
type cudf_parser = {
  lines : string Enum.t ;	(* TODO: to be converted to a _real_ parser *)
  mutable pos: int ;	(** last line read; start with 0, 1st line read is 1 *)
}

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

let rec parse_stanza p =
  match Enum.get p.lines with
    | Some line ->
	(try
	  let subs = Pcre.extract ~rex:prop_RE line in
	    p.pos <- p.pos + 1;
	    (subs.(1), subs.(2)) :: parse_stanza p
	with Not_found -> lstrip p; [])
    | None -> []
	    
let parse_item p =
  failwith "Not implemented: Cudf_parser.parse_item"

let parse_cudf p =
  failwith "Not implemented: Cudf_parser.parse_cudf"

let parse_packages p =
  failwith "Not implemented: Cudf_parser.parse_packages"

