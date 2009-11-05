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

open Printf

let file_arg = ref ""

let arg_spec = [
]

let usage_msg = "Usage: cudf-parse-822 FILE"

let die_usage () = Arg.usage arg_spec usage_msg ; exit (-2)

let pp_822 =
  let pp_stanza stanza =
    List.iter (fun (k, v) -> printf "%s: %s\n%!" k v) stanza;
    print_newline ()
  in
  List.iter pp_stanza

let pp_lpos { Lexing.pos_fname = _fname;
	      pos_lnum = lnum; pos_bol = bol; pos_cnum = cnum } =
  sprintf "%d:%d" lnum (cnum - bol)

let main () =
  Arg.parse arg_spec ((:=) file_arg) usage_msg;
  if !file_arg = "" then
    die_usage ();
  let ic = open_in !file_arg in
  let lexbuf = Lexing.from_channel ic in
  try
    let stanzas = Cudf_822_parser.main Cudf_822_lexer.token lexbuf in
    pp_822 stanzas
  with
    | Parsing.Parse_error ->
	failwith (sprintf "Parse error on file %s:%s--%s" !file_arg
		    (pp_lpos lexbuf.Lexing.lex_start_p)
		    (pp_lpos lexbuf.Lexing.lex_curr_p))
    | Cudf_822_lexer.Lexer_error (p1, p2) ->
	failwith (sprintf "Lexer error on file %s:%s--%s" !file_arg
		    (pp_lpos p1) (pp_lpos p2))
    

let _ = main ()
