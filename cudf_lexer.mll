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

{
  open Cudf_types
  open Cudf_822_parser
}

let lower_letter = [ 'a' - 'z' ]
let upper_letter = [ 'A' - 'Z' ]
let letter = lower_letter | upper_letter
let digit = [ '0' - '9' ]
let blank = [ ' ' '\t' ]
let blanks = blank+
let ident = lower_letter (lower_letter | digit | '-')*
let int = '-' digit+
let pkgname = (letter | digit | ['-' '+' '.' '/' '@' '(' ')' '%'])+

rule token_822 = parse
  | (ident as field) ':' ' '
    ([^'\n']* as rest)		{ FIELD(field, rest) }
  | ' ' ([^'\n']* as rest)	{ CONT(rest) }
  | '#' [^'\n']*		{ token_822 lexbuf }
  | blank* '\n'			{ Lexing.new_line lexbuf;
				  EOL }
  | eof				{ EOF }
  | _				{ raise (Parse_error_822
					   (lexbuf.Lexing.lex_start_p,
					    lexbuf.Lexing.lex_curr_p)) }
