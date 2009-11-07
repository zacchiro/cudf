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
  open Cudf_type_parser
}

let lower_letter = [ 'a' - 'z' ]
let upper_letter = [ 'A' - 'Z' ]
let letter = lower_letter | upper_letter
let digit = [ '0' - '9' ]
let blank = [ ' ' '\t' ]
let blanks = blank+
let ident = lower_letter (lower_letter | digit | '-')*
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

and token_cudf = parse
  | ident as s		{ IDENT s }
  | pkgname as s	{ PKGNAME s }
  | digit+ as s		{ POSINT (int_of_string s) }
  | '-' digit+ as s	{ NEGINT (- (int_of_string s)) }
  | (">=" | "<=") as op	{ RELOP op }
  | "!=" as op		{ RELOP op }
  | ('>' | '<') as op	{ RELOP op }
  | '['			{ LBRACKET }
  | ']'			{ RBRACKET }
  | '('			{ LPAREN }
  | ')'			{ RPAREN }
  | ','			{ COMMA }
  | '|'			{ PIPE }
  | ':'			{ COLON }
  | '='			{ EQ }
  | '"'			{ let buf = Buffer.create 11 in
			  qstring buf lexbuf;
			  QSTRING (Buffer.contents buf) }
  | blank+		{ token_cudf lexbuf }
  | eof			{ EOL } (* single-line parsing: EOF means in fact EOL *)

and qstring buf = parse
  | "\\\""				{ Buffer.add_string buf "\""; qstring buf }
  | "\\\\"				{ Buffer.add_string buf "\\"; qstring buf }
  | '"'					{ () }
  | [^ '\n' '\r' '\\' '"']+ as s	{ Buffer.add_string buf s; qstring buf }
  | _					{ raise (Parse_error_822
						   (lexbuf.Lexing.lex_start_p,
						    lexbuf.Lexing.lex_curr_p)) }
