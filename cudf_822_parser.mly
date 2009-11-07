/*****************************************************************************/
/*  libCUDF - CUDF (Common Upgrade Description Format) manipulation library  */
/*  Copyright (C) 2009  Stefano Zacchiroli <zack@pps.jussieu.fr>             */
/*                                                                           */
/*  This library is free software: you can redistribute it and/or modify     */
/*  it under the terms of the GNU Lesser General Public License as           */
/*  published by the Free Software Foundation, either version 3 of the       */
/*  License, or (at your option) any later version.  A special linking       */
/*  exception to the GNU Lesser General Public License applies to this       */
/*  library, see the COPYING file for more information.                      */
/*****************************************************************************/

/* RFC822-like parser: surface parser for CUDF stanzas

  This parser handle the surface syntax of CUDF documents: it recognizes
  RFC82 stanzas, folds together line continuations, and throws away comments
  and empty lines
*/

%token <string> CONT
%token <string * string> FIELD
%token EOL EOF
%type <(string * string) list list> doc_822
%type <(string * string) list option> stanza_822
%start doc_822 stanza_822

%%

doc_822:
  | stanzas 		{ $1 }
  | eols stanzas	{ $2 }
;

stanza_822:
  | stanza	{ Some $1 }
  | eols stanza	{ Some $2 }
  | eols EOF	{ None }
  | EOF		{ None }
;

eols:
  | EOL		{}
  | EOL eols	{}
;

stanzas:
  | 			{ [] }
  | stanza EOF		{ [ $1 ] }
  | stanza eols stanzas	{ $1 :: $3 }
;

stanza:
  | fields	{ $1 }
;

fields:
  | field		{ [ $1 ] }
  | field fields	{ $1 :: $2 }
;

field:
  | FIELD EOL		{ $1 }
  | FIELD EOL linecont	{ let k, v = $1 in
			  k, v ^ $3 }
;

linecont:
  | CONT EOL		{ $1 }
  | CONT EOL linecont	{ $1 ^ $3 }
;

%%

let error_wrapper f =
  fun lexer lexbuf ->
    try
      f lexer lexbuf
    with Parsing.Parse_error ->
      raise (Cudf_types.Parse_error_822 (lexbuf.Lexing.lex_start_p,
					 lexbuf.Lexing.lex_curr_p))

let doc_822 = error_wrapper doc_822
let stanza_822 = error_wrapper stanza_822
