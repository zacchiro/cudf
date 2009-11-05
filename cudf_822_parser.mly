/* RFC822-like parser: surface parser for CUDF stanzas

  This parser handle the surface syntax of CUDF documents: it recognizes
  RFC82 stanzas, folds together line continuations, and throws away comments
  and empty lines
*/

%token <string> CONT
%token <string * string> FIELD
%token EOL EOF
%type <(string * string) list list> main
%start main

%%

main:
  | stanzas 		{ $1 }
  | eols stanzas	{ $2 }
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
