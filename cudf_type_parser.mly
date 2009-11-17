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

/* CUDF type parser: parse values belonging to CUDF types.

   Used as the basic building block to parse CUDF stanzas retuned by
   Cudf_822_paser. Generally, this parser does not need to parse multi-line
   values (as they are all normalized to single-line values by
   Cudf_822_parser.)
*/

%{

(** a non-located parse error carrying an error message (...) *)
exception Parse_error_msg of string

let parse_relop = function
  | "="  -> `Eq
  | "!=" -> `Neq
  | ">=" -> `Geq
  | ">"  -> `Gt
  | "<=" -> `Leq
  | "<"  -> `Lt
  | _ -> assert false	(* lexer shouldn't have returned such a RELOP! *)

(** parse a type declaration with no default value *)
let parse_typename = function
  | "int"        -> `Int
  | "posint"     -> `Posint
  | "nat"        -> `Nat
  | "bool"       -> `Bool
  | "string"     -> `String
  | "pkgname"    -> `Pkgname
  | "ident"      -> `Ident
  | "vpkg"       -> `Vpkg
  | "vpkformula" -> `Vpkgformula
  | "vpkglist"   -> `Vpkglist
  | "veqpkg"     -> `Veqpkg
  | "veqpkglist" -> `Veqpkglist
  | s            -> raise (Parse_error_msg ("unknown type name: " ^ s))

%}

%token <string> IDENT PKGNAME QSTRING RELOP
%token <int> POSINT NEGINT
%token LBRACKET RBRACKET LPAREN RPAREN
%token COMMA PIPE COLON EQ
%token VPKGTRUE VPKGFALSE
%token EOL
%type <int> int
%type <string> ident
%type <string> qstring
%type <Cudf_types.pkgname> pkgname
%type <Cudf_types.vpkg> vpkg
%type <Cudf_types.vpkglist> vpkglist
%type <Cudf_types.vpkgformula> vpkgformula
%type <Cudf_types.typedecl> typedecl
%start int ident qstring pkgname vpkg vpkglist vpkgformula typedecl

%%

pkgname: PKGNAME { $1 } ;
ident: IDENT { $1 } ;
qstring: QSTRING { $1 } ;
version: POSINT { $1 } ;

relop:
  | RELOP	{ parse_relop $1 }
  | EQ		{ `Eq }
;

int:
  | POSINT	{ $1 }
  | NEGINT	{ $1 }
;

vpkg:
  | PKGNAME			{ ($1, None) }
  | PKGNAME relop version	{ ($1, Some ($2, $3)) }
;
vpkglist:
  |			{ [] }
  | vpkg vpkglist	{ $1 :: $2 }
;

vpkgformula:
  | and_formula		{ $1 }
  | VPKGTRUE		{ [] }
  | VPKGFALSE		{ [ [] ] }
;

and_formula:
  | or_formula				{ [ $1 ] }
  | or_formula COMMA and_formula	{ $1 :: $3 }
;

or_formula:
  | vpkg			{ [ $1 ] }
  | vpkg PIPE or_formula	{ $1 :: $3 }
;

typedecl:
  |				{ [] }
  | typedecl_ COMMA typedecl	{ $1 :: $3 }
;

typedecl_:
  | ident COLON typename		{ ($1, Cudf_types.typedecl_of_type $3) }
  | ident COLON typename
      EQ LBRACKET typed_value RBRACKET	{ let name, typ, v = $1, $3, $6 in
					  (name,
					   Cudf_types.typedecl_of_value
					     (Cudf_types.cast typ v)) }
;

typename:
  | ident			{ parse_typename $1 }
  | ident LPAREN enums RPAREN	{ `Enum $3 }
;

enums:
  | ident		{ [ $1 ] }
  | ident COMMA enums	{ $1 :: $3 }
;

typed_value:
  | ident		{ `Ident $1 }
  | POSINT		{ `Int $1 }
  | NEGINT		{ `Int $1 }
  | qstring		{ `String $1 }
  | vpkgformula		{ `Vpkgformula $1 }
;

%%

let error_wrapper f =
  fun lexer lexbuf ->
    try
      f lexer lexbuf
    with
      | Parsing.Parse_error ->
	  raise (Cudf_types.Syntax_error
		   ("", lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p))
      | Parse_error_msg msg ->
	  raise (Cudf_types.Syntax_error
		   (msg, lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p))

let int = error_wrapper int
let ident = error_wrapper ident
let pkgname = error_wrapper pkgname
let vpkg = error_wrapper vpkg
let vpkglist = error_wrapper vpkglist
let vpkgformula = error_wrapper vpkgformula
let typedecl = error_wrapper typedecl
let qstring = error_wrapper qstring
