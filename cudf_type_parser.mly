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
let parse_ty_nodefault = function
  | "int"        -> `Int None
  | "posint"     -> `Posint None
  | "nat"        -> `Nat None
  | "bool"       -> `Bool None
  | "string"     -> `String None
  | "pkgname"    -> `Pkgname None
  | "ident"      -> `Ident None
  | "vpkg"       -> `Vpkg None
  | "vpkformula" -> `Vpkgformula None
  | "vpkglist"   -> `Vpkglist None
  | "veqpkg"     -> `Veqpkg None
  | "veqpkglist" -> `Veqpkglist None
  | s            -> raise (Parse_error_msg ("unknown type: " ^ s))

(** check whether a formula uses only equality tests over package versions *)
let rec is_eq_formula f =
  not (List.exists
	 (fun vpkgs ->
	    List.exists
	      (function
		 | (_, Some ((`Neq | `Geq | `Gt | `Leq | `Lt), _)) -> true
		 | _ -> false)
	      vpkgs)
	 f)

(** Set a default value in a type declaration that have no default value, take
    care of CUDF sub-typing. *)
let rec set_default v ty =
  let type_error () =
    raise (Cudf_types.Type_error (Cudf_types.type_of_typedecl ty, v)) in
  let tyval =
    match ty, v with
      | `Int _, `Int n -> `Int (Some n)
      | `Posint _, `Int n when n > 0 -> `Posint (Some n)
      | `Nat _, `Int n when n >= 0 -> `Nat (Some n)
      | `Bool _, `Ident "true" -> `Bool (Some true)
      | `Bool _, `Ident "false" -> `Bool (Some false)
      | `String _, `String s -> `String (Some s)
      | `Pkgname _, `Vpkgformula [[(pkg, None)]] -> `Pkgname (Some pkg)
      | `Ident _, `Ident i -> `Ident (Some i)
      | `Vpkg _, `Vpkgformula [[vpkg]] -> `Vpkg (Some vpkg)
      | `Vpkglist _, `Vpkgformula f ->
	  if List.exists (function _ :: _ :: _ -> true | _ -> false) f then
	    type_error ()	(* there are OR-ed deps *)
	  else
	    `Vpkglist (Some (List.map (function [vpkg] -> vpkg
					 | _ -> assert false) f))
      | `Veqpkg old_default, `Vpkgformula f ->
	  if is_eq_formula f
	  then set_default v (`Vpkg None)
	  else type_error ()
      | `Veqpkglist old_default, `Vpkgformula f ->
	  if is_eq_formula f
	  then set_default v (`Vpkglist None)
	  else type_error ()
      | `Enum (enums, _), `Ident i ->
	  if List.mem i enums
	  then `Enum (enums, Some i)
	  else type_error ()
      | _ -> type_error ()
  in
  (tyval :> Cudf_types.typedecl1)

%}

%token <string> IDENT PKGNAME QSTRING RELOP
%token <int> POSINT NEGINT
%token LBRACKET RBRACKET LPAREN RPAREN
%token COMMA PIPE COLON EQ
%token EOL
%type <int> int
%type <Cudf_types.pkgname> pkgname
%type <Cudf_types.vpkg> vpkg
%type <Cudf_types.vpkglist> vpkglist
%type <Cudf_types.vpkgformula> vpkgformula
%type <Cudf_types.typedecl> typedecl
%start int pkgname vpkg vpkglist vpkgformula typedecl

%%

pkgname: PKGNAME { $1 } ;

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

vpkgformula: and_formula { $1 } ;

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
  | IDENT COLON typename		{ ($1, $3) }
  | IDENT COLON typename
      EQ LBRACKET default RBRACKET	{ ($1, set_default $6 $3) }
;

typename:
  | IDENT			{ parse_ty_nodefault $1 }
  | IDENT LPAREN enums RPAREN	{ `Enum ($3, None) }
;

enums:
  | IDENT		{ [ $1 ] }
  | IDENT COMMA enums	{ $1 :: $3 }
;

default:
  | IDENT		{ `Ident $1 }
  | POSINT		{ `Int $1 }
  | NEGINT		{ `Int $1 }
  | QSTRING		{ `String $1 }
  | vpkgformula		{ `Vpkgformula $1 }
;

%%

let error_wrapper f =
  fun lexer lexbuf ->
    try
      f lexer lexbuf
    with
      | Parsing.Parse_error ->
	  raise (Cudf_types.Parse_error_typelib
		   ("syntax error",
		    lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p))
      | Parse_error_msg msg ->
	  raise (Cudf_types.Parse_error_typelib
		   (msg, lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p))

let int = error_wrapper int
let pkgname = error_wrapper pkgname
let vpkg = error_wrapper vpkg
let vpkglist = error_wrapper vpkglist
let vpkgformula = error_wrapper vpkgformula
let typedecl = error_wrapper typedecl
