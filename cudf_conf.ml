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

open Cudf_types

type stanza_typedecl1 = [ typedecl1 | `Typedecl of typedecl option ]
type stanza_typedecl = (string * stanza_typedecl1) list

type stanza_types = (string * stanza_typedecl) list

let preamble_types = [
  "preamble",		`String (Some "") ;
  "property",		`Typedecl (Some []) ;
  "univ-checksum",	`String (Some "") ;
  "status-checksum",	`String (Some "") ;
  "req-checksum",	`String (Some "") ;
]

let package_types = [
  "package",		`Pkgname None ;
  "version",		`Posint None ;
  "depends",		`Vpkgformula (Some []) ;
  "conflicts",		`Vpkglist (Some []) ;
  "provides",		`Veqpkglist (Some []) ;
  "installed",		`Bool (Some false) ;
  "was-installed",	`Bool (Some false) ;
  "keep",		`Enum (["version"; "package"; "feature"; "none"],
			       Some "none") ;
]

let request_types = [
  "install",		`Vpkglist (Some []) ;
  "remove",		`Vpkglist (Some []) ;
  "upgrade",		`Vpkglist (Some []) ;
]


let stanza_types = [
  "preamble",	preamble_types ;
  "package",	package_types ;
  "request",	request_types ;
]
