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

(** Mapping (in the form of associative list) from stanza postmarks to type
    declarations.

    Keys in the map are usually only those prescribed by the CUDF
    specification, namely "preamble", "package", and "request". Values in the
    map are type declarations for each supported property of that stanza;
    usually they only represent core property schemata (see CUDF §2.2.3).
*)
type stanza_typedecl = (string * typedecl) list

(** {5 Global configuration} *)

(** Default stanza types for parsing CUDF documents.

    For what concerns package stanzas, available types can be extended by using
    "property" declaration in the preamble stanza.
*)
val stanza_typedecl: stanza_typedecl

