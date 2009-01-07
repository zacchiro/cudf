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

open Cudf

val satisfy_formula : universe -> vpkgformula -> bool
val disjoint : universe -> ?ignore:(package -> bool) -> vpkglist -> bool

(** @return [true, _] if the given installation is consistent, [false,
    msg] otherwise, where msg is the inconsistency reason *)
val is_consistent : universe -> bool * string

(** check whether a given solution fulfill the request of a given CUDF
    @return [true, _] if this is the case, [false, msg] otherwise,
    where msg is an explanatory message *)
val is_solution : cudf -> solution -> bool * string
