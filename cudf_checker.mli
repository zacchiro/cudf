(*****************************************************************************)
(*  libCUDF - CUDF (Common Upgrade Description Format) manipulation library  *)
(*  Copyright (C) 2009  Stefano Zacchiroli <zack@pps.jussieu.fr>             *)
(*                                                                           *)
(*  This program is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU General Public License as published by     *)
(*  the Free Software Foundation, either version 3 of the License, or (at    *)
(*  your option) any later version.                                          *)
(*                                                                           *)
(*  This program is distributed in the hope that it will be useful, but      *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of               *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU        *)
(*  General Public License for more details.                                 *)
(*                                                                           *)
(*  You should have received a copy of the GNU General Public License        *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
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
