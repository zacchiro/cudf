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

(** {5 CUDF types} *)

type version = int	(* required to be non 0 *)
type relop = [`Eq|`Neq|`Geq|`Gt|`Leq|`Lt]
type constr = (relop * version) option

(** {6 CUDF spec. types} *)

type pkgname = string
type vpkg = pkgname * constr
type vpkglist = vpkg list
type vpkgformula = (* XXX does not enforce CNF, whereas the spec requires it *)
    FTrue
  | FPkg of vpkg
  | FOr of vpkgformula list
  | FAnd of vpkgformula list
type veqpkg = pkgname * ([`Eq] * version) option
type veqpkglist = veqpkg list

(** {6 CUDF documents} *)

type enum_keep = [ `Keep_version | `Keep_package | `Keep_feature ]

(** Representation of a parsed package description item.

    With this representation, optional properties have already been
    expanded to their default values (if they have one). It is not
    possible to know whether they were present or not in the CUDF
    syntax. *)
type package = {
  package : pkgname ;
  version : version ;
  depends : vpkgformula ;	(* default : FTrue *)
  conflicts : vpkglist ;	(* default : [] *)
  provides : veqpkglist ;	(* default : [] *)
  installed : bool ;		(* default : false *)
  keep :  enum_keep option ;	(* default : None *)
  extra : (string * string) list ;	(* extra properties, unparsed *)
}

type request = {
  problem_id : string ;
  install : vpkglist ;	(* default : [] *)
  remove : vpkglist ;	(* default : [] *)
  upgrade : vpkglist ;	(* default : [] *)
}

(** {6 Syntactic CUDF representation} *)
type cudf_doc = package list * request

(** {6 Semantic CUDF representation} *)

(** violation of a constraint imposed by CUDF specification

    @param msg explanation of which constraint has been violated *)
exception Constraint_violation of string

(** package universe (including package status, i.e., installed packages) *)
type universe
type cudf = universe * request
val load_cudf : cudf_doc -> cudf
val load_universe : package list -> universe

(** {5 CUDF manipulation} *)

(** lookup a specific package via a <name, version> key
    @raise Not_found *)
val lookup_package : universe -> pkgname * version -> package

(** check wheather a given package constraint is satisfied in a given
    universe

    @param only_installed consider only [installed=true] packages.
      Default: false (i.e., consider all packages in the universe)
    @param include_features allow constraint to be satisfied by features
      (i.e., Provides). Default: true *)
val mem_package :
  ?only_installed:bool ->
  ?include_features:bool ->
  universe -> vpkg -> bool

(** lookup all available versions of a given package name *)
val lookup_packages : universe -> pkgname -> package list

(** lookup all installed versions of a given package name.
    Shorthand for [lookup_packages] composed with filtering on installed=true *)
val get_installed : universe -> pkgname -> package list

val iter_packages : universe -> (package -> unit) -> unit
val fold_packages : ('a -> package -> 'a) -> 'a -> universe -> 'a

(** conversion from universe to plain package list *)
val get_packages : universe -> package list

(** project on packages having "installed: true".
    Inefficient (involves hashtbl cloning), use with care. *)
val status : universe -> universe


