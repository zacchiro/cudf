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

(** package equality up to <name, version>
    i.e. 2 packages are equal if they have the same name and version *)
val (=%) : package -> package -> bool

type request = {
  problem_id : string ;
  install : vpkglist ;	(* default : [] *)
  remove : vpkglist ;	(* default : [] *)
  upgrade : vpkglist ;	(* default : [] *)
}

val default_package : package	(** implement package defaults *)
val default_request : request	(** implement request defaults *)

(** {6 Syntactic CUDF representation} *)

type cudf_doc = package list * request
type cudf_item = [ `Package of package | `Request of request ]

(** {6 Semantic CUDF representation} *)

(** violation of a constraint imposed by CUDF specification

    @param msg explanation of which constraint has been violated *)
exception Constraint_violation of string

(** package universe (including package status, i.e., installed packages) *)
type universe
type cudf = universe * request

(** XXX temporary encoding for CUDF solutions, as they are not yet
    defined by the CUDF spec

    A universe encoding a solution matters only for its [installed]
    packages, which are considered to be the resulting package
    status *)
type solution = universe

val load_universe : package list -> universe

(** {5 CUDF manipulation} *)

(** lookup a specific package via a <name, version> key
    @raise Not_found if the requested package cannot be found *)
val lookup_package : universe -> pkgname * version -> package

(** check wheather a given package constraint is satisfied in a given
    package status (i.e., the universe subset of [installed] packages)

    @param include_features allow constraint to be satisfied by features
    (i.e., Provides). Default: true
    @param ignore make the lookup skip over all packages matching the given
    package predicate *)
val mem_installed :
  ?include_features: bool ->
  ?ignore:(package -> bool) ->
  universe -> vpkg -> bool

(** lookup all available versions of a given package name *)
val lookup_packages : universe -> pkgname -> package list

(** lookup all installed versions of a given package name.
    Shorthand for [lookup_packages] composed with filtering on installed=true *)
val get_installed : universe -> pkgname -> package list

val iter_packages : (package -> unit) -> universe -> unit
val fold_packages : ('a -> package -> 'a) -> 'a -> universe -> 'a

(** conversion from universe to plain package list *)
val get_packages : universe -> package list

(** total numer of available packages (no matter whether they are
    installed or not) *)
val universe_size : universe -> int

(** total number of installed packages occurring in the universe *)
val installed_size : universe -> int

(** project on packages having "installed: true".
    Inefficient (involves hashtbl cloning), use with care. *)
val status : universe -> universe

