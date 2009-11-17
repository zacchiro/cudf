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

(** CUDF library *)

open Cudf_types

(** {6 CUDF documents} *)

(** Representation of a parsed package description item.

    With this representation, optional properties have already been
    expanded to their default values (if they have one). It is not
    possible to know whether they were present or not in the CUDF
    syntax. *)

type package = {
  package : pkgname ;
  version : version ;
  depends : vpkgformula ;			(* default : [] *)
  conflicts : vpkglist ;			(* default : [] *)
  provides : veqpkglist ;			(* default : [] *)
  installed : bool ;				(* default : false *)
  was_installed : bool ;			(* default : false *)
  keep :  enum_keep ;				(* default : Keep_none *)
  pkg_extra : (string * typed_value) list ;	(* extra properties *)
}

(** package equality up to <name, version>
    i.e. 2 packages are equal iff they have the same name and version *)
val (=%) : package -> package -> bool

type request = {
  request_id : string ;				(* default : "" *)
  install : vpkglist ;				(* default : [] *)
  remove : vpkglist ;				(* default : [] *)
  upgrade : vpkglist ;				(* default : [] *)
  req_extra : (string * typed_value) list ;	(* default : [] *)
}

type preamble = {
  preamble_id : string ;	(** text following the "preamble: " postmark *)
  property : typedecl ;		(** extra property declarations *)
  univ_checksum: string ;	(** universe checksum *)
  status_checksum: string ;	(** status checksum *)
  req_checksum: string ;	(** request checksum *)
}

val default_preamble : preamble	(** implement preamble defaults *)
val default_package : package	(** implement package defaults *)
val default_request : request	(** implement request defaults *)

(** {6 Syntactic CUDF representation} *)

(** a CUDF document with its information items *)
type cudf_doc = preamble * package list * request

(** a single information item *)
type cudf_item =
  [ `Preamble of preamble
  | `Package of package
  | `Request of request
  ]

(** {6 Semantic CUDF representation} *)

(** violation of a constraint imposed by CUDF specification

    @param msg explanation of which constraint has been violated *)
exception Constraint_violation of string

(** package universe (including package status, i.e., installed packages) *)
type universe

type cudf = preamble * universe * request

(** CUDF-based encoding of solutions, see CUDF 2.0, appendix B

    A universe encoding a solution matters only for its [installed]
    packages, which are considered to be the resulting package
    status *)
type solution = preamble * universe

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
    package predicate. Default: do not ignore any package *)
val mem_installed :
  ?include_features: bool ->
  ?ignore:(package -> bool) ->
  universe -> vpkg -> bool

(** Ask who provides a given feature (predicate).
    Note: only installed=true packages are considered by this function.

    @return a list of packages providing the requested feature. Each
    package is paired with an optional version; if it is None, the
    given package provides all possible version of the feature; it if
    is Some v, the given package only provides version [v] of the
    feature. *)
val who_provides : universe -> vpkg -> (package * version option) list

(** lookup all available versions of a given package name

    @param filter filter the found packages according to the given
    version constraint. Default: None (i.e., no filtering) *)
val lookup_packages : ?filter:constr -> universe -> pkgname -> package list

(** lookup all installed versions of a given package name.
    Shorthand for [lookup_packages] composed with filtering on installed=true *)
val get_installed : universe -> pkgname -> package list

val iter_packages : (package -> unit) -> universe -> unit
val fold_packages : ('a -> package -> 'a) -> 'a -> universe -> 'a

(** conversion from universe to plain package list

    @param filter only return packages matching a given
    predicate. Default is to return all packages *)
val get_packages : ?filter:(package -> bool) -> universe -> package list

(** total numer of available packages (no matter whether they are
    installed or not) *)
val universe_size : universe -> int

(** total number of installed packages occurring in the universe *)
val installed_size : universe -> int

(** Projection on packages having "installed: true".

    Inefficient (involves Hashtbl.t cloning), use with care. *)
val status : universe -> universe

(** {5 Low-level stanza manipulation} *)

(** low-level property lookup: given a package, lookup on it a
    property by name, returning its (pretty-printed, see
    {!Cudf_types}) value as a string

    @param pkg package to be inspected
    @param property property name to be lookup (case-sensitive)

    @raise Not_found if the given property name is not associated to
    the given package (note that "being associated with" does not
    necessarily mean that the property appears in the stanza, due to
    default values) *)
val lookup_package_property : package -> string -> string

(** Same as {!Cudf.lookup_package_property}, but acting on request
    information items.

    To lookup the problem identifier as a string (which strictly
    speaking is not a property) you should lookup "Problem" *)
val lookup_request_property : request -> string -> string

(** Check whether a version matches a version constraint,
    e.g. [version_matches 1 (Some(`Eq, 2)) = false] *)
val version_matches : version -> constr -> bool

(** Same as {!Cudf.version_matches} *)
val ( |= ) : version -> constr -> bool
