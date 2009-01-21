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
open Cudf

type inconsistency_reason =
  [ `Unsat_dep of (pkgname * version) * vpkgformula	(** unsatisfied dep. *)
  | `Conflict of (pkgname * version) * vpkglist	(** unsolved conflict(s) *)
  ]

type bad_solution_reason =
  [ inconsistency_reason
  | `Missing_install of vpkglist   (** install pkgs missing *)
  | `Missing_upgrade of vpkglist   (** upgrade pkgs missing *)
  | `Unremoved of vpkglist	   (** remove pkgs still there *)
  | `Downgrade of vpkglist	   (** upgrade pkgs downgraded *)
  | `Multi_upgrade of pkgname list (** upgrade pkgs aren't singleton *)
  | `Not_kept of pkgname * version * enum_keep	(** unattended "Keep" *)
  ]

(** provide a string explaining a given reason, meant for error messages *)
val explain_reason : bad_solution_reason -> string

(** check whether a given package formula is satisfied by a given
    package status

    @return [true, []] if the formula is satisfied; [false, f]
    otherwise, where f is a sub-formula of the input denoting an
    unsatisfiable formula (ideally, a witness of the unsatisfiability
    of the input formula) *)
val satisfy_formula : universe -> vpkgformula -> bool * vpkgformula

(** check whether a package list is not satisfied by a given package
    status

    @return [true, []] if the list is disjoin; [false, l]
    otherwise, where l is a list of packages satisfied by the universe
    (ideally, the reason of the non-disjointness) *)
val disjoint :
  universe -> ?ignore:(package -> bool) -> vpkglist -> bool * vpkglist

(** @return [true, None] if the given installation is consistent,
    [false, Some r] otherwise, where r is the inconsistency reason *)
val is_consistent : universe -> bool * inconsistency_reason option

(** check whether a given solution fulfill the request of a given CUDF
    @return [true, []] if this is the case, [false, l]
    otherwise, where r explains why the solution is bad *)
val is_solution : cudf -> solution -> bool * bad_solution_reason list
