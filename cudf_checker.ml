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

open ExtLib
open Printf

open Cudf_types
open Cudf

let (!!) pred = fun x -> not (pred x)

type inconsistency_reason =
  [ `Unsat_dep of (pkgname * version) * vpkgformula
  | `Conflict of (pkgname * version) * vpkglist
  ]

type bad_solution_reason =
  [ inconsistency_reason
  | `Missing_install of vpkglist
  | `Missing_upgrade of vpkglist
  | `Unremoved of vpkglist
  | `Downgrade of vpkglist
  | `Multi_upgrade of pkgname list
  ]

let explain_reason = function
  | `Unsat_dep ((name, ver), fmla) ->
      sprintf "Cannot satisfy dependencies %s of package %s (version %d)"
	(Cudf_types.string_of_vpkgformula fmla) name ver
  | `Conflict ((name, ver), pkgs) ->
      sprintf "Unresolved conflicts %s of package %s (version %d)"
	(Cudf_types.string_of_vpkglist pkgs) name ver
  | `Missing_install vpkgs ->
      "Unmet installation request, missing packages: " ^
	Cudf_types.string_of_vpkglist vpkgs
  | `Missing_upgrade vpkgs ->
      "Unmet upgrade request, missing packages: " ^
	Cudf_types.string_of_vpkglist vpkgs
  | `Unremoved vpkgs ->
      "Unment remove request, still present packages: " ^
	Cudf_types.string_of_vpkglist vpkgs
  | `Downgrade vpkgs ->
      "Unment upgrade request, not-upgraded: " ^
	Cudf_types.string_of_vpkglist vpkgs
  | `Multi_upgrade pkgs ->
      "Unment upgrade request, not-unique: " ^ String.concat ", " pkgs

	(* XXX not tail-recursive *)
let satisfy_formula univ fmla =
  let reason = ref None in
  let rec is_sat = function (* ASSUMPTION (for explanation): fmla is in CNF *)
    | FTrue -> true
    | FPkg pkg -> mem_installed ~include_features:true univ pkg
    | FOr fmlas -> List.exists is_sat fmlas
    | FAnd fmlas ->
	(match List.filter (!! is_sat) fmlas with
	  | [] -> true
	  | [unsat] -> reason := Some unsat ; false
	  | unsat -> reason := Some (FAnd unsat) ; false)
  in
  let sat = is_sat fmla in
    sat, !reason
      
let disjoint univ ?ignore pkgs =
  match
    List.filter (mem_installed ?ignore ~include_features:true univ) pkgs
  with
    | [] -> true, []
    | pkgs -> false, pkgs

let is_consistent univ =
  let msg = ref None in
    try
      iter_packages
	(fun pkg ->
	   if pkg.installed then begin
	     (match satisfy_formula univ pkg.depends with
		| false, Some fmla ->
		    msg := Some (`Unsat_dep ((pkg.package, pkg.version), fmla));
		    raise Exit
		| false, None -> assert false
		| _ -> ());
	     (match disjoint univ ~ignore:((=%) pkg) pkg.conflicts with
		| false, pkgs ->
		    msg := Some (`Conflict ((pkg.package, pkg.version), pkgs));
		    raise Exit
		| _ -> ());
	   end)
	univ;
      true, !msg
    with Exit -> false, !msg

(* for reference, see CUDF §3.3.4, "semantics of requets" *)
let is_solution (univ, req) sol =
  let _ =
    if universe_size sol <> installed_size sol then
      prerr_endline ("WARNING: solution contains not-installed packages,"
		     ^ " they have been ignored")
  in
  let sat vpkg = fst (satisfy_formula sol (FPkg vpkg)) in
  let is_succ () = (* XXX not implemented, as it will be pointless with a
		      diff-like encoding of solutions *)
    true, [] in
  let is_cons () =
    match is_consistent sol with
      | true, _ -> true, []
      | false, None -> assert false
      | false, Some reason -> false, [reason] in
  let install_ok () =
    match List.filter (!! sat) req.install with
      | [] -> true, []
      | l -> false, [`Missing_install l] in
  let remove_ok () =
    match disjoint sol req.remove with
      | true, _ -> true, []
      | false, pkgs -> false, [`Unremoved pkgs] in
  let upgrade_ok () =
    match List.filter (!! sat) req.upgrade with
      | (_ :: _) as l -> false, [`Missing_upgrade l]
      | [] ->
	  let res =
	    List.fold_left
	      (fun (ok, downgrades, multi) ((pkgname, _constr) as vpkg) ->
		 match get_installed sol pkgname with
		   | [pkg] ->
		       let old_installed = get_installed univ pkgname in
			 if not (List.for_all
				   (fun pkg' -> pkg'.version <= pkg.version)
				   old_installed) then
			   false, vpkg :: downgrades, multi
			 else
			   true && ok, downgrades, multi
		   | [] -> (* impossible, since the upgrade fmla is satisfied *)
		       assert false
		   | _ ->
		       false, downgrades, pkgname :: multi)
	      (true, [], [])
	      req.upgrade
	  in
	    (match res with
	       | true, _, _ -> true, []
	       | false, downgrades, multi ->
		   false,
		   (if downgrades <> [] then [`Downgrade downgrades] else [])
		   @ (if multi <> [] then [`Multi_upgrade multi] else []))
  in
  List.fold_left
    (fun (is_sol, msgs) test ->
       let res, msg = test () in
	 res && is_sol, msg @ msgs)
    (true, [])
    [is_succ; is_cons; install_ok; remove_ok; upgrade_ok]

