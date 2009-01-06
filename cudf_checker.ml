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

open ExtLib
open Printf

open Cudf

(* XXX not tail-recursive *)
let satisfy_formula univ =
  let rec aux = function
    | FTrue -> true
    | FPkg pkg -> mem_installed ~include_features:true univ pkg
    | FOr fmlas -> List.exists aux fmlas
    | FAnd fmlas -> List.for_all aux fmlas
  in
    aux

let disjoint univ ?ignore =
  List.for_all
    (fun vpkg -> not (mem_installed ?ignore ~include_features:true univ vpkg))

let is_consistent univ =
  let msg = ref "" in
    try
      iter_packages univ
	(fun pkg ->
	   if pkg.installed then begin
	     if not (satisfy_formula univ pkg.depends) then begin
	       msg := sprintf "Cannot satisfy dependency: (%s,%d) -> %s"
		 pkg.package pkg.version (dump pkg.depends);
	       raise Exit
	     end;
	     if not (disjoint univ ~ignore:((=%) pkg) pkg.conflicts) then begin
	       msg := sprintf "Unsolved conflicts: (%s,%d) -#- %s"
		 pkg.package pkg.version (dump pkg.conflicts);
	       raise Exit
	     end
	   end);
      true, !msg
    with Exit -> false, !msg

(* for reference, see CUDF §3.3.4, "semantics of requets" *)
let is_solution (univ, req) sol =
  let _ =
    if universe_size sol <> installed_size sol then
      prerr_endline ("WARNING: solution contains not-installed packages,"
		     ^ " they have been ignored")
  in
  let and_of_vpkglist l = FAnd (List.map (fun p -> FPkg p) l) in
  let is_succ = (* XXX not implemented, as it will be pointless with a
		   diff-like encoding of solutions *)
    lazy (true, "") in
  let is_cons = lazy (is_consistent sol) in
  let install_ok = lazy (
    if satisfy_formula sol (and_of_vpkglist req.install) then
      true, ""
    else
      false, "requested _install_ packages missing"
  ) in
  let remove_ok = lazy (
    if disjoint sol req.remove then
      true, ""
    else
      false, "requested _remove_ packages still present"
  ) in
  let upgrade_ok = lazy (
    if not (satisfy_formula sol (and_of_vpkglist req.upgrade)) then
      false, "requested _upgrade_ packages missing"
    else
      (* TODO implement extra check on upgrade requests
	 1) packages are not older than before
	 2) packages are singletons *)
      true, ""
  ) in
  let is_sol, msgs =
    List.fold_left
      (fun (is_sol, msgs) test ->
	 let res, msg = Lazy.force test in
	   res && is_sol, msg :: msgs)
      (true, [])
      [is_succ; is_cons; install_ok; remove_ok; upgrade_ok]
  in
    is_sol, String.concat "; " (List.rev (List.filter ((<>) "") msgs))

