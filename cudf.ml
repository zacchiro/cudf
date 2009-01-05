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

exception Constraint_violation of string

type version = int
type relop = [`Eq|`Neq|`Geq|`Gt|`Leq|`Lt]
type constr = (relop * version) option

type pkgname = string
type vpkg = pkgname * constr
type vpkglist = vpkg list
type vpkgformula =
    FTrue
  | FPkg of vpkg
  | FOr of vpkgformula list
  | FAnd of vpkgformula list
type veqpkg = pkgname * ([`Eq] * version) option
type veqpkglist = veqpkg list
type enum_keep = [ `Keep_version | `Keep_package | `Keep_feature ]

type package = {
  package : pkgname ;
  version : version ;
  depends : vpkgformula ;
  conflicts : vpkglist ;
  provides : veqpkglist ;
  installed : bool ;
  keep : enum_keep option ;
  extra : (string * string) list
}
type request = {
  problem_id : string ;
  install : vpkglist ;
  remove : vpkglist ;
  upgrade : vpkglist ;
}
type cudf_doc = package list * request
type universe =
    ((string * int), package) Hashtbl.t	(* <name, version> -> package *)
    * (string, package) Hashtbl.t	(* name -> package (multi-bindings) *)
type solution = universe
type cudf = universe * request

let empty_universe () =
  let id2pkg = Hashtbl.create 1023 in
  let name2pkg = Hashtbl.create 1023 in
    (id2pkg, name2pkg)

let load_universe pkgs =
  let id2pkg, name2pkg = empty_universe () in
    List.iter
      (fun pkg ->
	 let id = pkg.package, pkg.version in
	   if Hashtbl.mem id2pkg id then
	     raise (Constraint_violation
		      (sprintf "duplicate package: <%s, %d>"
			 pkg.package pkg.version));
	   Hashtbl.add id2pkg id pkg;
	   Hashtbl.add name2pkg pkg.package pkg)
      pkgs;
    (id2pkg, name2pkg)

let load_cudf (pkgs, req) = load_universe pkgs, req

let lookup_package (univ, _) = Hashtbl.find univ
let iter_packages (univ, _) f = Hashtbl.iter (fun _id pkg -> f pkg) univ
let fold_packages f init (univ, _) =
  Hashtbl.fold (fun _id pkg acc -> f acc pkg) univ init
let get_packages = fold_packages (fun acc pkg -> pkg :: acc) []

let status (univ, _) =
  let id2pkg, name2pkg = empty_universe () in
    Hashtbl.iter
      (fun id pkg ->
	 match pkg with
	   | { installed = true } ->
	       Hashtbl.add id2pkg id pkg;
	       Hashtbl.add name2pkg pkg.package pkg
	   | _ -> ())
      univ;
    (id2pkg, name2pkg)

let installation (_, univ) pkgname =
  List.filter_map
    (function { installed = true ; version = v} as pkg -> Some v | _ -> None)
    (Hashtbl.find_all univ pkgname)

