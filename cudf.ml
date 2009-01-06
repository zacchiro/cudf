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
type universe = {
  id2pkg: ((string * int), package) Hashtbl.t;	(** <name, ver> -> pkg *)
  name2pkgs: (string, package) Hashtbl.t; (** name -> pkg (multi-bindings) *)
  features: (string, version option) Hashtbl.t;
  (** feature -> avail feature versions (multi-bindings). Version
    "None" means "all possible versions" *)
}
type cudf = universe * request

let empty_universe () =
  { id2pkg = Hashtbl.create 1023 ;
    name2pkgs = Hashtbl.create 1023 ;
    features = Hashtbl.create 1023 ;
  }

(** process all features (i.e., Provides) provided by a given package
    and fill with them a given feature table *)
let expand_features pkg features =
  List.iter
    (fun feat ->
       match feat with
	 | name, None -> Hashtbl.add features name None
	 | name, Some (_, ver) ->
	     Hashtbl.add features name (Some ver))
    pkg.provides

let load_universe pkgs =
  let univ = empty_universe () in
    List.iter
      (fun pkg ->
	 let id = pkg.package, pkg.version in
	   if Hashtbl.mem univ.id2pkg id then
	     raise (Constraint_violation
		      (sprintf "duplicate package: <%s, %d>"
			 pkg.package pkg.version));
	   Hashtbl.add univ.id2pkg id pkg;
	   Hashtbl.add univ.name2pkgs pkg.package pkg;
	   expand_features pkg univ.features)
      pkgs;
    univ

let load_cudf (pkgs, req) = load_universe pkgs, req

let lookup_package univ = Hashtbl.find univ.id2pkg
let iter_packages univ f = Hashtbl.iter (fun _id pkg -> f pkg) univ.id2pkg
let fold_packages f init univ =
  Hashtbl.fold (fun _id pkg acc -> f acc pkg) univ.id2pkg init
let get_packages = fold_packages (fun acc pkg -> pkg :: acc) []

let mem_package ?(only_installed = false) ?(include_features = true) univ =
  assert false
  (* function *)
  (*   | name, None -> Hashtbl.mem name univ.name2pkgs *)
  (*   | name, Some constr -> *)

let status univ =
  let univ' = empty_universe () in
    Hashtbl.iter
      (fun id pkg ->
	 match pkg with
	   | { installed = true } ->
	       Hashtbl.add univ'.id2pkg id pkg;
	       Hashtbl.add univ'.name2pkgs pkg.package pkg;
	       expand_features pkg univ'.features
	   | _ -> ())
      univ.id2pkg;
    univ'

let lookup_packages univ pkgname = Hashtbl.find_all univ.name2pkgs pkgname

let get_installed univ pkgname =
  List.filter (fun { installed = i } -> i) (lookup_packages univ pkgname)

