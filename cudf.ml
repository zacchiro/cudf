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

exception Constraint_violation of string

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
type cudf_item = [ `Package of package | `Request of request ]
type universe = {
  id2pkg: ((string * int), package) Hashtbl.t;	(** <name, ver> -> pkg *)
  name2pkgs: (string, package) Hashtbl.t; (** name -> pkg (multi-bindings) *)
  inst_features: (string, (package * version option)) Hashtbl.t;
  (** feature -> avail feature versions (multi-bindings) among
      installed packages only. Each available feature is reported as a
      pair <owner, provided version>, where owner is the package
      providing it. Provided version "None" means "all possible
      versions" *)
  mutable univ_size : int;
  mutable inst_size : int;
}
type cudf = universe * request
type solution = universe

let universe_size univ = univ.univ_size
let installed_size univ = univ.inst_size

let (=%) pkg1 pkg2 =
  pkg1.package = pkg2.package && pkg1.version = pkg2.version

let default_package = {
  package = "" ;
  version = 0 ;
  depends = FTrue ;
  conflicts = [] ;
  provides = [] ;
  installed = false ;
  keep = None ;
  extra = [] ;
}

let default_request = {
  problem_id = "" ;
  install = [] ;
  remove = [] ;
  upgrade = [] ;
}

let empty_universe () =
  { id2pkg = Hashtbl.create 1023 ;
    name2pkgs = Hashtbl.create 1023 ;
    inst_features = Hashtbl.create 1023 ;
    univ_size = 0 ; inst_size = 0 ;
  }

(** process all features (i.e., Provides) provided by a given package
    and fill with them a given feature table *)
let expand_features pkg features =
  if pkg.installed then
    List.iter
      (fun feat ->
	 match feat with
	   | name, None -> Hashtbl.add features name (pkg, None)
	   | name, Some (_, ver) -> Hashtbl.add features name (pkg, (Some ver)))
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
	   expand_features pkg univ.inst_features;
	   univ.univ_size <- univ.univ_size + 1;
	   if pkg.installed then
	     univ.inst_size <- univ.inst_size + 1)
      pkgs;
    univ

let lookup_package univ = Hashtbl.find univ.id2pkg
let iter_packages f univ = Hashtbl.iter (fun _id pkg -> f pkg) univ.id2pkg
let fold_packages f init univ =
  Hashtbl.fold (fun _id pkg acc -> f acc pkg) univ.id2pkg init
let get_packages = fold_packages (fun acc pkg -> pkg :: acc) []

let (|=) v = function
  | None -> true
  | Some (`Eq, v') -> v = v'
  | Some (`Neq, v') -> v <> v'
  | Some (`Geq, v') -> v >= v'
  | Some (`Gt, v') -> v > v'
  | Some (`Leq, v') -> v <= v'
  | Some (`Lt, v') -> v < v'

let status univ =
  let univ' = empty_universe () in
    Hashtbl.iter
      (fun id pkg ->
	 match pkg with
	   | { installed = true } ->
	       Hashtbl.add univ'.id2pkg id pkg;
	       Hashtbl.add univ'.name2pkgs pkg.package pkg;
	       expand_features pkg univ'.inst_features
	   | _ -> ())
      univ.id2pkg;
    univ'

let lookup_packages ?(filter=None) univ pkgname = 
  let packages = Hashtbl.find_all univ.name2pkgs pkgname in
    match filter with
	None -> packages
      | Some _ as pred -> List.filter (fun p -> p.version |= pred) packages

let get_installed univ pkgname =
  List.filter (fun { installed = i } -> i) (lookup_packages univ pkgname)

let mem_installed ?(include_features = true) ?(ignore = fun _ -> false)
    univ (name, constr) =
  let pkg_filter = fun pkg -> not (ignore pkg) in
  let mem_feature constr =
    let feats = Hashtbl.find_all univ.inst_features name in
      List.exists
	(function
	     owner_pkg, None -> pkg_filter owner_pkg
	   | owner_pkg, Some v -> pkg_filter owner_pkg && v |= constr)
	feats in
  let pkgs = List.filter pkg_filter (get_installed univ name) in
    List.exists (fun pkg -> pkg.version |= constr) pkgs
    || (include_features && mem_feature constr)

