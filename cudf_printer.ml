(*****************************************************************************)
(*  libCUDF - CUDF (Common Upgrade Description Format) manipulation library  *)
(*  Copyright (C) 2009-2010  Stefano Zacchiroli <zack@pps.jussieu.fr>        *)
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

open Cudf
open Cudf_types
open Cudf_types_pp


let pp_property out (n, s) = fprintf out "%s: %s\n" n s

let pp_package out pkg =
  let pp = pp_property out in
  pp ("package", string_of_pkgname pkg.package);
  pp ("version", string_of_version pkg.version);
  if pkg.depends <> default_package.depends then
    pp ("depends", string_of_vpkgformula pkg.depends);
  if pkg.conflicts <> default_package.conflicts then
    pp ("conflicts", string_of_vpkglist pkg.conflicts);
  if pkg.provides <> default_package.provides then
    pp ("provides", string_of_vpkglist (pkg.provides :> vpkg list));
  if pkg.installed <> default_package.installed then
    pp ("installed", string_of_bool pkg.installed);
  if pkg.was_installed <> default_package.was_installed then
    pp ("was-installed", string_of_bool pkg.was_installed);
  if pkg.keep <> default_package.keep then
    pp ("keep", string_of_keep pkg.keep);
  List.iter (fun (k, v) -> pp (k, string_of_value v)) pkg.pkg_extra

let pp_request out req =
  let pp = pp_property out in
  pp ("request", req.request_id);
  if req.install <> default_request.install then
    pp ("install", string_of_vpkglist req.install);
  if req.remove <> default_request.remove then
    pp ("remove", string_of_vpkglist req.remove);
  if req.upgrade <> default_request.upgrade then
    pp ("upgrade", string_of_vpkglist req.upgrade);
  List.iter (fun (k, v) -> pp (k, string_of_value v)) req.req_extra

let pp_preamble out pre =
  let pp = pp_property out in
  pp ("preamble", pre.preamble_id);
  if pre.property <> default_preamble.property then
    pp ("property", string_of_typedecl pre.property);
  if pre.univ_checksum <> default_preamble.univ_checksum then
    pp ("univ-checksum", pre.univ_checksum);
  if pre.status_checksum <> default_preamble.status_checksum then
    pp ("status-checksum", pre.status_checksum);
  if pre.req_checksum <> default_preamble.req_checksum then
    pp ("req-checksum", pre.req_checksum)

let pp_universe out =
  iter_packages (fun pkg -> fprintf out "%a\n" pp_package pkg)

let pp_packages out =
  List.iter (fun pkg -> fprintf out "%a\n" pp_package pkg)

let pp_cudf out (pre, univ, req) =
  fprintf out "%a\n%a\n%a"
    pp_preamble pre pp_universe univ pp_request req

let pp_doc out (pre, pkgs, req) =
  Option.may (fun pre -> fprintf out "%a\n" pp_preamble pre) pre;
  List.iter (fun pkg -> fprintf out "%a\n" pp_package pkg) pkgs;
  pp_request out req

let pp_solution out (pre, univ) =
  fprintf out "%a\n%a"
    pp_preamble pre pp_universe univ

let pp_item out = function
  | `Package pkg -> pp_package out pkg
  | `Request req -> pp_request out req
  | `Preamble pre -> pp_preamble out pre
