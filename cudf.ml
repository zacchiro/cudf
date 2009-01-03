
open ExtLib

type version = int

type pkgname = string
type vpkg = ([`Eq|`Neq|`Geq|`Gt|`Leq|`Lt] * version) option * pkgname
type vpkglist = vpkg list
type vpkgformula =
    FTrue
  | FPkg of vpkg
  | FOr of vpkgformula list
  | FAnd of vpkgformula list
type veqpkg = ([`Eq] * version) option * pkgname
type veqpkglist = veqpkg list
type enum_keep = [ `Keep_version | `Keep_package | `Keep_feature ]

type package = {
  package : pkgname ;
  version : version ;
  depends : vpkgformula ;
  conflicts : vpkglist ;
  provides : veqpkglist ;
  installed : bool ;
  keep : [ `Keep_version | `Keep_package | `Keep_feature ] option ;
  extra : (string * string) list
}
type request = {
  problem_id : string ;
  install : vpkglist ;
  remove : vpkglist ;
  upgrade : vpkglist ;
}
type cudf = package list * request

let lookup_package (pkgs, _req) id =
  List.find (fun pkg -> (pkg.package, pkg.version) = id) pkgs

