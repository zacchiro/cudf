
(** {5 CUDF types} *)

type version = int	(* required to be non 0 *)

(** {6 CUDF spec. types} *)

type pkgname = string
type vpkg = ([`Eq|`Neq|`Geq|`Gt|`Leq|`Lt] * version) option * pkgname
type vpkglist = vpkg list
type vpkgformula = (* XXX does not enforce CNF, whereas the spec requires it *)
    FTrue
  | FPkg of vpkg
  | FOr of vpkgformula list
  | FAnd of vpkgformula list
type veqpkg = ([`Eq] * version) option * pkgname
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

type request = {
  problem_id : string ;
  install : vpkglist ;	(* default : [] *)
  remove : vpkglist ;	(* default : [] *)
  upgrade : vpkglist ;	(* default : [] *)
}

type cudf = package list * request

(** {5 CUDF manipulation} *)

(** lookup a specific package via a <name, version> key
    @raise Not_found *)
val lookup_package : cudf -> string * int -> package

