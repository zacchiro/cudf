
(** CUDF type library

    Implement parsing and pretty printing of core CUDF types (see CUDF
    spec. §3.2.2) *)

(** error while parsing the lexical representation of some type
    arguments:
    - type name
    - literal read *)
exception Parse_error of string * string

val parse_bool : string -> bool

val parse_pkgname : string -> string
val parse_version : string -> Cudf.version
val parse_vpkg : string -> Cudf.vpkg
val parse_vpkglist : string -> Cudf.vpkglist
val parse_vpkgformula : string -> Cudf.vpkgformula
val parse_veqpkg : string -> Cudf.veqpkg
val parse_veqpkglist : string -> Cudf.veqpkglist

val parse_keep : string -> Cudf.enum_keep

