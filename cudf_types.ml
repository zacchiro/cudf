
(* <type, literal> *)
exception Parse_error of string * string

let parse_bool = function
  | "true" -> true
  | "false" -> false
  | s -> raise (Parse_error ("bool", s))

(* TODO add checking of package name conventions *)
let parse_pkgname s = s
  
let parse_version s =
  try
    int_of_string s
  with Failure _ -> raise (Parse_error ("version", s))

let parse_keep = function
  | "version" -> `Keep_version
  | "package" -> `Keep_package
  | "feature" -> `Keep_feature
  | s -> raise (Parse_error ("enum('version,'package,'feature)", s))

let parse_vpkg s = assert false (* TODO *)
let parse_vpkglist s = assert false (* TODO *)
let parse_vpkgformula s = assert false (* TODO *)
let parse_veqpkg s = assert false (* TODO *)
let parse_veqpkglist s = assert false (* TODO *)

