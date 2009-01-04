
open ExtLib

let _ =
  let ic =
    try
      open_in Sys.argv.(1)
    with Invalid_argument _ -> stdin in
  let p = Cudf_parser.from_in_channel ic in
  let cudf = Cudf_parser.parse_cudf p in
    print_endline (dump cudf)
