
open ExtLib

let _ =
  let p = Cudf_parser.from_in_channel stdin in
  let stanza = Cudf_parser.parse_stanza p in
     print_endline (dump stanza)
