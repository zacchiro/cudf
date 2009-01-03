let _ =
  let cudf = Cudf_parser.parse_cudf (open_in Sys.argv.(1)) in
    print_endline "All done."
