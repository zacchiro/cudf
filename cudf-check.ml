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

open Cudf
open Cudf_checker

let cudf_arg = ref ""
let univ_arg = ref ""
let sol_arg = ref ""
let dump_arg = ref false

let cudf = ref None
let univ = ref None
let sol = ref None

let arg_spec = [
  "-cudf", Arg.Set_string cudf_arg, "parse the given CUDF (universe + request)" ;
  "-univ", Arg.Set_string univ_arg, "parse the given package universe" ;
  "-sol", Arg.Set_string sol_arg, "parse the given problem solution" ;
  "-dump", Arg.Set dump_arg, "dump parse results to standard output" ;
]

let usage_msg =
"Usage: cudf-check [OPTION...]
In particular:
  cudf-check -cudf FILE               validate CUDF
  cudf-check -cudf FILE -sol FILE     validate CUDF and its solution
  cudf-check -univ FILE               validate package universe (no request)
Options:"

let die_usage () = Arg.usage arg_spec usage_msg ; exit 2

let print_inst_info inst =
  match is_consistent inst with
    | true, _ -> printf "installation: consistent\n%!"
    | false, Some r ->
	printf "installation: broken (reason: %s)\n%!"
	  (explain_reason (r :> bad_solution_reason))
    | _ -> assert false

let print_cudf cudf =
  if !dump_arg then
    print_endline (Cudf_printer.string_of_cudf cudf)

let print_sol_info inst sol =
  match is_solution inst sol with
    | true, _ -> printf "is_solution: true\n%!"
    | false, rs ->
	printf "is: solution: false (reason: %s)\n%!"
	  (String.concat "; " (List.map explain_reason rs))

let main () =
  let load_univ p =
    match Cudf_parser.load p with
	univ, None -> univ
      | _, Some _ ->
	  eprintf "Error: unexpected problem description item.\n%!";
	  exit 1
  in
  if !cudf_arg <> "" then begin
    try
      let p = Cudf_parser.from_in_channel (open_in !cudf_arg) in
	eprintf "loading CUDF ...\n%!";
	(match Cudf_parser.load p with
	     univ, None -> 
	       eprintf "Error: problem description item.\n%!";
	       exit 1
	   | univ, Some req -> cudf := Some (univ, req))
    with
	Cudf_parser.Parse_error _
      | Cudf.Constraint_violation _ as exn ->
	  eprintf "Error while loading CUDF from %s: %s\n%!"
	    !cudf_arg (Printexc.to_string exn);
	  exit 1
  end;
  if !univ_arg <> "" then begin
    try
      let p = Cudf_parser.from_in_channel (open_in !univ_arg) in
	eprintf "loading package universe ...\n%!";
	univ := Some (load_univ p)
    with
	Cudf_parser.Parse_error _
      | Cudf.Constraint_violation _ as exn ->
	  eprintf "Error while loading universe from %s: %s\n%!"
	    !univ_arg (Printexc.to_string exn);
	  exit 1
  end;
  if !sol_arg <> "" then begin
    try
      let p = Cudf_parser.from_in_channel (open_in !sol_arg) in
	eprintf "loading solution ...\n%!";
	sol := Some (load_univ p)
    with
	Cudf_parser.Parse_error _
      | Cudf.Constraint_violation _ as exn ->
	  eprintf "Error while loading solution from %s: %s\n%!"
	    !sol_arg (Printexc.to_string exn);
	  exit 1
  end;
  match !cudf, !univ, !sol with
    | Some cudf, None, None ->
	print_inst_info (fst cudf);
	print_cudf cudf
    | Some cudf, None, Some sol ->
	print_inst_info (fst cudf);
	print_sol_info cudf sol;
	print_cudf cudf
    | None, Some univ, None ->
	print_inst_info univ
    | _ -> die_usage ()

let _ = 
  Arg.parse arg_spec ignore usage_msg;
  main()
