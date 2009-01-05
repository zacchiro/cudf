(*****************************************************************************)
(*  libCUDF - CUDF (Common Upgrade Description Format) manipulation library  *)
(*  Copyright (C) 2009  Stefano Zacchiroli <zack@pps.jussieu.fr>             *)
(*                                                                           *)
(*  This program is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU General Public License as published by     *)
(*  the Free Software Foundation, either version 3 of the License, or (at    *)
(*  your option) any later version.                                          *)
(*                                                                           *)
(*  This program is distributed in the hope that it will be useful, but      *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of               *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU        *)
(*  General Public License for more details.                                 *)
(*                                                                           *)
(*  You should have received a copy of the GNU General Public License        *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*****************************************************************************)

open ExtLib
open Printf

open Cudf

let cudf_arg = ref ""
let univ_arg = ref ""
let sol_arg = ref ""
let dump_arg = ref false

let cudf = ref None
let univ = ref None
let sol = ref None

let arg_spec = [
  "-cudf", Arg.Set_string cudf_arg,
    "parse the given CUDF (universe + request)" ;
  "-univ", Arg.Set_string univ_arg, "parse the given package universe" ;
  "-sol", Arg.Set_string sol_arg, "parse the given problem solution" ;
  "-dump", Arg.Set dump_arg, "dump parse results to standard output" ;
]

let print_univ_info univ =
  if Cudf_checker.is_healthy univ then
    printf "universe: healthy\n%!"
  else
    printf "universe: broken\n%!"

let print_cudf_info (univ, _req) = print_univ_info univ

let print_cudf cudf =
  (* TODO dummy implementation, should pretty print here ... *)
  if !dump_arg then
    print_endline (dump cudf)

let print_sol_info cudf sol =
  printf "is_solution: %b\n%!" (Cudf_checker.is_solution cudf sol)

let main () =
  if !cudf_arg <> "" then begin
    try
      let p = Cudf_parser.from_in_channel (open_in !cudf_arg) in
	eprintf "parsing CUDF ...\n%!";
	cudf := Some (Cudf_parser.load_cudf p)
    with
	Cudf_parser.Parse_error _
      | Cudf.Constraint_violation _ as exn ->
	  eprintf "Error while loading CUDF from %s: %s\n%!"
	    !cudf_arg (Printexc.to_string exn)
  end;
  if !univ_arg <> "" then begin
    try
      let p = Cudf_parser.from_in_channel (open_in !univ_arg) in
	eprintf "parsing package universe ...\n%!";
	univ := Some (Cudf_parser.load_universe p)
    with
	Cudf_parser.Parse_error _
      | Cudf.Constraint_violation _ as exn ->
	  eprintf "Error while loading universe from %s: %s\n%!"
	    !univ_arg (Printexc.to_string exn)
  end;
  if !sol_arg <> "" then begin
    try
      let p = Cudf_parser.from_in_channel (open_in !sol_arg) in
	eprintf "parsing solution ...\n%!";
	sol := Some (Cudf_parser.load_universe p)
    with
	Cudf_parser.Parse_error _
      | Cudf.Constraint_violation _ as exn ->
	  eprintf "Error while loading solution from %s: %s\n%!"
	    !sol_arg (Printexc.to_string exn)
  end;
  match !cudf, !univ, !sol with
    | Some cudf, None, None ->
	print_cudf_info cudf;
	print_cudf cudf
    | Some cudf, None, Some sol ->
	print_cudf_info cudf;
	print_sol_info cudf sol;
	print_cudf cudf
    | None, Some univ, None ->
	print_univ_info univ
    | _ -> failwith "Unsupported argument combination"

let _ = 
  Arg.parse arg_spec (fun _ -> ())
"Usage: cudf-check [OPTION...]
In particular:
  cudf-check -cudf FILE               validate CUDF
  cudf-check -cudf FILE -sol FILE     validate CUDF and its solution
  cudf-check -univ FILE               validate package universe (no request)
Options:";
  main()
