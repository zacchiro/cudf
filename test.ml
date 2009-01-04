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

open OUnit
open Printf

let cudf_test_path name = sprintf "./tests/%s.cudf" name

let good_cudf_docs =	(* CUDF whose parsing must suceed *)
  [ "conflict-comma-sep" ;
  ]

let bad_cudf_docs =	(* CUDF whose parsing must fail *)
  []

let assert_no_exn f arg = assert_equal true (try f arg ; true with _ -> false)

let good_parse_suite =
  let good_parse fname = TestCase (fun _ ->
    let ic = open_in fname in
    let p = Cudf_parser.from_in_channel ic in
      assert_no_exn Cudf_parser.parse_cudf p;
      Cudf_parser.close p;
      close_in ic)
  in
  let tests =
    List.map
      (fun name ->
	 TestLabel (name, good_parse (cudf_test_path name)))
      good_cudf_docs
  in
    TestLabel ("parsing of good CUDFs", TestList tests)

let all =
  TestList [
    good_parse_suite
  ]

let _ = run_test_tt_main all

