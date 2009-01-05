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

let good_cudfs = [	(* CUDF whose parsing must suceed *)
]
let bad_cudfs = [	(* CUDF whose parsing must fail *)
]
let good_pkgs = [	(* universes whose parsing must suceed *)
  "conflict-comma-sep" ;
]
let bad_pkgs = [	(* universes whose parsing must fail *)
]

(** {6 OUnit helpers}
    i.e., missing stuff which should better be integrated into OUnit *)

let assert_no_exn f = assert_equal true (try f () ; true with _ -> false)

let assert_raises' ?(cmp = (=)) ~exn f =
  assert_equal true (try f () ; false with exn' -> cmp exn exn')

(** {6 Test builders} *)

let good_parse ~parse_fun fname = TestCase (fun _ ->
  let ic = open_in fname in
  let p = Cudf_parser.from_in_channel ic in
    assert_no_exn (fun () -> parse_fun p);
    Cudf_parser.close p;
    close_in ic)

let bad_parse ~parse_fun fname = TestCase (fun _ ->
  let ic = open_in fname in
  let p = Cudf_parser.from_in_channel ic in
    assert_raises'
      ~cmp:(fun e1 e2 ->
	      match e1, e2 with
		| Cudf_parser.Parse_error _, Cudf_parser.Parse_error _ -> true
		| _ -> false)
      ~exn:(Cudf_parser.Parse_error (0, ""))
      (fun () -> parse_fun p);
    Cudf_parser.close p;
    close_in ic)

(** {6 Test suites} *)

let good_cudf_parse_suite =
  "parsing of good CUDFs" >::: List.map
      (fun n -> n >: good_parse ~parse_fun:Cudf_parser.parse_cudf
	 (cudf_test_path n))
      good_cudfs

let bad_cudf_parse_suite =
  "parsing of bad CUDFs" >::: List.map
      (fun n -> n >: bad_parse ~parse_fun:Cudf_parser.parse_cudf
	 (cudf_test_path n))
      bad_cudfs

let good_pkgs_parse_suite =
  "parsing of good package universes" >::: List.map
      (fun n -> n >: good_parse ~parse_fun:Cudf_parser.parse_packages
	 (cudf_test_path n))
      good_pkgs

let bad_pkgs_parse_suite =
  "parsing of bad package universes" >::: List.map
      (fun n -> n >: bad_parse ~parse_fun:Cudf_parser.parse_packages
	 (cudf_test_path n))
      bad_pkgs

(** {6 Assemble and run tests} *)

let all =
  "all tests" >::: [
    good_cudf_parse_suite ;
    bad_cudf_parse_suite ;
    good_pkgs_parse_suite ;
    bad_pkgs_parse_suite ;
  ]

let _ = run_test_tt_main all

