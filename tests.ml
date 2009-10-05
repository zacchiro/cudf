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
open OUnit
open Printf

open Cudf_types
open Cudf

let cudf_test_path name = sprintf "./tests/%s.cudf" name

let good_cudfs = [	(* CUDF whose parsing must suceed *)
  "adjacent-stanzas" ;
]
let bad_cudfs = [	(* CUDF whose parsing must fail *)
]
let good_pkgs = [	(* universes whose parsing must suceed *)
  "conflict-comma-sep" ;
  "plus-in-pkgname" ;
]
(* XXX: with the quoting mechanism this is not true anymore *)
let bad_pkgs = [	(* universes whose parsing must fail *)
  (* "weird-pkgname" ; *)

]
let good_prob_sol = [	(* pairs cudf/sol, with sol being a good solution *)
  "legacy", "legacy-sol" ;
  "fresher", "fresher-sol-good" ;
  "upgrade-singleton", "upgrade-singleton-sol-good" ;
  "keep", "keep-sol-good";
  "virt-upgrade", "virt-upgrade-sol-good";
]
let bad_prob_sol = [	(* pairs cudf/sol, with sol being a bad solution *)
  "fresher", "fresher-sol-bad" ;
  "upgrade-singleton", "upgrade-singleton-sol-bad" ;
  "keep", "keep-sol-bad";
  "keep", "keep-sol-bad2";
  "keep", "keep-sol-bad3";
  "virt-upgrade", "virt-upgrade-sol-bad";
  "virt-upgrade", "virt-upgrade-sol-bad2";
  "virt-upgrade", "virt-upgrade-sol-bad3";
]

(** {5 Helpers} *)

(** {6 OUnit helpers}
    i.e., missing stuff which should better be integrated into OUnit *)

let assert_no_exn f = assert_equal true (try f () ; true with _ -> false)

let assert_raises' ?(cmp = (=)) ~exn f =
  assert_equal true (try f () ; false with exn' -> cmp exn exn')

(** {6 CUDF helpers} *)

let parse_test ~parse_fun name =
  let ic = open_in (cudf_test_path name) in
  let p = Cudf_parser.from_in_channel ic in
  let out = parse_fun p in
  close_in ic;
  out

let parse_cudf_wrapper p =
  match Cudf_parser.parse p with
    | pre, pkgs, Some req -> pre, pkgs, req
    | pre, pkgs, None -> raise (Cudf_parser.Parse_error (-1, ""))
let parse_pkgs_wrapper p =
  match Cudf_parser.parse p with
    | pre, pkgs, Some req -> raise (Cudf_parser.Parse_error (-1, ""))
    | pre, pkgs, None -> pkgs
let load_cudf_wrapper p =
  match Cudf_parser.load p with
    | pre, pkgs, Some req -> pre, pkgs, req
    | pre, pkgs, None -> raise (Cudf_parser.Parse_error (-1, ""))
let load_pkgs_wrapper p =
  match Cudf_parser.load p with
    | pre, pkgs, Some req -> raise (Cudf_parser.Parse_error (-1, ""))
    | pre, pkgs, None -> pkgs

let parse_cudf_test = parse_test ~parse_fun:parse_cudf_wrapper
let parse_pkgs_test = parse_test ~parse_fun:parse_pkgs_wrapper
let load_cudf_test = parse_test ~parse_fun:load_cudf_wrapper
let load_univ_test = parse_test ~parse_fun:load_pkgs_wrapper

(** {5 Test builders} *)

let good_parse ~parse_fun name = TestCase (fun _ ->
  assert_no_exn (fun () -> parse_test ~parse_fun name))

let bad_parse ~parse_fun name = TestCase (fun _ ->
  assert_raises'
    ~cmp:(fun e1 e2 ->
	    match e1, e2 with
	      | Cudf_parser.Parse_error _, Cudf_parser.Parse_error _ -> true
	      | _ -> false)
    ~exn:(Cudf_parser.Parse_error (0, ""))
    (fun () -> parse_test ~parse_fun name))

let good_solution prob_name sol_name = TestCase (fun _ ->
  let (_,univ,req), sol = load_cudf_test prob_name, load_univ_test sol_name in
    sprintf "problem with correct solution: (%s,%s)" prob_name sol_name @?
    fst (Cudf_checker.is_solution (univ,req) sol))

let bad_solution prob_name sol_name = TestCase (fun _ ->
  let (_,univ,req), sol = load_cudf_test prob_name, load_univ_test sol_name in
    sprintf "problem with correct solution: (%s,%s)" prob_name sol_name @?
    not (fst (Cudf_checker.is_solution (univ,req) sol)))

(** {5 Test suites} *)

(** {6 Big suites} *)

let good_cudf_parse_suite =
  "parsing of good CUDFs" >::: List.map
      (fun n -> n >: good_parse ~parse_fun:parse_cudf_wrapper n)
      good_cudfs

let bad_cudf_parse_suite =
  "parsing of bad CUDFs" >::: List.map
      (fun n -> n >: bad_parse ~parse_fun:parse_cudf_wrapper n)
      bad_cudfs

let good_pkgs_parse_suite =
  "parsing of good package universes" >::: List.map
      (fun n -> n >: good_parse ~parse_fun:parse_pkgs_wrapper n)
      good_pkgs

let bad_pkgs_parse_suite =
  "parsing of bad package universes" >::: List.map
      (fun n -> n >: bad_parse ~parse_fun:parse_pkgs_wrapper n)
      bad_pkgs

(** {6 Regression tests} *)

let or_dep =
  "disjunctive dependencies" >:: (fun () ->
    assert_equal
      (lookup_package (load_univ_test "or-dep") ("electric-engine", 1)).depends
      [["solar-collector", None; "huge-battery", None]])

let parse_reg_suite =
  "regression tests - parsing" >::: [
    or_dep ;
  ]

(** {6 New feature tests}
    i.e., kinda test-driven development *)

let status_filtering =
  "status projection" >:: (fun () ->
    "status projection returned an \"installed: false\" package" @?
      let _, univ, _ = load_cudf_test "legacy" in
      List.for_all
        (fun { installed = i } -> i)
        (get_packages (status univ)))

let inst_version_lookup =
  "lookup installed versions" >:: (fun () ->
    let univ = load_univ_test "multi-versions" in
    let versions pkg = List.map (fun p -> p.version) (get_installed univ pkg) in
      assert_equal (List.sort (versions "gasoline-engine")) [1; 2];
      assert_equal (versions "battery") [3];
      assert_equal (versions "not-installed") [];
      assert_equal (versions "not-existent") [])

let mem_installed =
  "check whether an installation satisfy a package constraint" >:: (fun () ->
    let _, univ, _ = load_cudf_test "legacy" in
    let mem = mem_installed ~include_features:true univ in
    let mem' = mem_installed ~include_features:false univ in
      "'car' unsatisfied" @? mem ("car", None);
      "'car = 1' unsatisfied" @? mem ("car", Some (`Eq, 1));
      "'car > 1' satisfied'" @? not (mem ("car", Some (`Gt, 1)));
      "'car >= 1' unsatisfied" @? mem ("car", Some (`Leq, 1));
      "'engine' unsatisfied w features" @? mem ("engine", None);
      "'engine' satisfied w/o features" @? not (mem' ("engine", None));
  )

let satisfy_formula =
  "check formula satisfaction" >:: (fun () ->
    let _, univ, _ = load_cudf_test "legacy" in
    let sat f = fst (Cudf_checker.satisfy_formula univ f) in
      "true unsatisfied (WTF?)" @? sat [];
      "conjunction unsatisfied" @? sat [["battery", None]; ["wheel", None]];
      "disjunction unsatisfied" @?
	sat [["solar-collector", None; "wheel", None]];
      "unsat formula satisfied" @?
	not (sat [["wheel", Some (`Gt, 2); "tire", None]]);
  )

let disjoint =
  "check package disjunction (i.e., conflicts)" >:: (fun () ->
    let _, univ, _ = load_cudf_test "legacy" in
    let disj ps = fst (Cudf_checker.disjoint univ ps) in
      "missing package reported as existing" @? disj ["fubar", None];
      "undetected conflict" @? not (disj ["door", Some (`Eq, 1)]);
      "undetected partial conflict" @?
	not (disj ["door", Some (`Gt, 1); "turbo", None]);
  )

let self_conflicts =
  "check self-conflicts" >:: (fun () ->
    let consist u = fst (Cudf_checker.is_consistent u) in
      "direct self-conflict" @? consist (load_univ_test "direct-self-conflict");
      "indirect self-conflict" @?
	consist (load_univ_test "indirect-self-conflict"))

let consistency =
  "check universe consistency" >::: [
    "legacy example consistency" >:: (fun () ->
      let _, univ, _ = load_cudf_test "legacy" in
	"inconsistent legacy example" @? fst (Cudf_checker.is_consistent univ))
  ]

let univ_sizes =
  let _, univ, _ = load_cudf_test "legacy" in
    "check universe size measuring" >::: [
      "total size" >:: (fun () -> assert_equal (universe_size  univ) 20);
      "installed size" >:: (fun () -> assert_equal (installed_size  univ) 6);
    ]

let good_solution_suite = "good solutions" >:::
  List.map (fun (prob, sol) -> good_solution prob sol) good_prob_sol

let bad_solution_suite = "bad solutions" >:::
  List.map (fun (prob, sol) -> bad_solution prob sol) bad_prob_sol

let feature_suite =
  "new feature tests" >::: [
    status_filtering ;
    inst_version_lookup ;
    mem_installed ;
    satisfy_formula ;
    disjoint ;
    self_conflicts ;
    consistency ;
    univ_sizes ;
  ]

let test_encode =
  "encode" >:: (fun () ->
    let s = "@/bin/*-+" in
    assert_equal (Cudf_types.encode s) "@/bin/%2a-+"
  )
;;

let test_decode =
  "encode" >:: (fun () ->
    let s = "@/bin/%2a-+" in
    assert_equal (Cudf_types.decode s) "@/bin/*-+"
  )
;;

let encoding_suite =
  "encoding / decoding tests" >::: [
    test_encode;
    test_decode
  ]

(** {5 Assemble and run tests} *)

let all =
  "all tests" >::: [
    good_cudf_parse_suite ;
    bad_cudf_parse_suite ;
    good_pkgs_parse_suite ;
    bad_pkgs_parse_suite ;
    good_solution_suite ;
    bad_solution_suite ;
    parse_reg_suite ;
    feature_suite ;
    encoding_suite ;
  ]

