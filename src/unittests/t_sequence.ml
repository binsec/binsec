(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2024                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open OUnit2
open Sequence

let sequence_of_list l =
  List.fold_left (fun acc elt -> push_front elt acc) empty l

let list_of_sequence s = fold_backward (fun elt acc -> elt :: acc) s []
let list_of_seq s = Seq.fold_left (fun acc elt -> elt :: acc) [] s |> List.rev

let int_list_to_string l =
  Format.asprintf "[%a]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space Format.pp_print_int)
    l

let int_sequence_to_string l =
  let pp fmt = iter_forward (fun elt -> Format.fprintf fmt "%i " elt) in
  Format.asprintf "[%a]" pp l

let eq_list expected actual ctxt =
  assert_equal ~ctxt ~printer:int_list_to_string expected actual

let eq_sequence expected actual ctxt =
  assert_equal ~ctxt ~printer:int_sequence_to_string expected actual

(** compare the effect of supposedly equivalent functions on sequences and lists *)
let via_sequence name on_sequence on_list =
  QCheck.Test.make ~count:1000 ~name
    QCheck.(list small_nat)
    (fun l ->
      let via = l |> sequence_of_list |> on_sequence |> list_of_sequence in
      let direct = l |> on_list in
      direct = via)

(** checks that f @@ sequence_of_list is the identity *)
let check_id name f =
  QCheck.Test.make ~count:1000 ~name
    QCheck.(list small_nat)
    (fun l ->
      let via = l |> sequence_of_list |> f in
      l = via)

let tests = []
let qtests = []

(* actual tests start here *)

let sequence =
  empty |> push_front 1 |> push_front 2 |> push_front 3 |> push_front 4

let tests =
  ("list_of_sequence" >:: eq_list [ 1; 2; 3; 4 ] (list_of_sequence sequence))
  :: tests

let tests =
  ("sequence_of_list" >:: eq_sequence (sequence_of_list [ 1; 2; 3; 4 ]) sequence)
  :: tests

let qtests = via_sequence "identity" (fun x -> x) (fun x -> x) :: qtests

let qtests =
  via_sequence "map"
    (Sequence.map_forward (fun x -> x + 1))
    (List.map (fun x -> x + 1))
  :: qtests

let check_append =
  let name = "check_append" in
  QCheck.Test.make ~count:1000 ~name
    QCheck.(pair (list small_nat) (list small_nat))
    (fun (l1, l2) ->
      let s1 = sequence_of_list l1 in
      let s2 = sequence_of_list l2 in
      let res = append s1 s2 |> list_of_sequence in
      let direct = l2 @ l1 in
      let test = direct = res in
      if not test then
        Format.eprintf "Got %s\n Expected %s \n" (int_list_to_string res)
          (int_list_to_string direct);
      test)

let qtests = check_append :: qtests

let qtests =
  check_id "to_seq_forward" (fun s -> s |> to_seq_forward |> list_of_seq)
  :: qtests

(* run the tests *)
let () = run_test_tt_main ("Sequence" >::: tests)
let () = QCheck_runner.run_tests_main qtests
