(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2026                                               *)
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

module I = struct
  type t = int

  let compare i i' = i - i'
  let hash i = i land 0b11111
end

module H = Hmap.Make (I)
module M = Map.Make (I)

let check_equal m m' =
  let r = List.sort (fun (i, _) (i', _) -> I.compare i i') (H.bindings m) in
  let r' = M.bindings m' in
  List.equal ( = ) r r'

let test_create l =
  let m = List.fold_left (fun m (i, v) -> H.add i v m) H.empty l in
  let m' = List.fold_left (fun m (i, v) -> M.add i v m) M.empty l in
  check_equal m m'

let test_query (w, r) =
  let m = List.fold_left (fun m (i, v) -> H.add i v m) H.empty w in
  let m' = List.fold_left (fun m (i, v) -> M.add i v m) M.empty w in
  List.for_all
    (fun i ->
      match H.find i m with
      | exception Not_found -> not (M.mem i m')
      | v -> (
          match M.find i m' with v' -> v = v' | exception Not_found -> false))
    r

let test_freeze (l1, l2) =
  let m1 = List.fold_left (fun m (i, v) -> H.add i v m) H.empty l1 in
  let m1' = List.fold_left (fun m (i, v) -> M.add i v m) M.empty l1 in

  H.freeze m1;

  let m2 = List.fold_left (fun m (i, v) -> H.add i v m) m1 l2 in
  let m2' = List.fold_left (fun m (i, v) -> M.add i v m) m1' l2 in

  check_equal m1 m1' && check_equal m2 m2'

let test_union (l1, (l2, l3)) =
  let m1 = List.fold_left (fun m (i, v) -> H.add i v m) H.empty l1 in
  let m1' = List.fold_left (fun m (i, v) -> M.add i v m) M.empty l1 in

  let m2 = List.fold_left (fun m (i, v) -> H.add i v m) H.empty l2 in
  let m2' = List.fold_left (fun m (i, v) -> M.add i v m) M.empty l2 in

  let m3 = List.fold_left (fun m (i, v) -> H.add i v m) H.empty l3 in
  let m3' = List.fold_left (fun m (i, v) -> M.add i v m) M.empty l3 in

  let m4 = H.union_eq (Fun.const ( + )) m1 m2 in
  let m4' =
    M.merge
      (fun _ x y ->
        match x with
        | None -> y
        | Some a -> ( match y with None -> x | Some b -> Some (a + b)))
      m1' m2'
  in

  let m5 = H.union_eq (Fun.const ( - )) m4 m3 in
  let m5' =
    M.merge
      (fun _ x y ->
        match x with
        | None -> y
        | Some a -> ( match y with None -> x | Some b -> Some (a - b)))
      m4' m3'
  in

  check_equal m5 m5'

let () = QCheck_base_runner.set_seed 26186641
let count = 10000 (* 0 *)

let create =
  QCheck.Test.make ~count ~name:"create"
    (QCheck.set_print
       (QCheck.Print.list (QCheck.Print.pair QCheck.Print.int QCheck.Print.int))
       (QCheck.list (QCheck.pair QCheck.int_pos QCheck.int_pos)))
    (fun l -> test_create l)

let query =
  QCheck.Test.make ~count ~name:"query"
    (QCheck.set_print
       (QCheck.Print.pair
          (QCheck.Print.list
             (QCheck.Print.pair QCheck.Print.int QCheck.Print.int))
          (QCheck.Print.list QCheck.Print.int))
       (QCheck.pair
          (QCheck.list (QCheck.pair QCheck.int_pos QCheck.nat_small))
          (QCheck.list QCheck.int_pos)))
    (fun (w, r) -> test_query (w, r))

let freeze =
  QCheck.Test.make ~count ~name:"freeze"
    (QCheck.set_print
       (QCheck.Print.pair
          (QCheck.Print.list
             (QCheck.Print.pair QCheck.Print.int QCheck.Print.int))
          (QCheck.Print.list
             (QCheck.Print.pair QCheck.Print.int QCheck.Print.int)))
       (QCheck.pair
          (QCheck.list (QCheck.pair QCheck.int_pos QCheck.nat_small))
          (QCheck.list (QCheck.pair QCheck.int_pos QCheck.nat_small))))
    (fun (l1, l2) -> test_freeze (l1, l2))

let union_eq =
  QCheck.Test.make ~count ~name:"union_eq"
    (QCheck.set_print
       (QCheck.Print.pair
          (QCheck.Print.list
             (QCheck.Print.pair QCheck.Print.int QCheck.Print.int))
          (QCheck.Print.pair
             (QCheck.Print.list
                (QCheck.Print.pair QCheck.Print.int QCheck.Print.int))
             (QCheck.Print.list
                (QCheck.Print.pair QCheck.Print.int QCheck.Print.int))))
       (QCheck.pair
          (QCheck.list (QCheck.pair QCheck.int_pos QCheck.nat_small))
          (QCheck.pair
             (QCheck.list (QCheck.pair QCheck.int_pos QCheck.nat_small))
             (QCheck.list (QCheck.pair QCheck.int_pos QCheck.nat_small)))))
    (fun (l1, (l2, l3)) -> test_union (l1, (l2, l3)))

let () = QCheck_base_runner.run_tests_main [ create; query; freeze; union_eq ]

(* let () =
   let open OUnit2 in
   run_test_tt_main
     ("tests"
     >::: List.map QCheck_ounit.to_ounit2_test
            [ create; query; freeze; union_eq ]) *)
