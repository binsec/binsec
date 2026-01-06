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

module M = Map.Make (Z)
module S = Set.Make (Z)

let pp_print_list =
  Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf (i, v) ->
      Format.fprintf ppf "(%a -> %d)" Z.pp_print i v)

let pp_print_list2 =
  Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf (i, v) ->
      Format.fprintf ppf "(%a -> %c)" Z.pp_print i v)

let () = ignore pp_print_list
let debug = ref false

let rec unroll lo hi elt elts =
  if Z.equal lo hi then (lo, elt) :: elts
  else unroll lo (Z.pred hi) elt ((hi, elt) :: elts)

let rec unroll2 lo hi elt elts =
  if Z.gt lo hi then elts
  else
    unroll2 lo (Z.pred hi) elt
      (M.update hi
         (function
           | None -> Some elt | Some elt' -> Some (UnionFind.merge max elt elt'))
         elts)

let rec unroll3 lo hi elts =
  if Z.gt lo hi then elts else unroll3 lo (Z.pred hi) (S.add hi elts)

let full_bindings =
  Zmap.rev_fold
    (fun (Item { lo; hi; elt } : int Zmap.item) elts -> unroll lo hi elt elts)
    []

let full_bindings2 m =
  List.map (fun (i, v) -> (i, UnionFind.get v)) (M.bindings m)

let check_equal m m' =
  let r = full_bindings m in
  let r' = full_bindings2 m' in
  if !debug then
    Format.eprintf "@[<v>Witness: %a@ Result: %a@]@." pp_print_list r'
      pp_print_list r;
  r = r'

exception Error

let check_compact m =
  match
    Zmap.fold
      (fun (Item { lo = lo1; elt = elt1; _ } as x : int Zmap.item)
           (pred : int Zmap.item option) ->
        match pred with
        | None -> Some x
        | Some (Item { hi = hi0; elt = elt0; _ }) ->
            if elt0 = elt1 && Z.equal (Z.succ hi0) lo1 then raise_notrace Error
            else Some x)
      None m
  with
  | None | Some _ -> true
  | exception Error -> false

external cast_items : (int * (int * int)) list -> (Z.t * (Z.t * int)) list
  = "%identity"

external cast_index : int list -> Z.t list = "%identity"

let union : int Zmap.item -> int Zmap.item -> int =
 fun (Item { elt = elt0; _ }) (Item { elt = elt1; _ }) -> max elt0 elt1

let stich : int Zmap.item -> int Zmap.item -> int option =
 fun (Item { elt = elt0; _ }) (Item { elt = elt1; _ }) ->
  if elt0 = elt1 then Some elt0 else None

let of_items =
  List.fold_left
    (fun m (i, (o, v)) ->
      Zmap.union_eq ~stich union (Zmap.singleton ~lo:i ~hi:(Z.add i o) v) m)
    Zmap.empty

let of_items2 =
  List.fold_left
    (fun m (i, (o, v)) ->
      let v = UnionFind.make v in
      unroll2 i (Z.add i o) v m)
    M.empty

let of_items3 =
  List.fold_left (fun s (i, (o, _)) -> unroll3 i (Z.add i o) s) S.empty

let test_create l =
  let l = cast_items l in

  (* Format.eprintf "@[<v>Orig:@ @[<h>%a@]@]@ @." pp_print_list l; *)
  let m = of_items l in

  let m' = of_items2 l in

  check_equal m m' && check_compact m

let () = ignore test_create

let test_query (w, r) =
  let w = cast_items w and r = cast_index r in

  (* Format.eprintf "@[<v>Write:@ @[<h>%a@]@]@ @." pp_print_list w;
   * Format.eprintf "@[<v>Read:@ @[<h>%a@]@]@ @." pp_print_list r; *)
  let m = of_items w in
  let m' = of_items2 w in
  List.for_all
    (fun i ->
      let v0 =
        match Zmap.find i m with
        | exception Not_found -> None
        | Item { elt; _ } -> Some elt
      in
      let v1 = Option.map UnionFind.get (M.find_opt i m') in
      (* Format.eprintf "@[<v>Witness: %S@ Result: %S@]@." r' r; *)
      v0 = v1)
    r

let () = ignore test_query

let test_disjoint (l0, l1) =
  let l0 = cast_items l0 and l1 = cast_items l1 in
  let m0 = of_items l0 and m1 = of_items l1 in
  let s0 = of_items3 l0 and s1 = of_items3 l1 in
  let r = Zmap.disjoint m0 m1 and r' = S.disjoint s0 s1 in
  if !debug then (
    Format.eprintf "@[<v>Bindings0: %a@ Bindings1: %a@]@." pp_print_list
      (full_bindings m0) pp_print_list (full_bindings m1);
    Format.eprintf "@[<v>Witness: %b@ Result: %b@]@." r' r);
  r = r'

let () = ignore test_disjoint

let test_substract (l0, l1) =
  let l0 = cast_items l0 and l1 = cast_items l1 in
  let m0 = of_items l0 and m1 = of_items l1 in
  let m0' = of_items2 l0 and m1' = of_items2 l1 in
  let r = Zmap.substract m0 m1
  and r' = M.fold (fun i _ m' -> M.remove i m') m1' m0' in
  check_equal r r'

let () = ignore test_substract

let test_fold_inter (l0, l1) =
  let l0 = cast_items l0 and l1 = cast_items l1 in
  let m0 = of_items l0 and m1 = of_items l1 in
  match
    Zmap.fold_inter
      (fun (Item { lo = lo0; hi = hi0; elt = elt0; _ } : int Zmap.item)
           (Item { lo = lo1; hi = hi1; elt = elt1; _ } : int Zmap.item) acc ->
        if Z.lt hi0 lo1 || Z.lt hi1 lo0 then raise Error
        else
          Z.add
            (Z.mul
               (Z.succ (Z.sub (min hi0 hi1) (max lo0 lo1)))
               (Z.add (Z.of_int elt0) (Z.of_int elt1)))
            acc)
      Z.zero m0 m1
  with
  | exception Error -> false
  | r ->
      let m0' = of_items2 l0 and m1' = of_items2 l1 in
      let r' =
        M.fold
          (fun i x acc ->
            match M.find i m0' with
            | exception Not_found -> acc
            | x' ->
                Z.add
                  (Z.add
                     (Z.of_int (UnionFind.get x))
                     (Z.of_int (UnionFind.get x')))
                  acc)
          m1' Z.zero
      in
      r = r'

let () = ignore test_fold_inter

let benchmark_zmap l =
  ignore
  @@ List.fold_left
       (fun m (i, (o, v)) ->
         Zmap.union_eq union (Zmap.singleton ~lo:i ~hi:(Z.add i o) v) m)
       Zmap.empty (cast_items l);
  true

external cast_objects : (int * string) list -> (Z.t * string) list = "%identity"

let unroll4 =
  let rec loop x s l n =
    if n < 0 then l else loop (Z.pred x) s ((x, String.get s n) :: l) (n - 1)
  in
  fun x s l -> loop x s l (String.length s - 1)

let full_bindings2 =
  Zmap.rev_fold
    (fun (Item { hi; elt; _ } : string Zmap.item) elts -> unroll4 hi elt elts)
    []

let crop ~lo ~hi t =
  let lo = Z.to_int lo and hi = Z.to_int hi in
  (* Format.eprintf "crop %S{%d, %d}@." t lo hi; *)
  String.sub t lo (hi - lo + 1)

let of_items4 =
  List.fold_left
    (fun m (i, v) ->
      QCheck.assume (String.length v > 0);
      Zmap.union_left ~crop
        (Zmap.singleton ~lo:i ~hi:(Z.add i (Z.of_int (String.length v - 1))) v)
        m)
    Zmap.empty

let of_items5 =
  List.fold_left
    (fun m (i, v) ->
      let l' = unroll4 (Z.add i (Z.of_int (String.length v - 1))) v [] in
      List.fold_left (fun m (i, v) -> M.add i v m) m l')
    M.empty

let test_create2 l =
  let l = cast_objects l in

  (* Format.eprintf "@[<v>Orig:@ @[<h>%a@]@]@ @." pp_print_list l; *)
  let m = of_items4 l in
  let r = full_bindings2 m in

  let m' = of_items5 l in
  let r' = M.bindings m' in

  if !debug then
    Format.eprintf "@[<v>Witness: %a@ Result: %a@]@." pp_print_list2 r'
      pp_print_list2 r;
  r = r'

let () = ignore test_create2

let test_create3 (l0, l1) =
  let l0 = cast_items l0 and l1 = cast_items l1 in

  (* Format.eprintf "@[<v>Orig:@ @[<h>%a@]@]@ @." pp_print_list l; *)
  let m0 = of_items l0 and m1 = of_items l1 in

  let m0' = of_items2 l0 and m1' = of_items2 l1 in

  let m = Zmap.union_eq union m0 m1
  and m' =
    M.fold
      (fun i v m ->
        M.update i
          (function
            | None -> Some v | Some v' -> Some (UnionFind.merge max v v'))
          m)
      m0' m1'
  in

  check_equal m m' && check_compact m

let () = ignore test_create3

let test_create4 (l0, l1) =
  let l0 = cast_objects l0 and l1 = cast_objects l1 in

  (* Format.eprintf "@[<v>Orig:@ @[<h>%a@]@]@ @." pp_print_list l; *)
  let m0 = of_items4 l0 and m1 = of_items4 l1 in
  let m = Zmap.union_left ~crop m0 m1 in
  let r = full_bindings2 m in

  let m0' = of_items5 l0 and m1' = of_items5 l1 in
  let m' = M.union (fun _ v _ -> Some v) m0' m1' in
  let r' = M.bindings m' in

  if !debug then
    Format.eprintf "@[<v>Witness: %a@ Result: %a@]@." pp_print_list2 r'
      pp_print_list2 r;
  r = r'

let () = ignore test_create4

let test_overlap l =
  let check : (int * int) * (int * int) -> bool =
   fun ((lo0, o0), (lo1, o1)) ->
    let lo0 = Z.of_int lo0
    and hi0 = Z.of_int (lo0 + o0)
    and lo1 = Z.of_int lo1
    and hi1 = Z.of_int (lo1 + o1) in
    let i0 = { Interval.lo = lo0; hi = hi0 }
    and i1 = { Interval.lo = lo1; hi = hi1 } in
    Z.lt hi1 lo0 || Z.lt hi0 lo1
    ||
    (if !debug then
       Format.eprintf "{%a..%a} {%a..%a} -> " Z.pp_print lo0 Z.pp_print hi0
         Z.pp_print lo1 Z.pp_print hi1;

     let check_endpoint : Z.t -> bool =
      fun ep -> ep = lo0 || ep = hi0 || ep = lo1 || ep = hi1
     in
     let check_endpoints_2 : Z.t -> Z.t -> bool =
      fun ep0 ep1 -> check_endpoint ep0 && check_endpoint ep1 && Z.leq ep0 ep1
     and check_endpoints_3 : Z.t -> Z.t -> Z.t -> bool =
      fun ep0 ep1 ep2 ->
       check_endpoint ep0 && check_endpoint ep2 && Z.lt ep0 ep1 && Z.leq ep1 ep2
     and check_endpoints_4 : Z.t -> Z.t -> Z.t -> Z.t -> bool =
      fun ep0 ep1 ep2 ep3 ->
       check_endpoint ep0 && check_endpoint ep3 && Z.lt ep0 ep1 && Z.lt ep1 ep2
       && Z.leq ep2 ep3
     in
     let r = Interval.overlap i0 i1 in
     if !debug then
       Format.eprintf "%a"
         (fun ppf (r : Z.t Interval.overlap) ->
           match r with
           | LRl_LRh (ep0, ep1) ->
               Format.fprintf ppf "LRl_LRh (%a, %a)" Z.pp_print ep0 Z.pp_print
                 ep1
           | LRl_Lh_Rh (ep0, ep1, ep2) ->
               Format.fprintf ppf "LRl_Lh_Rh (%a, %a, %a)" Z.pp_print ep0
                 Z.pp_print ep1 Z.pp_print ep2
           | LRl_Rh_Lh (ep0, ep1, ep2) ->
               Format.fprintf ppf "LRl_Rh_Lh (%a, %a, %a)" Z.pp_print ep0
                 Z.pp_print ep1 Z.pp_print ep2
           | Ll_Rl_LRh (ep0, ep1, ep2) ->
               Format.fprintf ppf "Ll_Rl_LRh (%a, %a, %a)" Z.pp_print ep0
                 Z.pp_print ep1 Z.pp_print ep2
           | Rl_Ll_LRh (ep0, ep1, ep2) ->
               Format.fprintf ppf "Rl_Ll_LRh (%a, %a, %a)" Z.pp_print ep0
                 Z.pp_print ep1 Z.pp_print ep2
           | Ll_Rl_Lh_Rh (ep0, ep1, ep2, ep3) ->
               Format.fprintf ppf "Ll_Rl_Lh_Rh (%a, %a, %a, %a)" Z.pp_print ep0
                 Z.pp_print ep1 Z.pp_print ep2 Z.pp_print ep3
           | Ll_Rl_Rh_Lh (ep0, ep1, ep2, ep3) ->
               Format.fprintf ppf "Ll_Rl_Rh_Lh (%a, %a, %a, %a)" Z.pp_print ep0
                 Z.pp_print ep1 Z.pp_print ep2 Z.pp_print ep3
           | Rl_Ll_Lh_Rh (ep0, ep1, ep2, ep3) ->
               Format.fprintf ppf "Rl_Ll_Lh_Rh (%a, %a, %a, %a)" Z.pp_print ep0
                 Z.pp_print ep1 Z.pp_print ep2 Z.pp_print ep3
           | Rl_Ll_Rh_Lh (ep0, ep1, ep2, ep3) ->
               Format.fprintf ppf "Rl_Ll_Rh_Lh (%a, %a, %a, %a)" Z.pp_print ep0
                 Z.pp_print ep1 Z.pp_print ep2 Z.pp_print ep3)
         r;
     let r =
       match r with
       | LRl_LRh (ep0, ep1) ->
           check_endpoints_2 ep0 ep1 && Z.equal lo0 lo1 && Z.equal hi0 hi1
       | LRl_Lh_Rh (ep0, ep1, ep2) ->
           check_endpoints_3 ep0 ep1 ep2 && Z.equal lo0 lo1 && Z.lt hi0 hi1
       | LRl_Rh_Lh (ep0, ep1, ep2) ->
           check_endpoints_3 ep0 ep1 ep2 && Z.equal lo0 lo1 && Z.lt hi1 hi0
       | Ll_Rl_LRh (ep0, ep1, ep2) ->
           check_endpoints_3 ep0 ep1 ep2 && Z.lt lo0 lo1 && Z.equal hi0 hi1
       | Rl_Ll_LRh (ep0, ep1, ep2) ->
           check_endpoints_3 ep0 ep1 ep2 && Z.lt lo1 lo0 && Z.equal hi0 hi1
       | Ll_Rl_Lh_Rh (ep0, ep1, ep2, ep3) ->
           check_endpoints_4 ep0 ep1 ep2 ep3 && Z.lt lo0 lo1 && Z.lt hi0 hi1
       | Ll_Rl_Rh_Lh (ep0, ep1, ep2, ep3) ->
           check_endpoints_4 ep0 ep1 ep2 ep3 && Z.lt lo0 lo1 && Z.lt hi1 hi0
       | Rl_Ll_Lh_Rh (ep0, ep1, ep2, ep3) ->
           check_endpoints_4 ep0 ep1 ep2 ep3 && Z.lt lo1 lo0 && Z.lt hi0 hi1
       | Rl_Ll_Rh_Lh (ep0, ep1, ep2, ep3) ->
           check_endpoints_4 ep0 ep1 ep2 ep3 && Z.lt lo1 lo0 && Z.lt hi1 hi0
     in
     if !debug then Format.eprintf " %b@." r;
     r)
  in

  List.for_all check l

let () = ignore test_overlap
let () = QCheck_base_runner.set_seed 26186641

let itervals_generator =
  QCheck.set_print
    (QCheck.Print.list (fun ((i, o), (i', o')) ->
         Format.sprintf "{%d..%d}, {%d..%d}" i (i + o) i' (i' + o')))
    (QCheck.list
       (QCheck.pair
          (QCheck.pair QCheck.nat_small QCheck.int_pos_small)
          (QCheck.pair QCheck.nat_small QCheck.int_pos_small)))

let () = ignore itervals_generator

let item_generator =
  QCheck.set_print
    (QCheck.Print.list (fun (i, (o, v)) ->
         Format.sprintf "{%d..%d}: %d" i (i + o) v))
    (QCheck.list
       (QCheck.pair QCheck.nat_small
          (QCheck.pair QCheck.int_pos_small QCheck.int)))

let () = ignore item_generator

let object_generator =
  QCheck.set_print
    (QCheck.Print.list
       (QCheck.Print.pair QCheck.Print.int (fun s -> Format.sprintf "%S" s)))
    (QCheck.list
       (QCheck.pair
          (QCheck.map (fun i -> i lsl 2) QCheck.nat_small)
          (QCheck.string_size_of (* (fun _ -> 4) *)
             (QCheck.gen (QCheck.int_range 1 16))
             (QCheck.gen QCheck.printable))))

let () = ignore object_generator

let () =
  if false && not !debug then
    QCheck_base_runner.run_tests_main
      [
        QCheck.Test.make ~count:1_000_000 ~name:"benchmark" item_generator
          benchmark_zmap;
      ]

let () = debug := false
let count = 10_000

let () =
  if not !debug then
    QCheck_base_runner.run_tests_main
      [
        QCheck.Test.make ~count ~name:"create" item_generator test_create;
        QCheck.Test.make ~count ~name:"create2" object_generator test_create2;
        QCheck.Test.make ~count ~name:"create3"
          (QCheck.pair item_generator item_generator)
          test_create3;
        QCheck.Test.make ~count ~name:"create4"
          (QCheck.pair object_generator object_generator)
          test_create4;
        QCheck.Test.make ~count ~name:"query"
          (QCheck.pair item_generator (QCheck.list QCheck.nat_small))
          test_query;
        QCheck.Test.make ~count ~name:"disjoint"
          (QCheck.pair item_generator item_generator)
          test_disjoint;
        QCheck.Test.make ~count ~name:"substract"
          (QCheck.pair item_generator item_generator)
          test_substract;
        QCheck.Test.make ~count ~name:"fold_inter"
          (QCheck.pair item_generator item_generator)
          test_fold_inter;
        QCheck.Test.make ~count ~name:"overlap" itervals_generator test_overlap;
      ]

let () =
  let ignore : bool -> unit = Format.eprintf "%b@." in
  if !debug then (
    ignore @@ test_create [ (2, (0, 0)); (0, (0, 0)); (1, (0, 0)) ];
    ignore @@ test_create [ (2, (0, 0)); (0, (0, 0)); (1, (0, -1)) ];
    ignore
    @@ test_disjoint ([ (0, (0, 0)); (3, (0, 0)) ], [ (1, (0, 0)); (2, (0, 1)) ]);
    ignore @@ test_substract ([ (0, (1, 0)) ], [ (0, (0, 0)) ]);
    ignore @@ test_create2 [ (36, "aa") ];
    ignore @@ test_create2 [ (0, "aaaaaaa"); (2, "bb") ];
    ignore @@ test_create2 [ (0, "aaaaaaa"); (2, "bb"); (5, "c") ];
    ignore @@ test_create4 ([ (36, "a") ], [ (28, "aaaaaaaaa") ]);
    ignore @@ test_overlap [ ((0, 1), (0, 0)) ])
