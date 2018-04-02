(**************************************************************************)
(*  This file is part of Binsec.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
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

module LhsMap2 = Map.Make (
  struct
    type t = Dba.lhs * Dba.lhs
    let compare = Pervasives.compare
  end
)

(* Debug level *)
let level = 3


module PaMake (Val : Ai_sigs.Domain) = struct

  type t = data
  and data = (int * (Dba.lhs option) * (Val.t option)) Basic_types.Int.Map.t

  let pp ppf t =
    let open Format in
    fprintf ppf "@[<hov 0>Map:@ ";
    Basic_types.Int.Map.iter
      (fun key -> function
         | i, Some lhs, Some v ->
           fprintf ppf "%d -> (%d, %a, %s);@ "
            key i
            Dba_printer.Ascii.pp_lhs lhs (Val.to_string v)
        | i, Some lhs, None ->
          fprintf ppf "%d -> (%d, %a, _);@ "
            key i
            Dba_printer.Ascii.pp_lhs lhs
        | i, None, Some v ->
          fprintf ppf "%d -> (%d, _, %s);@ " key i (Val.to_string v)
        | _, None, None -> fprintf ppf ""
      ) t;
    fprintf ppf "@]"

  let to_string t = 
    pp Format.str_formatter t;
    Format.flush_str_formatter ()
    
  let _print a = Logger.debug "%s" (to_string a)

  let create () = Basic_types.Int.Map.empty

  let init (lhs, abs_v) = Basic_types.Int.Map.add 0 (0, lhs, abs_v) Basic_types.Int.Map.empty

  let get a i =
    try Basic_types.Int.Map.find i a
    with Not_found -> (i, None, None)

  let set a i (id, _lhs, _v) =
    let old_id, old_lhs, old_v =
      try Basic_types.Int.Map.find i a with Not_found -> failwith "union_find6 " in
    if old_id == id then a
    else Basic_types.Int.Map.add i (id, old_lhs, old_v) a


  let set2 a i (id, lhs, v) = Basic_types.Int.Map.add i (id, lhs, v) a

end



module Make (Val : Ai_sigs.Domain) =
struct
  module Pa = PaMake (Val)

  type record = {
    mutable father: Pa.t; (* mutable to allow path compression *)
    cur_id : int;
    map : int Dba_types.LValue.Map.t;
    c: Pa.t; (* ranks *)
  }

  type t = record option
  type data = Pa.data
  type thresholds = (int array * int array * int array * int array)

  let bottom = None

  open Format

  let pp ppf = function
    | None -> fprintf ppf "{}"
    | Some h ->
      fprintf ppf
        "@[<v 2>{@ \
         id = %d@ \
         c = %a@ \
         father = %a@ \
         nmap = %a@ \
         @]}"
        h.cur_id
        Pa.pp h.c
        Pa.pp h.father
        (fun ppf lvmap ->
           fprintf ppf "@[<hov 0>";
           Dba_types.LValue.Map.iter
             (fun lvalue id ->
                fprintf ppf "@[%a -> %d;@]@ "
                  Dba_printer.Ascii.pp_lhs lvalue id 
             ) lvmap;
           fprintf ppf "@]"
        ) h.map

  let to_string h = pp str_formatter h; flush_str_formatter ()

  
  let print h = Format.printf "%s" (to_string h)

  let assert_well_formed h =
    Dba_types.LValue.Map.iter (fun lhs id ->
      let (_, some_lhs, _some_v) = Pa.get h.father id in
      match some_lhs with
        None -> print (Some h); Format.printf "id = %d; lhs = None\n%!" id; assert (false)
      | Some flhs ->
        if Dba_types.LValue.equal lhs flhs
        then ()
        else (print (Some h);
              Format.printf "id = %d; lhs = %a"
                id Dba_printer.Ascii.pp_lhs flhs; assert (false))
    ) h.map


  let create () =
    let h =  { c = Pa.create ();
               cur_id = 0;
               map = Dba_types.LValue.Map.empty;
               father = Pa.create () }
    in
    assert_well_formed h;
    Some h


  let make n v =
    let h = { c = Pa.create ();
              cur_id = 1;
              map = Dba_types.LValue.Map.add n 0 Dba_types.LValue.Map.empty;
              father = Pa.init (Some n, Some v) }
    in
    assert_well_formed h;
    Some h


  let rec find_aux f i =
    let (fi, lhs, v) = Pa.get f i in
    if fi == i then f, (i, lhs, v)
    else
      let f, r = find_aux f fi in
      let f = Pa.set f i r in
      f, r


  let find_bis h x =
    assert_well_formed h;
    let i = Dba_types.LValue.Map.find x h.map in
    let f, rx = find_aux h.father i in
    h.father <- f;
    assert_well_formed h;
    rx



  let copy h = {h with father = h.father}



  let copy_equalities equalities =
    match equalities with
      None -> None
    | Some h -> Some (copy h)



  let _reset_equalities equalities1 equalities2 =
    match equalities1, equalities2 with
      None, None -> None
    | Some _, None | None, Some _ -> failwith "reset equalities"
    | Some h1, Some h2 -> h1.father <- h2.father; Some h1



  let _print_equalities equalities =
    match equalities with
      None -> Logger.debug ~level:3 "equalties vide"
    | Some equalities ->
      Logger.debug ~level:3 "%s" (to_string equalities)



  let find h x =
    match h with
      None -> None, None
    | Some h -> (
      try
        let _id, lhs, v = find_bis h x in
        lhs, v
      with Not_found -> None, None
    )



  let rec union h x_lhs y_lhs v =
    match h with
      None -> let h = create () in union h x_lhs y_lhs v
    | Some h ->
      let (idx, lhs_x, v_x), h =
        try find_bis h x_lhs, h
        with Not_found ->
          let map' = Dba_types.LValue.Map.add x_lhs h.cur_id h.map in
          let father = Pa.set2 h.father h.cur_id (h.cur_id, Some x_lhs, None) in
          h.father <- father;
          (h.cur_id, Some x_lhs, None), {h with cur_id = h.cur_id + 1; map = map'}
      in
      let (idy, lhs_y, v_y), h =
        try find_bis h y_lhs, h
        with Not_found ->
          let map' = Dba_types.LValue.Map.add y_lhs h.cur_id h.map in
          let father = Pa.set2 h.father h.cur_id (h.cur_id, Some y_lhs, None) in
          h.father <- father;
          (h.cur_id, Some y_lhs, None), {h with cur_id = h.cur_id + 1; map = map'}
      in
      let v = match v_x, v_y with
        | None, None -> v
        | Some vv, None | None, Some vv -> Val.meet v vv
        | Some v1, Some v2 -> Val.meet (Val.meet v1 v2) v
      in
      if idx != idy then begin
      (* c = Pa.set h.c rx ((fst rxc + 1), snd rxc); *)
        let h = { h with father = Pa.set2 h.father idx (idx, lhs_x, Some v) } in
        let h = { h with father = Pa.set2 h.father idy (idx, lhs_y, None) } in
        assert_well_formed h;
        Some h
      end else (
        let h = { h with father = Pa.set2 h.father idy (idy, lhs_y, Some v) } in
        assert_well_formed h;
        Some h
      )



  let join equalities1 equalities2 =
    match equalities1, equalities2 with
      None, eq -> eq
    | eq, None -> eq
    | Some eq1, Some _eq2 ->
      let (_, equalities) =
        Dba_types.LValue.Map.fold (fun lhs _id1 (acc_map2, acc_eq) ->
          let lhs_class1, v_class1 = find equalities1 lhs in
          let lhs_class2, v_class2 = find equalities2 lhs in
          match lhs_class1, lhs_class2 with
            None, _ | _, None -> (acc_map2, acc_eq)
          | Some lhs1, Some lhs2 ->
            try let lhs_class, v_class = LhsMap2.find (lhs1, lhs2) acc_map2 in
                (acc_map2, union acc_eq lhs lhs_class v_class)
            with Not_found ->
              let v1, v2 =
                match v_class1, v_class2 with
                | None, _ | _, None -> failwith "empty env class!"
                | Some v1, Some v2 -> (v1, v2)
              in
              let v = Val.join v1 v2 in
              let acc_map2 = LhsMap2.add (lhs1, lhs2) (lhs, v) acc_map2 in
              let acc_eq = union acc_eq lhs lhs v in
              (acc_map2, acc_eq)
        ) eq1.map (LhsMap2.empty, None)
      in equalities



  let widen equalities1 equalities2 thresholds =
    match equalities1, equalities2 with
      None, eq -> eq
    | eq, None-> eq
    | Some eq1, Some _eq2 ->
      let (_, equalities) =
        Dba_types.LValue.Map.fold (fun lhs _id1 (acc_map2, acc_eq) ->
          let lhs_class1, v_class1 = find equalities1 lhs in
          let lhs_class2, v_class2 = find equalities2 lhs in
          match lhs_class1, lhs_class2 with
            None, _ | _, None -> (acc_map2, acc_eq)
          | Some lhs1, Some lhs2 ->
            try let lhs_class, v_class = LhsMap2.find (lhs1, lhs2) acc_map2 in
                (acc_map2, union acc_eq lhs lhs_class v_class)
            with Not_found ->
              let v1, v2 =
                match v_class1, v_class2 with
                | None, _ | _, None -> failwith "empty env class!"
                | Some v1, Some v2 -> (v1, v2)
              in
              let v = Val.widen v1 v2 thresholds in
              let acc_map2 = LhsMap2.add (lhs1, lhs2) (lhs, v) acc_map2 in
              let acc_eq = union acc_eq lhs lhs v in
              (acc_map2, acc_eq)
        ) eq1.map (LhsMap2.empty, None)
      in equalities



  let remove h x =
    match h with
    | None -> h
    | Some h ->
      Logger.debug ~level "@[<v 0>before removing: %a@ %a@]"
        Dba_printer.Ascii.pp_lhs x pp (Some h) ;
      try let i = Dba_types.LValue.Map.find x h.map in
          let i_class, _x_class, v_class = find_bis h x in
          let new_map = Dba_types.LValue.Map.remove x h.map in
          if i = i_class then
            let res = Dba_types.LValue.Map.fold
              (
                fun cur_x cur_i acc ->
                  let cur_i_class, _cur_x_class, _cur_v_class = find_bis h cur_x in
                  if (cur_i_class = i_class) then (
                    let (cur_fi, some_cur_x, _some_cur_v) = Pa.get h.father cur_i in
                    match some_cur_x with
                      None ->
                        print (Some h);
                        Format.printf "removing %a@."
                          Dba_printer.Ascii.pp_lhs x;
                        failwith "??????"
                    | Some xx-> (cur_i, (cur_fi, xx)) :: acc
                  )
                  else acc
              ) new_map []
            in
            try
              assert (res <> []);
              let (new_i, (new_fi, new_x)) = List.hd res in
              Logger.debug ~level "Removing and replacing by %a"
                Dba_printer.Ascii.pp_lhs new_x ;
              let new_map = Dba_types.LValue.Map.remove new_x new_map in
              let new_map = Dba_types.LValue.Map.add new_x i new_map in
              h.father <- Pa.set2 h.father new_i (new_fi, None, None);
              h.father <- Pa.set2 h.father i (i, Some new_x, v_class);
              let equalities = { h with map = new_map } in
              assert_well_formed equalities;
              Logger.debug ~level "@[<v 0>after removing1: %a@ %a@]"
                Dba_printer.Ascii.pp_lhs x
                pp (Some equalities) ;
              (Some equalities)
            with _ ->
              let (fi, _lhs', _v') = Pa.get h.father i in
              assert_well_formed h;
              let equalities = { h with father = Pa.set2 h.father i (fi, None, None); map = new_map } in
              Logger.debug ~level "@[<v 0>after removing2: %a@ %a@]"
                Dba_printer.Ascii.pp_lhs x
                pp (Some equalities);
              assert_well_formed equalities;
              (Some equalities)
          else
            let (fi, _lhs', _v') = Pa.get h.father i in
            let equalities = { h with father = Pa.set2 h.father i (fi, None, None); map = new_map } in
            Logger.debug ~level "@[<v 0>after removing3: %a@ %a@]"
              Dba_printer.Ascii.pp_lhs x
              pp (Some equalities) ;
            assert_well_formed equalities;
            (Some equalities)
      with
        Not_found ->
        Logger.debug ~level "@[<v 0>after removing (not found): %a@ %a@]"
          Dba_printer.Ascii.pp_lhs x
          pp (Some h) ;
          assert_well_formed h;
          (Some h)



  let remove_syntax_overlaps h x =
    match h with
    | None -> h
    | Some h ->
      let todo =
        Dba_types.LValue.Map.filter
          (fun lhs _id -> (Dba_utils.contains_lhs lhs x)) h.map in
      let rec update h todo =
        match h with
          None -> h
        | Some h ->
          try
            let lhs , _ = Dba_types.LValue.Map.choose todo in
            let todo = Dba_types.LValue.Map.remove lhs todo in
            let h = remove (Some h) lhs in
            update h todo
          with Not_found -> (Some h)
      in
      let equalities = update (Some h) todo in
      (match equalities with None -> () | Some h -> assert_well_formed h);
      equalities


  let get_elements h =
    match h with
      None -> []
    | Some h -> Dba_types.LValue.Map.fold (fun lhs _id acc -> lhs :: acc) h.map []


  let get_nb_names equalities =
    match equalities with
      None -> 0
    | Some equalities -> Dba_types.LValue.Map.cardinal equalities.map


  let get_nb_classes equalities =
    match equalities with
      None -> 0
    | Some equalities ->
      Dba_types.LValue.Map.fold (
        fun _lhs id nb -> let (id_father, _x, _v) = Pa.get equalities.father id in
                         if id = id_father then nb + 1 else nb
      ) equalities.map 0




  let is_same_class equalities lhs1 lhs2 =
    let lhs_class1, _v_class1 = find equalities lhs1 in
    let lhs_class2, _v_class2 = find equalities lhs2 in
    match lhs_class1, lhs_class2 with
    | None, None -> false
    | Some _lhs1, None -> false
    | None, Some _lhs2 -> false
    | Some lhs1, Some lhs2 -> Dba_types.LValue.equal lhs1 lhs2


 let get_nb_names_in_class lhs1 equalities =
    match equalities with
      None -> 0
    | Some eq ->
      Dba_types.LValue.Map.fold (
        fun lhs2 _id nb -> if is_same_class equalities lhs1 lhs2 then nb + 1 else nb
      ) eq.map 0


  let refine e v equalities =
    let t0 = Unix.gettimeofday () in
    match equalities with
      None ->
        Options.time_equalities := Unix.gettimeofday () -. t0 +. !Options.time_equalities;
        None
    | Some eq ->
      begin
        match Dba_types.LValue.of_expr e with
        | lhs ->
          let lhs_class, _v_class = find equalities lhs in
          begin
            match lhs_class with
            | None ->
              Options.time_equalities := Unix.gettimeofday () -. t0 +. !Options.time_equalities;
              equalities
            | Some lhs ->
              Options.nb_refined_lhs := !Options.nb_refined_lhs + (get_nb_names_in_class lhs equalities) - 1;
              let id = Dba_types.LValue.Map.find lhs eq.map in
              let id_father, old_lhs, _old_v = Pa.get eq.father id in
              let father = Pa.set2 eq.father id (id_father, old_lhs, Some v) in
              eq.father <- father;
              Options.time_equalities := Unix.gettimeofday () -. t0 +. !Options.time_equalities;
              Some eq
          end
        | exception Failure _ ->
          Options.time_equalities := Unix.gettimeofday () -. t0 +. !Options.time_equalities;
          equalities
      end

end


(* let equalities = copy_equalities eq in *)
(* let equalities = *)
(*      match equalities with *)
(*        None -> Union_find.create () *)
(*      | Some equalities -> equalities *)
(* in *)
(* let equalities = Union_find.remove_addresses_conflicts equalities lhs in *)
(* let equalities, m =  *)
(*      Display.display (Display.RemoveEqualities (lhs, equalities)); *)
(*      let equalities, new_class = Union_find.remove equalities lhs in *)
(*      match new_class with  *)
(*        None ->  equalities, m *)
(*      | Some new_class ->  *)
(*        if Dba.is_egal_lhs class1 new_class  *)
(*        then equalities, m *)
(*        else  *)
(*          let e = Dba.dba_lhs_to_expr class1 in *)
(*          let m, _, _ = assign addrStack new_class e m assumes globals recordMap elements (Some equalities) in  *)
(*          equalities, m *)
(* in   *)
(* Some equalities, m *)
