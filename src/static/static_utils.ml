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

let rec check_widening_pt addr addr_loop insts =
  match Dba_types.Caddress.Map.find addr insts |> fst with
  | Dba.IkAssign (_, _, next)
  | Dba.IkAssert (_, next)
  | Dba.IkAssume (_, next)
  | Dba.IkNondetAssume (_, _, next)
  | Dba.IkNondet (_, _, next)
  | Dba.IkUndef (_, next)
  | Dba.IkMalloc (_, _, next)
  | Dba.IkFree (_, next)
  | Dba.IkPrint (_, next)
  | Dba.IkSJump (Dba.JInner next, _) ->
    let anext = Dba_types.Caddress.reid addr next in
    if Dba_types.Caddress.equal anext addr_loop
    then None
    else check_widening_pt anext addr_loop insts

  | Dba.IkSJump (Dba.JOuter a, _) ->
    if Dba_types.Caddress.equal a addr_loop
    then None
    else check_widening_pt a addr_loop insts

  | Dba.IkIf (_, t, id2) ->
    let a1 =
      match t with
      | Dba.JInner id -> Dba_types.Caddress.reid addr id
      | Dba.JOuter a -> a
    and a2 = Dba_types.Caddress.reid addr id2 in
    Some (addr, a1, a2)

  | Dba.IkDJump _
  | Dba.IkStop _ -> None
  | exception Not_found -> None


(* FIXME: why is this module needed here only *)
module TagSet = Set.Make (
  struct
    type t = int * Dba.address
    let compare (i1, _) (i2, _) = compare i1 i2
  end
  )


let update_w_pts addr insts tag w_pts visited w_delays unrolled_loops =
  try
    let w_tag = Dba_types.Caddress.Map.find addr visited in
    let global_delay, local_delays = w_delays in
    if TagSet.subset w_tag tag
    then (
      let nb =
        try Dba_types.Caddress.Map.find addr local_delays
        with Not_found -> global_delay
      in
      let addr_if = check_widening_pt addr addr insts in
      match addr_if with
      | None -> Dba_types.Caddress.Map.add addr nb w_pts, unrolled_loops
      | Some (a, a1, a2) ->
        if Static_options.NaiveWidening.get ()
        then Dba_types.Caddress.Map.add addr nb w_pts, unrolled_loops
        else
          let w_tag =
            try Dba_types.Caddress.Map.find a visited
            with Not_found ->  failwith "disas.ml: not_found!"
          in
          let t = TagSet.diff tag w_tag in
          if TagSet.cardinal t = 1
          then (
            let (_, addr) = TagSet.choose t in
            let addrSet =
              try Dba_types.Caddress.Map.find a unrolled_loops
              with Not_found -> Dba_types.Caddress.Set.empty
            in
            if Dba_types.Caddress.equal addr a1
            then
              let addrSet = Dba_types.Caddress.Set.add a1 addrSet in
              let unrolled_loops = Dba_types.Caddress.Map.add a addrSet unrolled_loops in
              Dba_types.Caddress.Map.add a nb w_pts, unrolled_loops
            else if Dba_types.Caddress.equal addr a2
            then
              let addrSet = Dba_types.Caddress.Set.add a2 addrSet in
              let unrolled_loops = Dba_types.Caddress.Map.add a addrSet unrolled_loops in
              Dba_types.Caddress.Map.add a nb w_pts, unrolled_loops
            else
              Dba_types.Caddress.Map.add a nb w_pts, unrolled_loops
          )
          else if TagSet.cardinal t = 0
          then
            let addrSet =
              try Dba_types.Caddress.Map.find a unrolled_loops
              with Not_found -> Dba_types.Caddress.Set.empty
            in
            try
              let w_tag1 = Dba_types.Caddress.Map.find a1 visited in
              if TagSet.equal w_tag1 tag  then
                let addrSet = Dba_types.Caddress.Set.add a1 addrSet in
                let unrolled_loops = Dba_types.Caddress.Map.add a addrSet unrolled_loops in
                Dba_types.Caddress.Map.add a nb w_pts, unrolled_loops
              else
                let addrSet = Dba_types.Caddress.Set.add a2 addrSet in
                let unrolled_loops = Dba_types.Caddress.Map.add a addrSet unrolled_loops in
                Dba_types.Caddress.Map.add a nb w_pts, unrolled_loops
            with Not_found ->
              let addrSet = Dba_types.Caddress.Set.add a2 addrSet in
              let unrolled_loops = Dba_types.Caddress.Map.add a addrSet unrolled_loops in
              Dba_types.Caddress.Map.add a nb w_pts, unrolled_loops
          else Dba_types.Caddress.Map.add a nb w_pts, unrolled_loops
    )
    else w_pts, unrolled_loops
  with Not_found -> w_pts, unrolled_loops


let get_widening_pts l0 insts w_delays djmps =
  (* FIXME *)
  let tag = ref (TagSet.singleton (0, l0)) in
  let id = ref 0 in
  let visited = Dba_types.Caddress.Map.empty in
  let visited = ref visited in
  let w_pts = Dba_types.Caddress.Map.empty in
  let w_pts = ref w_pts in
  let unrolled_loops = Dba_types.Caddress.Map.empty in
  let unrolled_loops = ref unrolled_loops in
  let addr_list = ref [(l0, None)] in

  while !addr_list <> [] do
    let (addr, is_ite), addr_list_tl =
      match !addr_list with
      | hd :: tl -> hd, tl
      | [] -> assert false
    in
    if Dba_types.Caddress.Map.mem addr !visited
    then begin
      let w_pts', unrolled_loops' =
        update_w_pts addr insts !tag !w_pts !visited w_delays !unrolled_loops in
      w_pts := w_pts';
      unrolled_loops := unrolled_loops';
      addr_list := addr_list_tl
    end
    else (
      (
        match is_ite with
        | Some tagg ->
          id := !id + 1;
          tag := TagSet.add (!id, addr) tagg;
          visited := Dba_types.Caddress.Map.add addr !tag !visited
        | None ->
          visited := Dba_types.Caddress.Map.add addr !tag !visited
      );
      try
        let (inst, _) = Dba_types.Caddress.Map.find addr insts in
        let reid = Dba_types.Caddress.reid addr in
        match inst with
        | Dba.IkAssign (_, _, next)
        | Dba.IkAssert (_, next)
        | Dba.IkAssume (_, next)
        | Dba.IkNondetAssume (_, _, next)
        | Dba.IkNondet (_, _, next)
        | Dba.IkUndef (_, next)
        | Dba.IkMalloc (_, _, next)
        | Dba.IkFree (_, next)
        | Dba.IkPrint (_, next) ->
          addr_list := (reid next, None) :: addr_list_tl
        | Dba.IkIf (_c, Dba.JInner next1, next2) ->
          addr_list :=
            (reid next1, Some !tag) :: (reid next2, Some !tag) :: addr_list_tl
        | Dba.IkIf (_c, Dba.JOuter a_next, next) ->
          addr_list :=
            (a_next, Some !tag) :: (reid next, Some !tag) :: addr_list_tl
        | Dba.IkSJump (Dba.JInner next, _) ->
          addr_list := (reid next, None) :: addr_list_tl
        | Dba.IkSJump (Dba.JOuter a_next, _) ->
          addr_list := (a_next, None) :: addr_list_tl
        | Dba.IkDJump _ ->
          let successors =
            try Dba_types.Caddress.Map.find addr djmps
            with Not_found -> Dba_types.Caddress.Set.empty
          in
          let new_addr_list =
            Dba_types.Caddress.Set.fold
              (fun addr_suiv acc -> (addr_suiv, Some !tag) :: acc) successors addr_list_tl
          in
          addr_list := new_addr_list
        | Dba.IkStop _state -> addr_list := addr_list_tl
      with
        Not_found -> addr_list := addr_list_tl
    )
  done;
  !w_pts, !unrolled_loops


let merge_maps =
  Dba_types.Caddress.Map.merge
    (fun _ elt1 elt2 -> if elt1 <> None then elt1 else elt2)


let discover_from_address disasm imap address =
  let vaddr = Dba_types.Virtual_address.of_code_address address in
  let add_block_starts addr _ set =
    if addr.Dba.id = 0 then
      let open Dba_types.Virtual_address in
      Set.add (of_code_address addr) set
    else set in
  let visited =
    Dba_types.Caddress.Map.fold
      add_block_starts imap Dba_types.Virtual_address.Set.empty in
  let worklist = Disasm_core.W.singleton vaddr in
  let program = Disasm.Program.empty in
  let prog = disasm visited worklist program in
  Disasm_types.Program.to_pmap prog.Disasm.Program.instructions
  |> Simplification_dba.simplify_dba


(* Insert newly discovered instruction into an already existing program *)
let fix_map disasm imap address =
  let imap' = discover_from_address disasm imap address in
  merge_maps imap imap'

(* Restart disassembly from address, add newly discovered elements into the
   current program, with widening points.
*)
let redisassemble disasm widen imap address =
  let inst_map = fix_map disasm imap address in
  Logger.debug "Setting widening points ...";
  let w_pts, unrolled_loops = widen inst_map in
  Dba_types.Caddress.Map.find address inst_map,
  inst_map, w_pts, unrolled_loops


let update_instr_map
    l0 address inst_map stops w_pts w_delays djmps unrolled_loops =
  match Dba_types.Caddress.Map.find address inst_map with
  | i -> i, inst_map, w_pts, unrolled_loops
  | exception Not_found ->
    if Static_options.Disassembly.get () then
      let widen = fun imap -> get_widening_pts l0 imap w_delays djmps in
      let disasm =
        fun visited worklist p ->
          Disasm.Recursive.disassemble ~visited ~worklist ~stops p in
      redisassemble disasm widen inst_map address
    else begin
      Logger.fatal "No instruction can be retrieved from executable.";
      exit 3
    end
