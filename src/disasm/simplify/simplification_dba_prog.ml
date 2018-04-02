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

open Dba_types
open Simplification_dba_utils

let set_id instr nid =
  match instr with
  | Dba.IkAssign (lhs, e, _id) -> Dba.IkAssign (lhs, e, nid)
  | Dba.IkSJump (Dba.JInner _id, tag) -> Dba.IkSJump (Dba.JInner nid, tag)
  | Dba.IkUndef (lhs, _id) ->  Dba.IkUndef (lhs, nid)
  | Dba.IkIf (_, _, _) -> failwith "set_id of if"
  | Dba.IkSJump (Dba.JOuter _, _) -> failwith "set_id of non loxcal jump"
  | Dba.IkDJump (_, _) -> failwith "set_id of djump"
  | Dba.IkStop (_) -> failwith "set_id of stop"
  | Dba.IkPrint (l, _id) -> Dba.IkPrint (l, nid)
  | Dba.IkNondetAssume (l, c, _id) -> Dba.IkNondetAssume (l, c, nid)
  | Dba.IkAssume (c, _id) -> Dba.IkAssume (c, nid)
  | Dba.IkAssert (c, _id) -> Dba.IkAssert (c, nid)
  | Dba.IkMalloc (lhs, e, _id) -> Dba.IkMalloc (lhs, e, nid)
  | Dba.IkFree (e, _id) -> Dba.IkFree (e, nid)
  | Dba.IkNondet (l, r, _id) -> Dba.IkNondet (l, r, nid)


let remove_goto m =
  Logger.debug ~level:3 "Removing gotos ...";
  let rec f addr (*(ca, id)*) (ik, opcode) accu =
    let local_find a =
      try Caddress.Map.find a accu
      with Not_found -> Caddress.Map.find a m
    in
    match ik with
    | Dba.IkAssign (_, _, nid)
    | Dba.IkUndef (_, nid)
    | Dba.IkPrint (_, nid)
    | Dba.IkNondetAssume (_, _, nid)
    | Dba.IkAssume (_, nid)
    | Dba.IkAssert (_, nid)
    | Dba.IkMalloc (_, _, nid)
    | Dba.IkFree (_, nid)
    | Dba.IkNondet (_, _, nid) ->
      begin
        try
          let a = Dba_types.Caddress.reid addr nid in
          let nik, _ = local_find a in
          match nik with
          | Dba.IkSJump (Dba.JInner nnid, _) ->
            f addr (set_id ik nnid, opcode) accu
          | _ -> Caddress.Map.add addr (ik, opcode) accu
        with
          Not_found -> Caddress.Map.add addr (ik, opcode) accu
      end
    | Dba.IkSJump (Dba.JInner nid, _) ->
      begin
        try
          let a = Dba_types.Caddress.reid addr nid in
          let (nik, nopcode) = local_find a in
          match nik with
          | Dba.IkSJump (Dba.JInner nnid, _) ->
            f addr (set_id ik nnid, opcode) accu
          | _ ->
            if addr.Dba.id = 0 then
              let accu = Caddress.Map.add a (set_id ik 0, nopcode) accu in
              f addr (nik, opcode) accu
            else Caddress.Map.add addr (ik, opcode) accu
        with
          Not_found -> Caddress.Map.add addr (ik, opcode) accu
      end
    | Dba.IkIf (cond, Dba.JOuter addr' (*(nca, nid1)*), nid2) ->
      begin
        try
          let a = Dba_types.Caddress.reid addr nid2 in
          let nik, _ = local_find a in
          match nik with
          | Dba.IkSJump (Dba.JInner nnid, _) ->
            let inst = Dba.IkIf (cond, Dba.JOuter addr', nnid) in
            f addr (inst, opcode) accu
          | _ -> Caddress.Map.add addr (ik, opcode) accu
        with
          Not_found -> Caddress.Map.add addr (ik, opcode) accu
      end
    | Dba.IkIf (cond, Dba.JInner nid1, nid2) ->
      begin
        let a1 = Dba_types.Caddress.reid addr nid1 in
        let a2 = Dba_types.Caddress.reid addr nid2 in
        let nik1 =
          try let elt1 = local_find a1 in Some (fst elt1)
          with Not_found -> None in
        let nik2 =
          try let elt2 = local_find a2 in Some (fst elt2)
          with Not_found -> None in
        match nik1, nik2 with
        | None, None -> Caddress.Map.add addr (ik, opcode) accu
        | Some nik, None ->
          begin
            match nik with
            | Dba.IkSJump (Dba.JInner nnid, _) ->
              let inst = Dba.IkIf (cond, Dba.JInner nnid, nid2) in
              f addr (inst, opcode) accu
            | _ -> Caddress.Map.add addr (ik, opcode) accu
          end
        | None, Some nik ->
          begin
            match nik with
            | Dba.IkSJump (Dba.JInner nnid, _ntag) ->
              let inst = Dba.IkIf (cond, Dba.JInner nid1, nnid) in
              f addr (inst, opcode) accu
            | _ -> Caddress.Map.add addr (ik, opcode) accu
          end
        | Some ik1, Some ik2 ->
          begin
            match ik1, ik2 with
            | Dba.IkSJump (Dba.JInner nnid1, _), Dba.IkSJump (Dba.JInner nnid2, _) ->
              let inst = Dba.IkIf (cond, Dba.JInner nnid1, nnid2) in
              f addr (inst, opcode) accu
            | _, Dba.IkSJump (Dba.JInner nnid2, _ntag) ->
              let inst = Dba.IkIf (cond, Dba.JInner nid1, nnid2) in
              f addr (inst, opcode) accu
            | Dba.IkSJump (Dba.JInner nnid1, _ntag), _ ->
              let inst = Dba.IkIf (cond, Dba.JInner nnid1, nid2) in
              f addr (inst, opcode) accu
            | _, _ -> Caddress.Map.add addr (ik, opcode) accu
          end
      end
    | Dba.IkSJump (Dba.JOuter _, _)
    | Dba.IkDJump (_, _)
    | Dba.IkStop (_) ->
      Caddress.Map.add addr (ik, opcode) accu
  in
  let m = Caddress.Map.fold f m Caddress.Map.empty in
  let rec g addr (ik, opcode) accu =
    if Caddress.Map.mem addr accu then accu
    else
      match ik with
      | Dba.IkAssign (_, _, nid)
      | Dba.IkUndef (_, nid)
      | Dba.IkPrint (_, nid)
      | Dba.IkNondetAssume (_, _, nid)
      | Dba.IkAssume (_, nid)
      | Dba.IkAssert (_, nid)
      | Dba.IkMalloc (_, _, nid)
      | Dba.IkFree (_, nid)
      | Dba.IkNondet (_, _, nid)
      | Dba.IkSJump (Dba.JInner nid, _) ->
        begin
          let accu = Caddress.Map.add addr (ik, opcode) accu in
          try
            let a = Caddress.reid addr nid in
            let (nik, nopcode) = Caddress.Map.find a m in
            g a (nik, nopcode) accu
          with Not_found -> accu
        end
      | Dba.IkIf (_, Dba.JOuter _, nid2) ->
        begin
          let accu = Caddress.Map.add addr (ik, opcode) accu in
          try
            let a = Caddress.reid addr nid2 in
            g a (Caddress.Map.find a m) accu
          with Not_found -> accu
        end
      | Dba.IkIf (_cond, Dba.JInner nid1, nid2) ->
        begin
          let accu = Caddress.Map.add addr (ik, opcode) accu in
          let accu =
            try
            let a = Caddress.reid addr nid1 in
              let nik1, _ = Caddress.Map.find a m in
              g a (nik1, opcode) accu
            with Not_found -> accu
          in
          try
            let a = Caddress.reid addr nid2 in
            let (nik2, _nopcode) = Caddress.Map.find a m in
            (* FIXME : really ?*)
            g a (nik2, opcode) accu
          with Not_found -> accu
        end
      | Dba.IkSJump (Dba.JOuter _, _)
      | Dba.IkDJump (_, _)
      | Dba.IkStop (_) -> Caddress.Map.add addr (ik, opcode) accu
  in
  let g_aux addr e accu = if addr.Dba.id = 0 then g addr e accu else accu in
  Caddress.Map.fold g_aux m Caddress.Map.empty


let remove_mustkill prog env_flags =
  let look_ahead_limit = 100 in
  let f addr (ik, opcode) (accu, env_flags, has_changed) =
    let mk_sjump id = Dba.IkSJump (Dba.JInner id, None) in
    let eflags, ik', has_changed =
      match ik with
      | Dba.IkAssign (lhs, _, id_next)
      | Dba.IkUndef (lhs, id_next) ->
        let eflags, ik, has_changed =
          if Dba_types.LValue.is_flag lhs || Dba_types.LValue.is_temporary lhs
          then
            let env_flags, has_changed =
              let a = Dba_types.Caddress.reid addr id_next in
              is_not_mayused prog a look_ahead_limit lhs env_flags in
            let ik' = if has_changed then  mk_sjump id_next else ik in
            env_flags, ik', has_changed
          else env_flags, ik, has_changed
        in eflags, ik, has_changed
      | _ -> env_flags, ik, has_changed
    in Caddress.Map.add addr (ik', opcode) accu, eflags, has_changed
  in Caddress.Map.fold f prog (Caddress.Map.empty, env_flags, false)


let remove_mustkill_lfp inst_map =
  Logger.debug ~level:3 "Removing mustkills ...";
  let rec loop inst_map env_flags =
    match remove_mustkill inst_map env_flags with
    | (imap, env_flags, true) ->  loop imap env_flags
    | (imap, _, false) -> imap
  in loop inst_map Caddress.Map.empty
