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

module V = Virtual_address
module A = Dba_types.Caddress
module I = Dba.Instr

type state = {
  cfg : Ghidra_cfg.t;
  (* The Ghidra cfg *)
  ims : string V.Htbl.t;
  (* Instruction mnemonic storage *)
  ctp : V.Set.t;
  (* Call to process *)
  dis : I.t A.Htbl.t;
  (* Dba instruction storage *)
  dap : A.t list A.Htbl.t;
  (* Dba address predecessors *)
  opa : bool A.Htbl.t; (* Opaque predicate addresses *)
}

let fetch state v =
  Dhunk.iteri
    ~f:(fun i instr ->
      let a = A.create v i in
      A.Htbl.add state.dis a instr;
      match instr with
      | I.Stop _ -> ()
      | I.DJump _ ->
          Ghidra_cfg.iter_succ_e
            (function
              | _, Return r, _ when not (V.Set.mem r state.ctp) -> ()
              | _, _, d ->
                  let t = A.of_virtual_address d in
                  A.Htbl.replace state.dap t
                    (a :: (try A.Htbl.find state.dap t with Not_found -> [])))
            state.cfg v
      | I.If (_, JOuter t, n) ->
          A.Htbl.replace state.dap t
            (a :: (try A.Htbl.find state.dap t with Not_found -> []));
          let f = A.create v n in
          A.Htbl.replace state.dap f
            (a :: (try A.Htbl.find state.dap f with Not_found -> []))
      | I.If (_, JInner b, n) ->
          let t = A.create v b in
          A.Htbl.replace state.dap t
            (a :: (try A.Htbl.find state.dap t with Not_found -> []));
          let f = A.create v n in
          A.Htbl.replace state.dap f
            (a :: (try A.Htbl.find state.dap f with Not_found -> []))
      | I.SJump (JOuter t, _) ->
          A.Htbl.replace state.dap t
            (a :: (try A.Htbl.find state.dap t with Not_found -> []))
      | I.SJump (JInner n, _)
      | I.Assert (_, n)
      | I.Assume (_, n)
      | I.Assign (_, _, n)
      | I.Undef (_, n)
      | I.Nondet (_, n) ->
          let f = A.create v n in
          A.Htbl.replace state.dap f
            (a :: (try A.Htbl.find state.dap f with Not_found -> [])))
    (Instruction.hunk (fst (Disasm_core.decode v)))

let find_condition state v =
  if not (A.Htbl.mem state.dis (A.of_virtual_address v)) then fetch state v;
  match Ghidra_cfg.succ_e state.cfg v with
  | [ (_, Branch, t); (_, Fallthrough, _) ]
  | [ (_, Fallthrough, _); (_, Branch, t) ] -> (
      let addr =
        List.find
          (fun a -> Virtual_address.equal v a.Dba.base)
          (A.Htbl.find state.dap (A.of_virtual_address t))
      in
      match A.Htbl.find state.dis addr with
      | I.If (e, _, _) -> (addr, e)
      | _ -> raise Not_found)
  | _ -> raise Not_found

(* FIXME: we need all ISA to declare their registers *)
let havoc_eax =
  I.non_deterministic (Dba.LValue.var "eax" ~bitsize:Size.Bit.bits32) 1

let havoc_ecx =
  I.non_deterministic (Dba.LValue.var "ecx" ~bitsize:Size.Bit.bits32) 2

let havoc_edx =
  I.non_deterministic (Dba.LValue.var "edx" ~bitsize:Size.Bit.bits32) 3

let havoc_rax =
  I.non_deterministic (Dba.LValue.var "rax" ~bitsize:Size.Bit.bits64) 1

let havoc_rcx =
  I.non_deterministic (Dba.LValue.var "rcx" ~bitsize:Size.Bit.bits64) 2

let havoc_rdx =
  I.non_deterministic (Dba.LValue.var "rdx" ~bitsize:Size.Bit.bits64) 3

(* we provide default stubs but we can lose correctness here *)
let ignore_call state c v =
  match Kernel_options.Machine.isa () with
  | Machine.X86 { bits = `x32 } ->
      let a0 = A.create c 0
      and a1 = A.create c 1
      and a2 = A.create c 2
      and a3 = A.create c 3
      and v0 = A.of_virtual_address v in
      A.Htbl.add state.dap a1 [ a0 ];
      A.Htbl.add state.dap a2 [ a1 ];
      A.Htbl.add state.dap a3 [ a2 ];
      A.Htbl.add state.dap v0 (a3 :: A.Htbl.find state.dap v0);
      A.Htbl.add state.dis a0 havoc_eax;
      A.Htbl.add state.dis a1 havoc_ecx;
      A.Htbl.add state.dis a2 havoc_edx;
      A.Htbl.add state.dis a3 (I.static_jump ~tag:(Call v0) (Dba.JOuter v0))
  | Machine.X86 { bits = `x64 } ->
      let a0 = A.create c 0
      and a1 = A.create c 1
      and a2 = A.create c 2
      and a3 = A.create c 3
      and v0 = A.of_virtual_address v in
      A.Htbl.add state.dap a1 [ a0 ];
      A.Htbl.add state.dap a2 [ a1 ];
      A.Htbl.add state.dap a3 [ a2 ];
      A.Htbl.add state.dap v0 (a3 :: A.Htbl.find state.dap v0);
      A.Htbl.add state.dis a0 havoc_rax;
      A.Htbl.add state.dis a1 havoc_rcx;
      A.Htbl.add state.dis a2 havoc_rdx;
      A.Htbl.add state.dis a3 (I.static_jump ~tag:(Call v0) (Dba.JOuter v0))
  | isa ->
      raise
        (Errors.not_yet_implemented
           (Format.asprintf "default stubs not available for skipped %a calls"
              Machine.ISA.pp isa))

let prefetch state v =
  let a = A.of_virtual_address v in
  if not (A.Htbl.mem state.dap a) then A.Htbl.add state.dap a [];
  let fallthrough = ref false in
  let caller = ref None in
  Ghidra_cfg.iter_pred_e
    (function
      | v, Call, _ when not (V.Set.mem v state.ctp) -> ()
      | _, Return r, _ when not (V.Set.mem r state.ctp) -> fallthrough := true
      | v, Presumed, _ -> caller := Some v
      | v, _, _ when A.Htbl.mem state.dis (A.of_virtual_address v) -> ()
      | v, _, _ -> fetch state v)
    state.cfg v;
  if !fallthrough then ignore_call state (Option.get !caller) v

let enumerate_path state n a =
  let rec loop state result = function
    | [] -> result
    | (a, c, j, n) :: worklist -> (
        if a.Dba.id = 0 then prefetch state a.Dba.base;
        match (A.Htbl.find state.dap a, n) with
        (* if we reach the entry point *)
        | [], _
        (* or if we reach both bound and end of basic bloc *)
        | _ :: _ :: _, 0 ->
            (* end of path *) loop state ((a, c, j) :: result) worklist
        | ([ _ ] as preds), n ->
            step_backward state result worklist 1 a c j n preds
        | preds, n -> step_backward state result worklist 0 a c j (n - 1) preds)
  and step_backward state result worklist delta a c j n = function
    | [] -> loop state result worklist
    | p :: preds -> (
        match (A.Htbl.find state.dis p, n) with
        (* if we reach both bound and end of basic bloc *)
        | I.If _, 0 | I.DJump _, 0 | I.SJump (_, Call _), 0 ->
            (* end of path *)
            step_backward state ((a, c, j) :: result) worklist delta a c j n
              preds
        | I.If (_, JOuter t, _), n when A.equal a t -> (
            match A.Htbl.find state.opa p with
            | (exception Not_found) | true ->
                step_backward state result
                  ((p, true :: c, j, n - delta) :: worklist)
                  delta a c j n preds
            | false -> step_backward state result worklist delta a c j n preds)
        | I.If (_, JInner t, _), n when A.equal a (A.reid p t) -> (
            match A.Htbl.find state.opa p with
            | (exception Not_found) | true ->
                step_backward state result
                  ((p, true :: c, j, n - delta) :: worklist)
                  delta a c j n preds
            | false -> step_backward state result worklist delta a c j n preds)
        | I.If (_, _, f), n -> (
            assert (A.equal a (A.reid p f));
            match A.Htbl.find state.opa p with
            | (exception Not_found) | false ->
                step_backward state result
                  ((p, false :: c, j, n - delta) :: worklist)
                  delta a c j n preds
            | true -> step_backward state result worklist delta a c j n preds)
        | I.DJump _, n ->
            step_backward state result
              ((p, c, A.to_virtual_address a :: j, n - delta) :: worklist)
              delta a c j n preds
        | I.SJump (JOuter t, Call _), n ->
            assert (A.equal a t);
            step_backward state result
              ((p, c, j, n - 1) :: worklist)
              delta a c j n preds
        | I.SJump (JOuter t, _), n ->
            assert (A.equal a t);
            step_backward state result ((p, c, j, n) :: worklist) delta a c j n
              preds
        | I.SJump (JInner f, _), n
        | I.Assume (_, f), n
        | I.Assert (_, f), n
        | I.Assign (_, _, f), n
        | I.Undef (_, f), n
        | I.Nondet (_, f), n ->
            assert (A.equal a (A.reid p f));
            step_backward state result ((p, c, j, n) :: worklist) delta a c j n
              preds
        | I.Stop _, _ -> assert false)
  in
  loop state [] [ (a, [], [], n) ]
