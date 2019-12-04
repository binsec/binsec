(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2019                                               *)
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


open Path_predicate
open Path_predicate_env
open Trace_type
open Solver
open Path_predicate_formula
open Formula
open Dse_options

(* Policy in the separate file:
 * :: *           :: esp          :: *                                             => Pc
 * :: *           :: ebp          :: *                                             => Pc
 * :: *           :: ebx          :: *                                             => Pc
 * :: *           :: edi          :: *                                             => Pc
 * :: *           :: esi          :: *                                             => Pc
 * :: *           :: eax          :: istainted(Ti, eax)                            => S
 * :: _ := ?e     :: @[!$$] <: !e :: not(istainted(Ti, !$$) || istainted(Tp, !$$)) => C
 * :: goto ?e     :: @[!$$] <: !e :: not(istainted(Ti, !$$) || istainted(Tp, !$$)) => C
 * :: @[?e] := _  :: !e           :: not(istainted(Ti, !e)  || istainted(Tp, !e))  => C
   default                                                                           => Ps
*)

class sploit1 conf =
  object(self) inherit dse_analysis conf

    val mutable buff_addr = 0L
    val mutable size_read = 0
    val mutable data = ""

    method! private pre_execution _ =
      do_compute_taint <- true (* Activate taint computation *)

    method! private visit_instr_before (key:int) (tr_inst:trace_inst) _:
      trace_visit_action =
      let m = tr_inst.mnemonic in
      Logger.result "%d %Lx\t\t%s" key tr_inst.location m;
      if m = "\xcd\x80" then
        (*       let num, sys = get_syscall(tr_inst.concrete_infos) in
                 (match sys with
                 | Read(fd,addr,count,sz_read,content) ->
                 buff_addr <- addr ; size_read <- sz_read ; data <- content
                 | _ -> ()) ; *)
        SkipExec
      else
        DoExec

    method! private visit_dbainstr_before _ _ _ _ = DoExec

    method! private post_execution (env:Path_predicate_env.t): int =
      let eax = Dba.Expr.var "eax" 32 in (* Jump on eax *)
      let shellcode_addr =
        (* Arbitrary address *)
        Bitvector.create (Bigint.big_int_of_int 0x61626364) 32 in
      let cond =
        (* F_eax(eip) == addr shellcode *)
        Dba.Expr.equal eax (Dba.Expr.constant shellcode_addr) in
      Logger.result "Let's build the predicate ...";
      (* Logic formula *)
      let f_cond = self#build_cond_predicate cond env in
      build_formula_file env.formula f_cond "sploit.smt2" |> ignore;
      let solver =
        Formula_options.Solver.of_piqi
          config.Config_piqi.Configuration.solver in
      let res, model =
        solve_model "sploit.smt2" solver in
      if res = SAT then self#generate_new_file model;
      Logger.result "%a" Formula_pp.pp_status res;
      status_to_exit_code res

    method generate_new_file model =
      let fd = open_out "exploit.txt" in
      let rec iter_buff addr i k =
        if k >= 0 then
          let address =
            let v =
              Int64.(add addr (of_int i))
              |> Bigint.big_int_of_int64 in
            Bitvector.create v (Kernel_options.Machine.word_size ()) in
          match Smt_model.find_address_contents model address with
          | Some value ->
            Printf.fprintf fd "%c" (char_of_int @@ Bitvector.to_int value)
          | None ->
            Printf.fprintf fd "%c" data.[k];
            iter_buff addr (i+1) (k-1)
      in
      iter_buff buff_addr 0 (size_read-1);
      Printf.fprintf fd "\n";
      Logger.result "exploit.txt written..";
      close_out fd

  end
