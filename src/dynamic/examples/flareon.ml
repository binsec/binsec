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
open Config_piqi
open Trace_type
open Path_predicate_formula
open Solver
open Formula
open Common_piqi
open Dse_options

class flare_one (input_config:Trace_config.t) =
  object(self) inherit dse_analysis input_config as super

    val smt_file = "/tmp/flareon.smt2"
    val base_input = 0x402158L
    val jnz_addr = 0x40105bL
    val fail_addr = 0x40107bL
    val success_addr = 0x401063L
    val patch_addr = 0x40104dL
    val cmp_addr = 0x401055L
    val final_key = Buffer.create 32
    val mutable counter = 0L
    val mutable stop_code = 1 (* so false *)


    method! private pre_execution (_env:Path_predicate_env.t) : unit =
      ()

    method! private visit_instr_before (key:int) (tr_inst:trace_inst) (_env:Path_predicate_env.t): trace_visit_action =
      Logger.result "%d %Lx\t%s" key tr_inst.location tr_inst.mnemonic;
      DoExec

    method! private visit_dbainstr_before (_key:int) inst dbainst (env:Path_predicate_env.t): trace_visit_action =
      if inst.location = jnz_addr then
        begin
          begin
            match Dba_types.Statement.instruction dbainst with
            | Dba.Instr.If (cond, Dba.JOuter _ , _off) ->
              if self#is_symbolic_condition cond env then
                let next_addr = get_next_address inst.concrete_infos in
                let pred = mk_bl_not (self#build_cond_predicate cond env) in
                build_formula_file env.formula pred smt_file |> ignore;
                let solver = Formula_options.Solver.of_piqi
                               config.Configuration.solver in
                let timeout = Int32.to_int config.Configuration.timeout in
                let res, model = solve_model ~timeout smt_file solver in
                begin
                  match res with
                  | SAT ->
                    begin
                      let addr =
                        let v =
                          Int64.add base_input counter
                          |> Bigint.big_int_of_int64 in
                        Bitvector.create v (Kernel_options.Machine.word_size ()) in
                      match Smt_model.find_address_contents model addr with
                      | Some c ->
                        counter <- Int64.succ counter;
                        let c = Char.chr (Bitvector.to_int c) in
                        Logger.info "[%a] -> %c" Formula_pp.pp_status res c;
                        Buffer.add_char final_key c;
                        if next_addr = fail_addr then
                          self#generate_new_configuration model
                      | None -> ()
                    end
                  | UNSAT -> Logger.error "[UNSAT]@."
                  | TIMEOUT | UNKNOWN -> Logger.warning "[TIMEOUT]@."
                end
              else
                Logger.result "Not a symbolic formula"
            | _ -> ();
          end;
          DoExec
        end
      else if inst.location = success_addr then begin
        Logger.info "Success target reached ! Key:%s" (Buffer.contents final_key);
        stop_code <- 0;
        StopExec
      end
      else
        DoExec

    method! private visit_dbainstr_after (key:int) (_inst:trace_inst) dbainst (env:Path_predicate_env.t): trace_visit_action =
      match Dba_types.Statement.instruction dbainst with
      | Dba.Instr.DJump(_,_) ->
        Libcall_stubs.apply_default_stub key config.Configuration.callcvt env;
        DoExec
      | _ -> DoExec

    method! private visit_instr_after (_key:int) (_tr_inst:trace_inst) (_env:Path_predicate_env.t): trace_visit_action =
      DoExec

    method! private input_message_received (cmd:string) (data:string): unit =
      match cmd with
      | "BP_REACHED" ->
        Logger.warning "Breakpoint reached : %s" data;
        self#send_message "PATCH_ZF" "1";
        self#send_message "RESUME" "STUB"
      | _ -> super#input_message_received cmd data


    method private generate_new_configuration model =
      let open Trace_config in
      match input_config.trace_input with
      | Stream _ -> Logger.warning "No need to generate new config file"
      | Chunked _ ->
         let b = Buffer.create 32 in
         let rec aux bv =
           match Smt_model.find_address_contents model bv with
           | Some value ->
              Buffer.add_char b (Char.chr @@ Bitvector.to_int value);
              aux (Bitvector.succ bv)
           | None -> Buffer.contents b
         in

        let value =
          let bv =
            let value = Bigint.big_int_of_int64 base_input in
            Bitvector.create value (Kernel_options.Machine.word_size ()) in
          aux bv in
        let input  =
          Input_t.({typeid = `mem;
                    address = patch_addr;
                    when_ = `before;
                    iteration = 0l;
                    action = `patch;
                    reg = None;
                    mem = Some Memory_t.({addr = base_input; value});
                    indirect = None}) in
        config.Configuration.inputs <- input :: config.Configuration.inputs;
        let data = Config_piqi_ext.gen_configuration config `json_pretty in
        let filename =
          Printf.sprintf "%s/new_config.json"
            (Filename.dirname input_config.trace_file) in
        let fd = open_out_bin filename in
        output_string fd data;
        stop_code <- 0;
        close_out fd

    method! private post_execution _ = stop_code

  end;;
