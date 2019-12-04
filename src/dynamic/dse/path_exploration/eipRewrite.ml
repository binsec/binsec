(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2019                                               *)
(*    VERIMAG                                                             *)
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
(**************************************************************************)

open Dba
open Path_predicate_env
open Trace_type
open Path_predicate_formula
open SymbolicInput
open Config_piqi
open Configuration
open Dse_options

let get_addr_size e = e.Path_predicate_env.formula.addr_size

class eip_rewrite (trace_config:Trace_config.t) =
  object(self) inherit InvertChild.invert_child trace_config

    val mutable eip_is_rewrite = false

    method is_eip_rewrite = eip_is_rewrite

    method add_eip_check predicate (env:Path_predicate_env.t) =
      let open Formula in
      (* First get the last name of esp and add +4 (because of ret ) *)
      let _, esp =
        get_var_or_create env.formula "esp" (get_addr_size env) 0 31 in
      let esp =
        mk_bv_sub esp
          (mk_bv_cst
             (Bitvector.create (Bigint.big_int_of_int 0x4) (get_addr_size env)))
      in
      (* Get the last val of memory *)
      let mem_name =
        Printf.sprintf "memory%d" ((get_varindex env.formula "memory") - 1) in
      (* Load value in esp +4 *)
      let load_esp =
        mk_select 4 (mk_ax_var (ax_var mem_name 0 (get_addr_size env))) esp in
      let val_eip =
        mk_bv_cst
          (Bitvector.create (Bigint.big_int_of_int 0x41414141) (get_addr_size env))
      in
      (* compare esp +4 with \x41414141 *)
      mk_bl_and predicate (mk_bv_equal val_eip load_esp)

    method! visit_dbainstr_before (_key:int) inst (dbainst:Dba_types.Statement.t) (env:Path_predicate_env.t) =
      let addr = inst.location in
      let it = try Hashtbl.find instruction_counter addr with Not_found -> 0 in
      let instruction = Dba_types.Statement.instruction dbainst in
      let open Instr in
      (match instruction with
       | If _ | DJump _ -> name_trace <- Printf.sprintf "%s_0x%Lx-%d" name_trace addr it
       | Assign _ | SJump _ | Stop _ | Assert _| Assume _
       | NondetAssume _|Nondet _
       | Undef _|Malloc _|Free _|Print _-> ()
      );
      (match instruction with
       (* Try to invert all ret instruction *)
       | DJump _  when inst.mnemonic ="ret" ->
         begin
           let open Trace_config in
           let open Formula in
           let formula_file = "formula-eip"^(name_trace)^".smt2" in
           let static_predicate = self#build_cond_predicate Dba.Expr.one env in
           let predicate = self#add_eip_check static_predicate env in
           ignore @@ Path_predicate_formula.build_formula_file env.formula
                       predicate formula_file;
           let solver =
             Formula_options.Solver.of_piqi trace_config.configuration.solver in
           let result, model = Solver.solve_model formula_file solver in
           match result with
           | SAT ->
             eip_is_rewrite <- true;
             Logger.debug "SAT!";
             let new_inputs = SymbolicInput.update input_entries model in
             let config = trace_config.configuration in
             let new_inputs_export =
               SymbolicInput.export_inputs `patch new_inputs in
             let inputs =
               self#update_inputs config.Configuration.inputs new_inputs_export in
             config.Configuration.inputs <- inputs;
             let new_file =
               "config-EIP-"^(name_trace(*Printf.sprintf "%04d" key*))^".json" in
             let oc = open_out new_file in
             (*  let opts = Piqirun_ext.make_options ~json_omit_missing_fields:true () in*)
             let data = Config_piqi_ext.gen_configuration config `json_pretty in
             output_string oc data;
             close_out oc;
             new_conf_files <- new_file::new_conf_files

           | UNSAT | TIMEOUT | UNKNOWN ->
             Logger.debug
               "inversed input of conditional jump at 0x%Lx not found"
               inst.location
         end
       | DJump _ | If _   | Assign _
       | SJump _ | Stop _ | Assert _
       | Assume _
       | NondetAssume _
       | Nondet _
       | Undef _
       | Malloc _
       | Free _
       | Print _ -> ()
      );
      Path_predicate.DoExec


  end;;
