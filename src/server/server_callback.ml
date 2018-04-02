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

open Decode_utils
open Dba_io
open Message_piqi
open Config_piqi
open Configuration

exception Stop

let request_decode_instr ident (message:message_decode_instr): unit =
  let f = match message.Message_decode_instr.kind with
    | `hexa -> decode_hex_opcode
    | `bin -> decode_bin_opcode
  in
  try
    match message.Message_decode_instr.irkind with
    | `dba ->
       let result =
         let open Message_decode_instr_instr_entry in
         List.fold_left
           (fun acc e ->
             let opc, insts = f ~addr:e.base_addr e.instr in
             Logger.debug  "Decoded:%s" opc;
             let insts = Dba_types.Block.to_dbainstrs insts (Dba_types.Virtual_address.of_int64 e.base_addr) in
             (opc, insts) :: acc
          ) [] message.Message_decode_instr.instrs
      in
      let entries =
        let open Message_decode_instr_reply_instr_entry in
        List.fold_left
          (fun acc (opc, insts) ->
             let dbalist = generate_dbalist insts in
             let entry = {opcode=opc; irkind=`dba; dba_instrs=Some dbalist} in
             entry::acc
          ) [] result
      in
      let data =
        Piqirun.to_string
          (gen_message_decode_instr_reply
             ({Message_decode_instr_reply.instrs=entries})) in
      Network_io.send_client_message ident ~block:true "DECODE_INSTR_REPLY" data
    | _ -> 
      Network_io.send_client_message ident ~block:true "END" "ir_kind not supported"
  with _ ->
    Network_io.send_client_message ident ~block:true "END" "Error occured during decoding"

let request_start_analysis ident (config:configuration): int =
  let full_config = {Options.configuration=config;
                     trace_file="";
                     trace_input=Options.Stream(ident)} in
  Libcall_stubs.check_libcall_policy_consistency config.libcalls config.default_action |> ignore;
  match String.uppercase config.analysis_name with
  | "GENERIC" ->
    let analyzer = new Generic_analyse.generic_analyzer full_config in
    analyzer#compute
  | "CALLRET" ->
    let analyzer = new Call_ret.callret_analyzer full_config in
    analyzer#compute
  | "OPAQUE" ->
    let analyer = new Opaque_predicate.opaque_analyzer full_config in
    analyer#compute
  | "STATIC OPAQUE" ->
    let analyer = new Opaque_predicate.static_opaque_analyzer full_config in
    analyer#compute
  | _ ->
    Logger.error "Unknown analysis %s" config.analysis_name;
    1

let request_infos ident: unit =
  let open Message_infos in
  let infos = { nb_workers=(Int32.of_int !Options.nb_workers); analyses=["generic"; "callret"; "opaque"]; solvers=["Z3";"boolector";"CVC4";"yices"] } in
  let data = Piqirun.to_string (gen_message_infos infos) in
  Network_io.send_client_message ident ~block:true "INFOS" data

let request_dispatcher ident (cmd:string) (data:string): unit =
  let buf = Piqirun.init_from_string data in
  match cmd with
  | "DIE" ->
    Logger.debug ~level:1 "Worker[%s]: stop" ident;
    raise Stop
  | "DECODE_INSTR" ->
    let message = parse_message_decode_instr buf in
    request_decode_instr ident message;
  | "START_ANALYSIS" ->
    Logger.result "Worker[%s]: START_ANALYSIS request received" ident;
    Network_io.log_to_zmq true ident;
    let conf = parse_configuration buf in
    let code = request_start_analysis ident conf in
    Network_io.log_to_zmq false ident;
    Logger.info "Worker[%s]: analysis terminated" ident;
    Network_io.send_client_message ident ~block:true "END" (string_of_int code);
  | "GET_INFOS" ->
    request_infos ident;
  | _ ->
    Logger.warning "Worker[%s]: unhandled command %s" ident cmd;
