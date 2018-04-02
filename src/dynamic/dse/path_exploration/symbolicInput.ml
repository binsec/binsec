(**************************************************************************)
(*  This file is part of Binsec.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
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

(*************************************************************************************************)
(* Information about input is parsed from configuration file of Pintool. Each line of this file  *)
(* is one of the following form:                                                                 *)
(*                                                                                               *)
(*  @ins_address, iteration, register_name : low high,  value, after/before,   symbolic_name     *)
(*  @ins_address, iteration, address : size,            value, after/before,   symbolic_name     *)
(*  @ins_address, iteration, [register_name] : size,    value, after/before,   symbolic_name     *)
(*                                                                                               *)
(* then they are represented by a record:                                                        *)
(*                                                                                               *)
(*    Big_int,      int,       input_id,                  bool,         Big_int, string          *)
(*                                                                                               *)
(* Notes: before = 0, after = 1; iteration starting from 0                                       *)
(*************************************************************************************************)

open Config_piqi
open Input_t
open Common
open Memory_t
open Configuration

module type SymbolicInput =
sig
  type input_id_t
  type input_entry_t
  type input_entries_t

  val init                         : unit -> input_entries_t
  val concat                       : input_entries_t -> input_entries_t -> input_entries_t
  val length                       : input_entries_t -> int

  val get_input_entry_address      : input_entry_t -> int64
  val get_input_entry_iteration    : input_entry_t -> int
  val get_input_entry_name         : input_entry_t -> string
  val get_all_names                : input_entries_t -> string list
  val get_value_of_input           : input_entries_t -> string -> int64
  val get_input_entry_value        : input_entry_t -> int64

  val get_input_entry_register     : input_entry_t -> string option
  val get_input_entry_mem          : input_entry_t -> (int64 * int) option

  val add_inputs_mem               : input_entries_t -> int64 -> int -> int64 -> int -> bool -> input_entries_t
  val parse_inputs                 : Common.action -> Config_piqi.input_t list -> input_entries_t
  val parse_config_json_patch      : string -> input_entries_t
  val parse_config_json_conc       : string -> input_entries_t

  val export_inputs                : Common.action -> input_entries_t -> Config_piqi.input_t list
(*  val update_config_json           : string -> ?new_file:string -> ?conc:input_entries_t -> ?patch:input_entries_t -> unit*)

  val find_input_from_name         : input_entries_t -> string -> input_entry_t
  val find_input_from_position     : input_entries_t -> int64 * int -> input_entry_t
  val find_all_input_from_position : input_entries_t -> int64 * int -> bool -> input_entries_t
  val remove_all_input_from_position : input_entries_t -> int64 * int -> bool -> input_entries_t

  val modify_input_entry_value     : input_entry_t -> int64 -> input_entry_t
  val create_mem_values            : int64 -> string -> (int64 * int64) list

  val to_list                      : input_entries_t -> input_entry_t list
  val to_list_order_decr           : input_entries_t -> input_entry_t list

  val update                       : input_entries_t -> Smt_model.t -> input_entries_t
end

module SymbolicInput : SymbolicInput =
struct
  type input_id_t =
    | Register of string * int          (* register name and size *)
    | Address of int64 * int            (* address and size *)
    (*| Indirect of string * int            (* indirect, name and size *)*)

  type input_entry_t = { addr_input  : int64;
                         it_input    : int;
                         id_input    : input_id_t;
                         value_input : int64;
                         pos_input   : bool;
                         name_input  : string }

  type input_entries_t = input_entry_t list

  (*******************************************************************************)

  let init () = []

  let concat a b  = a@b

  let length a = List.length a

  let get_input_entry_address (input_entry:input_entry_t) = input_entry.addr_input
  let get_input_entry_iteration (input_entry:input_entry_t) = input_entry.it_input
  let get_input_entry_name (input_entry:input_entry_t) = input_entry.name_input
  let get_all_names input_entries = List.map get_input_entry_name input_entries
  let get_input_entry_value (input_entry:input_entry_t) = input_entry.value_input

  let get_value_of_input input_entries name =
    try
       let input = List.find (fun x -> (String.compare x.name_input name) =0) input_entries in
       input.value_input
    with Not_found -> failwith (Printf.sprintf "Error %s not found\n" name)

  let get_input_entry_register (input_entry:input_entry_t) =
    match input_entry.id_input with
    | Register (name, _) -> Some name
    | Address _  -> None

  let get_input_entry_mem input_entry =
    match input_entry.id_input with
    | Address (addr, size) -> Some (addr, size)
    | Register _  -> None

  (*******************************************************************************)

  let char_list_of_string str =
    let char_list = ref [] in
    String.iter (fun c -> char_list := c :: !char_list) str;
    List.rev !char_list

  (* split addr  * string ->  (addr * c) list *)
  let create_mem_values addr s =
    char_list_of_string s |>
    List.mapi (fun i c ->
        Int64.add addr @@ Int64.of_int i,
        Int64.of_int @@ Char.code c
    )

  (*******************************************************************************)

  let construct_register_input (reg:Common.register_t) input_address input_iteration input_position input_idx =
    let (reg_value, reg_size) =
      let value = (reg.Register_t.value) in
      let typeid = (value.Register_value_t.typeid) in
      match typeid with
      | `bit8 -> (match (value.Register_value_t.value_8) with
          | Some v8 -> (Int64.of_int32 v8, 8) | None -> failwith "malformed json format")
      | `bit16 -> (match (value.Register_value_t.value_16) with
          | Some v16 -> (Int64.of_int32 v16, 16) | None -> failwith "malformed json format")
      | `bit32 -> (match (value.Register_value_t.value_32) with
          | Some v32 -> (Int64.of_int32 v32, 32) | None -> failwith "malformed json format")
      | `bit64 -> (match (value.Register_value_t.value_64) with
          | Some v64 -> (v64, 64) | None -> failwith "malformed json format")
      | `bit80 | `bit128 | `bit256 -> (Int64.zero, 0)
      | _ -> failwith "malformed json format"
    in
    { addr_input   = input_address;
      it_input     = input_iteration;
      id_input     = Register (Register_t.(reg.name), reg_size);
      value_input  = reg_value;
      pos_input    = input_position;
      name_input   = Printf.sprintf "input%d" input_idx }

  let construct_memory_input (mem:Common.memory_t) input_address input_iteration input_position input_idx =
    let mem_offset = ref (-1) and idx = ref (input_idx - 1)
    and mem_char_values = char_list_of_string mem.value in
    List.map (fun mem_char_value ->
        idx := !idx + 1; mem_offset := !mem_offset + 1;
        { addr_input   = input_address;
          it_input     = input_iteration;
          id_input     = Address (Int64.add mem.addr @@ Int64.of_int !mem_offset, 1);
          value_input  = Int64.of_int @@ Char.code mem_char_value;
          pos_input    = input_position;
          name_input   = Printf.sprintf "input%d" !idx }
      ) mem_char_values


  let construct_memory_input_sized mem sz input_address it input_position prev_index  =
    let mem_offset = ref (Int64.sub mem 1L) in
    let idx = ref (prev_index-1) in
    let mem_char_values = Array.to_list (Array.make sz 0x41) in
    List.map (fun mem_char_value ->
        idx := !idx + 1; mem_offset := (Int64.add (!mem_offset)  1L);
        { addr_input  = input_address;
          it_input    = it;
          id_input    = Address (!mem_offset, 1);
          value_input = Int64.of_int mem_char_value;
          pos_input   = input_position;
          name_input  = Printf.sprintf "input%d" !idx }
      ) mem_char_values

  let add_inputs_mem inputs mem_addr size mem_inst_addr  mem_inst_addr_it pos =
    let prev_index = List.length inputs in
    let new_inputs = construct_memory_input_sized mem_addr size mem_inst_addr mem_inst_addr_it pos prev_index in
    new_inputs


  (* transform list of inputs in symbolic input format *)
  let parse_inputs input_action piqi_inputs =
    let inputs = List.filter (fun pq_input -> pq_input.action = input_action
        (* match pq_input.action with *)
        (* | act_type -> act_type = input_type  | _ -> false *)
      ) piqi_inputs
    in
    let input_idx = ref 0 in
    List.fold_left (fun acc_inputs (input:Input_t.t) ->
        let pos = match input.when_ with | `before -> false | `after -> true in
        match input.reg, input.mem with
        | Some reg, None ->
          let new_input = construct_register_input reg input.address (Int32.to_int input.iteration) pos !input_idx in
          (input_idx := !input_idx + 1; new_input :: acc_inputs)

        | None, Some mem ->
          let new_inputs = construct_memory_input mem input.address (Int32.to_int input.iteration) pos !input_idx in
          (input_idx := !input_idx + List.length new_inputs; new_inputs @ acc_inputs)

        | _, _ -> failwith "error during input parsing"
      ) [] inputs

  (*******************************************************************************)

  (* open json file, and extract inputs of type "patch" *)
  let parse_config_json_patch config_file =
    let file_ic = open_in config_file in
    let file_len = in_channel_length file_ic in
    let raw_buffer = String.make file_len ' ' in
    let raw_buffer_bytes = Bytes.make file_len ' ' in
    really_input file_ic raw_buffer_bytes 0 file_len;
    close_in file_ic;
    let opts = Piqirun_ext.make_options ~json_omit_missing_fields:true () in
    let config = Config_piqi_ext.parse_configuration ~opts raw_buffer `json in
    let inputs = config.inputs in
    parse_inputs `patch inputs

  (*******************************************************************************)

  (* open json file, and extract inputs of type "conc" *)
  let parse_config_json_conc config_file =
    let file_ic = open_in config_file in
    let file_len = in_channel_length file_ic in
    let raw_buffer = String.make file_len ' ' in
    let raw_buffer_bytes = Bytes.make file_len ' ' in
    really_input file_ic raw_buffer_bytes 0 file_len;
    close_in file_ic;
    let opts = Piqirun_ext.make_options ~json_omit_missing_fields:true () in
    let config = Config_piqi_ext.parse_configuration ~opts raw_buffer `json in
    let inputs = config.inputs in
    parse_inputs `conc inputs

  (*******************************************************************************)

  (* Debuging function, please do not delete it until the release *)
 (* let print_input_info (input_entries:input_entries_t) =
    List.iter (fun input_entry ->
        let entry_addr = input_entry.addr_input
        and entry_value = input_entry.value_input
        and entry_name = input_entry.name_input in
        Int64.to_int entry_addr |> (Int64.to_int entry_value |> Char.unsafe_chr |> Logger.debug "%s : %c (%d)\n" entry_name)
      ) input_entries*)


  let construct_register_json_format reg_entry input_value input_address input_iteration input_position input_action =
    match reg_entry with
    | Register (reg_name, reg_size) ->
      let json_reg_value = Common.default_register_value_t () in
      (match reg_size with
       | 8 ->
         json_reg_value.Register_value_t.typeid <- `bit8;
         json_reg_value.Register_value_t.value_8 <- Some (Int64.to_int32 input_value)
       | 16 ->
         json_reg_value.Register_value_t.typeid <- `bit16;
         json_reg_value.Register_value_t.value_16 <- Some (Int64.to_int32 input_value)
       | 32 ->
         json_reg_value.Register_value_t.typeid <- `bit32;
         json_reg_value.Register_value_t.value_32 <- Some (Int64.to_int32 input_value)
       | 64 ->
         json_reg_value.Register_value_t.typeid <- `bit64;
         json_reg_value.Register_value_t.value_64 <- Some input_value
        | s -> failwith (Printf.sprintf "Reg with wrong size ? %d" s)
      );
      { Input_t.typeid = `reg;
        Input_t.address = input_address;
        Input_t.iteration = input_iteration;
        Input_t.when_ = input_position;
        Input_t.action = input_action;
        Input_t.reg = Some { Register_t.name = reg_name; Register_t.value = json_reg_value };
        Input_t.mem = None ;
        Input_t.indirect=  None}
    | Address _  -> failwith "register constructor called with wrong entry type"

  let construct_memory_json_format mem_entry input_value input_address input_iteration input_position input_action =
    match mem_entry with
    | Address (mem_addr, _) ->
      { typeid  = `mem;
        address = input_address;
        iteration = input_iteration;
        when_   = input_position;
        action  = input_action;
        reg     = None;
        (* Conversion tricky int64 -> int -> char (unsafe_chr because of values outside of the range 0-255) -> string *)
        mem     = Some { addr = mem_addr; value = String.make 1 @@ Char.unsafe_chr @@ Int64.to_int input_value } ;
        indirect=None;}
    | Register _ -> failwith "memory constructor called with wrong entry type"

  (* convert symbolic inputs to json format *)
  let export_inputs input_action inputs_entries =
    List.map (fun input_entry ->
        let entry_addr = input_entry.addr_input
        and entry_iteration = (Int32.of_int input_entry.it_input)
        and entry_id = input_entry.id_input
        and entry_value = input_entry.value_input
        and entry_pos = if input_entry.pos_input then `after else `before in
        match entry_id with
        | Register _ -> construct_register_json_format entry_id entry_value entry_addr entry_iteration entry_pos input_action
        | Address _ -> construct_memory_json_format entry_id entry_value entry_addr entry_iteration entry_pos input_action
      ) inputs_entries

  (*******************************************************************************)

  (* open json file, and export inputs vals *)
(*
  let update_config_json config_file ?new_file:new_filename ?conc:conc_input_entries ?patch:patch_input_entries =
    let file_ic = open_in config_file in
    let file_len = in_channel_length file_ic in
    let raw_buffer = String.make file_len ' ' in
    (
      really_input file_ic raw_buffer 0 file_len; close_in file_ic;
      close_in file_ic;
      let new_config_file =  match new_filename with
        | None -> config_file
        | Some filename -> filename
      in
      let file_oc = open_out new_config_file in
      let opts = Piqirun_ext.make_options ~json_omit_missing_fields:true () in
      let config = Config_piqi_ext.parse_configuration ~opts raw_buffer `json in
      (
        let conc_entries =
          match conc_input_entries with
          | None -> [] | Some entries -> export_inputs `conc entries
        and patch_entries =
          match patch_input_entries with
          | None -> [] | Some entries -> export_inputs `patch entries
        in
        config.inputs <- List.append conc_entries patch_entries;
        (* config.inputs <- export_inputs input_entries; *)
        Config_piqi_ext.gen_configuration config `json_pretty |> output_string file_oc;

        close_out file_oc
      )
    )
*)
  (*******************************************************************************)

  let find_input_from_name input_entries input_name =
    List.find (fun entry -> entry.name_input = input_name) input_entries

  (*******************************************************************************)

  let find_input_from_position (input_entries:input_entries_t) location =
    List.find (fun entry ->
        entry.addr_input = fst location && entry.it_input = snd location
      ) input_entries

  let is_input (loc1, loc2) position entry  =
    entry.addr_input = loc1
    && entry.it_input = loc2
    && entry.pos_input = position

  let find_all_input_from_position (input_entries:input_entries_t) location position  =
    List.find_all (is_input location position) input_entries


  let remove_all_input_from_position (input_entries:input_entries_t) location position  =
    List.filter
      (fun entry -> not (is_input location position entry))
      input_entries

  (*******************************************************************************)

  let modify_input_entry_value (input_entry:input_entry_t) value_input =
    { input_entry with value_input }

  let get_id x = Scanf.sscanf x "input%d" (fun x -> x)

  let to_list l = l
  let to_list_order_decr l =
    List.sort
      (fun x y -> compare (get_id y.name_input) (get_id x.name_input) ) l

  let update entries model =
      List.map (
        fun x ->
          try
            let new_val =
              Bigint.int64_of_big_int (
                Bitvector.value_of (
                  Smt_model.find_register model
                    (get_input_entry_name x)))
            in modify_input_entry_value x new_val
          with Not_found -> x
      ) entries


end
