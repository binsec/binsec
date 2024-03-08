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

open Smt_options

let byte_size = Natural.to_int Basic_types.Constants.bytesize

module F = struct
  let full name index = name ^ "_" ^ string_of_int index
  let memory = "__memory"
  let full_mem = full memory
  let memory_type word_size = Formula.ax_sort word_size byte_size
  let pc = "__pc"
  let full_pc = full pc

  let var name =
    let open Formula in
    function
    | BlSort -> BlVar (bl_var name)
    | BvSort i -> BvVar (bv_var name i)
    | AxSort (i, j) -> AxVar (ax_var name i j)

  let decl =
    let open Formula in
    function
    | BlVar v -> mk_bl_decl v []
    | BvVar v -> mk_bv_decl v []
    | AxVar v -> mk_ax_decl v []

  let def value var =
    let open Formula in
    match (value.term_desc, var) with
    | BlTerm value, BlVar v -> mk_bl_def v [] value
    | BvTerm value, BvVar v -> mk_bv_def v [] value
    | AxTerm value, AxVar v -> mk_ax_def v [] value
    | _ -> failwith "F.def has incompatible types"
end

(* global mutable state to assign unique indices *)
let next_index = ref 0

let mk_index () =
  next_index := !next_index + 1;
  !next_index

(* variable bindings but one per branch *)
module State = struct
  module S = Basic_types.String.Map
  module B = Bitvector.Collection.Map

  type infos = int * Formula.sort (* index, type *)

  type t = {
    fml : Formula.formula;
    var_infos : infos S.t;
    initialisation : int B.t;
    (* list of memory locations to read *)
    uncontrolled : Formula.VarSet.t;
  }

  let add_entry entry state =
    let fml = Formula.push_front entry state.fml in
    { state with fml }

  let add_uncontrolled var st =
    { st with uncontrolled = Formula.VarSet.add var st.uncontrolled }

  let havoc ?(wild = false) name var_type state =
    let open Formula in
    let index = mk_index () in
    let var = F.var (F.full name index) var_type in
    let declaration = F.decl var in
    let var_infos = S.add name (index, var_type) state.var_infos in
    let state' = add_entry (mk_declare declaration) { state with var_infos } in
    if wild then add_uncontrolled var state' else state'

  let declare ?wild name var_type state =
    if S.mem name state.var_infos then failwith "variable already declared";
    havoc ?wild name var_type state

  let assign ?(wild = false) name var_type value state =
    let open Formula in
    (match S.find name state.var_infos with
    | _, vtype when vtype <> var_type ->
        failwith "Store.get_last_index with wrong type"
    | _ -> ()
    | exception Not_found -> ());
    let index = mk_index () in
    let var = F.var (F.full name index) var_type in
    let definition = F.def value var in
    let var_infos = S.add name (index, var_type) state.var_infos in
    let state' = add_entry (mk_define definition) { state with var_infos } in
    if wild then add_uncontrolled var state' else state'

  let create () =
    let open Formula in
    let fml = Formula.empty
    and var_infos =
      S.singleton F.memory
        (0, F.memory_type (Kernel_options.Machine.word_size ()))
    in
    let initialisation = B.empty in
    let uncontrolled = Formula.VarSet.empty in
    let self = { fml; var_infos; initialisation; uncontrolled } in
    assign F.pc bl_sort (mk_bl_term mk_bl_true) self

  let initializations st = st.initialisation
  let comment cmt state = add_entry (Formula.mk_comment cmt) state

  let pp ppf state =
    let open Format in
    fprintf ppf "@[<v 0># State var_infos @ @[<hov 0>%a@]@]"
      (fun ppf m -> S.iter (fun name (n, _) -> fprintf ppf "%s:%d;@ " name n) m)
      state.var_infos

  let get_last_index state name var_type =
    match S.find name state.var_infos with
    | _, v when v <> var_type -> failwith "State.get_last_index with wrong type"
    | n, _ -> n

  let get_memory state =
    let word_size = Kernel_options.Machine.word_size () in
    let index = get_last_index state F.memory (F.memory_type word_size) in
    let name = F.full_mem index in
    Formula.(mk_ax_var (ax_var name word_size byte_size))

  let get_path_constraint state =
    let index = get_last_index state F.pc Formula.bl_sort in
    let name = F.full_pc index in
    Formula.(mk_bl_var (bl_var name))

  let constrain cond state =
    let open Formula in
    let current_pc = get_path_constraint state in
    let pc = mk_bl_and current_pc cond in
    assign F.pc bl_sort (mk_bl_term pc) state

  let get_bv name size state =
    let sort = Formula.bv_sort (Size.Bit.to_int size) in
    let index, state'' =
      match get_last_index state name sort with
      | n -> (n, state)
      | exception Not_found ->
          let state' = declare (*FIXME wild ? *) name sort state in
          (get_last_index state' name sort, state')
    in
    let name = F.full name index in
    let value = Formula.(mk_bv_var (bv_var name (Size.Bit.to_int size))) in
    (value, state'')

  let init_mem_at ~addr ~size state =
    { state with initialisation = B.add addr size state.initialisation }

  let get_entries state =
    let open Formula in
    let word_size = Kernel_options.Machine.word_size () in
    let var = F.(var memory (memory_type word_size)) in
    let declaration = F.decl var in
    let symbolic_memory = mk_ax_var (ax_var F.memory word_size byte_size) in
    let read_bitvector addr sz =
      let b = Buffer.create (2 * sz) in
      (* The loop below is little-endian *)
      let rec loop offset =
        if offset < 0 then (
          let v = Z.of_string ("0x" ^ Buffer.contents b) in
          let bv = Bitvector.create v (byte_size * sz) in
          Logger.debug ~level:5 "[sse] C bitvector %s (%d): %a" (Z.to_string v)
            sz Bitvector.pp_hex bv;
          bv)
        else
          let off_bv = Bitvector.of_int ~size:word_size offset in
          let load_addr = Bitvector.add addr off_bv in
          let img = Kernel_functions.get_img () in
          let byte = Loader_utils.get_byte_at img load_addr in
          let byte_str = Format.sprintf "%02x" byte in
          Buffer.add_string b byte_str;
          loop (offset - 1)
      in
      loop (sz - 1)
    in
    let load_at addr size mem =
      assert (word_size = Bitvector.size_of addr);
      mk_store size mem (mk_bv_cst addr) (mk_bv_cst (read_bitvector addr size))
    in
    let initial_memory_value =
      mk_ax_term (B.fold load_at state.initialisation symbolic_memory)
    in
    let definition =
      F.var (F.full_mem 0) (F.memory_type word_size)
      |> F.def initial_memory_value
    in
    state.fml
    |> Formula.push_back_define definition
    |> Formula.push_back_declare declaration

  let formula state =
    Formula.push_front_assert (get_path_constraint state) (get_entries state)

  let uncontrolled st = st.uncontrolled

  let memory_term fml =
    ( F.memory,
      F.memory_type (Kernel_options.Machine.word_size ()),
      Formula.mk_ax_term fml )
end
