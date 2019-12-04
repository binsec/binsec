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

open Sse_options
;;

let byte_size = Natural.to_int Basic_types.Constants.bytesize

module F = struct
  let full name index = name ^ "_" ^ (string_of_int index)
  let memory = "__memory"

  let full_mem = full memory
  let memory_type word_size = Formula.ax_sort word_size byte_size

  let pc = "__pc"
  let full_pc = full pc

  let var name =
    let open Formula in
    function
    | BlSort       -> BlVar (bl_var name)
    | BvSort i     -> BvVar (bv_var name i)
    | AxSort (i,j) -> AxVar (ax_var name i j)
  ;;

  let decl =
    let open Formula in
    function
    | BlVar v -> mk_bl_decl v []
    | BvVar v -> mk_bv_decl v []
    | AxVar v -> mk_ax_decl v []
  ;;

  let def value var =
    let open Formula in
    match value.term_desc, var with
    | BlTerm value, BlVar v -> mk_bl_def v [] value
    | BvTerm value, BvVar v -> mk_bv_def v [] value
    | AxTerm value, AxVar v -> mk_ax_def v [] value
    | _ -> failwith "F.def has incompatible types"
  ;;

end

(* common assignments for all branches
 * mutable; shared by states *)
module Store = struct
  module M = Basic_types.String.Map
  type infos = int * Formula.sort (* index, type *)
  type t = {
    mutable assignments : Formula.formula;
    mutable var_infos : infos M.t;
  }

  let add_entry store entry =
    store.assignments <- Formula.push_front entry store.assignments

  let declare ~index store name var_type =
    let open Formula in
    let initial_index = index in
    let var = F.var (F.full name initial_index) var_type in
    let declaration = F.decl var  in
    store.var_infos <- M.add name (initial_index, var_type) store.var_infos;
    add_entry store (mk_declare declaration)


  (* doesn't declare if necessary; raises Not_Found *)
  let get_infos store name = M.find name store.var_infos

  let next_index store name var_type =
    match get_infos store name with
    | _, vtype when vtype <> var_type ->
       failwith "Store.get_last_index with wrong type"
    | n, _ -> n + 1
    | exception Not_found -> 0

  (* declares the variable if necessary *)
  let get_last_index store name var_type =
    match get_infos store name with
    | _, vtype when vtype <> var_type ->
      failwith "Store.get_last_index with wrong type"
    | n, _ -> n
    | exception Not_found ->
       Logger.debug "Store : adding %s" name;
       (declare ~index:0 store name var_type; 0)
  ;;

  let declare store name var_type =
    let index = next_index store name var_type in
    declare ~index store name var_type; index


  let assign store name var_type value =
    let open Formula in
    let index = match M.find name store.var_infos with
      | _, vtype when vtype <> var_type ->
        failwith "Store.get_last_index with wrong type"
      | n, _ -> n + 1
      | exception Not_found -> 0
    in
    let var = F.var (F.full name index) var_type in
    let definition = F.def value var in
    store.var_infos <- M.add name (index, var_type) store.var_infos;
    add_entry store (mk_define definition);
    index

  let create () =
    let open Formula in
    let assignments = Formula.empty
    and var_infos = M.singleton F.memory
                      (0, F.memory_type (Kernel_options.Machine.word_size ())) in
    let self = { assignments; var_infos } in
    assign self F.pc bl_sort (mk_bl_term mk_bl_true) |> ignore;
    self

end

(* variable bindings but one per branch *)
module State = struct
  module S = Basic_types.String.Map
  module B = Bitvector.Collection.Map

  type t = {
    store : Store.t;
    initialisation : int B.t; (* list of memory locations to read *)
    var_index : int S.t;
    uncontrolled : Formula.VarSet.t;
  }

  let initializations st = st.initialisation

  let create () =
    let store = Store.create ()in
    let var_index = S.empty in
    let initialisation = B.empty in
    let uncontrolled = Formula.VarSet.empty in
    { store; var_index ; initialisation; uncontrolled; }

  let add_uncontrolled name index sort st =
    let var =
      let id = F.full name index in
      F.var id sort in
    { st with uncontrolled = Formula.VarSet.add var st.uncontrolled }

  let assign ?(wild=false) name sort value state =
    let new_index = Store.assign state.store name sort value in
    let st = { state with var_index = S.add name new_index state.var_index } in
    if wild then add_uncontrolled name new_index sort st else st

  let declare ?(wild=false) name sort state =
    let new_index = Store.declare state.store name sort in
    let st =
      { state with var_index = S.add name new_index state.var_index } in
    if wild then add_uncontrolled name new_index sort st else st

  let comment cmt state =
    Formula.mk_comment cmt
    |> Store.add_entry state.store;
    state

  let pp ppf state =
    let open Format in
    fprintf ppf
    "@[<v 0># State var_index @ @[<hov 0>%a@]@,# Store var_infos@ @[<hov 0>%a@]@]"
    (fun ppf m -> S.iter (fun name n -> fprintf ppf "%s:%d;@ " name n) m)
    state.var_index
    (fun ppf m -> S.iter (fun name (n, _) -> fprintf ppf "%s:%d;@ " name n) m)
    state.store.Store.var_infos

  (* BEGIN the 3 functions below should not be useful but they are *)
  let has_empty_vinfos st = S.is_empty st.var_index

  let copy_store st =
    let var_index =
      S.fold (fun name (idx, _) vidx -> S.add name idx vidx)
        st.store.Store.var_infos st.var_index
    in { st with var_index }

  let sync st =
    if has_empty_vinfos st then begin
        Logger.debug "Syncing state ...";
        copy_store st
      end
    else st
  (* END *)

  let get_last_index state name var_type =
    match S.find name state.var_index with
    | n -> n
    | exception Not_found ->
      (* add a declare-fun if necessary *)
       Logger.debug "Creating new variable %s" name;
       Store.get_last_index state.store name var_type |> ignore;
       0

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
    let open Formula in
    let index = get_last_index state name (bv_sort (Size.Bit.to_int size)) in
    let name = F.full name index in
    mk_bv_var (bv_var name (Size.Bit.to_int size))

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
        if offset < 0 then
          let v = Bigint.big_int_of_string ("0x" ^ Buffer.contents b) in
          let bv = Bitvector.create v (byte_size * sz) in
          Logger.debug ~level:5 "[sse] C bitvector %s (%d): %a"
            (Bigint.string_of_big_int v) sz Bitvector.pp_hex bv;
          bv
        else
          let off_bv = Bitvector.of_int ~size:word_size offset in
          let load_addr = Bitvector.add addr off_bv in
          let img = Kernel_functions.get_img () in
          let byte = Loader_utils.get_byte_at img load_addr in
          let byte_str = Format.sprintf "%02x" byte in
          Buffer.add_string b byte_str;
          loop (offset - 1)
      in loop (sz - 1)
    in
    let load_at addr size mem =
      assert (word_size = Bitvector.size_of addr);
      mk_store size mem (mk_bv_cst addr) (mk_bv_cst (read_bitvector addr size))
    in
    let initial_memory_value =
      mk_ax_term (B.fold load_at state.initialisation symbolic_memory) in
    let definition =
      F.var (F.full_mem 0) (F.memory_type word_size)
      |> F.def initial_memory_value in
    state.store.Store.assignments
    |> Formula.push_back_define definition
    |> Formula.push_back_declare declaration


  let formula state =
    Formula.push_front_assert (get_path_constraint state) (get_entries state)


  let uncontrolled st = st.uncontrolled

  let memory_term fml =
    F.memory, F.memory_type (Kernel_options.Machine.word_size ()), Formula.mk_ax_term fml

  (* Proxy function : will be removed soon *)
  let add_entry e t = Store.add_entry t.store e
end
