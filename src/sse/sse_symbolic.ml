(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2018                                               *)
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

let word_size = Machine.Word_size.get ()
let byte_size = Natural.to_int Basic_types.Constants.bytesize
let memory_name = "__memory"
let memory_type = Formula.ax_sort word_size byte_size
let path_constraint_name = "__pc"
let fullname name index = name ^ "_" ^ (string_of_int index)

let make_declaration name var_type =
  let open Formula in
  match var_type with
  | BlSort -> mk_bl_decl (bl_var name) []
  | BvSort i -> mk_bv_decl (bv_var name i) []
  | AxSort (i,j) -> mk_ax_decl (ax_var name i j) []

let make_definition name value var_type =
  let open Formula in
  match value.term_desc, var_type with
  | BlTerm value, BlSort -> mk_bl_def (bl_var name) [] value
  | BvTerm value, BvSort i -> mk_bv_def (bv_var name i) [] value
  | AxTerm value, AxSort (i,j) -> mk_ax_def (ax_var name i j) [] value
  | _ -> failwith "make_definition with wrong type"

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
    let declaration = make_declaration (fullname name initial_index) var_type in
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
    let definition = make_definition (fullname name index) value var_type in
    store.var_infos <- M.add name (index, var_type) store.var_infos;
    add_entry store (mk_define definition);
    index

  let create () =
    let open Formula in
    let assignments = Formula.empty
    and var_infos = M.singleton memory_name (0, memory_type) in
    let self = { assignments; var_infos } in
    assign self path_constraint_name bl_sort (mk_bl_term mk_bl_true) |> ignore;
    self

end

(* variable bindings but one per branch *)
module State = struct
  module M = Basic_types.String.Map
  module S = Basic_types.Int64.Map

  type t = {
    store : Store.t;
    initialisation : int S.t; (* list of memory locations to read *)
    var_index : int M.t
  }

  let create store =
    let var_index = M.empty in
    let initialisation = S.empty in
    { store; var_index ; initialisation }

  let assign state name var_type value =
    let new_index = Store.assign state.store name var_type value in
    { state with var_index = M.add name new_index state.var_index }

  let declare state name var_type =
    let new_index = Store.declare state.store name var_type in
    { state with var_index = M.add name new_index state.var_index }


  let pp ppf state =
    let open Format in
    fprintf ppf
    "@[<v 0># State var_index @ @[<hov 0>%a@]@,# Store var_infos@ @[<hov 0>%a@]@]"
    (fun ppf m -> M.iter (fun name n -> fprintf ppf "%s:%d;@ " name n) m)
    state.var_index
    (fun ppf m -> M.iter (fun name (n, _) -> fprintf ppf "%s:%d;@ " name n) m)
    state.store.Store.var_infos

  let has_empty_vinfos st = M.is_empty st.var_index

  let copy_store st =
    let var_index =
      M.fold (fun name (idx, _) vidx -> M.add name idx vidx)
        st.store.Store.var_infos st.var_index
    in { st with var_index }

  let get_last_index state name var_type =
    match M.find name state.var_index with
    | n -> n
    | exception Not_found ->
      (* add a declare-fun if necessary *)
       Logger.debug "Creating new variable %s" name;
       Store.get_last_index state.store name var_type |> ignore;
       0

  let get_memory state =
    let index = get_last_index state memory_name memory_type in
    let name = fullname memory_name index in
    Formula.(mk_ax_var (ax_var name word_size byte_size))

  let get_path_constraint state =
    let index = get_last_index state path_constraint_name Formula.bl_sort in
    let name = fullname path_constraint_name index in
    Formula.(mk_bl_var (bl_var name))

  let get_bv state name size =
    let open Formula in
    let index = get_last_index state name (bv_sort (Size.Bit.to_int size)) in
    let name = fullname name index in
    mk_bv_var (bv_var name (Size.Bit.to_int size))

  let merge st1 st2 =
    let open Formula in
    assert (st1.store = st2.store);
    assert (S.equal (=) st1.initialisation st2.initialisation);
    let store = st1.store in
    let merge_index name idx1 idx2 =
      let indices =
        match idx1, idx2 with
        | None, None -> None (* should not happen, but... *)
        | Some(i), None -> Some(i, 0)
        | None, Some(i) -> Some(0, i)
        | Some(i), Some(j) -> Some(i, j)
      in
      match indices with
      | None -> None
      | Some(i, j) ->
        if i=j then Some(i) else
          let pc1 = get_path_constraint st1 in
          let pc2 = get_path_constraint st2 in
          let _, var_type = Store.get_infos store name in
          let value =
            match var_type with
            | BlSort ->
              let v1 = mk_bl_var (bl_var (fullname name i)) in
              let v2 = mk_bl_var (bl_var (fullname name j)) in
              mk_bl_term
                (if name = path_constraint_name
                 then mk_bl_or v1 v2
                 else mk_bl_or (mk_bl_and pc1 v1) (mk_bl_and pc2 v2))
            | BvSort sz ->
              let v1 = mk_bv_var (bv_var (fullname name i) sz) in
              let v2 = mk_bv_var (bv_var (fullname name j) sz) in
              mk_bv_term (mk_bv_ite pc1 v1 v2)
            | AxSort (idx,elt) ->
              let v1 = mk_ax_var (ax_var (fullname name i) idx elt) in
              let v2 = mk_ax_var (ax_var (fullname name j) idx elt) in
              mk_ax_term (mk_ax_ite pc1 v1 v2)
          in
          Some(Store.assign store name var_type value)
    in
    let var_index = M.merge merge_index st1.var_index st2.var_index in
    let initialisation = st1.initialisation in
    { store; var_index; initialisation }

  let init_mem_at ~addr ~size state =
    { state with initialisation = S.add addr size state.initialisation }

  let get_entries state =
    let open Formula in
    let declaration = make_declaration memory_name memory_type in
    let symbolic_memory = mk_ax_var (ax_var memory_name word_size byte_size) in
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
          let load_addr =
            Int64.add addr (Int64.of_int offset) |> Bigint.big_int_of_int64 in
          let img = Kernel_functions.get_img () in
          let byte = Loader_utils.get_byte_at img load_addr in
          let byte_str = Format.sprintf "%02x" byte in
          Buffer.add_string b byte_str;
          loop (offset - 1)
      in loop (sz - 1)
    in
    let load_at addr size mem =
      let index_bigint = Bigint.big_int_of_int64 addr in
      mk_store size mem
        (mk_bv_cst (Bitvector.create index_bigint word_size))
        (mk_bv_cst (read_bitvector addr size))
    in
    let initial_memory_value =
      mk_ax_term (S.fold load_at state.initialisation symbolic_memory) in
    let definition =
      make_definition (fullname memory_name 0) initial_memory_value memory_type in
    state.store.Store.assignments
    |> Formula.push_back_define definition
    |> Formula.push_back_declare declaration

  let get_path_variables state =
    let open Formula in
    Basic_types.String.Map.fold
      (fun string (int, sort) set ->
         match sort with
         | BlSort -> VarSet.add (BlVar (bl_var (fullname string int))) set
         | BvSort i -> VarSet.add (BvVar (bv_var (fullname string int) i)) set
         | AxSort (i,j) -> VarSet.add (AxVar (ax_var (fullname string int) i j)) set)
      state.store.Store.var_infos (VarSet.empty)

end
