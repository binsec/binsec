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

(* Parsing utilities to extract information from models produced by supported
 * SMT-Provers:
 * - Z3
 * - Boolector
 * - CVC4
 * - Yices
 *
 * The expected grammar is detailed in the SMT-LIB reference
 * http://smtlib.cs.uiowa.edu/papers/smt-lib-reference-v2.5-r2015-06-28.pdf, p 90
 *
 * However it seems that not one prover follows the standard ...
 *)

open Smtlib_options

type address = Bitvector.t
type identifier = string

module Stbl = Basic_types.String.Htbl

(** Print a character only if is visible *)
let maybe_pp_char ppf n =
  match Char.chr n with
  | c -> if String_utils.is_char_printable c then Format.fprintf ppf "(%c)" c
  | exception Invalid_argument _ -> ()

module Memory = struct
  module H = Bitvector.Collection.Htbl

  type t = { map : Bitvector.t H.t; mutable default : Bitvector.t option }

  let create ?default length = { map = H.create length; default }
  let empty () = create 7
  let add t address value = H.add t.map address value

  let find t address =
    match H.find t.map address with
    | value -> Some value
    | exception Not_found -> t.default

  let has_value t address = H.mem t.map address
  let addresses t = H.fold (fun k _ l -> k :: l) t.map []

  let bindings t =
    let ar = Array.make (H.length t.map) (Bitvector.zero, Bitvector.zero)
    and i = ref 0 in
    H.iter
      (fun k v ->
        Array.set ar !i (k, v);
        incr i)
      t.map;
    ar

  let is_empty t = H.length t.map = 0 && t.default = None

  let _length t =
    let len = H.length t.map in
    match t.default with None -> len | Some _ -> len + 1

  let set_default t d = t.default <- Some d

  let sorted_list t =
    H.fold (fun k v l -> (k, v) :: l) t.map []
    |> List.sort (fun (addr, _) (addr2, _) -> compare addr addr2)

  let pp ppf t =
    let open Format in
    if is_empty t then pp_print_string ppf "-- empty memory --"
    else
      let l = sorted_list t in
      let section_name =
        match Kernel_functions.get_img () with
        | img -> (
            fun addr ->
              let address = Bitvector.to_int addr in
              match Loader_utils.find_section_by_address ~address img with
              | None -> None
              | Some section -> Some (Loader.Section.name section))
        | exception Failure _ -> fun _ -> None
      in
      let last_section_name = ref None in
      pp_open_vbox ppf 0;
      pp_print_string ppf "# Memory";
      pp_print_cut ppf ();
      List.iter
        (fun (address, value) ->
          let name = section_name address in
          if name <> !last_section_name then
            fprintf ppf "; section %a@,"
              (Print_utils.pp_opt pp_print_string)
              name;
          fprintf ppf "%a : %a %a@," Bitvector.pp_hex_or_bin address
            Bitvector.pp_hex_or_bin value maybe_pp_char (Bitvector.to_int value);
          last_section_name := name)
        l;
      let d = t.default in
      if d <> None then (
        (* Manually aligned *)
        let d = Utils.unsafe_get_opt d in
        fprintf ppf "default    : %a %a@," Bitvector.pp_hex_or_bin d
          maybe_pp_char (Bitvector.to_int d);

        pp_close_box ppf ())

  let filter ?(keep_default = false) p t =
    let m =
      let length = H.length t.map in
      if keep_default then create ?default:t.default length else create length
    in
    H.iter (fun addr value -> if p addr then add m addr value) t.map;
    m
end

type t = {
  memory : Memory.t;
  variables : Bitvector.t Basic_types.String.Htbl.t;
}

let create ?(len = 43) () =
  { memory = Memory.empty (); variables = Basic_types.String.Htbl.create len }

let empty = create ~len:1 ()
let add_var t name bv = Basic_types.String.Htbl.replace t.variables name bv
let add_memcell t = Memory.add t.memory
let add_default t bv = Memory.set_default t.memory bv

let find_variable t name =
  match Basic_types.String.Htbl.find t.variables name with
  | value -> Some value
  | exception Not_found -> None

let find_address_contents t = Memory.find t.memory
let find_address_content _ _ _ _ = assert false
let is_memory_set t = Memory.has_value t.memory
let memory_addresses t = Memory.addresses t.memory
let memory_bindings t = Memory.bindings t.memory

let variables t =
  Basic_types.String.Htbl.fold (fun k _ l -> k :: l) t.variables []

let is_empty m = Memory.is_empty m.memory && Stbl.length m.variables = 0

let pp ppf t =
  let open Format in
  let pp_variables ppf r =
    if Stbl.length r > 0 then (
      pp_print_string ppf "# Variables";
      pp_print_cut ppf ();
      let l =
        Basic_types.String.Htbl.fold (fun k v acc -> (k, v) :: acc) r []
        |> List.sort (fun (k1, _) (k2, _) ->
               match String.length k1 - String.length k2 with
               | 0 -> String.compare k1 k2
               | x -> x)
      in
      List.iter
        (fun (name, bv) ->
          fprintf ppf "%s\t: %a\t" name Bitvector.pp_hex bv;
          if Bitvector.size_of bv = 8 then
            maybe_pp_char ppf (Bitvector.to_uint bv);
          pp_print_space ppf ())
        l;
      pp_print_cut ppf ())
  and pp_memory ppf m = Memory.pp ppf m.memory in

  if is_empty t then fprintf ppf "@[<h>--- Empty model ---@]"
  else
    fprintf ppf "@[<v 0>--- Model ---@,%a@,%a@]" pp_variables t.variables
      pp_memory t

let filter ?(keep_default = false) ~addr_p ~var_p t =
  let vars = t.variables in
  let len = Basic_types.String.Htbl.length vars in
  let variables = Basic_types.String.Htbl.create len in
  Basic_types.String.Htbl.iter
    (fun name v ->
      if var_p name then Basic_types.String.Htbl.add variables name v)
    vars;
  let memory = Memory.filter ~keep_default addr_p t.memory in
  { variables; memory }

(* 2 pass algorithm to construct a model which has values for variables and for
 * some memory addresses.
 *   1. Extract all information related to named constants (i.e. functions of
 *      arity 0 in SMT-LIB): this represents values of variables at various
 *      steps.
 *
 *   2. Extract from the remaining functions the contents of the memory
 *      (i.e. arbitrary addresses). The way provers do that does not seem to be
 *      standardized. We need different techniques for different provers.
 *)

open Smtlib

let get_symbol_name symbol =
  match symbol.symbol_desc with SimpleSymbol str | QuotedSymbol str -> str

let value_of_constant cst =
  match cst with
  | CstHexadecimal s -> Bitvector.of_hexstring ("0x" ^ s)
  | CstBinary s -> Bitvector.of_string ("0b" ^ s)
  | CstDecimalSize (value, size) ->
      Bitvector.create (Z.of_string value) (int_of_string size)
  | CstNumeral _ | CstString _ | CstBool _ | CstDecimal _ ->
      Logger.error
        "Model construction: unexpected constant %a as bitvector value"
        Smtlib_pp.pp_spec_constant cst;
      exit 2

let value_of_index converter = function
  | IdxNum num ->
      Logger.debug ~level:5 "idx: %s" num;
      Some (converter num)
  | IdxSymbol _ -> None

(* Bitvector values are encoded in various ways in models
 *  - bv<decimal_value> : found in CVC4
 *  - #x<hexa>
 *  - #b<binary>
 *)
let extract_bitvector term =
  match term.term_desc with
  | TermSpecConstant cst -> value_of_constant cst
  | TermQualIdentifier _ | TermAnnotatedTerm _ | TermExistsTerm _
  | TermForallTerm _ | TermLambdaTerm _ | TermLetTerm _
  | TermQualIdentifierTerms _ ->
      assert false

let get_bitvector_size sort =
  match sort.sort_desc with
  | SortIdentifier identifier -> (
      match identifier.id_desc with
      | IdSymbol _ -> None
      | IdUnderscore (symbol, indexes) ->
          if String.compare (get_symbol_name symbol) "BitVec" = 0 then
            match indexes with
            | [ idx ] -> value_of_index int_of_string idx
            | [] | _ :: _ -> None
          else None)
  | SortFun _ -> None

let get_bitvector cmd =
  match cmd.command_desc with
  | CmdDefineFun fdef -> (
      match fdef.fun_def_desc with
      | FunDef (symbol, _, _, sort, term) ->
          let size =
            match get_bitvector_size sort with Some sz -> sz | None -> 0
          in
          let value = extract_bitvector term in
          assert (Bitvector.size_of value = size);
          (symbol, value))
  | _ -> assert false

let add_variable smt_model cmd =
  let symbol, value = get_bitvector cmd in
  let name = get_symbol_name symbol in
  Logger.debug ~level:2 "Add variable %s as bv %a" name Bitvector.pp_hex value;
  add_var smt_model name value

let is_bitvector sort = get_bitvector_size sort <> None

let sort_of_sorted_var svar =
  match svar.sorted_var_desc with SortedVar (_, sort) -> sort

let name_of_sorted_var svar =
  match svar.sorted_var_desc with SortedVar (sy, _) -> get_symbol_name sy

let is_bv_bv_function args sort =
  match args with
  | [ arg ] ->
      let arg_sort = sort_of_sorted_var arg in
      is_bitvector arg_sort && is_bitvector sort
  | [] | _ -> false

let is_bv_bv_array sort =
  match sort.sort_desc with
  | SortIdentifier _ -> false
  | SortFun (id, sorts) -> (
      match (id.id_desc, sorts) with
      | IdUnderscore _, _ -> false
      | IdSymbol symbol, [ s1; s2 ] ->
          if String.compare (get_symbol_name symbol) "Array" = 0 then
            is_bitvector s1 && is_bitvector s2
          else false
      | IdSymbol _, ([] | _ :: _) -> false)

let is_bv_variable cmd =
  match cmd.command_desc with
  | CmdDefineFun fdef -> (
      match fdef.fun_def_desc with
      | FunDef (_, optargs, args, sort, _) ->
          optargs = None && args = [] && is_bitvector sort)
  | _ -> assert false

(* Memory is initially encoded as an array.
 * Z3 in particular likes to give this array back in a function.
 * Both are handled.
 *)
let is_memory funcmd =
  match funcmd.command_desc with
  | CmdDefineFun fdef -> (
      match fdef.fun_def_desc with
      | FunDef (_, _, args, sort, _) ->
          is_bv_bv_array sort || is_bv_bv_function args sort)
  | _ -> false

let name_of_id id =
  match id.id_desc with
  | IdSymbol symb | IdUnderscore (symb, _) -> get_symbol_name symb

let name_of_qid qid =
  match qid.qual_identifier_desc with
  | QualIdentifierIdentifier id | QualIdentifierAs (id, _) -> name_of_id id

let is_as s qid =
  match qid.qual_identifier_desc with
  | QualIdentifierIdentifier _ -> false
  | QualIdentifierAs (id, _) -> (
      match id.id_desc with
      | IdSymbol symb -> String.compare (get_symbol_name symb) s = 0
      | IdUnderscore _ -> false)

let is_string s qid =
  match qid.qual_identifier_desc with
  | QualIdentifierIdentifier id -> (
      match id.id_desc with
      | IdSymbol symb -> String.compare (get_symbol_name symb) s = 0
      | IdUnderscore _ -> false)
  | QualIdentifierAs _ -> false

let is_ite = is_string "ite"
let is_eq = is_string "="
let is_store = is_string "store"
let is_as_const = is_as "const"

let extract_address eqterm =
  match eqterm.term_desc with
  | TermQualIdentifierTerms (qid, terms) -> (
      assert (is_eq qid);
      match terms with
      | [ _vname; tcst ] -> (
          match tcst.term_desc with
          | TermSpecConstant c -> value_of_constant c
          | _ -> assert false)
      | _ -> assert false)
  | TermSpecConstant _ | TermLetTerm _ | TermForallTerm _ | TermExistsTerm _
  | TermLambdaTerm _ | TermAnnotatedTerm _ | TermQualIdentifier _ ->
      assert false

let extract_byteval tbyteval =
  match tbyteval.term_desc with
  | TermSpecConstant c -> value_of_constant c
  | TermQualIdentifierTerms _ | TermLetTerm _ | TermForallTerm _
  | TermExistsTerm _ | TermAnnotatedTerm _ | TermLambdaTerm _
  | TermQualIdentifier _ ->
      assert false

(* The memory is represented by a chain of if-then-else *)
let memory_from_ite smt_model term =
  let rec extract_from term =
    match term.term_desc with
    | TermQualIdentifierTerms (qid, terms) -> (
        (* This a term of the form (ite (= X v) v2 t) *)
        assert (is_ite qid);
        match terms with
        | [ teqcond; tbyteval; term ] ->
            let address = extract_address teqcond in
            let byteval = extract_byteval tbyteval in
            add_memcell smt_model address byteval;
            extract_from term
        | _ -> assert false)
    | TermSpecConstant c ->
        (* Last default case *)
        let bv = value_of_constant c in
        add_default smt_model bv
    | TermLetTerm _ | TermForallTerm _ | TermExistsTerm _ | TermLambdaTerm _
    | TermAnnotatedTerm _ | TermQualIdentifier _ ->
        assert false
  in
  extract_from term

(* Function used to retrieve memory models as printed by:
    - CVC4
    (model
    (define-fun __memory () (Array (_ BitVec 32) (_ BitVec 8))
       (store (store (store (
       (as const (Array (_ BitVec 32) (_ BitVec 8))) (_ bv0 8)) <- default value
       (_ bv1784268929 32) (_ bv126 8))
       (_ bv1784268930 32) (_ bv10 8))
       (_ bv1784268928 32) (_ bv8 8))))
    - Boolector 2.4/3.0
    (model
   (define-fun __memory (
    (__memory_x0 (_ BitVec 32))) (_ BitVec 8)
     (ite (= __memory_x0 #b01101010010110011100100010000010) #b00001010
     (ite (= __memory_x0 #b01101010010110011100100010000000) #b00001000
     (ite (= __memory_x0 #b01101010010110011100100010000001) #b01001111
       #b00000000)))))
*)
let memory_from_store smt_model term =
  let rec extract_from term =
    match term.term_desc with
    | TermQualIdentifierTerms (qid, terms) -> (
        if is_store qid then
          match terms with
          | [ prev_store; taddr; tbyteval ] ->
              let addr = extract_bitvector taddr in
              let byte = extract_bitvector tbyteval in
              add_memcell smt_model addr byte;
              extract_from prev_store
          | _ -> assert false
        else if is_as_const qid then
          (* this is where we get the default value for CVC4
             it is the first array with identical values everywhere *)
          match terms with
          | [ term ] -> (
              match term.term_desc with
              | TermSpecConstant default ->
                  let bv = value_of_constant default in
                  add_default smt_model bv
              | _ -> assert false)
          | _ -> assert false)
    | _ -> assert false
  in
  extract_from term

let is_ite_memory term =
  match term.term_desc with
  | TermQualIdentifierTerms (qid, _) -> is_ite qid
  | _ -> false

let is_store_memory term =
  match term.term_desc with
  | TermQualIdentifierTerms (qid, _) -> is_string "store" qid || is_as_const qid
  | _ -> false

let is_lambda_memory term =
  match term.term_desc with TermLambdaTerm _ -> true | _ -> false

let is_fun_app_memory terms =
  List.for_all
    (function
      | ( {
            term_desc =
              TermQualIdentifierTerms
                (_, [ { term_desc = TermSpecConstant _; _ } ]);
            _;
          },
          { term_desc = TermSpecConstant _; _ } ) ->
          true
      | _ -> false)
    terms

let get_function_body fcmd =
  match fcmd.command_desc with
  | CmdDefineFun fdef -> (
      match fdef.fun_def_desc with FunDef (_, _, _, _, term) -> term)
  | _ -> assert false

let get_function_name fcmd =
  match fcmd.command_desc with
  | CmdDefineFun fdef -> (
      match fdef.fun_def_desc with
      | FunDef (name, _, _, _, _) -> get_symbol_name name)
  | _ -> assert false

let get_indirection_name term =
  match term.term_desc with
  | TermQualIdentifier qid -> (
      match qid.qual_identifier_desc with
      | QualIdentifierAs _ -> assert false
      | QualIdentifierIdentifier id -> (
          match id.id_desc with
          | IdSymbol _ -> assert false
          | IdUnderscore (sym, indexes) -> (
              assert (String.compare (get_symbol_name sym) "as-array" = 0);
              match indexes with
              | [ name ] -> (
                  match name with
                  | IdxNum _ -> assert false
                  | IdxSymbol real_symb -> get_symbol_name real_symb)
              | _ -> assert false)))
  | _ -> assert false

let lambda_ite_memory ~basename model t =
  let rec loop assocs t =
    match t.term_desc with
    | TermLetTerm ([ { var_binding_desc = VarBinding (sy, let_t); _ } ], term)
      ->
        let a = ite_seq ~basename:(get_symbol_name sy) ~acc:[] assocs let_t in
        loop a term
    | TermQualIdentifierTerms (qid, _terms) ->
        assert (is_ite qid);
        ite_seq ~basename ~acc:[] assocs t
    | _ -> assert false
  and ite_seq ~basename ~acc assocs t =
    match t.term_desc with
    | TermQualIdentifierTerms (qid, terms) -> (
        assert (is_ite qid);
        match terms with
        | [ tcond; tcsq; talt ] ->
            let addr = extract_address tcond in
            let byteval = extract_byteval tcsq in
            let acc = (addr, byteval) :: acc in
            ite_seq ~basename ~acc assocs talt
        | _ -> assert false)
    | TermSpecConstant cst ->
        let bv = value_of_constant cst in
        add_default model bv;
        (basename, List.rev acc) :: assocs
    | TermQualIdentifier _qid -> (basename, List.rev acc) :: assocs
    | _ -> assert false
  in

  List.iter
    (fun (_name, l) ->
      List.iter
        (fun (addr, v) ->
          if not @@ is_memory_set model addr then add_memcell model addr v)
        l)
    (loop [] t)

(* Example model for Z3 4.8
   (model
     (define-fun ebx_0 () (_ BitVec 32)
       #x90909090)
     (define-fun __memory () (Array (_ BitVec 32) (_ BitVec 8))
       (lambda ((x!1 (_ BitVec 32)))
     (let ((a!1 (ite (= x!1 #x00060001)
                     #x4d
                     (ite (= x!1 #x00008007)
                          #x20
                          (ite (= x!1 #x00060000) #x3d #x01)))))
       (ite (= x!1 #x00060002) #x41 a!1))))
     (define-fun bs_unknown1_0 () (_ BitVec 32)
       #x00008000)
     (define-fun ebp_0 () (_ BitVec 32)
       #xf4909010)
*)
let memory_from_lambda model term =
  match term.term_desc with
  | TermLambdaTerm ([ svar ], t) ->
      let basename = name_of_sorted_var svar in
      lambda_ite_memory ~basename model t
  | _ -> assert false

(*
  (define-fun __memory () (Array (_ BitVec 32) (_ BitVec 8))
    (let ((a!1 (store (store (store ((as const (Array (_ BitVec 32) (_ BitVec 8)))
     #x3d)
     #x00060008
     #x52)
     #x00060009
     #x45)
     #x00060004
     #x54)))
     (let ((a!2 (store (store (store
       (store a!1 #x00060003 #x4e) #x00060006 #x43) #x00060001 #x4d)
        #x00060002 #x41)))
       (store (store a!2 #x00060005 #x49) #x00060007 #x4f))))
*)
let is_let_store body =
  match body.term_desc with TermLetTerm _ -> true | _ -> false

(* Each memory named by a let is bound inside [tbl].
   Whenever we reach the base case of a store sequence, we lookup the identifier
   to retrieve its value.
*)
let memory_from_let_store model body =
  Logger.debug "Looks like let-store memory type (from z3 4.8.5 ?) ...";
  let module M = Bitvector.Collection.Map in
  let module Sm = Basic_types.String.Htbl in
  let tbl = Sm.create 7 in
  (* Debug map *)
  let pp_map ppf map =
    let open Format in
    fprintf ppf "-- ";
    M.iter
      (fun k v -> fprintf ppf "%a - %a; " Bitvector.pp_hex k Bitvector.pp_hex v)
      map
  in
  let rec store_seq t =
    match t.term_desc with
    | TermQualIdentifierTerms (qid, [ map_t; key; value ]) ->
        assert (is_store qid);
        let map' =
          match map_t.term_desc with
          | TermQualIdentifier qid -> (
              let name = name_of_qid qid in
              match Sm.find tbl name with
              | map -> map
              | exception Not_found ->
                  Logger.warning "Could not find memory map for identifier %s"
                    name;
                  M.empty)
          | _ -> store_seq map_t
        in
        Logger.debug "Post %a" pp_map map';
        let bv_key = extract_bitvector key
        and bv_val = extract_bitvector value in
        Logger.debug "Add %a -> %a" Bitvector.pp_hex bv_key Bitvector.pp_hex
          bv_val;
        M.add bv_key bv_val map'
    | TermQualIdentifierTerms (qid, [ default_term_value ]) ->
        assert (is_as_const qid);
        let bv = extract_bitvector default_term_value in
        Logger.debug "Found default value in let-store model %a" Bitvector.pp bv;
        add_default model bv;
        M.empty
    | _ ->
        Logger.fatal "Unable to reconstruct expected store sequence %a"
          Smtlib_pp.pp_term t
  in

  let do_binding vb =
    match vb.var_binding_desc with
    | VarBinding (symb, term) ->
        let name = get_symbol_name symb in
        Logger.debug "%s" name;
        let map = store_seq term in
        Sm.add tbl name map
  in

  let rec loop_let_seq term =
    match term.term_desc with
    | TermLetTerm ([ var_binding ], t) ->
        do_binding var_binding;
        loop_let_seq t
    | TermQualIdentifierTerms (qid, _) when is_store qid -> store_seq term
    | _ ->
        Logger.fatal
          "Do not know what to do with term %a in Z3 4.8.5-style memory"
          Smtlib_pp.pp_term term
  in
  let map = loop_let_seq body in
  M.iter (add_memcell model) map

(* The memory is represented by a list of function application *)
(* ((__memory_0 #b11111111111111111111111100000100) #b11011110)
   ((__memory_0 #b11111111111111111111111100000111) #b11011110)
   ((__memory_0 #b11111111111111111111111100000110) #b10101101)
   ((__memory_0 #b11111111111111111111111100000101) #b11000000) *)
let memory_from_fun_app smt_model terms =
  List.iter
    (function
      | ( {
            term_desc =
              TermQualIdentifierTerms
                (_, [ { term_desc = TermSpecConstant addr; _ } ]);
            _;
          },
          { term_desc = TermSpecConstant value; _ } ) ->
          let address = value_of_constant addr
          and byteval = value_of_constant value in
          add_memcell smt_model address byteval
      | _ -> assert false)
    terms

(* Memory is defined as an array of addresses to bytes.
 * In SMT2 parlance this means BitVec 32 -> BitVec 8 for 32 bits architectures
 * The memory function is declared in the initial SMT-LIB Script but
 * it is not always present as such in the result
 *  - CVC4 :: it is.
 *  - Z3 :: there is an indirection to another Array,
 *  - Boolector :: the name is absent
 *)
let find_and_add_memory smt_model functions =
  match List.filter is_memory functions with
  | [] -> Logger.warning ~level:2 "No memory found in SMT model"
  | [ f ] ->
      (* Case of Boolector and CVC4 *)
      let body = get_function_body f in
      if is_ite_memory body then memory_from_ite smt_model body
      else if is_store_memory body then memory_from_store smt_model body
      else if is_lambda_memory body then memory_from_lambda smt_model body
      else if is_let_store body then memory_from_let_store smt_model body
      else Logger.warning "No extractor for this memory representation"
  | f :: tail ->
      let fbody = get_function_body f in
      if is_ite_memory fbody then memory_from_ite smt_model fbody
      else if is_store_memory fbody then memory_from_store smt_model fbody
      else if is_let_store fbody then memory_from_let_store smt_model fbody
      else
        let name = get_indirection_name fbody in
        let filter_f g = get_function_name g = name in
        let g =
          match List.filter filter_f tail with [ g ] -> g | _ -> assert false
        in
        let gbody = get_function_body g in
        if is_ite_memory gbody then memory_from_ite smt_model gbody
        else Logger.warning "Unexpected memory encoding from SMT model"

let add_memory_term smt_model = function
  | [] -> ()
  | [ ({ term_desc = TermQualIdentifier _; _ }, body) ] when is_ite_memory body
    ->
      memory_from_ite smt_model body
  | [ ({ term_desc = TermQualIdentifier _; _ }, body) ]
    when is_store_memory body ->
      memory_from_store smt_model body
  | [ ({ term_desc = TermQualIdentifier _; _ }, body) ]
    when is_lambda_memory body ->
      memory_from_lambda smt_model body
  | [ ({ term_desc = TermQualIdentifier _; _ }, body) ] when is_let_store body
    ->
      memory_from_let_store smt_model body
  | ts when is_fun_app_memory ts -> memory_from_fun_app smt_model ts
  | _ -> Logger.fatal "No extractor for this memory representation"

let extract model_ast =
  let smt_model = create () in

  (* List.iter (fun cmd ->
   *     match cmd.command_desc with
   *     | CmdDefineFun { fun_def_desc = FunDef
   *                          (symbol, None, [], sort,
   *                           { term_desc = TermSpecConstant cst; _ }); _ }
   *       when is_bitvector sort ->
   *       add_var smt_model (get_symbol_name symbol) (value_of_constant cst)
   *     | _ -> assert false ) model_ast.model_commands;
   * smt_model *)
  let variables, functions =
    List.partition is_bv_variable model_ast.model_commands
  in
  Logger.debug ~level:2 "Found : %d variables, %d functions"
    (List.length variables) (List.length functions);
  List.iter (add_variable smt_model) variables;
  Logger.debug ~level:2 "%d functions are potential memories"
    (List.length (List.filter is_memory functions));
  find_and_add_memory smt_model functions;
  smt_model

(* like extract but after get-value instead of get-model *)
(* currently only supports bitvectors *)
let extract_value (_t, v) = value_of_constant v

let yices_extract (raw : string) =
  let read_bv s =
    Bitvector.of_string ("0" ^ String.sub s 1 (String.length s - 1))
  in
  let parse_line (smt_model, memory_name) raw_line =
    let raw_line =
      String_utils.remove_char '(' raw_line |> String_utils.remove_char ')'
    in
    let line = Str.split (Str.regexp " ") raw_line in
    match line with
    | [ "="; _memname; funname ] when funname.[0] = '@' ->
        (smt_model, Some funname)
    | [ "="; funname; addr; value ] when Some funname = memory_name ->
        let addr = read_bv addr and value = read_bv value in
        add_memcell smt_model addr value;
        (smt_model, memory_name)
    | [ "="; varname; value ] ->
        add_var smt_model varname (read_bv value);
        (smt_model, memory_name)
    | [ "default"; value ] ->
        add_default smt_model (read_bv value);
        (smt_model, memory_name)
    | _ ->
        Logger.debug ~level:4 "while reading model, ignoring@ «%s»" raw_line;
        (smt_model, memory_name)
  in
  Str.split (Str.regexp "\n") raw
  |> List.fold_left parse_line (create (), None)
  |> fst
