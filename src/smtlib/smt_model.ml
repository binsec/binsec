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

type address = int64
type identifier = string

module Stbl = Basic_types.String.Htbl
module Itbl = Basic_types.Int64.Htbl


type t = {
  memory : int Basic_types.Int64.Htbl.t;
  registers : Bitvector.t Basic_types.String.Htbl.t;
}

let create () = {
  memory = Basic_types.Int64.Htbl.create 7;
  registers = Basic_types.String.Htbl.create 43;
}

let empty = {
  memory = Basic_types.Int64.Htbl.create 1;
  registers = Basic_types.String.Htbl.create 1;
}

let add_var t name bv =
  Basic_types.String.Htbl.replace t.registers name bv

let add_memcell t address value =
  Basic_types.Int64.Htbl.replace t.memory address value

let find_register t name =
  Basic_types.String.Htbl.find t.registers name

let find_address_contents t addr =
  Basic_types.Int64.Htbl.find t.memory addr

let find_address_content _ _ _ _ = assert false

let is_memory_set t addr = Basic_types.Int64.Htbl.mem t.memory addr

let memory_addresses t =
  Basic_types.Int64.Htbl.fold (fun k _ l -> k :: l) t.memory []

let registers t =
  Basic_types.String.Htbl.fold (fun k _ l -> k :: l) t.registers []

let is_empty m = Itbl.length m.memory = 0 && Stbl.length m.registers = 0

let pp ppf t =
  let open Format in
  let maybe_pp_char ppf n =
    match Char.chr n with
    | c ->
      if String_utils.is_char_printable c then
        fprintf ppf "(%c)" c
    | exception Invalid_argument _ -> ()
  in
  let pp_registers ppf r =
    if Stbl.length r > 0 then begin
        pp_print_string ppf "# Registers";
        pp_print_cut ppf ();
        Basic_types.String.Htbl.iter
          (fun name bv -> fprintf ppf "%s : %a@ " name Bitvector.pp_hex bv) r;
        pp_print_cut ppf ();
      end
  and pp_memory ppf m =
    if Itbl.length m > 0 then begin
        pp_print_string ppf "# Memory";
        pp_print_cut ppf ();
        (* let's sort memory before printing *)
        let l =
          Basic_types.Int64.Htbl.fold
            (fun addr value acc -> (addr, value)::acc) t.memory []
          |> List.sort (fun (addr, _) (addr2, _) -> compare addr addr2)
        in
        let section_name =
          let img = Kernel_functions.get_img() in
          fun addr ->
          let address = Int64.to_int addr in
          match Loader_utils.find_section_by_address ~address img with
          | None -> None
          | Some section -> Some (Loader.Section.name section) in
        let last_section_name = ref None in
        List.iter
          (fun (address, value) ->
            let name = section_name address in
            if name <> !last_section_name then
              fprintf ppf "; section %a@,"
                (Print_utils.pp_opt pp_print_string) name;
            fprintf ppf "0x%08Lx : %02x %a@," address value maybe_pp_char value;
            last_section_name := name
          ) l;
      end
  in
  if is_empty t then fprintf ppf "@[<h>--- Empty model ---@]"
  else fprintf ppf "@[<v 0>--- Model ---@,%a@,%a@]"
         pp_registers t.registers
         pp_memory t.memory


(* 2 pass algorithm to construct a model which has values for registers and for
 * some memory addresses.
 *   1. Extract all information related to named constants (i.e. functions of
 *      arity 0 in SMT-LIB): this represents values of registers at various
 *      steps.
 *
 *   2. Extract from the remaining functions the contents of the memory
 *      (i.e. arbitrary addresses). The way provers do that does not seem to be
 *      standardized. We need different techniques for different provers.
*)

open Smtlib

let get_symbol_name symbol =
  match symbol.symbol_desc with
  | SimpleSymbol str
  | QuotedSymbol str -> str


let value_of_constant cst =
  match cst with
  | CstHexadecimal s -> Bitvector.of_hexstring ("0x" ^ s)
  | CstBinary s -> Bitvector.of_string ("0b" ^ s)
  | CstDecimalSize (value, size) ->
    Bitvector.create (Bigint.big_int_of_string value) (int_of_string size)
  | CstNumeral _
  | CstString _
  | CstBool _
  | CstDecimal _ ->
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
  | TermQualIdentifier _
  | TermAnnotatedTerm _
  | TermExistsTerm _
  | TermForallTerm _
  | TermLetTerm _
  | TermQualIdentifierTerms _  -> assert false


let get_bitvector_size sort =
  match sort.sort_desc with
  | SortIdentifier identifier ->
    begin
      match identifier.id_desc with
      | IdSymbol _ -> None
      | IdUnderscore (symbol, indexes) ->
        if String.compare (get_symbol_name symbol) "BitVec" = 0 then
          match indexes with
          | [idx] -> value_of_index int_of_string idx
          | [] | _ :: _ -> None
        else None
    end
  | SortFun _ -> None

let get_bitvector cmd =
  match cmd.command_desc with
  | CmdDefineFun fdef ->
    begin
      match fdef.fun_def_desc with
      | FunDef (symbol, _, _, sort, term) ->
        let size =
          match get_bitvector_size sort with
          | Some sz -> sz
          | None -> 0
        in
        let value = extract_bitvector term in
        assert ((Bitvector.size_of value) = size);
        symbol, value
    end
  | _ -> assert false

let add_variable smt_model cmd =
  let symbol, value = get_bitvector cmd in
  let name = get_symbol_name symbol in
  Logger.debug ~level:2 "Add register %s as bv %a" name Bitvector.pp_hex value;
  add_var smt_model name value

let is_bitvector sort = get_bitvector_size sort <> None


let sort_of_sorted_var svar =
  match svar.sorted_var_desc with
  | SortedVar (_, sort) -> sort


let is_bv_bv_function args sort =
  match args with
  | [arg] ->
    let arg_sort = sort_of_sorted_var arg in
    is_bitvector arg_sort && is_bitvector sort
  | [] | _ -> false


let is_bv_bv_array sort =
  match sort.sort_desc with
  | SortIdentifier _ -> false
  | SortFun (id, sorts) ->
    match id.id_desc, sorts with
    | IdUnderscore _, _ -> false
    | IdSymbol symbol, s1 :: [s2] ->
      if String.compare (get_symbol_name symbol) "Array" = 0 then
        is_bitvector s1 && is_bitvector s2
      else false
    | IdSymbol _, ([] | _ :: _) -> false


let is_bv_variable cmd =
  match cmd.command_desc with
  | CmdDefineFun fdef ->
    begin
      match fdef.fun_def_desc with
      | FunDef (_, optargs, args, sort, _) ->
        optargs = None && args = [] && is_bitvector sort
    end
  | _ -> assert false


(* Memory is initially encoded as an array.
 * Z3 in particular likes to give this array back in a function.
 * Both are handled.
 *)
let is_memory funcmd =
  match funcmd.command_desc with
  | CmdDefineFun fdef ->
    begin
      match fdef.fun_def_desc with
      | FunDef (_, _, args, sort, _) ->
        is_bv_bv_array sort
        ||
        is_bv_bv_function args sort
    end
  | _ -> false


let is_string s qid =
  match qid.qual_identifier_desc with
  | QualIdentifierIdentifier id ->
    begin
      match id.id_desc with
      | IdSymbol symb -> String.compare (get_symbol_name symb) s = 0
      | IdUnderscore _ -> false
    end
  | QualIdentifierAs _ -> false

let is_ite = is_string "ite"
let is_eq = is_string "="
let is_store = is_string "store"


let extract_address eqterm =
  match eqterm.term_desc with
  | TermQualIdentifierTerms (qid, terms) ->
    begin
      assert (is_eq qid);
      match terms with
      | _vname :: [tcst] ->
        begin
          match tcst.term_desc with
          | TermSpecConstant c ->
            value_of_constant c |> Bitvector.value_of |> Bigint.int64_of_big_int
          | _ -> assert false
        end
      | _ -> assert false
    end
  | TermSpecConstant _
  | TermLetTerm _
  | TermForallTerm _
  | TermExistsTerm _
  | TermAnnotatedTerm _
  | TermQualIdentifier _ -> assert false


let extract_byteval tbyteval =
  match tbyteval.term_desc with
  | TermSpecConstant c -> value_of_constant c
  | TermQualIdentifierTerms _
  | TermLetTerm _
  | TermForallTerm _
  | TermExistsTerm _
  | TermAnnotatedTerm _
  | TermQualIdentifier _ -> assert false


(* The memory is represented by a chain of if-then-else *)
let memory_from_ite smt_model term =
  let rec extract_from term =
    match term.term_desc with
    | TermQualIdentifierTerms (qid, terms) ->
      (* This a term of the form (ite (= X v) v2 t) *)
      assert (is_ite qid);
      begin
        match terms with
        | teqcond :: tbyteval :: [term] ->
          let address = extract_address teqcond in
          let byteval = extract_byteval tbyteval
                        |> Bitvector.value_of
                        |> Bigint.int_of_big_int
          in
          add_memcell smt_model address byteval;
          extract_from term
        | _ -> assert false
      end
    | TermSpecConstant _ ->
      (* Last default case : ignored. Usually #x00 *)
      ()
    | TermLetTerm _
    | TermForallTerm _
    | TermExistsTerm _
    | TermAnnotatedTerm _
    | TermQualIdentifier _ -> assert false
  in extract_from term


let memory_from_store smt_model term =
  let rec extract_from term =
    match term.term_desc with
    | TermQualIdentifierTerms (qid, terms) ->
      begin
        if is_store qid then
          match terms with
          | prev_store :: taddr :: [tbyteval] ->
            let addr = extract_bitvector taddr |> Bitvector.to_int64 in
            let byte = extract_bitvector tbyteval |> Bitvector.to_int64 in
            add_memcell smt_model addr (Int64.to_int byte);
            extract_from prev_store
          | _ ->  assert false
      end
    | _ -> assert false
  in extract_from term


let is_ite_memory term =
  match term.term_desc with
  | TermQualIdentifierTerms (qid, _) -> is_ite qid
  | _ -> false


let is_store_memory term =
  match term.term_desc with
  | TermQualIdentifierTerms (qid, _) -> is_string "store" qid
  | _ -> false


let get_function_body fcmd =
  match fcmd.command_desc with
  | CmdDefineFun fdef ->
    begin
      match fdef.fun_def_desc with
      | FunDef (_, _, _, _, term) -> term
    end
  | _ -> assert false

let get_function_name fcmd =
  match fcmd.command_desc with
  | CmdDefineFun fdef ->
    begin
      match fdef.fun_def_desc with
      | FunDef (name, _, _, _, _) -> get_symbol_name name
    end
  | _ -> assert false

let get_indirection_name term =
  match term.term_desc with
  | TermQualIdentifier qid ->
    begin match qid.qual_identifier_desc with
      | QualIdentifierAs _ -> assert false
      | QualIdentifierIdentifier id ->
        begin match id.id_desc with
          | IdSymbol _ -> assert false
          | IdUnderscore (sym, indexes) ->
            assert (String.compare (get_symbol_name sym) "as-array" = 0);
            begin match indexes with
              | [ name ] ->
                begin match name with
                  | IdxNum _ -> assert false
                  | IdxSymbol real_symb -> get_symbol_name real_symb
                end
              | _ -> assert false
            end
        end
    end
  | _ -> assert false


(* Memory is defined as an array of addresses to bytes.
 * In SMT2 parlance this means BitVec 32 -> BitVec 8
 * The memory function is declared in the initial SMT-LIB Script but
 * it is not always present as such in the result
 *  - CVC4 :: it is.
 *  - Z3 :: there is an indirection to another Array,
 *  - Boolector :: the name is absent
*)
let find_and_add_memory smt_model functions =
  match List.filter is_memory functions with
  | [] -> Logger.warning ~level:2 "No memory found in SMT model"
  | [f] ->
    (* Case of Boolector and CVC4 *)
    let body = get_function_body f in
    if is_ite_memory body then memory_from_ite smt_model body
    else if is_store_memory body then memory_from_store smt_model body
    else
      Logger.warning ~level:2 "No extractor for this memory representation"
  | f :: tail ->
    let fbody = get_function_body f in
    if is_ite_memory fbody then memory_from_ite smt_model fbody
    else
      let name = get_indirection_name fbody in
      let filter g =
        let fname = get_function_name g in
        fname = name
      in
      let g =
        match List.filter filter tail with
        | [ g ] -> g
        | _ -> assert false
      in
      let gbody = get_function_body g in
      if is_ite_memory gbody then memory_from_ite smt_model gbody
      else
        Logger.warning "Unexpected memory encoding from SMT model"

let extract model_ast =
  let smt_model = create () in
  let variables, functions =
    List.partition is_bv_variable model_ast.model_commands in
  Logger.debug ~level:2 "Found : %d variables, %d functions"
    (List.length variables) (List.length functions);
  List.iter (add_variable smt_model) variables;
  Logger.debug ~level:2 "%d functions are potential memories"
    (List.length (List.filter is_memory functions));
  find_and_add_memory smt_model functions;
  smt_model

(* like extract but after get-value instead of get-model *)
(* currently only supports bitvectors *)
let extract_value ast =
  let _t, v = ast in
  value_of_constant v

let yices_extract (raw:string) =
  let smt_model = create () in
  let read_bv s =
    Bitvector.of_string ("0"^String.sub s 1 (String.length s-1)) in
  let parse_line memory_name raw_line =
    let raw_line =
      String_utils.remove_char '(' raw_line |> String_utils.remove_char ')' in
    let line = Str.split (Str.regexp " ") raw_line in
    match line with
    | "=" :: _memname :: funname :: [] when funname.[0] = '@' -> Some funname
    | "=" :: funname :: addr :: value :: [] when Some funname = memory_name ->
      let addr = read_bv addr |> Bitvector.to_int64
      and value = read_bv value |> Bitvector.value_of |> Bigint.int_of_big_int in
      add_memcell smt_model addr value;
      memory_name
    | "=" :: varname :: value :: [] ->
      add_var smt_model varname (read_bv value);
      memory_name
    | _ ->
      Logger.debug ~level:4 "while reading model, ignoring@ «%s»" raw_line;
      memory_name
  in
  let lines = Str.split (Str.regexp "\n") raw in
  ignore(List.fold_left parse_line None lines);
  smt_model
