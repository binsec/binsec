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

open Ida_options
open Graph
open Dot_ast
open Format
module VA = Virtual_address

let to_vaddr s_addr = int_of_string s_addr |> VA.create

(* remove first and last character from a string *)
let strip_enclosing_chars s =
  match String.length s with 0 | 1 | 2 -> "" | len -> String.sub s 1 (len - 2)

let parse_calls calls_str =
  let calls = strip_enclosing_chars calls_str |> String.split_on_char ',' in
  List.map
    (fun p ->
      let s = String.split_on_char '-' p in
      match s with
      | [ caller; callee; ret ] ->
          (to_vaddr caller, to_vaddr callee, to_vaddr ret)
      | l ->
          Logger.error "Expected 3 arguments, got %d" (List.length l);
          assert false)
    calls

(* Ida mnemonics contains lots of contiguous spaces, just keep one of
   * them. Also remove enclosing " ".
*)
let clean_mnemonic s =
  let b = Buffer.create @@ String.length s in
  let last_char = ref 'c' in
  (* Any character but ' ' will do here *)
  let is_space = ( = ) ' ' in
  String.iter
    (fun c ->
      if c <> '"' && not (is_space !last_char && is_space c) then (
        Buffer.add_char b c;
        last_char := c))
    s;
  Buffer.contents b

let to_supported addr mnemonic =
  let open Mnemonic in
  match mnemonic with
  | Supported _ -> mnemonic
  | Unknown | Unsupported None ->
      Logger.warning "IDA returned unknown instruction at address %a" VA.pp addr;
      mnemonic
  | Unsupported (Some s) -> Mnemonic.supported s Format.pp_print_string

let read_list s = String.split_on_char ',' @@ strip_enclosing_chars s

module Dot = struct
  let pp_id ppf = function
    | Ident s -> fprintf ppf "id %s" s
    | Number s -> fprintf ppf "num %s" s
    | String s -> fprintf ppf "str %s" s
    | Html _ -> fprintf ppf "html"

  let pp_a ppf = function
    | id, Some i -> fprintf ppf "%a = %a" pp_id id pp_id i
    | id, None -> fprintf ppf "%a" pp_id id

  let rec pp_attr ppf = function
    | [] -> ()
    | a :: aa -> fprintf ppf "%a, %a" pp_a a pp_attr aa

  let rec pp_attrs ppf = function
    | [] -> ()
    | at :: ats -> fprintf ppf "%a : %a" pp_attr at pp_attrs ats

  let pp_node_id ppf (nid, _port_opt) = pp_id ppf nid

  let pp_node ppf = function
    | NodeId nid -> pp_node_id ppf nid
    | NodeSub _ -> fprintf ppf "subgraph"

  let pp_stmt ppf = function
    | Node_stmt (nid, attrs) ->
        fprintf ppf "%a [%a]" pp_node_id nid pp_attrs attrs
    | Edge_stmt (n, ns, _attrs) ->
        fprintf ppf "@[<h>%a -> @[<hov>%a@]@]" pp_node n
          (fun ppf l -> List.iter (fun n -> fprintf ppf "%a;@ " pp_node n) l)
          ns
    | _ -> ()
end
