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


let get_byte_at img addr =
  Bitvector.value_of addr
  |> Bigint.int_of_big_int
  |> Loader.read_address img


let address_of_symbol ~name img =
  let symbols = Loader.Img.symbols img in
  try
    Some (Loader.Symbol.value
            (Array_utils.find
               (fun sy -> String.compare (Loader.Symbol.name sy) name = 0)
               symbols))
  with Not_found -> None



let interval section =
  let open Loader_types in
  let sec_start = (Loader.Section.pos section).virt  in
  let sec_end = sec_start + (Loader.Section.size section).virt - 1 in
  sec_start, sec_end


let in_section section addr =
  let open Loader_types in
  let lo = (Loader.Section.pos section).virt
  and sz = (Loader.Section.size section).virt in
  let hi = lo + sz in
  addr >= lo && addr < hi


exception SectionFound of Loader.Section.t
let find_section ~p img =
  let sections = Loader.Img.sections img in
  try
    Array.iter
      (fun section -> if p section then raise (SectionFound section))
      sections;
    None
  with SectionFound section -> Some section

let find_section_by_address ~address img =
  let p = fun s -> in_section s address in
  find_section img ~p


let find_section_by_address_exn ~address img =
  match find_section_by_address ~address img with
  | None ->
    let msg = Format.sprintf "No section found for address %x" address in
    failwith msg
  | Some section -> section


let find_section_by_name section_name img =
  let p =
    (fun section ->
       String.compare section_name (Loader.Section.name section) = 0) in
  match find_section ~p img with
  | None ->
    let msg = Format.sprintf "No section named %s" section_name in
    failwith msg
  | Some section -> section


let section_slice_by_name section_name img =
  find_section_by_name section_name img |> interval


let section_slice_by_address ~address img =
  find_section_by_address_exn img ~address |> interval


let find_symbol ~symbol img =
  let symbols = Loader.Img.symbols img in
  match
    Array_utils.find (fun sy -> Loader.Symbol.name sy = symbol) symbols
  with
  | v -> Some (Loader.Symbol.value v)
  | exception Not_found -> None

let find_function ~funcname img =
  find_symbol ~symbol:funcname img

let entry_point img =
  Loader.Img.entry img |> Virtual_address.create
