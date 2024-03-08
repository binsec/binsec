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

let get_byte_at img addr =
  Bitvector.value_of addr |> Z.to_int |> Loader.read_address img

(* { Manipulation of symbols } *)

let symbol_by_name ~name img =
  let symbols = Loader.Img.symbols img in
  try
    Some
      (Array_utils.find
         (fun sy -> String.compare (Loader.Symbol.name sy) name = 0)
         symbols)
  with Not_found -> None

let address_of_symbol symbol = Loader.Symbol.value symbol

let address_of_symbol_by_name ~name img =
  match symbol_by_name ~name img with
  | Some symbol -> Some (address_of_symbol symbol)
  | None -> None

let size_of_symbol symbol =
  let header = Loader.Symbol.header symbol in
  match header with
  | Loader.ELF elf -> elf.Loader_elf.Sym.size
  | Loader.PE _ -> failwith "No size for PE symbols"
  | Loader.Raw _ -> failwith "No size for Raw symbols"
  | Loader.TI83 _ -> failwith "No size for TI83 symbols"

let size_of_symbol_by_name ~name img =
  match symbol_by_name ~name img with
  | Some symbol -> Some (size_of_symbol symbol)
  | None -> None

let symbol_interval symbol =
  let start_addr = address_of_symbol symbol in
  let size = size_of_symbol symbol in
  let end_addr = start_addr + size in
  (Virtual_address.create start_addr, Virtual_address.create end_addr)

let symbol_interval_by_name ~name img =
  match symbol_by_name ~name img with
  | Some symbol -> Some (symbol_interval symbol)
  | None -> None

let belongs_to_symbol symbol addr =
  let start_addr, end_addr = symbol_interval symbol in
  Virtual_address.compare start_addr addr <= 0
  && Virtual_address.compare end_addr addr > 0

let belongs_to_symbol_by_name ~name img addr =
  match symbol_by_name ~name img with
  | Some symbol -> belongs_to_symbol symbol addr
  | None -> raise Not_found

(* { End of Manipulation of symbols } *)

let interval section =
  let open Loader_types in
  let sec_start = (Loader.Section.pos section).virt in
  let sec_end = sec_start + (Loader.Section.size section).virt - 1 in
  (sec_start, sec_end)

let in_section section addr =
  let open Loader_types in
  let lo = (Loader.Section.pos section).virt
  and sz = (Loader.Section.size section).virt in
  let hi = lo + sz in
  addr >= lo && addr < hi

let find_section ~p img =
  try Some (Array_utils.find p (Loader.Img.sections img))
  with Not_found -> None

let find_section_by_address ~address img =
  find_section ~p:(fun s -> in_section s address) img

let find_section_by_address_exn ~address img =
  Array_utils.find (fun s -> in_section s address) (Loader.Img.sections img)

let find_section_by_name section_name img =
  Array_utils.find
    (fun section -> section_name = Loader.Section.name section)
    (Loader.Img.sections img)

let section_slice_by_name section_name img =
  find_section_by_name section_name img |> interval

let section_slice_by_address ~address img =
  find_section_by_address_exn img ~address |> interval

let entry_point img = Loader.Img.entry img |> Virtual_address.create

let address_of_symbol_or_section_by_name ~name img =
  match address_of_symbol_by_name ~name img with
  | Some _ as a -> a
  | None -> (
      try
        Some
          (Loader.Section.pos (find_section_by_name name img)).Loader_types.virt
      with Not_found -> None)

let size_of_symbol_or_section_by_name ~name img =
  match size_of_symbol_by_name ~name img with
  | Some _ as s -> s
  | None -> (
      try
        Some
          (Loader.Section.size (find_section_by_name name img))
            .Loader_types.virt
      with Not_found -> None)

let interval_of_symbol_or_section_by_name ~name img =
  match symbol_interval_by_name ~name img with
  | Some _ as i -> i
  | None -> (
      try
        let section = find_section_by_name name img in
        let p = (Loader.Section.pos section).Loader_types.virt
        and s = (Loader.Section.size section).Loader_types.virt in
        Some (Virtual_address.create p, Virtual_address.create (p + s))
      with Not_found -> None)

module Binary_loc = struct
  type t = Address of Virtual_address.t | Name of string | Offset of t * int

  let name s = Name s
  let address a = Address a

  let offset n t =
    if n = 0 then t
    else
      match t with
      | Name _ as t -> Offset (t, n)
      | Offset (t, m) -> Offset (t, m + n)
      | Address a -> Address (Virtual_address.add_int n a)

  let rec pp ppf = function
    | Name s -> Format.pp_print_string ppf s
    | Offset (t, n) ->
        Format.fprintf ppf "<%a %c %d>" pp t (if n < 0 then '-' else '+') n
    | Address a -> Virtual_address.pp ppf a

  let rec of_string s =
    match s.[0] with
    | '<' ->
        let pos_end = String.rindex s '>' in
        let pos_plus = String.index s '+' in
        let base = of_string (String.sub s 1 (pos_plus - 1))
        and int_off =
          let start = pos_plus + 1 in
          let len = pos_end - start in
          int_of_string (String.sub s start len)
        in
        offset int_off base
    | '0' ->
        if s.[1] = 'x' || s.[1] = 'X' then
          Address (Virtual_address.create (int_of_string s))
        else Name s
    | _ -> Name s

  (* match int_of_string s with
   * | addr -> Address (Virtual_address.create addr)
   * | exception Failure "int_of_string" -> Name s *)

  let ( >> ) g f = match g with None -> None | Some v -> Some (f v)

  let address_from_img name img =
    match address_of_symbol_by_name img ~name with
    | None -> None
    | Some i -> Some (Virtual_address.create i)

  let to_virtual_address_from_file ~filename t =
    let rec eval = function
      | Address addr -> Some addr
      | Name name ->
          let img = Loader.load_file filename in
          address_from_img name img
      | Offset (t, n) -> eval t >> Virtual_address.add_int n
    in
    eval t

  let to_virtual_address ~img t =
    let rec loop = function
      | Address addr -> Some addr
      | Name name -> address_from_img name img
      | Offset (t, n) -> loop t >> Virtual_address.add_int n
    in
    loop t
end
