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

let isa = function
  | Loader_types.AMD64 -> Machine.AMD64
  | Loader_types.ARM -> Machine.ARM
  | Loader_types.PPC -> Machine.PowerPC
  | Loader_types.X86 -> Machine.X86
  | _ -> Machine.Unknown

let endianness = function
  | Loader_types.LittleEndian -> Machine.LittleEndian
  | Loader_types.BigEndian -> Machine.BigEndian

let get_img =
  let img : Loader.Img.t option ref = ref None in
  fun () ->
    match !img with
    | None ->
      (try
         (* Not good to have Disasm_option be there *)
         let i = Loader.load_file (Disasm_options.get_file ()) in
         Logger.debug "Detected ISA %a"
           Machine.ISA.pp (Loader.Img.arch i |> isa);
         img := Some i;
         i
       with
       | Unix.Unix_error (e,_,_) -> invalid_arg (Unix.error_message e))
    | Some img -> img


let get_byte_at addr =
  Loader.read_address (get_img ()) (Bigint.int_of_big_int addr)

let set_arch () =
  let isa = Loader.Img.arch (get_img ()) |> isa in
  let endian = Loader.Img.endian (get_img ()) |> endianness in
  Machine.ISA.set isa;
  Machine.Endianness.set endian

exception SectionFound of Loader.Section.t

let find_section section_name img =
  let sections = Loader.Img.sections img in
  try
    Array.iter
      (fun section ->
        if String.compare section_name (Loader.Section.name section) = 0
        then raise (SectionFound section)
      ) sections;
    raise Not_found
  with SectionFound section -> section

let section_slice section_name img =
  let open Loader_types in
  let section = find_section section_name img in
  let sec_start = (Loader.Section.pos section).virt  in
  let sec_end = sec_start + (Loader.Section.size section).virt in
  sec_start, sec_end


open Format

let pp_symbol ppf symbol =
  fprintf ppf "@[<h>%-8x %s@]"
    (Loader.Symbol.value symbol)
    (Loader.Symbol.name  symbol)

let pp_symbols ppf symbols =
  let nsymbols = Array.length symbols in
  if nsymbols <> 0 then 
  fprintf ppf
    "@[<v 2># Symbols (%d) @ %a@]"
    nsymbols
    (fun ppf a -> Array.iter (fun sy -> fprintf ppf "%a@ " pp_symbol sy) a)
    symbols


let pp_section i ppf section =
  let pp_imap ppf m =
    fprintf ppf "@[<h>%8x %8x@]"
      m.Loader_types.raw m.Loader_types.virt
  in fprintf ppf "@[<h>%2d %-20s %8x %a %a@]"
    i
    (Loader.Section.name section)
    (Loader.Section.flag section)
    pp_imap (Loader.Section.pos section)
    pp_imap (Loader.Section.size section)


let pp_sections ppf sections =
  let nsections = Array.length sections in
  if nsections <> 0 then 
  fprintf ppf
    "@[<v 2># Sections (%d)@ %a@]"
    nsections
    (fun ppf a ->
       Array.iteri (fun i sy -> fprintf ppf "%a@ " (pp_section i) sy) a)
    sections

let pp_arch ppf (isa, endianness) =
  fprintf ppf "@[Machine: %a (%a)@]"
    Machine.ISA.pp isa
    Machine.Endianness.pp endianness

let pp_ep ppf ep = fprintf ppf "@[Entry point address: 0x%x@]" ep

let pp_header ppf img =
  fprintf ppf "@[<v 2># Header@ %a@ %a@]"
    pp_arch (Loader.Img.arch img |> isa,
             Loader.Img.endian img |> endianness)
    pp_ep (Loader.Img.entry img)

let pp_loader_summary ppf file =
  let img = Loader.load_file file in
  fprintf ppf
    "@[<v 0>%a@ %a@ %a@]"
    pp_header img
    pp_symbols (Loader.Img.symbols img)
    pp_sections (Loader.Img.sections img)
