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

module KO = Kernel_options

let get_ep () =
  match KO.Entry_point.get_opt () with
  | None -> None
  | Some s ->
     match KO.ExecFile.get () with
     | "" -> None
     | filename ->
        let bloc = Binary_loc.of_string s in
        Binary_loc.to_virtual_address_from_file ~filename bloc

let get_img =
  let img = ref None in
  fun () ->
  (match !img with
   | None ->
      (match KO.ExecFile.get_opt () with
       | None ->
          let msg = "Cannot get image since you have not set any binary file" in
          failwith msg
       | Some f ->
          let i = Loader.load_file f in
          img := Some i;
          i
      )
   | Some i -> i
  )


module Loader = struct
let isa img =
  match Loader.Img.arch img with
  | Loader_types.AMD64 -> Machine.AMD64
  | Loader_types.ARM -> Machine.ARMv7
  | Loader_types.PPC -> Machine.PowerPC
  | Loader_types.X86 -> Machine.X86
  | _ -> Machine.Unknown


let endianness = function
  | Loader_types.LittleEndian -> Machine.LittleEndian
  | Loader_types.BigEndian -> Machine.BigEndian

let set_arch img =
  let isa = isa img in
  let endian = Loader.Img.endian img |> endianness in
  Kernel_options.Machine.ISA.set isa;
  Machine.Endianness.set endian

let set_arch_from_file ~filename =
  Loader.load_file filename |> set_arch

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
  let aux fmt section (f,s) =
    fprintf fmt "%s" (if Loader.Section.has_flag f section then s else "-")
  in
  let pp_flags fmt section =
    List.iter (aux fmt section) Loader_types.([ (Read, "r"); (Write, "w"); (Exec, "x") ])
  in
  let pp_imap ppf m =
    fprintf ppf "@[<h>%8x %8x@]"
      m.Loader_types.raw m.Loader_types.virt
  in fprintf ppf "@[<h>%2d %-20s %8x %a %a %a@]"
    i
    (Loader.Section.name section)
    (Loader.Section.flag section)
    pp_imap (Loader.Section.pos section)
    pp_imap (Loader.Section.size section)
    pp_flags section


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
    Machine.pp_isa isa
    Machine.Endianness.pp endianness

let pp_ep ppf ep = fprintf ppf "@[Entry point address: 0x%x@]" ep

let pp_header ppf img =
  fprintf ppf "@[<v 2># Header@ %a@ %a@]"
    pp_arch (isa img,
             Loader.Img.endian img |> endianness)
    pp_ep (Loader.Img.entry img)

let pp_loader_summary ppf file =
  let img = Loader.load_file file in
  fprintf ppf
    "@[<v 0>%a@ %a@ %a@]"
    pp_header img
    pp_symbols (Loader.Img.symbols img)
    pp_sections (Loader.Img.sections img)

end
