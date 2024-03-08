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

open Binpatcher_options

module PatchMap = struct
  type t = Loader_types.u8 Virtual_address.Map.t

  let add_bytes vbase bytes vmap =
    let len = Binstream.length bytes in
    let rec aux map i =
      if i < len then
        let byte = Binstream.get_byte_exn bytes i in
        let map' =
          Virtual_address.Map.add (Virtual_address.add_int i vbase) byte map
        in
        aux map' (succ i)
      else map
    in
    aux vmap 0

  let of_list l =
    List.fold_left
      (fun vmap (vaddr, opcode) -> add_bytes vaddr opcode vmap)
      Virtual_address.Map.empty l

  let load_file filename =
    let map =
      let parser = Parser.patchmap and lexer = Lexer.token in
      Parse_utils.read_file ~parser ~lexer ~filename
    in
    let open! Virtual_address in
    Map.fold add_bytes map Map.empty

  let empty = Virtual_address.Map.empty
end

module WritableLoader = struct
  open Loader_types

  type t = { img : Loader.Img.t; patches : int Basic_types.Int.Map.t }

  let create img patches = { img; patches }

  let get_offset img address =
    match Loader_utils.find_section_by_address img ~address with
    | Some section ->
        let p = Loader.Section.pos section in
        let offset = address - p.virt in
        Some (p.raw + offset)
    | None ->
        Logger.debug "Concrete offset not found for address %x" address;
        None

  let offset img vmap =
    Virtual_address.Map.fold
      (fun vaddr byte cmap ->
        match get_offset img (vaddr :> int) with
        | Some caddr -> Basic_types.Int.Map.add caddr byte cmap
        | None -> cmap)
      vmap Basic_types.Int.Map.empty

  let create img patchmap = create img (patchmap |> offset img)

  let create_from_files ~executable ~patch_file () =
    let img = Loader.load_file executable in
    let patches = PatchMap.load_file patch_file in
    create img patches

  let get_int i t =
    match Basic_types.Int.Map.find i t.patches with
    | c ->
        Logger.debug "Patching address %x with byte %x" i c;
        c
    | exception Not_found -> Loader.read_offset t.img i

  let dim t = Loader.Offset.dim t.img

  let pp_to_file ~filename t =
    Logger.debug "Writing patched binary to file %s" filename;
    let oc = open_out_bin filename in
    for i = 0 to dim t - 1 do
      let v = get_int i t in
      let c = Char.chr v in
      Printf.fprintf oc "%c" c
    done;
    close_out oc

  let get i t = get_int i t
end

let run ~executable =
  let patch_file = Binpatcher_options.PatchFile.get () in
  let wloader = WritableLoader.create_from_files ~executable ~patch_file () in
  let dst = Binpatcher_options.PatchOutFile.get () in
  WritableLoader.pp_to_file ~filename:dst wloader

let run_default () =
  if Kernel_options.ExecFile.is_set () && Binpatcher_options.PatchFile.is_set ()
  then run ~executable:(Kernel_options.ExecFile.get ())

let _ = Cli.Boot.enlist ~name:"binpatcher" ~f:run_default
