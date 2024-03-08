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

module KO = Kernel_options

let get_img, reset_img =
  let img = ref None in
  ( (fun () ->
      match !img with
      | None -> (
          match KO.ExecFile.get_opt () with
          | None ->
              failwith "Cannot get image since you have not set any binary file"
          | Some f ->
              let i = Loader.load_file f in
              img := Some i;
              i)
      | Some i -> i),
    fun () -> img := None )

let get_ep () =
  match KO.Entry_point.get_opt () with
  | None -> None
  | Some s ->
      let bloc = Loader_utils.Binary_loc.of_string s in
      Loader_utils.Binary_loc.to_virtual_address ~img:(get_img ()) bloc

module Loader = struct
  let set_arch img =
    let isa = Loader.Img.arch img in
    KO.Machine.set isa

  let set_arch_from_file ~filename = Loader.load_file filename |> set_arch
  let pp_loader_summary ppf file = Loader.(Img.pp ppf (load_file file))
end
