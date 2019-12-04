(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2019                                               *)
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

(** Kernel general command-line options. *)

include Cli.Make
          (
            struct
              let shortname = "" (* This is the only one :-) *)
              let name = "Kernel"
            end
          )

module Config_file = Builder.String_option(
struct
  let name = "config"
  let doc = "Use this configuration file"
end
)

module Dba_config = Builder.String_option(
struct
    let name = "dba-config"
    let doc = "Set dba configuration file name"
end
)

module Dba_file = Builder.String_option(
struct
  let name = "dba-file"
  let doc = "Set DBA file "
end
)

module Describe_binary = Builder.False(
struct
  let name = "describe"
  let doc = "Display a description of the binary and exits"
end
)

(** Server options *)

module Experimental = Builder.False(
struct
  let name = "X"
  let doc = "Only for developmental purposes"
end
)

module Share_directories = struct
  module O = Builder.String_option(
      struct
        let name = "share"
        let doc = "Set additional shared directory"
      end)

  type t = string list
  let default_dirs =
    let dirs = [Config.sharedir] in
    try Sys.getenv "BINSEC_SHARE" :: dirs
    with Not_found -> dirs

  let search_dirs = ref default_dirs

  let set l = search_dirs := l @ !search_dirs


  let get () = !search_dirs

  let is_set () = O.is_set ()
  let is_default () = get () = default_dirs

  let pp ppf dirs =
    Format.pp_open_hovbox ppf 0;
    List.iter (fun dname -> Format.fprintf ppf "%s;@ " dname) dirs;
    Format.pp_close_box ppf ()

  let find_file ~filename =
    let dirs = get () in
    let rec loop = function
      | [] ->
        Logger.error "Could not find %s in %a" filename pp dirs;
        raise Not_found
      | d :: dirs ->
        let fname = Filename.concat d filename in
        if Sys.file_exists fname && not (Sys.is_directory fname) then fname
        else loop dirs
    in loop dirs
end


module ExecFile =
  Builder.String_option(
  struct
    let name = "file"
    let doc  = "Set binary file"
  end
  )

module Entry_point =
  Builder.String_option(
    struct
      let name = "entrypoint"
      let doc = "Set entry point"
    end
    )

module Decoder = Builder.String (
  struct
    let name = "decoder"
    let default = "unisim-armsec"
    let doc = "External decoder command"
  end
  )

module Version = Builder.False (
  struct
    let name = "version"
    let doc = "Print the version identifier and exit"
  end)

module Machine = struct
  (** Abstract representation of hardware architecture *)
  include
    Builder.Variant_choice_assoc(
        struct
          type t = Machine.isa

          let name = "isa"

          let doc = Format.asprintf " Set isa [set by loader]"

          let assoc_map = [
              "x86",     Machine.x86;
              "arm32",   Machine.(armv7 LittleEndian);
              "riscv",   Machine.riscv `x32;
              "unknown", Machine.unknown;
            ]

          let default = Machine.unknown
        end)

    let pp ppf () = Machine.ISA.pp ppf (get ())

    let isa = get

    let endianness () = Machine.ISA.endianness (get ())

    let word_size () =
      Size.Bit.to_int Machine.(Bitwidth.bitsize (ISA.bits (get ())))

    let bits () = Machine.ISA.bits (get ())
end
