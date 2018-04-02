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

(** Definitions for binary patcher *)
(** {2 Patch map} *)

module PatchMap : sig
  type t

  val empty : t
  (** The empty patch map *)

  val load_file : string -> t
  (** [load_file filename] loads patches contained in [filename]

      The patches can be given in the following format,
      defined in {v parser/parser.mly v}:

      {ul {- (address "....") }
      {- (address (i_1 ... i_n)) }
      }

      i.e. the file contains a list of small S-expressions where:
      {ul {- the first element is an address (hexadecimal or decimal integer); }
      {- the second element is either
        {ul {- a string (interpreted as a sequence of bytes); }
        {- or a (non-empty) list of integers, where each integer corresponds to
         a byte (0 <= n <= 255). }}
      }
      }
  *)

  val of_list :
    (Dba_types.Virtual_address.t * Basic_types.Binstream.t) list -> t
  (** [of_list l] converts an association list [l] of addresses to opcodes to a
      patch map  *)


  val add_bytes :
    Dba_types.Virtual_address.t -> Basic_types.Binstream.t -> t -> t
  (** [add_bytes address bytes patchmap] writes the opcode [bytes] to [address]
      in the [patchmap]. The full length of the byte sequence is writtent
      starting at [address]. *)
end


(** {2 Writable loader}

    This module is a simple extension to the loaders of [binsec] where it is
    allowed to rewrite parts of the binary.

    The extension is non-destructive.
*)
module WritableLoader : sig
  type t

  val create : Loader.Img.t -> PatchMap.t -> t

  val create_from_files : executable:string -> patch_file:string -> unit -> t
  (** [create_from_files ~executable ~patch_file] creates a writable loader from
      a binary file and a series of patches read from a given file
  *)

  val get : int -> t -> Loader_types.u8
  (** [get addr t] gets the byte stored at address [addr] *)

  val dim : t -> int
  (** [dim t] gives the size of the loaded image in bytes *)

  val pp_to_file : filename:string -> t -> unit
  (** [pp_to_file ~filename w] writes the contents of the image to the file
      [filename] *)
end

val run : executable:string -> unit
(** Run the binary patcher on patch file {!Binpatcher_options.PatchFile}
    for the given [executable].

    The patched result is written to {!Binpatcher_options.PatchOutFile}.
*)
