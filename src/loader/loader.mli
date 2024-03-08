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

type ('a, 'b, 'c, 'd) t_pack = ELF of 'a | PE of 'b | Raw of 'c | TI83 of 'd

include
  Loader_sigs.S
    with type Section.t =
      ( Loader_elf.Section.t,
        Loader_pe.Section.t,
        Loader_raw.Section.t,
        Loader_ti83.Section.t )
      t_pack
     and type Symbol.t =
      ( Loader_elf.Symbol.t,
        Loader_pe.Symbol.t,
        Loader_raw.Symbol.t,
        Loader_ti83.Symbol.t )
      t_pack
     and type Img.t =
      ( Loader_elf.Img.t,
        Loader_pe.Img.t,
        Loader_raw.Img.t,
        Loader_ti83.Img.t )
      t_pack
     and type Section.header =
      ( Loader_elf.Section.header,
        Loader_pe.Section.header,
        Loader_raw.Section.header,
        Loader_ti83.Section.header )
      t_pack
     and type Symbol.header =
      ( Loader_elf.Symbol.header,
        Loader_pe.Symbol.header,
        Loader_raw.Symbol.header,
        Loader_ti83.Symbol.header )
      t_pack
     and type Img.header =
      ( Loader_elf.Img.header,
        Loader_pe.Img.header,
        Loader_raw.Img.header,
        Loader_ti83.Img.header )
      t_pack

module View : sig
  type t

  val create : Virtual_address.t -> Img.t -> Section.t -> t
  (** [create addr img section]
        create a virtual buffer with the content of [section]
        as if [img] was loaded at [add].
    *)

  val get : t -> Virtual_address.t -> Loader_types.u8
  (** [get t addr]
        read a byte at address [addr] in the view [t].

        @raise Not_found if [addr] is outside of [t].
    *)

  val unsafe_get : t -> Virtual_address.t -> Loader_types.u8
  (** [unsafe_get t addr]
        read a byte at address [addr] in the view [t].

        For avanced user only,
        it does not check that [addr] belongs to [t].
    *)
end
