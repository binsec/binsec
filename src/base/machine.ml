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

let set_get_pair initial_value =
  let v = ref initial_value in
  (fun v' -> v := v'),
  (fun () -> !v)

type isa =
  | X86
  | AMD64
  | PowerPC
  | ARM
  | Unknown

type endianness =
  | LittleEndian
  | BigEndian


module type Valued = sig
  type t
  val value : t
  val pp : Format.formatter -> t -> unit
end

module type Gettable_settable = sig
  type t
  val set : t -> unit
  val get : unit -> t
  val pp : Format.formatter -> t -> unit
  val pp_current : Format.formatter -> unit -> unit
end

module GetterSetterMake (V: Valued) = struct
  type t = V.t

  let set, get = set_get_pair V.value

  let pp = V.pp
  let pp_current ppf () = pp ppf (get ())
end

module ISA =
  GetterSetterMake(
  struct
    type t = isa
    let value = Unknown
    let pp ppf = function
      | X86 -> Format.fprintf ppf "x86"
      | AMD64 -> Format.fprintf ppf "amd64"
      | PowerPC -> Format.fprintf ppf "powerpc"
      | ARM -> Format.fprintf ppf "arm"
      | Unknown -> Format.fprintf ppf "unknown"
  end)


module Endianness = GetterSetterMake(
  struct
    type t = endianness
    let value = LittleEndian
    let pp ppf = function
      | LittleEndian -> Format.fprintf ppf "little endian"
      | BigEndian -> Format.fprintf ppf "big endian"
  end
  )

module Word_size = GetterSetterMake(
  struct
    type t = int
    let value = 32
    let pp = Format.pp_print_int
  end)

let set_x86 () =
  ISA.set X86;
  Endianness.set LittleEndian;
  Word_size.set 32

let set_amd64 () =
  ISA.set AMD64;
  Endianness.set LittleEndian;
  Word_size.set 64

let set_powerpc () = assert false

let set_arm e =
  ISA.set ARM;
  Endianness.set e;
  Word_size.set 32

let set_arm_little () = set_arm LittleEndian

let set_arm_big () = set_arm BigEndian

let is_unknown () = ISA.get () = Unknown

let set_unknown () =
  ISA.set Unknown;
  Endianness.set LittleEndian;
  Word_size.set 32

let pp ppf () =
  Format.fprintf ppf
    "%a (%a) %a bits"
    ISA.pp_current ()
    Endianness.pp_current ()
    Word_size.pp_current ()
