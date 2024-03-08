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

module Interrupt : sig
  val default : float -> int
  val timeout : float -> int
end = struct
  let check timestamp = Float.compare (Unix.gettimeofday ()) timestamp

  let interrupt f x =
    match Unix.sigpending () with
    | [] | (exception Invalid_argument _) -> f x
    | l when List.mem Sys.sigint l -> 1
    | _ -> f x

  let zero = Fun.const 0
  let default = interrupt zero
  let timeout = interrupt check
end

let watchdog ~timeout (f : ?interrupt:('a -> int) * 'a -> 'b) =
  let interrupt =
    if timeout = 0 then (Interrupt.default, 0.)
    else (Interrupt.timeout, Unix.gettimeofday () +. float_of_int timeout)
  in
  f ~interrupt
