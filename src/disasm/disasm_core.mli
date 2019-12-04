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

(** Core functionalities for disassembly *)
exception Decode_error of string


(** {2 Worklist definition for disassembly } *)

module W : sig
  include Worklist.S with type elt = Virtual_address.t
  include Sigs.PRINTABLE with type t:= t

  val of_list  : Virtual_address.t list -> t
  val add_list : t -> Virtual_address.t list -> t

  val add_set : t -> Virtual_address.Set.t -> t
  val of_set  : Virtual_address.Set.t -> t

  val add_filtered_set :
    (Virtual_address.t -> bool) -> t -> Virtual_address.Set.t ->t

  val of_filtered_set :
    (Virtual_address.t -> bool) -> Virtual_address.Set.t ->t

  val singleton : Virtual_address.t -> t
end

(** {2 Basic successors definition } *)

module Successors : sig
  val recursive : Instruction.t ->  Virtual_address.Set.t

  val linear : Instruction.t ->  Virtual_address.Set.t

  val linear_bytewise : Instruction.t ->  Virtual_address.Set.t
end

val decode : Virtual_address.t -> Instruction.t * Virtual_address.t option
(** [decode addr] decodes the contents of address [addr]
    @return the contents of this address and its linear successor (if applicable)
*)


(** {2 Iterators} *)

val fold :
  ('a -> W.t ->
   Instruction.t -> Virtual_address.Set.t -> 'a * W.t ) ->
  'a -> W.t -> 'a
(** [fold f wl v] starts disassembly from worklist [wl] (i.e. an initial
    state) using function [f] to guide its choices to compute a value intialized
    to [v].

    In particular, [f] is called after each successful disassembly to compute a
    new worklist and a new value. It receives from the disassembly loop the current
    value, the current worklist, the decoded instruction, and a list of
    identified successors to this instruction.

    The set of successors is computed according to the disassembly mode. The
    default is recursive as stated in [Disasm_options].

    If you want the linear successors only do:

    Disasm_options.DisassemblyMode.set "linear";

    before starting the fold functions.

*)


val iter:
  (W.t -> Instruction.t -> Virtual_address.Set.t -> W.t) -> W.t -> unit
(** [iter f worklist] iterates disassembles an executable with function [f].

    Given the signature of the function, all computations, except worklist
    management, must take place as imperative side-effects.
*)


(** {2 Functors } *)

module type Iterable = sig
  val successors : Instruction.t -> Virtual_address.Set.t
end

module Make (I:Iterable) : sig
  val fold :
    ('a -> W.t ->
     Instruction.t -> Virtual_address.Set.t -> 'a * W.t ) ->
    'a -> W.t -> 'a

  val iter:
    (W.t -> Instruction.t -> Virtual_address.Set.t -> W.t) -> W.t -> unit
end

val decode_binstream :
  ?base:int ->
  Binstream.t ->
  Instruction.t * Virtual_address.t option
(** [decode_binstream b] decodes stream [b] and returns
    a tuple of the first instruction contained in [b] and its possible
    linear successor.
*)
