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

(** Internal model representation *)

(** {2 Model types }*)
type t
type address = int64
type identifier = string

(** {2 Constructors} *)
val empty : t
(** Empty model. Any lookup in it will raise Not_found *)

val extract : Smtlib_ast.model -> t
(** [extract model] extracts relevant information in SMT-LIB [model] concerning
 ** registers and memory values.
 ** This function takes into account the various "model dialects" spoken by:
 ** - Z3;
 ** - CVC4;
 ** - Boolector
*)

val yices_extract : string -> t
(** [yices_extract s] extracts the same information as [extract] but for a
 ** yices-smt2 produced model
 ** yices-smt2 models use another syntax and are parsed on their own
 ** [s] is expected to be the raw string of the model and not, for example, a
 ** filename
*)

(** {2 Pretty-printer } *)

val pp : Format.formatter -> t -> unit


(** {2 Accessors } *)

val find_register : t -> identifier -> Bitvector.t
(** [find_register model name] finds the bitvector value of register [name] in
 ** the model
 ** Raise Not_found
*)

val find_address_contents : t -> address -> int
(** [find_address_contents model addr] find the (byte-sized) value of address
 ** [addr] from the model.
 ** Raise Not_found
*)

val find_address_content :
  t -> address -> Basic_types.ByteSize.t -> Dba.endianness -> Bitvector.t
(** Not yet implemented *)

val registers : t -> identifier list
(** [get_register model] gets the list of registers of this model *)



val memory_addresses : t -> address list
(** [get_register model] gets the list of addresses with specific values of this
 ** model *)

val is_memory_set : t -> address -> bool
(** [is_memory_set model address] checks if the given address is present in the
 ** memory of this model
*)
