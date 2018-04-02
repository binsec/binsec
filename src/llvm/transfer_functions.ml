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

module Integer = Bigint

module type Arity = sig
  (* Symbols and their arities. 'r represents the result type. *)
  type ('r) ar0
  type ('a,'r) ar1
  type ('a,'b,'r) ar2
  type ('a,'b,'c,'r) ar3
  type ('a,'r) variadic
end

(* Note: in the following, we distinguish between backward and forward
   because there is no need to implement backward transfer functions
   for symbols with arity 0. *)

(****************************************************************)
(* Boolean transfer functions.  *)

module type Boolean_Backward = sig
  type boolean
  module Arity:Arity
  val not: (boolean,boolean) Arity.ar1
  val (&&): (boolean,boolean,boolean) Arity.ar2
  val (||): (boolean,boolean,boolean) Arity.ar2
end

module type Boolean_Forward = sig
  include Boolean_Backward
  val true_: boolean Arity.ar0
  val false_: boolean Arity.ar0
end

(****************************************************************)
(* Binary transfer functions.  *)

(* Note: the size argument, when provided, refers to the size of the
   result. *)
module type Binary_Backward = sig
  type binary
  type boolean
  module Arity: Arity
  val biadd: size:int -> (binary,binary,binary) Arity.ar2
  val bisub: size:int -> (binary,binary,binary) Arity.ar2    
  val bimul: size:int -> (binary,binary,binary) Arity.ar2

  val beq:   size:int -> (binary,binary,boolean) Arity.ar2
  val bisle: size:int -> (binary,binary,boolean) Arity.ar2
  val bislt: size:int -> (binary,binary,boolean) Arity.ar2
  val biule: size:int -> (binary,binary,boolean) Arity.ar2
  val biult: size:int -> (binary,binary,boolean) Arity.ar2

  (* First argument become most significant. *)
  val bconcat: size1:int -> size2:int -> (binary,binary,binary) Arity.ar2
  val bextract: size:int -> index:int -> oldsize:int -> (binary,binary) Arity.ar1
  val band: size:int -> (binary,binary,binary) Arity.ar2
  val bor: size:int -> (binary,binary,binary) Arity.ar2
  val bxor: size:int -> (binary,binary,binary) Arity.ar2

  val buext: size:int -> (binary,binary) Arity.ar1
  val bsext: size:int -> (binary,binary) Arity.ar1
  val bisdiv: size:int -> (binary,binary,binary) Arity.ar2
  val bismod: size:int -> (binary,binary,binary) Arity.ar2
  val biudiv: size:int -> (binary,binary,binary) Arity.ar2
  val biumod: size:int -> (binary,binary,binary) Arity.ar2

  val bshl: size:int -> (binary,binary,binary) Arity.ar2
  val bashr: size:int -> (binary,binary,binary) Arity.ar2
  val blshr: size:int -> (binary,binary,binary) Arity.ar2
end

module type Binary_Forward = sig
  include Binary_Backward
  val biconst: size:int -> Integer.t -> binary Arity.ar0
end
