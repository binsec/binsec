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

module type Monad = sig
  type 'a m
  val return: 'a -> 'a m
  val bind: 'a m -> ('a -> 'b m) -> 'b m
end

module Monadic_Arity(M:Monad) = struct
  type 'a m = 'a M.m
  type 'r ar0 = 'r m
  type ('a,'r) ar1 = 'a -> 'r m
  type ('a,'b,'r) ar2 = 'a -> 'b -> 'r m
  type ('a,'b,'c,'r) ar3 = 'a -> 'b -> 'c -> 'r m
  type ('a,'r) variadic = 'a list -> 'r m
end

module type Expr_Input = sig
  module M:Monad
  type boolean
  type binary
  module Binary:sig
    include Transfer_functions.Binary_Forward with module Arity := Monadic_Arity(M)
                                               and type binary = binary
                                               and type boolean = boolean
  end
  module Boolean:sig
    include Transfer_functions.Boolean_Forward with module Arity := Monadic_Arity(M)
                                                and type boolean = boolean
  end

  val bin_of_bool: boolean -> binary M.m
  val bool_of_bin: binary -> boolean M.m
  val select: boolean -> binary -> binary -> binary M.m

  val get_var: size:int -> string -> binary M.m
  val load: size:int -> Dba.endianness -> binary -> binary M.m

end

module State_Monad(State:sig type t end):Monad with type 'a m = State.t -> ('a * State.t) = struct
  type 'a m = State.t -> ('a * State.t)
  let return x = fun state -> (x,state)
  let bind a f = fun state1 -> let (x,state2) = a state1 in (f x) state2
end


module type Instr_Input = sig
  module State: sig type t end
  include Expr_Input with module M := State_Monad(State)

  val unknown: size:int ->  binary State_Monad(State).m
  val undef: size:int ->  binary State_Monad(State).m      

  (* Note: could return a unit State_Monad(State).m instead, but this
     is more convenient. *)
  val store: size:int -> Dba.endianness -> binary -> binary -> State.t -> State.t
  val set_var: size:int -> string -> binary -> State.t -> State.t
end

type 'bin jump_target =
  | Static of Dba.jump_target
  | Dynamic of 'bin

type ('bool,'bin) jump_kind =
  | JKIf of 'bool * 'bin jump_target * 'bin jump_target
  | JKJump of 'bin jump_target
  | JKStop
  | JKAssume of 'bool * 'bin jump_target

