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

(** Functors for command-line parameters declarations *)

module type Generic = sig
  type t
  val set : t -> unit
  val get : unit -> t
  val arg : string * Arg.spec * string
end
                    
module type OptionalGeneric = sig
  include Generic
  val is_set : unit -> bool
end

module type Boolean = Generic with type t = bool
module type Integer = Generic with type t = int
module type String = Generic with type t = string
module type OptionalString = OptionalGeneric with type t = string
module type StringSet = OptionalGeneric with type t = Basic_types.String.Set.t

module Builder : sig
  module type ParameterDeclaration = sig
    val name : string
    val doc : string
  end

  module Boolean(P: sig include ParameterDeclaration val default : bool end) :
    Boolean
  module False(P:ParameterDeclaration) : Boolean
  module True(P:ParameterDeclaration) : Boolean
    
  module Integer(P: sig include ParameterDeclaration val default : int end) :
    Integer
  module Zero(P:ParameterDeclaration) : Integer
  module String(P: sig include ParameterDeclaration val default : string end) :
    String
      
  module OptionalString(P: ParameterDeclaration) : OptionalString
  module StringSet(P: ParameterDeclaration) : StringSet
end
