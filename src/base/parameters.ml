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

                           
module Builder = struct
  module type ParameterDeclaration = sig
    val name : string
    val doc : string
  end

  let mk_arg name action doc = "-"^name, action, " "^doc

  module Boolean(P : sig include ParameterDeclaration val default : bool end) = struct
    type t = bool
    let value = ref P.default

    let set v = value := v
    let get () = !value

    let arg = mk_arg P.name (Arg.Set value) P.doc

  end

  module False(P:ParameterDeclaration) = struct
    include Boolean(struct include P let default = false end)
  end

  module True(P:ParameterDeclaration) = struct
    include Boolean(struct include P let default = true end)
  end

  module Integer(P : sig include ParameterDeclaration val default : int end) = struct
    type t = int
    let value = ref P.default

    let set v = value := v
    let get () = !value

    let arg = mk_arg P.name (Arg.Set_int value) P.doc
  end

  module Zero(P:ParameterDeclaration) = struct
    include Integer(struct include P let default = 0 end)
  end

  module String(P : sig include ParameterDeclaration val default : string end) = struct
    type t = string
    let value = ref P.default

    let set v = value := v
    let get () = !value

    let arg = mk_arg P.name (Arg.String set) P.doc
  end

  module OptionalString(P: ParameterDeclaration) = struct
    type t = string
    let value = ref None

    let set v = value := Some v
    let is_set () = !value <> None
    let get () =
      assert(is_set ());
      match !value with
      | None -> assert false
      | Some v -> v

    let arg = mk_arg P.name (Arg.String set) P.doc
  end

  module StringSet(P:ParameterDeclaration) = struct
    type t = Basic_types.String.Set.t
    let value = ref Basic_types.String.Set.empty

    let is_set () = not (Basic_types.String.Set.is_empty !value)
    let set v = value := v
    let get () = !value
    let of_string s =
      let rexp = Str.regexp "," in
      Str.split rexp s
      |> Basic_types.String.Set.of_list
      |> set


    let arg = mk_arg P.name (Arg.String of_string) P.doc
            
  end
                                                 
(*
  module Option(G:Generic) = struct
    type t = G.t option
    let value = ref None

    let set (v : G.t) = value := Some v
    let reset () = value := None
    let get () = !value


    let arg =
      let switch, setter, docstring = G.arg in
      switch, setter, docstring
  end
*)
end
