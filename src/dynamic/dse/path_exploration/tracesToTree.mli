(**************************************************************************)
(*  This file is part of Binsec.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
(*    VERIMAG                                                             *)
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
(**************************************************************************)

type node_type =
  | NODE_EIP
  | NODE_JMP
  | NODE_CALL
  | NODE_LIBCALL
  | NODE_RET
  | NODE_DYN_JMP
  | NODE_OTHER
  | LEAF of string list

type node_tree
type node_addr = int64
type t = (node_addr * node_type * int64 list) list

val contains : node_tree -> t -> bool
val list_to_tree : t -> string -> node_tree 
val add_list_to_tree : node_tree -> t -> string -> unit
val add_id_to_tree : node_tree -> unit
val sat_child : node_tree -> Exploration_type.location -> unit
val unsat_child : node_tree -> Exploration_type.location -> unit
    
val traces_to_dot : string list -> unit
val export : ?filename:string -> node_tree -> unit
val loop_export : ?filename:string -> node_tree -> unit  
 
