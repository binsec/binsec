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


type state = Active | Inactive

(* representation of a node -- must be hashable *)
module Node = struct
  type t = (string * state)
  let compare (n1, _) (n2, _) = Pervasives.compare n1 n2
  let hash = Hashtbl.hash
  let equal = ( = )
end

(* representation of an edge -- must be comparable *)
module Edge = struct
  type t = string
  let compare = Pervasives.compare
  let default = ""
end


(* a functional/persistent graph *)
module G = struct
  include Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Node)(Edge)
  let mk_active_node name = V.create (name, Active)
  let mk_inactive_node name = V.create (name, Inactive)
end

module D = Graph.Traverse.Dfs(G)

module Dot = struct
  include Graph.Graphviz.Dot(struct
      include G (* use the graph module from above *)
      let edge_attributes (_, e, _) = [`Label e; `Color 4711]
      let default_edge_attributes _ = []
      let get_subgraph _ = None
      let vertex_attributes (_name, state) =
        match state with
        | Active ->
          [`Shape `Box;
           `Style `Rounded;
           `Style `Filled;
           `Fillcolor 10092492]
        | _ ->
          [`Shape `Box;
           `Style `Rounded;
           `Style `Filled;
           `Fillcolor 16741680]
      let vertex_name (name, _state) = "\"" ^ name ^ "\""
      (* "\""^(String.sub (Dba.string_of_dbaaddress addr) 1 10) ^ "\"" *)
      let default_vertex_attributes _ = []
      let graph_attributes _ = []
    end)

  let output_graph_to_file filename g =
    let oc = open_out_bin filename in
    output_graph oc g;
    close_out oc

end



