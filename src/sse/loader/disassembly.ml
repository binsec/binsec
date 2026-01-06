(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2026                                               *)
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

module IntTbl = Basic_types.Integers.Int.Htbl
module IntSet = Basic_types.Integers.Int.Set
module VarSet = Dba_types.Var.Set

module Revision = struct
  type vertex = Ir.Graph.vertex
  type edge = Ir.Graph.edge
  type t = { n : vertex; graph : Ir.Graph.t }

  module V = Ir.Graph.V
  module E = Ir.Graph.E

  let is_directed = Ir.Graph.is_directed
  let is_empty { graph; _ } = Ir.Graph.is_empty graph
  let nb_vertex { graph; _ } = Ir.Graph.nb_vertex graph
  let nb_edges { graph; _ } = Ir.Graph.nb_edges graph
  let out_degree { graph; _ } vertex = Ir.Graph.out_degree graph vertex
  let in_degree { graph; _ } vertex = Ir.Graph.in_degree graph vertex
  let mem_vertex { graph; _ } vertex = Ir.Graph.mem_vertex graph vertex
  let mem_edge { graph; _ } src dst = Ir.Graph.mem_edge graph src dst
  let mem_edge_e { graph; _ } edge = Ir.Graph.mem_edge_e graph edge
  let find_edge { graph; _ } src dst = Ir.Graph.find_edge graph src dst

  let find_all_edges { graph; _ } src dst =
    Ir.Graph.find_all_edges graph src dst

  let succ { graph; _ } vertex = Ir.Graph.succ graph vertex
  let pred { graph; _ } vertex = Ir.Graph.pred graph vertex
  let succ_e { graph; _ } vertex = Ir.Graph.succ_e graph vertex
  let pred_e { graph; _ } vertex = Ir.Graph.pred_e graph vertex
  let iter_vertex f { graph; _ } = Ir.Graph.iter_vertex f graph
  let fold_vertex f { graph; _ } data = Ir.Graph.fold_vertex f graph data
  let iter_edges f { graph; _ } = Ir.Graph.iter_edges f graph
  let fold_edges f { graph; _ } data = Ir.Graph.fold_edges f graph data
  let iter_edges_e f { graph; _ } = Ir.Graph.iter_edges_e f graph
  let fold_edges_e f { graph; _ } data = Ir.Graph.fold_edges_e f graph data
  let map_vertex f { n; graph } = { n; graph = Ir.Graph.map_vertex f graph }
  let iter_succ f { graph; _ } vertex = Ir.Graph.iter_succ f graph vertex
  let fold_succ f { graph; _ } data = Ir.Graph.fold_succ f graph data
  let iter_pred f { graph; _ } vertex = Ir.Graph.iter_pred f graph vertex
  let fold_pred f { graph; _ } data = Ir.Graph.fold_pred f graph data
  let iter_succ_e f { graph; _ } vertex = Ir.Graph.iter_succ_e f graph vertex
  let fold_succ_e f { graph; _ } data = Ir.Graph.fold_succ_e f graph data
  let iter_pred_e f { graph; _ } vertex = Ir.Graph.iter_pred_e f graph vertex
  let fold_pred_e f { graph; _ } data = Ir.Graph.fold_pred_e f graph data
  let node { graph; _ } vertex = Ir.Graph.node graph vertex
  let iter_entries f { graph; _ } = Ir.Graph.iter_entries f graph
  let iter_exits f { graph; _ } = Ir.Graph.iter_exits f graph
  let fold_entries f { graph; _ } data = Ir.Graph.fold_entries f graph data
  let fold_exits f { graph; _ } data = Ir.Graph.fold_exits f graph data

  let insert_before_v { n; graph } vertex ?label opcode =
    if vertex < n then raise (Invalid_argument "insert_before");
    Ir.Graph.insert_before_v graph vertex ?label opcode

  let insert_before ir vertex ?label opcode =
    ignore (insert_before_v ir vertex ?label opcode)

  let insert_list_before_v { n; graph } vertex ?label opcodes =
    if vertex < n then raise (Invalid_argument "insert_list_before");
    Ir.Graph.insert_list_before_v graph vertex ?label opcodes

  let insert_list_before ir vertex ?label opcodes =
    ignore (insert_list_before_v ir vertex ?label opcodes)

  let make graph = { n = Ir.Graph.length graph; graph }
  let is_new_vertex { n; _ } vertex = n <= vertex

  let iter_new_vertex f { n; graph } =
    let last = Ir.Graph.length graph - 1 in
    for i = n to last do
      f i
    done
end

module Hook = struct
  type fetch = Virtual_address.t -> Ir.Graph.t option
  and decode = Virtual_address.t -> int Reader.t -> Ir.Graph.t option
  and disasm = Instruction.t -> Ir.Graph.t option
  and rewrite = Ir.Graph.t -> unit
  and instrument = Revision.t -> unit
end

type stage = Early | Late
type 'a staged = { early : 'a; late : 'a }

let get : stage -> 'a staged -> 'a =
 fun stage { early; late } -> match stage with Early -> early | Late -> late

let stage_update : stage -> ('a -> 'a) -> 'a staged -> 'a staged =
 fun stage f { early; late } ->
  match stage with
  | Early -> { early = f early; late }
  | Late -> { early; late = f late }

type 'a hook =
  | Fetch : Hook.fetch hook
  | Decode : Hook.decode hook
  | Disasm : Hook.disasm hook
  | Rewrite : Hook.rewrite hook

(** Information to be used by optimization. *)
type 'a knowledge =
  | May_read : VarSet.t option knowledge
  | Must_write : VarSet.t knowledge

module Callback = struct
  module Q = Basic_types.Integers.Int.Map

  type t = {
    n : int;
    fetch : Hook.fetch Q.t Zmap.t staged;
    decode : Hook.decode Q.t Zmap.t staged;
    disasm : Hook.disasm Q.t Zmap.t staged;
    rewrite : Hook.rewrite Q.t Zmap.t staged;
    instrument : Hook.instrument Q.t;
    may_read : (Ir.builtin -> VarSet.t option option) list;
    must_write : (Ir.builtin -> VarSet.t option) list;
  }

  let empty : t =
    {
      n = 0;
      fetch = { early = Zmap.empty; late = Zmap.empty };
      decode = { early = Zmap.empty; late = Zmap.empty };
      disasm = { early = Zmap.empty; late = Zmap.empty };
      rewrite = { early = Zmap.empty; late = Zmap.empty };
      instrument = Q.empty;
      may_read = [];
      must_write = [];
    }

  let update : 'a Q.t Zmap.item -> 'a Q.t Zmap.item -> 'a Q.t Zmap.t =
    let union_left : 'a Q.t -> 'a Q.t -> 'a Q.t =
     fun q0 q1 -> Q.union (fun _ v _ -> Some v) q0 q1
    in
    let update_ordered : 'a Q.t Zmap.item -> 'a Q.t Zmap.item -> 'a Q.t Zmap.t =
     fun (Item { lo = lo0; hi = hi0; elt = q0 })
         (Item { lo = lo1; hi = hi1; elt = q1 }) ->
      match Interval.overlap { lo = lo0; hi = hi0 } { lo = lo1; hi = hi1 } with
      | LRl_LRh (ep0, ep1) -> Zmap.singleton ~lo:ep0 ~hi:ep1 (union_left q0 q1)
      | LRl_Lh_Rh (ep0, ep1, ep2) ->
          Zmap.union_left
            (Zmap.singleton ~lo:ep0 ~hi:(Z.pred ep1) (union_left q0 q1))
            (Zmap.singleton ~lo:ep1 ~hi:ep2 q1)
      | LRl_Rh_Lh (ep0, ep1, ep2) ->
          Zmap.union_left
            (Zmap.singleton ~lo:ep0 ~hi:(Z.pred ep1) (union_left q0 q1))
            (Zmap.singleton ~lo:ep1 ~hi:ep2 q0)
      | Ll_Rl_LRh (ep0, ep1, ep2) ->
          Zmap.union_left
            (Zmap.singleton ~lo:ep0 ~hi:(Z.pred ep1) q0)
            (Zmap.singleton ~lo:ep1 ~hi:ep2 (union_left q0 q1))
      | Rl_Ll_LRh (ep0, ep1, ep2) ->
          Zmap.union_left
            (Zmap.singleton ~lo:ep0 ~hi:(Z.pred ep1) q1)
            (Zmap.singleton ~lo:ep1 ~hi:ep2 (union_left q0 q1))
      | Ll_Rl_Lh_Rh (ep0, ep1, ep2, ep3) ->
          Zmap.union_left
            (Zmap.union_left
               (Zmap.singleton ~lo:ep0 ~hi:(Z.pred ep1) q0)
               (Zmap.singleton ~lo:ep1 ~hi:(Z.pred ep2) (union_left q0 q1)))
            (Zmap.singleton ~lo:ep2 ~hi:ep3 q1)
      | Ll_Rl_Rh_Lh (ep0, ep1, ep2, ep3) ->
          Zmap.union_left
            (Zmap.union_left
               (Zmap.singleton ~lo:ep0 ~hi:(Z.pred ep1) q0)
               (Zmap.singleton ~lo:ep1 ~hi:(Z.pred ep2) (union_left q0 q1)))
            (Zmap.singleton ~lo:ep2 ~hi:ep3 q0)
      | Rl_Ll_Lh_Rh (ep0, ep1, ep2, ep3) ->
          Zmap.union_left
            (Zmap.union_left
               (Zmap.singleton ~lo:ep0 ~hi:(Z.pred ep1) q1)
               (Zmap.singleton ~lo:ep1 ~hi:(Z.pred ep2) (union_left q0 q1)))
            (Zmap.singleton ~lo:ep2 ~hi:ep3 q1)
      | Rl_Ll_Rh_Lh (ep0, ep1, ep2, ep3) ->
          Zmap.union_left
            (Zmap.union_left
               (Zmap.singleton ~lo:ep0 ~hi:(Z.pred ep1) q1)
               (Zmap.singleton ~lo:ep1 ~hi:(Z.pred ep2) (union_left q0 q1)))
            (Zmap.singleton ~lo:ep2 ~hi:ep3 q0)
    in
    fun (Item { lo = lo0; _ } as item0) (Item { lo = lo1; _ } as item1) ->
      if Z.lt lo0 lo1 then update_ordered item1 item0
      else update_ordered item0 item1

  let register_hook :
      type a.
      t -> Virtual_address.t Interval.t -> ?stage:stage -> a hook -> a -> t =
   fun ({ n; fetch; decode; disasm; rewrite; _ } as env) { lo; hi }
       ?(stage = Early) hook f ->
    let lo = Virtual_address.to_bigint lo
    and hi = Virtual_address.to_bigint hi in
    match hook with
    | Fetch ->
        {
          env with
          n = n + 1;
          fetch =
            stage_update stage
              (Zmap.union_update update
                 (Zmap.singleton ~lo ~hi (Q.singleton n f)))
              fetch;
        }
    | Decode ->
        {
          env with
          n = n + 1;
          decode =
            stage_update stage
              (Zmap.union_update update
                 (Zmap.singleton ~lo ~hi (Q.singleton n f)))
              decode;
        }
    | Disasm ->
        {
          env with
          n = n + 1;
          disasm =
            stage_update stage
              (Zmap.union_update update
                 (Zmap.singleton ~lo ~hi (Q.singleton n f)))
              disasm;
        }
    | Rewrite ->
        {
          env with
          n = n + 1;
          rewrite =
            stage_update stage
              (Zmap.union_update update
                 (Zmap.singleton ~lo ~hi (Q.singleton n f)))
              rewrite;
        }

  let register_instrumentation : t -> Hook.instrument -> t =
   fun ({ n; instrument; _ } as env) f ->
    { env with n = n + 1; instrument = Q.add n f instrument }

  let register_knowledge :
      type a. t -> a knowledge -> (Ir.builtin -> a option) -> t =
   fun ({ may_read; must_write; _ } as env) info callback ->
    match info with
    | May_read -> { env with may_read = callback :: may_read }
    | Must_write -> { env with must_write = callback :: must_write }

  let fetch : t -> Virtual_address.t -> stage -> Hook.fetch Seq.t =
   fun { fetch; _ } addr stage ->
    match Zmap.find (Virtual_address.to_bigint addr) (get stage fetch) with
    | exception Not_found -> Seq.empty
    | Item { elt = q; _ } -> Seq.map snd (Q.to_seq q)

  let decode : t -> Virtual_address.t -> stage -> Hook.decode Seq.t =
   fun { decode; _ } addr stage ->
    match Zmap.find (Virtual_address.to_bigint addr) (get stage decode) with
    | exception Not_found -> Seq.empty
    | Item { elt = q; _ } -> Seq.map snd (Q.to_seq q)

  let disasm : t -> Virtual_address.t -> stage -> Hook.disasm Seq.t =
   fun { disasm; _ } addr stage ->
    match Zmap.find (Virtual_address.to_bigint addr) (get stage disasm) with
    | exception Not_found -> Seq.empty
    | Item { elt = q; _ } -> Seq.map snd (Q.to_seq q)

  let rewrite : t -> Virtual_address.t -> stage -> Ir.Graph.t -> unit =
   fun { rewrite; _ } addr stage ir ->
    match Zmap.find (Virtual_address.to_bigint addr) (get stage rewrite) with
    | exception Not_found -> ()
    | Item { elt = q; _ } -> Q.iter (fun _ f -> f ir) q

  let instrument : t -> Revision.t -> unit =
   fun { instrument; _ } ir -> Q.iter (fun _ f -> f ir) instrument

  let rec resolve_knowledge :
      Ir.builtin -> (Ir.builtin -> 'a option) list -> default:'a -> 'a =
   fun builtin callbacks ~default ->
    match callbacks with
    | [] -> default
    | f :: callbacks -> (
        match f builtin with
        | None -> resolve_knowledge builtin callbacks ~default
        | Some knowledge -> knowledge)

  let may_read : t -> Ir.builtin -> VarSet.t option =
   fun { may_read; _ } builtin ->
    resolve_knowledge builtin may_read ~default:None

  let must_write : t -> Ir.builtin -> VarSet.t =
   fun { must_write; _ } builtin ->
    resolve_knowledge builtin must_write ~default:VarSet.empty
end

type t = {
  callback : Callback.t;
  decoder : int Reader.t -> Virtual_address.t -> Instruction.t;
  base : Virtual_address.t;
  reader : Virtual_address.t Reader.t;
  size : Z.t;
  mutable graph : Ir.Graph.t;
  entries : Ir.Graph.vertex Virtual_address.Htbl.t;
  killset : VarSet.t IntTbl.t;
}

let create :
    Callback.t ->
    decoder:(int Reader.t -> Virtual_address.t -> Instruction.t) ->
    Virtual_address.t ->
    Virtual_address.t Reader.t ->
    Z.t ->
    t =
 fun callback ~decoder base reader size ->
  {
    callback;
    decoder;
    base;
    reader;
    size;
    graph = Ir.Graph.empty ();
    entries = Virtual_address.Htbl.create 16;
    killset = IntTbl.create 16;
  }

let create_small :
    Callback.t ->
    decoder:(int Reader.t -> Virtual_address.t -> Instruction.t) ->
    Virtual_address.t ->
    int Reader.t ->
    int ->
    t =
  let offset addr n = Virtual_address.add_int n addr in
  fun callback ~decoder base reader size ->
    {
      callback;
      decoder;
      base;
      reader = Reader.rebase ~offset ~distance:Virtual_address.diff base reader;
      size = Z.of_int size;
      graph = Ir.Graph.empty ();
      entries = Virtual_address.Htbl.create 16;
      killset = IntTbl.create 16;
    }

let callback : t -> Callback.t = fun { callback; _ } -> callback
let address : t -> Virtual_address.t = fun { base; _ } -> base
let graph : t -> Ir.View.t = fun { graph; _ } -> (graph :> Ir.View.t)

let killset : t -> Ir.Graph.vertex -> VarSet.t =
 fun { killset; _ } vertex ->
  try IntTbl.find killset vertex with Not_found -> VarSet.empty

exception Continue

let relink_eoh : Ir.Graph.t -> unit =
 fun acc ->
  let succ = Ir.Graph.length acc in
  Ir.Graph.iter_exits
    (fun vertex ->
      match Ir.Graph.node acc vertex with
      | Terminator { label; kind = Builtin Ir.EndOfHook; _ } ->
          Ir.Graph.replace_node acc vertex
            (Fallthrough { label; kind = Nop; succ })
      | _ -> ())
    acc

let has_eoh : Ir.Graph.t -> bool =
 fun acc ->
  match
    Ir.Graph.iter_exits
      (fun vertex ->
        match Ir.Graph.node acc vertex with
        | Terminator { kind = Builtin Ir.EndOfHook; _ } ->
            raise_notrace Continue
        | _ -> ())
      acc
  with
  | exception Continue -> true
  | () -> false

let sub : t -> Virtual_address.t -> int Reader.t =
 fun code address ->
  Reader.move code.reader address;
  Reader.sub code.reader
    (try
       Z.to_int
         (Z.sub code.size
            (Z.sub
               (Virtual_address.to_bigint address)
               (Virtual_address.to_bigint code.base)))
     with Z.Overflow -> max_int)

let rec fetch :
    t ->
    Virtual_address.t ->
    stage ->
    Hook.fetch Seq.t ->
    Ir.Graph.t ->
    Ir.Graph.t =
 fun code address stage hooks acc ->
  match hooks () with
  | Nil -> (
      match stage with
      | Early ->
          fetch code address Late
            (Callback.fetch code.callback address Late)
            acc
      | Late ->
          decode code address Early
            (Callback.decode code.callback address Early)
            acc (sub code address))
  | Cons (hook, hooks) -> (
      match hook address with
      | None -> fetch code address stage hooks acc
      | Some ir ->
          relink_eoh acc;
          ignore (Ir.Graph.append ~from:ir acc);
          if has_eoh acc then fetch code address stage hooks acc
          else rewrite code address acc)

and decode :
    t ->
    Virtual_address.t ->
    stage ->
    Hook.decode Seq.t ->
    Ir.Graph.t ->
    int Reader.t ->
    Ir.Graph.t =
 fun code address stage hooks acc reader ->
  Reader.move reader 0;
  match hooks () with
  | Nil -> (
      match stage with
      | Early ->
          decode code address Late
            (Callback.decode code.callback address Late)
            acc reader
      | Late ->
          disasm code
            (code.decoder reader address)
            Early
            (Callback.disasm code.callback address Early)
            acc)
  | Cons (hook, hooks) -> (
      match hook address reader with
      | None -> decode code address stage hooks acc reader
      | Some ir ->
          relink_eoh acc;
          ignore (Ir.Graph.append ~from:ir acc);
          if has_eoh acc then decode code address stage hooks acc reader
          else rewrite code address acc)

and disasm :
    t -> Instruction.t -> stage -> Hook.disasm Seq.t -> Ir.Graph.t -> Ir.Graph.t
    =
 fun code inst stage hooks acc ->
  match hooks () with
  | Nil -> (
      let address = Instruction.address inst in
      match stage with
      | Early ->
          disasm code inst Late (Callback.disasm code.callback address Late) acc
      | Late ->
          let ir = Ir.Graph.of_instruction inst in
          relink_eoh acc;
          ignore (Ir.Graph.append ~from:ir acc);
          rewrite code address acc)
  | Cons (hook, hooks) -> (
      match hook inst with
      | None -> disasm code inst stage hooks acc
      | Some ir ->
          relink_eoh acc;
          ignore (Ir.Graph.append ~from:ir acc);
          if has_eoh acc then disasm code inst stage hooks acc
          else rewrite code (Instruction.address inst) acc)

and rewrite : t -> Virtual_address.t -> Ir.Graph.t -> Ir.Graph.t =
 fun code address acc ->
  Callback.rewrite code.callback address Early acc;
  Callback.rewrite code.callback address Late acc;
  acc

let fetch_no_link : t -> Virtual_address.t -> Ir.Graph.vertex =
 fun code address ->
  try Virtual_address.Htbl.find code.entries address
  with Not_found ->
    let offset =
      Z.sub
        (Virtual_address.to_bigint address)
        (Virtual_address.to_bigint code.base)
    in
    if Z.lt offset Z.zero || Z.leq code.size offset then raise Not_found
    else
      let ir =
        fetch code address Early
          (Callback.fetch code.callback address Early)
          (Ir.Graph.empty ())
      in
      let revision = Revision.make code.graph in
      let vertex = Ir.Graph.append ~from:ir code.graph in
      let sink =
        Ir.Graph.fold_exits
          (fun i sink -> IntSet.add (vertex + i) sink)
          ir IntSet.empty
      in
      Callback.instrument code.callback revision;
      code.graph <- Ir.Graph.copy code.graph;
      Ir.Killset.analyze
        ~may_read:(Callback.may_read code.callback)
        ~must_write:(Callback.must_write code.callback)
        (code.graph :> Ir.View.t)
        ~sink code.killset;
      Virtual_address.Htbl.add code.entries address vertex;
      vertex

let disassemble_from : t -> Virtual_address.t -> Ir.Graph.vertex =
  let rec fetch_and_link :
      t ->
      (Ir.Graph.vertex * Ir.label * Dba.tag) list Virtual_address.Map.t ->
      IntSet.t ->
      IntSet.t =
   fun code addresses sink ->
    if Virtual_address.Map.is_empty addresses then sink
    else
      let address, to_link = Virtual_address.Map.choose addresses in
      let addresses = Virtual_address.Map.remove address addresses in
      match Virtual_address.Htbl.find code.entries address with
      | vertex -> link_and_fetch code addresses sink address vertex to_link
      | exception Not_found ->
          let offset =
            Z.sub
              (Virtual_address.to_bigint address)
              (Virtual_address.to_bigint code.base)
          in
          if Z.lt offset Z.zero || Z.leq code.size offset then
            fetch_and_link code addresses sink
          else
            let ir =
              fetch code address Early
                (Callback.fetch code.callback address Early)
                (Ir.Graph.empty ())
            in
            let vertex = Ir.Graph.append ~from:ir code.graph in
            let sink =
              Ir.Graph.fold_exits
                (fun i sink -> IntSet.add (vertex + i) sink)
                ir sink
            in
            let addresses =
              Ir.Graph.fold_exits
                (fun pred addresses ->
                  match Ir.Graph.node ir pred with
                  | Terminator { label; kind = Goto { target; tag } } ->
                      Virtual_address.Map.add target
                        ((vertex + pred, label, tag)
                        ::
                        (try Virtual_address.Map.find target addresses
                         with Not_found -> []))
                        addresses
                  | _ -> addresses)
                ir addresses
            in
            Virtual_address.Htbl.add code.entries address vertex;
            link_and_fetch code addresses sink address vertex to_link
  and link_and_fetch :
      t ->
      (Ir.Graph.vertex * Ir.label * Dba.tag) list Virtual_address.Map.t ->
      IntSet.t ->
      Virtual_address.t ->
      Ir.Graph.vertex ->
      (Ir.Graph.vertex * Ir.label * Dba.tag) list ->
      IntSet.t =
   fun code addresses sink address vertex to_link ->
    List.iter
      (fun (pred, label, tag) ->
        Ir.Graph.replace_node code.graph pred
          (Fallthrough
             { label; kind = Goto { target = address; tag }; succ = vertex }))
      to_link;
    fetch_and_link code addresses sink
  in
  fun code address ->
    try Virtual_address.Htbl.find code.entries address
    with Not_found ->
      let revision = Revision.make code.graph in
      let sink =
        IntSet.filter
          (fun vertex ->
            match Ir.Graph.node code.graph vertex with
            | Fallthrough { succ = vertex; _ } -> vertex < revision.n
            | _ -> true)
          (fetch_and_link code
             (Virtual_address.Map.singleton address [])
             IntSet.empty)
      in
      Callback.instrument code.callback revision;
      code.graph <- Ir.Graph.copy code.graph;
      Ir.Killset.analyze
        (code.graph :> Ir.View.t)
        ~may_read:(Callback.may_read code.callback)
        ~must_write:(Callback.must_write code.callback)
        ~sink code.killset;
      Virtual_address.Htbl.find code.entries address
