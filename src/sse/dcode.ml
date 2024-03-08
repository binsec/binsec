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

open Types
open Ir

module Make
    (Stats : EXPLORATION_STATISTICS_FULL)
    (Path : Path.S)
    (State : STATE) : sig
  module Fiber :
    Fiber.S
      with type builtin :=
        Virtual_address.t ->
        Path.t ->
        int ->
        State.t ->
        (State.t, Types.status) Result.t

  type t

  val single :
    ?hooks:(string * Script.Instr.t list) list * Script.env ->
    task:unit Basic_types.Int.Htbl.t ->
    Virtual_address.t ->
    Lreader.t ->
    int ->
    [ `All ] Fiber.t * Instruction.t option

  val script :
    task:unit Basic_types.Int.Htbl.t ->
    Virtual_address.t ->
    ?fallthrough:bool ->
    Script.Instr.t list ->
    Script.env ->
    [ `All ] Fiber.t

  val create :
    ?volatile:bool ->
    ?hooks:
      (string * Script.Instr.t list) list Virtual_address.Map.t * Script.env ->
    task:unit Basic_types.Int.Htbl.t ->
    Virtual_address.t ->
    Lreader.t ->
    int ->
    t

  val get : t -> Virtual_address.t -> [ `All ] Fiber.t

  module type CALLBACK = sig
    val instruction_callback :
      (Ast.Instr.t -> Script.env -> Ir.fallthrough list) option

    val process_callback :
      ((module Ir.GRAPH with type t = 'a) -> 'a -> unit) option

    val builtin_callback :
      (Ir.builtin ->
      (Virtual_address.t ->
      Path.t ->
      int ->
      State.t ->
      (State.t, Types.status) Result.t)
      option)
      option
  end

  val register_callback : (module CALLBACK) -> unit

  val register_opcode_hook :
    (Lreader.t -> (Script.Instr.t list * Script.env) option) -> unit
end = struct
  module type CALLBACK = sig
    val instruction_callback :
      (Ast.Instr.t -> Script.env -> Ir.fallthrough list) option

    val process_callback :
      ((module Ir.GRAPH with type t = 'a) -> 'a -> unit) option

    val builtin_callback :
      (Ir.builtin ->
      (Virtual_address.t ->
      Path.t ->
      int ->
      State.t ->
      (State.t, Types.status) Result.t)
      option)
      option
  end

  module Fiber = struct
    type 'a t =
      | Debug : { msg : string; mutable succ : [ `All ] t } -> [< `All ] t
      | Print : { output : Output.t; mutable succ : [ `All ] t } -> [< `All ] t
      | Step : {
          addr : Virtual_address.t;
          n : int;
          mutable succ : [ `All ] t;
        }
          -> [< `All ] t
      | Assign : {
          var : Var.t;
          rval : Expr.t;
          mutable succ : [ `All ] t;
        }
          -> [< `All ] t
      | Clobber : { var : Var.t; mutable succ : [ `All ] t } -> [< `All ] t
      | Load : {
          var : Var.t;
          base : A.t;
          dir : Machine.endianness;
          addr : Expr.t;
          mutable succ : [ `All ] t;
        }
          -> [< `All ] t
      | Store : {
          base : A.t;
          dir : Machine.endianness;
          addr : Expr.t;
          rval : Expr.t;
          mutable succ : [ `All ] t;
        }
          -> [< `All ] t
      | Symbolize : { var : Var.t; mutable succ : [ `All ] t } -> [< `All ] t
      | Assume : {
          test : Expr.t;
          mutable succ : [ `All ] t;
        }
          -> [< `Assume | `All ] t
      | Assert : {
          test : Expr.t;
          mutable succ : [ `All ] t;
        }
          -> [< `Assert | `All ] t
      | Branch : {
          test : Expr.t;
          mutable taken : [ `All ] t;
          mutable fallthrough : [ `All ] t;
        }
          -> [< `Branch | `All ] t
      | Goto : Virtual_address.t -> [< `All ] t
      | Jump : Expr.t -> [< `Jump | `All ] t
      | Halt : [< `All ] t
      | Probe : {
          kind : Probe.t;
          mutable succ : [ `All ] t;
        }
          -> [< `Probe | `All ] t
      | Builtin : {
          f :
            Virtual_address.t ->
            Path.t ->
            int ->
            State.t ->
            (State.t, Types.status) Result.t;
          mutable succ : [ `All ] t;
        }
          -> [ `All ] t
      | Cut : [< `All ] t
      | Die : string -> [< `All ] t

    let relink ?(taken = false) ~(pred : [ `All ] t) (succ : [ `All ] t) =
      match pred with
      | Debug t -> t.succ <- succ
      | Print t -> t.succ <- succ
      | Step t -> t.succ <- succ
      | Assign t -> t.succ <- succ
      | Clobber t -> t.succ <- succ
      | Load t -> t.succ <- succ
      | Store t -> t.succ <- succ
      | Symbolize t -> t.succ <- succ
      | Assume t -> t.succ <- succ
      | Assert t -> t.succ <- succ
      | Branch t when taken -> t.taken <- succ
      | Branch t -> t.fallthrough <- succ
      | Probe t -> t.succ <- succ
      | Builtin t -> t.succ <- succ
      | Goto _ | Jump _ | Cut | Halt | Die _ -> ()
  end

  module Var = struct
    module Tag = Dba.Var.Tag
    include Dba_types.Var

    let rec collect (e : Dba.Expr.t) (d : Set.t) : Set.t =
      match e with
      | Cst _ -> d
      | Var v -> Set.add v d
      | Load (_, _, e, _) | Unary (_, e) -> collect e d
      | Binary (_, e, e') -> collect e (collect e' d)
      | Ite (e, e', e'') -> collect e (collect e' (collect e'' d))

    let rec appears_in v (e : Dba.Expr.t) =
      match e with
      | Cst _ -> false
      | Var v' -> equal v v'
      | Load (_, _, e, _) | Unary (_, e) -> appears_in v e
      | Binary (_, e, e') -> appears_in v e || appears_in v e'
      | Ite (e, e', e'') ->
          appears_in v e || appears_in v e' || appears_in v e''
  end

  type t = {
    mutable n : int;
    nodes : node I.Htbl.t;
    preds : int list I.Htbl.t;
    entries : int Virtual_address.Htbl.t;
    mutable exits : I.Set.t;
    base : Virtual_address.t;
    reader : Lreader.t;
    size : int;
    volatile : bool;
    mutable last : Instruction.t option;
    mutable sinks : I.Set.t;
    killset : Var.Set.t I.Htbl.t;
    task : unit I.Htbl.t;
    fibers : [ `All ] Fiber.t I.Htbl.t;
  }

  let is_deadstore t var vertex =
    try Var.Set.mem var (I.Htbl.find t.killset vertex) with Not_found -> false

  let entropy = Printf.sprintf "%%entropy%%%d"

  let push todo addr vertex (tag : Dba.tag) =
    todo :=
      Virtual_address.Map.add addr
        ((vertex, tag)
        :: (try Virtual_address.Map.find addr !todo with Not_found -> []))
        !todo

  let add_node t vertex node =
    if vertex < t.n then raise (Invalid_argument "persistent vertex");
    (match I.Htbl.find t.nodes vertex with
    | exception Not_found -> ()
    | Fallthrough { succ; _ } | Goto { succ = Some succ; _ } ->
        I.Htbl.replace t.preds succ
          (List.filter (( != ) vertex) (I.Htbl.find t.preds succ));
        if succ < t.n then t.sinks <- I.Set.remove vertex t.sinks
    | Branch { target; fallthrough; _ } ->
        I.Htbl.replace t.preds target
          (List.filter (( != ) vertex) (I.Htbl.find t.preds target));
        I.Htbl.replace t.preds fallthrough
          (List.filter (( != ) vertex) (I.Htbl.find t.preds fallthrough));
        if target < t.n || fallthrough < t.n then
          t.sinks <- I.Set.remove vertex t.sinks
    | Goto { succ = None; _ } | Terminator _ ->
        t.exits <- I.Set.remove vertex t.exits;
        t.sinks <- I.Set.remove vertex t.sinks);
    I.Htbl.replace t.nodes vertex node;
    if not (I.Htbl.mem t.preds vertex) then I.Htbl.add t.preds vertex [];
    match node with
    | Fallthrough { succ; _ } | Goto { succ = Some succ; _ } ->
        I.Htbl.replace t.preds succ
          (vertex :: (try I.Htbl.find t.preds succ with Not_found -> []));
        if succ < t.n then t.sinks <- I.Set.add vertex t.sinks
    | Branch { target; fallthrough; _ } ->
        I.Htbl.replace t.preds target
          (vertex :: (try I.Htbl.find t.preds target with Not_found -> []));
        I.Htbl.replace t.preds fallthrough
          (vertex
          :: (try I.Htbl.find t.preds fallthrough with Not_found -> []));
        if target < t.n || fallthrough < t.n then
          t.sinks <- I.Set.add vertex t.sinks
    | Goto { succ = None; _ } | Terminator _ ->
        t.exits <- I.Set.add vertex t.exits;
        t.sinks <- I.Set.add vertex t.sinks

  let make_goto t todo vertex target tag =
    try
      let succ = Virtual_address.Htbl.find t.entries target in
      add_node t vertex (Goto { target; tag; succ = Some succ })
    with Not_found ->
      add_node t vertex (Goto { target; tag; succ = None });
      if not t.volatile then push todo target vertex tag

  module G : GRAPH with type t = t = struct
    type nonrec t = t

    let node { nodes; _ } vertex = I.Htbl.find nodes vertex

    module V : Graph.Sig.VERTEX with type t = int = struct
      type t = int

      let compare = ( - )
      let equal = ( == )
      let hash = Fun.id

      type label = t

      let create = Fun.id
      let label = Fun.id
    end

    type vertex = V.t

    module E :
      Graph.Sig.EDGE with type t = V.t * bool * V.t and type vertex = V.t =
    struct
      type t = V.t * bool * V.t

      let compare = compare

      type vertex = V.t

      let src (vertex, _, _) = vertex
      let dst (_, _, vertex) = vertex

      type label = bool

      let create src branch dst = (src, branch, dst)
      let label (_, branch, _) = branch
    end

    type edge = E.t

    let is_directed = true
    let is_empty { nodes; _ } = I.Htbl.length nodes = 0
    let nb_vertex { nodes; _ } = I.Htbl.length nodes

    let nb_edges { preds; _ } =
      I.Htbl.fold (fun _ preds n -> n + List.length preds) preds 0

    let is_new_vertex { n; _ } vertex = vertex >= n

    let out_degree { nodes; _ } vertex =
      match I.Htbl.find nodes vertex with
      | Terminator _ -> 0
      | Goto { succ = None; _ } -> 0
      | Goto { succ = Some _; _ } | Fallthrough _ -> 1
      | Branch _ -> 2

    let in_degree { preds; _ } vertex = List.length (I.Htbl.find preds vertex)
    let mem_vertex { nodes; _ } vertex = I.Htbl.mem nodes vertex

    let mem_edge { nodes; _ } src dst =
      match I.Htbl.find nodes src with
      | Fallthrough { succ; _ } | Goto { succ = Some succ; _ } -> succ = dst
      | Branch { target; fallthrough; _ } -> target = dst || fallthrough = dst
      | Goto { succ = None; _ } | Terminator _ -> false

    let mem_edge_e { nodes; _ } (src, branch, dst) =
      match I.Htbl.find nodes src with
      | Fallthrough { succ; _ } | Goto { succ = Some succ; _ } -> succ = dst
      | Branch { target; fallthrough; _ } ->
          (branch && target = dst) || ((not branch) && fallthrough = dst)
      | Goto { succ = None; _ } | Terminator _ -> false

    let find_edge { nodes; _ } src dst =
      match I.Htbl.find nodes src with
      | Fallthrough { succ; _ }
      | Goto { succ = Some succ; _ }
      | Branch { fallthrough = succ; _ }
        when succ = dst ->
          (src, false, dst)
      | Branch { target; _ } when target = dst -> (src, true, dst)
      | Fallthrough _ | Goto _ | Branch _ | Terminator _ -> raise Not_found

    let find_all_edges t src dst =
      try [ find_edge t src dst ] with Not_found -> []

    let succ { nodes; _ } vertex =
      match I.Htbl.find nodes vertex with
      | Fallthrough { succ; _ } | Goto { succ = Some succ; _ } -> [ succ ]
      | Branch { target; fallthrough; _ } -> [ target; fallthrough ]
      | Goto { succ = None; _ } | Terminator _ -> []

    let pred { preds; _ } vertex = I.Htbl.find preds vertex

    let succ_e { nodes; _ } vertex =
      match I.Htbl.find nodes vertex with
      | Fallthrough { succ; _ } | Goto { succ = Some succ; _ } ->
          [ (vertex, false, succ) ]
      | Branch { target; fallthrough; _ } ->
          [ (vertex, true, target); (vertex, false, fallthrough) ]
      | Goto { succ = None; _ } | Terminator _ -> []

    let pred_e t vertex =
      List.map (fun src -> find_edge t src vertex) (pred t vertex)

    let iter_vertex f { nodes; _ } =
      let last = I.Htbl.length nodes - 1 in
      for i = 0 to last do
        f i
      done

    let iter_new_vertex f { n; nodes; _ } =
      let last = I.Htbl.length nodes - 1 in
      for i = n to last do
        f i
      done

    let iter_entries f { entries; _ } =
      Virtual_address.Htbl.iter (fun _ i -> f i) entries

    let iter_exits f { exits; _ } = I.Set.iter f exits

    let fold_vertex f { nodes; _ } data =
      I.Htbl.fold (fun vertex _ -> f vertex) nodes data

    let iter_edges f { nodes; _ } =
      I.Htbl.iter
        (fun vertex node ->
          match node with
          | Fallthrough { succ; _ } | Goto { succ = Some succ; _ } ->
              f vertex succ
          | Branch { target; fallthrough; _ } ->
              f vertex target;
              f vertex fallthrough
          | Goto { succ = None; _ } | Terminator _ -> ())
        nodes

    let fold_edges f { nodes; _ } data =
      I.Htbl.fold
        (fun vertex node data ->
          match node with
          | Fallthrough { succ; _ } | Goto { succ = Some succ; _ } ->
              f vertex succ data
          | Branch { target; fallthrough; _ } ->
              f vertex target (f vertex fallthrough data)
          | Goto { succ = None; _ } | Terminator _ -> data)
        nodes data

    let iter_edges_e f { nodes; _ } =
      I.Htbl.iter
        (fun vertex node ->
          match node with
          | Fallthrough { succ; _ } | Goto { succ = Some succ; _ } ->
              f (vertex, false, succ)
          | Branch { target; fallthrough; _ } ->
              f (vertex, true, target);
              f (vertex, false, fallthrough)
          | Goto { succ = None; _ } | Terminator _ -> ())
        nodes

    let fold_edges_e f { nodes; _ } data =
      I.Htbl.fold
        (fun vertex node data ->
          match node with
          | Fallthrough { succ; _ } | Goto { succ = Some succ; _ } ->
              f (vertex, false, succ) data
          | Branch { target; fallthrough; _ } ->
              f (vertex, true, target) (f (vertex, false, fallthrough) data)
          | Goto { succ = None; _ } | Terminator _ -> data)
        nodes data

    let map_vertex f r =
      let t =
        {
          r with
          nodes = I.Htbl.create (I.Htbl.length r.nodes);
          preds = I.Htbl.create (I.Htbl.length r.preds);
          exits = I.Set.map f r.exits;
          sinks = I.Set.map f r.sinks;
          entries =
            Virtual_address.Htbl.create (Virtual_address.Htbl.length r.entries);
          killset = I.Htbl.create (I.Htbl.length r.killset);
        }
      in
      I.Htbl.iter
        (fun vertex node -> I.Htbl.add t.nodes (f vertex) (shuffle f node))
        r.nodes;
      I.Htbl.iter
        (fun vertex preds -> I.Htbl.add t.preds (f vertex) (List.map f preds))
        r.preds;
      Virtual_address.Htbl.iter
        (fun addr vertex -> Virtual_address.Htbl.add t.entries addr (f vertex))
        r.entries;
      I.Htbl.iter
        (fun vertex set -> I.Htbl.add t.killset (f vertex) set)
        r.killset;
      t

    let iter_succ f { nodes; _ } vertex =
      match I.Htbl.find nodes vertex with
      | Fallthrough { succ; _ } | Goto { succ = Some succ; _ } -> f succ
      | Branch { target; fallthrough; _ } ->
          f target;
          f fallthrough
      | Goto { succ = None; _ } | Terminator _ -> ()

    let iter_pred f { preds; _ } vertex = List.iter f (I.Htbl.find preds vertex)

    let fold_succ f { nodes; _ } vertex data =
      match I.Htbl.find nodes vertex with
      | Fallthrough { succ; _ } | Goto { succ = Some succ; _ } -> f succ data
      | Branch { target; fallthrough; _ } -> f target (f fallthrough data)
      | Goto { succ = None; _ } | Terminator _ -> data

    let fold_pred f { preds; _ } vertex data =
      List.fold_left (Fun.flip f) data (I.Htbl.find preds vertex)

    let iter_succ_e f { nodes; _ } vertex =
      match I.Htbl.find nodes vertex with
      | Fallthrough { succ; _ } | Goto { succ = Some succ; _ } ->
          f (vertex, false, succ)
      | Branch { target; fallthrough; _ } ->
          f (vertex, true, target);
          f (vertex, false, fallthrough)
      | Goto { succ = None; _ } | Terminator _ -> ()

    let iter_pred_e f t vertex =
      List.iter
        (fun src -> f (find_edge t src vertex))
        (I.Htbl.find t.preds vertex)

    let fold_succ_e f { nodes; _ } vertex data =
      match I.Htbl.find nodes vertex with
      | Fallthrough { succ; _ } | Goto { succ = Some succ; _ } ->
          f (vertex, false, succ) data
      | Branch { target; fallthrough; _ } ->
          f (vertex, true, target) (f (vertex, false, fallthrough) data)
      | Goto { succ = None; _ } | Terminator _ -> data

    let fold_pred_e f t vertex data =
      List.fold_left
        (fun data src -> f (find_edge t src vertex) data)
        data
        (I.Htbl.find t.preds vertex)

    let insert_before t vertex kind =
      let cur = I.Htbl.length t.nodes in
      iter_pred
        (fun pred ->
          add_node t pred
            (shuffle (fun i -> if i = vertex then cur else i) (node t pred)))
        t vertex;
      add_node t cur (Fallthrough { kind; succ = vertex });
      cur

    let insert_list_before t vertex = function
      | [] -> vertex
      | kind :: tl ->
          let vertex' = insert_before t vertex kind in
          List.iter (fun kind -> ignore (insert_before t vertex kind)) tl;
          vertex'
  end

  let instruction_callback = ref []
  let process_callback = Queue.create ()
  let builtin_callback = ref []

  let register_callback callback =
    let module C = (val callback : CALLBACK) in
    Option.iter
      (fun callback ->
        instruction_callback := callback :: !instruction_callback)
      C.instruction_callback;
    Option.iter
      (fun callback ->
        Queue.push
          (callback (module G : Ir.GRAPH with type t = G.t))
          process_callback)
      C.process_callback;
    Option.iter
      (fun callback -> builtin_callback := callback :: !builtin_callback)
      C.builtin_callback

  let rec resolve_instruction inst env callbacks =
    match callbacks with
    | [] -> raise (Invalid_argument "missing instruction callback")
    | convert :: callbacks -> (
        match convert inst env with
        | [] -> resolve_instruction inst env callbacks
        | l -> l)

  let rec resolve_builtin p callbacks =
    match callbacks with
    | [] -> raise (Invalid_argument "missing builtin callback")
    | exec :: callbacks -> (
        match exec p with None -> resolve_builtin p callbacks | Some f -> f)

  let opcode_hook = ref []
  let register_opcode_hook hook = opcode_hook := hook :: !opcode_hook

  let analyze_fallthrough kind killset =
    match kind with
    | Nop | Debug _ | Instruction _ | Hook _ -> killset
    | Clobber var | Forget var | Symbolize var -> Var.Set.add var killset
    | Assign { var; rval } ->
        if Var.Set.mem var killset then killset
        else
          Var.Set.diff (Var.Set.add var killset)
            (Var.collect rval Var.Set.empty)
    | Load { var; addr; _ } ->
        Var.Set.diff (Var.Set.add var killset) (Var.collect addr Var.Set.empty)
    | Store { addr; rval; _ } ->
        Var.Set.diff
          (Var.Set.diff killset (Var.collect addr Var.Set.empty))
          (Var.collect rval Var.Set.empty)
    | Assume expr
    | Assert expr
    | Print (Value (_, expr))
    | Enumerate { enum = expr; _ } ->
        Var.Set.diff killset (Var.collect expr Var.Set.empty)
    | Print _ (* TODO: refine? *) | Reach _ | Builtin _ -> Var.Set.empty

  let analyze_branch test target fallthrough =
    match (target, fallthrough) with
    | None, None -> assert false
    | None, Some killset | Some killset, None ->
        Var.Set.diff killset (Var.collect test Var.Set.empty)
    | Some target, Some fallthrough ->
        let killset = Var.Set.inter target fallthrough in
        Var.Set.diff killset (Var.collect test Var.Set.empty)

  let rec closure t todo push =
    if not (Queue.is_empty todo) then
      let vertex = Queue.pop todo in
      let killset =
        match G.node t vertex with
        | Fallthrough { kind; succ } ->
            analyze_fallthrough kind (I.Htbl.find t.killset succ)
        | Branch { test; target; fallthrough } ->
            analyze_branch test
              (I.Htbl.find_opt t.killset target)
              (I.Htbl.find_opt t.killset fallthrough)
        | Goto { succ = Some succ; _ } -> I.Htbl.find t.killset succ
        | Goto { succ = None; _ } | Terminator _ -> assert false
      in
      match I.Htbl.find t.killset vertex with
      | old when Var.Set.equal old killset -> closure t todo push
      | (exception Not_found) | _ ->
          I.Htbl.replace t.killset vertex killset;
          G.iter_pred push t vertex;
          closure t todo push

  let analyze t =
    let todo = Queue.create () in
    let push = Fun.flip Queue.push todo in
    I.Set.iter
      (fun vertex ->
        G.iter_pred push t vertex;
        match G.node t vertex with
        | Fallthrough { kind; succ } ->
            I.Htbl.add t.killset vertex
              (analyze_fallthrough kind (I.Htbl.find t.killset succ))
        | Branch { test; target; fallthrough } ->
            I.Htbl.add t.killset vertex
              (analyze_branch test
                 (I.Htbl.find_opt t.killset target)
                 (I.Htbl.find_opt t.killset fallthrough))
        | Goto { succ = Some succ; _ } ->
            I.Htbl.add t.killset vertex (I.Htbl.find t.killset succ)
        | Goto { succ = None; _ } | Terminator _ ->
            I.Htbl.add t.killset vertex Var.Set.empty)
      t.sinks;
    closure t todo push;
    t.sinks <- I.Set.empty

  let _export_to_file t =
    let filename = Filename.temp_file "dba" ".dot" in
    let oc = open_out_bin filename in
    let module C_dot = struct
      include G

      let graph_attributes _ = []
      let default_vertex_attributes _ = [ `Shape `Box ]
      let vertex_name v = Format.asprintf "\"%d: %a\"" v pp_node (node t v)

      let vertex_attributes v =
        match node t v with
        | Fallthrough { kind = Forget _; _ } -> [ `Color 0xff0000 ]
        | Fallthrough
            {
              kind =
                ( Assign { var; _ }
                | Clobber var
                | Load { var; _ }
                | Symbolize var );
              succ;
            }
          when is_deadstore t var succ ->
            [ `Color 0xff0000 ]
        | _ -> []

      let get_subgraph _ = None
      let default_edge_attributes _ = []
      let edge_attributes _ = []
    end in
    let module D = Graph.Graphviz.Dot (C_dot) in
    D.output_graph oc t;
    close_out oc;
    filename

  (* let f = _export_to_file t in *)
  (* ignore (Sys.command (Format.sprintf "xdot %s" f)); *)
  (* ignore (Sys.command (Format.sprintf "rm %s" f)); *)

  let add_dhunk t todo vertex hunk =
    let next = ref (vertex + Dhunk.length hunk) in
    let temps = ref Var.Set.empty and exits = ref I.Set.empty in
    Dhunk.iteri
      ~f:(fun i inst ->
        let cur = vertex + i in
        match inst with
        | Assign (Var var, rval, succ) ->
            if var.info = Var.Tag.Temp then temps := Var.Set.add var !temps;
            add_node t cur
              (Fallthrough { kind = Assign { var; rval }; succ = vertex + succ })
        | Assign (Restrict (var, { hi; lo }), rval, succ) ->
            if var.info = Var.Tag.Temp then temps := Var.Set.add var !temps;
            add_node t cur
              (Fallthrough
                 {
                   kind =
                     Assign
                       {
                         var;
                         rval = Dba_utils.Expr.complement rval ~hi ~lo var;
                       };
                   succ = vertex + succ;
                 })
        | Assign (Store (_, dir, addr, base), rval, succ) ->
            add_node t cur
              (Fallthrough
                 {
                   kind = Store { base; dir; addr; rval };
                   succ = vertex + succ;
                 })
        | Undef (Var var, succ) ->
            if var.info = Var.Tag.Temp then temps := Var.Set.add var !temps;
            add_node t cur
              (Fallthrough { kind = Clobber var; succ = vertex + succ })
        | Undef _ ->
            raise
              (Invalid_argument
                 (Format.asprintf "unexpected instruction kind %a"
                    Dba_printer.Ascii.pp_instruction inst))
        | Nondet (Var var, succ) ->
            if var.info = Var.Tag.Temp then temps := Var.Set.add var !temps;
            add_node t cur
              (Fallthrough { kind = Symbolize var; succ = vertex + succ })
        | Nondet (Restrict (var, { hi; lo }), succ) ->
            if var.info = Var.Tag.Temp then temps := Var.Set.add var !temps;
            let size' = hi - lo + 1 in
            let name' = entropy size' in
            let var' = Dba.Var.temporary name' (Size.Bit.create size') in
            temps := Var.Set.add var' !temps;
            let rval = Dba_utils.Expr.complement (Expr.v var') ~lo ~hi var in
            let succ' = !next in
            incr next;
            add_node t cur (Fallthrough { kind = Symbolize var'; succ = succ' });
            add_node t succ'
              (Fallthrough { kind = Assign { var; rval }; succ = vertex + succ })
        | Nondet (Store (bytes, dir, addr, base), succ) ->
            let size' = 8 * bytes in
            let name' = entropy size' in
            let var' = Dba.Var.temporary name' (Size.Bit.create size') in
            let rval = Expr.v var' in
            let succ' = !next in
            incr next;
            add_node t cur (Fallthrough { kind = Symbolize var'; succ = succ' });
            add_node t succ'
              (Fallthrough
                 {
                   kind = Store { base; dir; addr; rval };
                   succ = vertex + succ;
                 })
        | Assume (test, succ) ->
            add_node t cur
              (Fallthrough { kind = Assume test; succ = vertex + succ })
        | Assert (test, succ) ->
            add_node t cur
              (Fallthrough { kind = Assert test; succ = vertex + succ })
        | If (test, JInner target, fallthrough) ->
            add_node t cur
              (Branch
                 {
                   test;
                   target = vertex + target;
                   fallthrough = vertex + fallthrough;
                 })
        | If (test, JOuter { base; _ }, fallthrough) ->
            let succ = !next in
            incr next;
            exits := I.Set.add succ !exits;
            make_goto t todo succ base Default;
            add_node t cur
              (Branch
                 { test; target = succ; fallthrough = vertex + fallthrough })
        | DJump (target, tag) ->
            exits := I.Set.add cur !exits;
            add_node t cur (Terminator (Jump { target; tag }))
        | SJump (JOuter { base; _ }, tag) ->
            exits := I.Set.add cur !exits;
            make_goto t todo cur base tag
        | SJump (JInner succ, _) ->
            add_node t cur (Fallthrough { kind = Nop; succ = vertex + succ })
        | Stop (None | Some OK) ->
            exits := I.Set.add cur !exits;
            add_node t cur (Terminator Halt)
        | Stop (Some KO) ->
            exits := I.Set.add cur !exits;
            add_node t cur (Terminator (Die "KO"))
        | Stop (Some (Undecoded msg | Unsupported msg)) ->
            exits := I.Set.add cur !exits;
            add_node t cur (Terminator (Die msg)))
      hunk;
    I.Set.iter
      (fun vertex ->
        let temps =
          match G.node t vertex with
          | Terminator (Jump { target; _ }) ->
              Var.Set.diff !temps (Var.collect target Var.Set.empty)
          | _ -> !temps
        in
        Var.Set.iter
          (fun var -> ignore (G.insert_before t vertex (Forget var)))
          temps)
      !exits

  let mk_label =
    let n = ref Suid.zero in
    fun _ ->
      n := Suid.incr !n;
      Suid.to_string !n

  let inline_dhunk t todo labels tolink temps exits vertex hunk =
    let anchors = Array.init (Dhunk.length hunk) mk_label in
    let mk_local_goto anchors vertex tolink i succ =
      if succ <> i + 1 then (
        S.Htbl.replace tolink (Array.get anchors succ)
          ((None, !vertex)
          ::
          (try S.Htbl.find tolink (Array.get anchors succ)
           with Not_found -> []));
        incr vertex)
    in
    Dhunk.iteri
      ~f:(fun i inst ->
        S.Htbl.add labels (Array.get anchors i) !vertex;
        match inst with
        | Assign (Var var, rval, succ) ->
            if var.info = Temp then temps := Var.Set.add var !temps;
            add_node t !vertex
              (Fallthrough { kind = Assign { var; rval }; succ = !vertex + 1 });
            incr vertex;
            mk_local_goto anchors vertex tolink i succ
        | Assign (Restrict (var, { hi; lo }), rval, succ) ->
            if var.info = Temp then temps := Var.Set.add var !temps;
            add_node t !vertex
              (Fallthrough
                 {
                   kind =
                     Assign
                       {
                         var;
                         rval = Dba_utils.Expr.complement rval ~hi ~lo var;
                       };
                   succ = !vertex + 1;
                 });
            incr vertex;
            mk_local_goto anchors vertex tolink i succ
        | Assign (Store (_, dir, addr, base), rval, succ) ->
            add_node t !vertex
              (Fallthrough
                 { kind = Store { base; dir; addr; rval }; succ = !vertex + 1 });
            incr vertex;
            mk_local_goto anchors vertex tolink i succ
        | Undef (Var var, succ) ->
            if var.info = Temp then temps := Var.Set.add var !temps;
            add_node t !vertex
              (Fallthrough { kind = Clobber var; succ = !vertex + 1 });
            incr vertex;
            mk_local_goto anchors vertex tolink i succ
        | Undef _ ->
            raise
              (Invalid_argument
                 (Format.asprintf "unexpected instruction kind %a"
                    Dba_printer.Ascii.pp_instruction inst))
        | Nondet (Var var, succ) ->
            if var.info = Temp then temps := Var.Set.add var !temps;
            add_node t !vertex
              (Fallthrough { kind = Symbolize var; succ = !vertex + 1 });
            incr vertex;
            mk_local_goto anchors vertex tolink i succ
        | Nondet (Restrict (var, { hi; lo }), succ) ->
            if var.info = Temp then temps := Var.Set.add var !temps;
            let size' = hi - lo + 1 in
            let name' = entropy size' in
            let var' = Dba.Var.temporary name' (Size.Bit.create size') in
            temps := Var.Set.add var' !temps;
            let rval = Dba_utils.Expr.complement (Expr.v var') ~lo ~hi var in
            add_node t !vertex
              (Fallthrough { kind = Symbolize var'; succ = !vertex + 1 });
            incr vertex;
            add_node t !vertex
              (Fallthrough { kind = Assign { var; rval }; succ = !vertex + 1 });
            incr vertex;
            mk_local_goto anchors vertex tolink i succ
        | Nondet (Store (bytes, dir, addr, base), succ) ->
            let size' = 8 * bytes in
            let name' = entropy size' in
            let var' = Dba.Var.temporary name' (Size.Bit.create size') in
            temps := Var.Set.add var' !temps;
            let rval = Expr.v var' in
            add_node t !vertex
              (Fallthrough { kind = Symbolize var'; succ = !vertex + 1 });
            incr vertex;
            add_node t !vertex
              (Fallthrough
                 { kind = Store { base; dir; addr; rval }; succ = !vertex + 1 });
            incr vertex;
            mk_local_goto anchors vertex tolink i succ
        | Assume (test, succ) ->
            add_node t !vertex
              (Fallthrough { kind = Assume test; succ = !vertex + 1 });
            incr vertex;
            mk_local_goto anchors vertex tolink i succ
        | Assert (test, succ) ->
            add_node t !vertex
              (Fallthrough { kind = Assert test; succ = !vertex + 1 });
            incr vertex;
            mk_local_goto anchors vertex tolink i succ
        | If (test, JInner target, fallthrough) ->
            S.Htbl.replace tolink (Array.get anchors target)
              ((Some test, !vertex)
              ::
              (try S.Htbl.find tolink (Array.get anchors target)
               with Not_found -> []));
            incr vertex;
            mk_local_goto anchors vertex tolink i fallthrough
        | If (test, JOuter { base; _ }, fallthrough) ->
            S.Htbl.replace tolink
              (Array.get anchors fallthrough)
              ((Some (Dba.Expr.lognot test), !vertex)
              ::
              (try S.Htbl.find tolink (Array.get anchors fallthrough)
               with Not_found -> []));
            incr vertex;
            exits := I.Set.add !vertex !exits;
            make_goto t todo !vertex base Default
        | DJump (target, tag) ->
            exits := I.Set.add !vertex !exits;
            add_node t !vertex (Terminator (Jump { target; tag }));
            incr vertex
        | SJump (JOuter { base; _ }, tag) ->
            exits := I.Set.add !vertex !exits;
            make_goto t todo !vertex base tag;
            incr vertex
        | SJump (JInner succ, _) ->
            S.Htbl.replace tolink (Array.get anchors succ)
              ((None, !vertex)
              ::
              (try S.Htbl.find tolink (Array.get anchors succ)
               with Not_found -> []));
            incr vertex
        | Stop (None | Some OK) ->
            exits := I.Set.add !vertex !exits;
            add_node t !vertex (Terminator Halt);
            incr vertex
        | Stop (Some KO) ->
            exits := I.Set.add !vertex !exits;
            add_node t !vertex (Terminator (Die "KO"));
            incr vertex
        | Stop (Some (Undecoded msg | Unsupported msg)) ->
            exits := I.Set.add !vertex !exits;
            add_node t !vertex (Terminator (Die msg));
            incr vertex)
      hunk

  let add_script t todo addr stmts env fallthrough =
    let vertex = ref (I.Htbl.length t.nodes) in
    let labels = S.Htbl.create 16
    and tolink = S.Htbl.create 16
    and temps = ref Var.Set.empty
    and exits = ref I.Set.empty in
    List.iter
      (function
        | Script.Instr.Nop -> ()
        | Script.Instr.Label name -> S.Htbl.add labels name !vertex
        | Script.Instr.Assign (lval, rval) -> (
            let lval = Script.eval_loc lval env in
            let rval =
              Script.eval_expr ~size:(Dba.LValue.size_of lval) rval env
            in
            match lval with
            | Var var ->
                if var.info = Temp then temps := Var.Set.add var !temps;
                add_node t !vertex
                  (Fallthrough
                     { kind = Assign { var; rval }; succ = !vertex + 1 });
                incr vertex
            | Restrict (var, { hi; lo }) ->
                if var.info = Temp then temps := Var.Set.add var !temps;
                add_node t !vertex
                  (Fallthrough
                     {
                       kind =
                         Assign
                           {
                             var;
                             rval = Dba_utils.Expr.complement rval ~hi ~lo var;
                           };
                       succ = !vertex + 1;
                     });
                incr vertex
            | Store (_, dir, addr, base) ->
                add_node t !vertex
                  (Fallthrough
                     {
                       kind = Store { base; dir; addr; rval };
                       succ = !vertex + 1;
                     });
                incr vertex)
        | Script.Instr.Nondet lval -> (
            match Script.eval_loc lval env with
            | Var var ->
                if var.info = Temp then temps := Var.Set.add var !temps;
                add_node t !vertex
                  (Fallthrough { kind = Symbolize var; succ = !vertex + 1 });
                incr vertex
            | Restrict (var, { hi; lo }) ->
                if var.info = Temp then temps := Var.Set.add var !temps;
                let size' = hi - lo + 1 in
                let name' = entropy size' in
                let var' = Dba.Var.temporary name' (Size.Bit.create size') in
                temps := Var.Set.add var' !temps;
                let rval =
                  Dba_utils.Expr.complement (Expr.v var') ~lo ~hi var
                in
                add_node t !vertex
                  (Fallthrough { kind = Symbolize var'; succ = !vertex + 1 });
                incr vertex;
                add_node t !vertex
                  (Fallthrough
                     { kind = Assign { var; rval }; succ = !vertex + 1 });
                incr vertex
            | Store (bytes, dir, addr, base) ->
                let size' = 8 * bytes in
                let name' = entropy size' in
                let var' = Dba.Var.temporary name' (Size.Bit.create size') in
                temps := Var.Set.add var' !temps;
                let rval = Expr.v var' in
                add_node t !vertex
                  (Fallthrough { kind = Symbolize var'; succ = !vertex + 1 });
                incr vertex;
                add_node t !vertex
                  (Fallthrough
                     {
                       kind = Store { base; dir; addr; rval };
                       succ = !vertex + 1;
                     });
                incr vertex)
        | Script.Instr.Undef ((_, p) as lval) -> (
            match Script.eval_loc lval env with
            | Var var ->
                if var.info = Temp then temps := Var.Set.add var !temps;
                add_node t !vertex
                  (Fallthrough { kind = Clobber var; succ = !vertex + 1 });
                incr vertex
            | _ -> raise (Script.Invalid_operation (Script.Expr.loc lval, p)))
        | Script.Instr.Assume test ->
            let test = Script.eval_expr ~size:1 test env in
            add_node t !vertex
              (Fallthrough { kind = Assume test; succ = !vertex + 1 });
            incr vertex
        | Script.Instr.Assert test ->
            let test = Script.eval_expr ~size:1 test env in
            add_node t !vertex
              (Fallthrough { kind = Assert test; succ = !vertex + 1 });
            incr vertex
        | Script.Instr.If (test, target) ->
            let test = Script.eval_expr ~size:1 test env in
            S.Htbl.replace tolink target
              ((Some test, !vertex)
              :: (try S.Htbl.find tolink target with Not_found -> []));
            incr vertex
        | Script.Instr.Goto target ->
            S.Htbl.replace tolink target
              ((None, !vertex)
              :: (try S.Htbl.find tolink target with Not_found -> []));
            incr vertex
        | Script.Instr.Jump target -> (
            exits := I.Set.add !vertex !exits;
            match Script.eval_expr ~size:env.wordsize target env with
            | Cst bv ->
                make_goto t todo !vertex
                  (Virtual_address.of_bitvector bv)
                  Default;
                incr vertex
            | target ->
                add_node t !vertex (Terminator (Jump { target; tag = Default }));
                incr vertex)
        | Script.Instr.Halt ->
            exits := I.Set.add !vertex !exits;
            add_node t !vertex (Terminator Halt);
            incr vertex
        | Script.Cut None ->
            exits := I.Set.add !vertex !exits;
            add_node t !vertex (Terminator Cut);
            incr vertex
        | Script.Cut (Some test) ->
            let test = Script.eval_expr ~size:1 test env in
            add_node t !vertex
              (Branch { test; target = !vertex + 1; fallthrough = !vertex + 2 });
            incr vertex;
            exits := I.Set.add !vertex !exits;
            add_node t !vertex (Terminator Cut);
            incr vertex
        | Script.Reach (n, guard, actions) ->
            let tid = I.Htbl.length t.task in
            I.Htbl.add t.task tid ();
            let guard =
              Option.fold ~none:Dba.Expr.one
                ~some:(fun test -> Script.eval_expr ~size:1 test env)
                guard
            in
            let actions = List.map (Script.Output.eval env) actions in
            add_node t !vertex
              (Fallthrough
                 { kind = Reach { tid; n; guard; actions }; succ = !vertex + 1 });
            incr vertex
        | Script.Print output ->
            add_node t !vertex
              (Fallthrough
                 {
                   kind = Print (Script.Output.eval env output);
                   succ = !vertex + 1;
                 });
            incr vertex
        | Script.Enumerate (n, enum) ->
            let tid = I.Htbl.length t.task in
            I.Htbl.add t.task tid ();
            let enum = Script.eval_expr enum env in
            add_node t !vertex
              (Fallthrough
                 {
                   kind = Enumerate { tid; enum; format = Hex; n };
                   succ = !vertex + 1;
                 });
            incr vertex
        | Script.Argument (lval, n) -> (
            let rval = Isa_helper.get_arg n in
            let lval = Script.eval_loc ~size:(Dba.Expr.size_of rval) lval env in
            match lval with
            | Var var ->
                if var.info = Temp then temps := Var.Set.add var !temps;
                add_node t !vertex
                  (Fallthrough
                     { kind = Assign { var; rval }; succ = !vertex + 1 });
                incr vertex
            | Restrict (var, { hi; lo }) ->
                if var.info = Temp then temps := Var.Set.add var !temps;
                add_node t !vertex
                  (Fallthrough
                     {
                       kind =
                         Assign
                           {
                             var;
                             rval = Dba_utils.Expr.complement rval ~hi ~lo var;
                           };
                       succ = !vertex + 1;
                     });
                incr vertex
            | Store (_, dir, addr, base) ->
                add_node t !vertex
                  (Fallthrough
                     {
                       kind = Store { base; dir; addr; rval };
                       succ = !vertex + 1;
                     });
                incr vertex)
        | Script.Return value ->
            let value =
              Option.map
                (fun value -> Script.eval_expr ~size:env.wordsize value env)
                value
            in
            let hunk = Isa_helper.make_return ?value () in
            inline_dhunk t todo labels tolink temps exits vertex hunk
        | inst ->
            List.iter
              (fun kind ->
                add_node t !vertex (Fallthrough { kind; succ = !vertex + 1 });
                incr vertex)
              (resolve_instruction inst env !instruction_callback))
      stmts;
    S.Htbl.iter
      (fun label preds ->
        let target = S.Htbl.find labels label in
        List.iter
          (fun (test, pred) ->
            match test with
            | None ->
                add_node t pred (Fallthrough { kind = Nop; succ = target })
            | Some test ->
                add_node t pred
                  (Branch { test; target; fallthrough = pred + 1 }))
          preds)
      tolink;
    if stmts = [] || I.Htbl.mem t.preds !vertex then (
      exits := I.Set.add !vertex !exits;
      if fallthrough then (
        add_node t !vertex (Goto { target = addr; tag = Default; succ = None });
        push todo addr !vertex Default)
      else add_node t !vertex (Terminator Halt));
    I.Set.iter
      (fun vertex ->
        Var.Set.iter
          (fun var -> ignore (G.insert_before t vertex (Forget var)))
          !temps)
      !exits

  let add_hook t todo addr anchor stmts env fallthrough =
    let vertex = I.Htbl.length t.nodes in
    let succ = vertex + 1 in
    let info = Format.sprintf "hook at %s" anchor in
    add_node t vertex (Fallthrough { kind = Hook { addr; info }; succ });
    if not (Virtual_address.Htbl.mem t.entries addr) then
      Virtual_address.Htbl.add t.entries addr vertex;
    (try
       List.iter
         (fun (pred, tag) ->
           add_node t pred (Goto { target = addr; tag; succ = Some vertex }))
         (Virtual_address.Map.find addr !todo);
       todo := Virtual_address.Map.remove addr !todo
     with Not_found -> ());
    add_script t todo addr stmts env fallthrough

  let rec resolve_decode t todo addr pos decoders =
    match decoders with
    | [] ->
        let inst, _ = Disasm_core.decode_from t.reader addr in
        t.last <- Some inst;
        let hunk = Instruction.hunk inst in
        let vertex = I.Htbl.length t.nodes in
        add_node t vertex
          (Fallthrough { kind = Instruction inst; succ = vertex + 1 });
        add_dhunk t todo (vertex + 1) hunk;
        vertex
    | decode :: decoders -> (
        match decode t.reader with
        | None ->
            Lreader.rewind t.reader (Lreader.get_pos t.reader - pos);
            resolve_decode t todo addr pos decoders
        | Some (stmts, env) ->
            let pos' = Lreader.get_pos t.reader in
            let vertex = I.Htbl.length t.nodes in
            let succ = vertex + 1 in
            let opcode = Lreader.get_slice t.reader ~lo:pos ~hi:(pos' - 1) in
            let info =
              Format.asprintf "hook for opcode %a" Binstream.pp
                (Binstream.of_bytes (Bytes.unsafe_to_string opcode))
            in
            add_node t vertex (Fallthrough { kind = Hook { addr; info }; succ });
            add_script t todo
              (Virtual_address.add_int (Bytes.length opcode) addr)
              stmts env true;
            vertex)

  let rec disasm t todo =
    if not (Virtual_address.Map.is_empty !todo) then (
      let addr, tolink = Virtual_address.Map.choose !todo in
      todo := Virtual_address.Map.remove addr !todo;
      let pos' = Virtual_address.diff addr t.base in
      if pos' < 0 || pos' >= t.size then disasm t todo
      else
        let pos = Lreader.get_pos t.reader in
        if pos > pos' then Lreader.rewind t.reader (pos - pos')
        else Lreader.advance t.reader (pos' - pos);
        let vertex =
          resolve_decode t todo addr (Lreader.get_pos t.reader) !opcode_hook
        in
        if not (Virtual_address.Htbl.mem t.entries addr) then
          Virtual_address.Htbl.add t.entries addr vertex;
        List.iter
          (fun (pred, tag) ->
            add_node t pred (Goto { target = addr; tag; succ = Some vertex }))
          tolink;
        Stats.register_address addr;
        disasm t todo)

  let extract_loads =
    let rec fold m (e : Expr.t) =
      match e with
      | Cst _ -> (m, e)
      | Var _ -> (m, e)
      | Load (sz, dir, addr, base) ->
          let m', addr' = fold m addr in
          let k = (sz, dir, addr', base) in
          let v =
            try List.assoc k m'
            with Not_found ->
              Dba.Var.(
                create
                  (Printf.sprintf "$$%d" (List.length m'))
                  ~bitsize:(Size.Bit.create (8 * sz))
                  ~tag:Tag.Temp)
          in
          ((k, v) :: m', Expr.v v)
      | Unary (o, x) ->
          let m', x' = fold m x in
          let e' = if x == x' then e else Expr.unary o x' in
          (m', e')
      | Binary (o, x, y) ->
          let m', x' = fold m x in
          let m', y' = fold m' y in
          let e' = if x == x' && y == y' then e else Expr.binary o x' y' in
          (m', e')
      | Ite (c, x, y) ->
          let m', c' = fold m c in
          let m', x' = fold m' x in
          let m', y' = fold m' y in
          let e' =
            if c == c' && x == x' && y == y' then e else Expr.ite c' x' y'
          in
          (m', e')
    in
    fold

  let define_loads kind =
    match kind with
    | Nop | Debug _ | Print _ | Instruction _ | Hook _ | Clobber _ | Forget _
    | Symbolize _ | Enumerate _ | Reach _ | Builtin _ ->
        ([], kind)
    | Assign { var; rval = Load (_, dir, addr, base) } -> (
        match extract_loads [] addr with
        | [], _ -> ([], Load { var; base; dir; addr })
        | loads, addr -> (loads, Load { var; base; dir; addr }))
    | Assign { var; rval } -> (
        match extract_loads [] rval with
        | [], _ -> ([], kind)
        | loads, rval -> (loads, Assign { var; rval }))
    | Load { var; base; dir; addr } -> (
        match extract_loads [] addr with
        | [], _ -> ([], kind)
        | loads, addr -> (loads, Load { var; base; dir; addr }))
    | Store { base; dir; addr; rval } -> (
        let loads, addr = extract_loads [] addr in
        match extract_loads loads rval with
        | [], _ -> ([], kind)
        | loads, rval -> (loads, Store { base; dir; addr; rval }))
    | Assume test -> (
        match extract_loads [] test with
        | [], _ -> ([], kind)
        | loads, test -> (loads, Assume test))
    | Assert test -> (
        match extract_loads [] test with
        | [], _ -> ([], kind)
        | loads, test -> (loads, Assert test))

  let define_load t vertex =
    match G.node t vertex with
    | Fallthrough { kind; succ } -> (
        match define_loads kind with
        | [], kind' when kind == kind' -> ()
        | loads, kind ->
            add_node t vertex (Fallthrough { kind; succ });
            List.fold_right
              (fun ((_, dir, addr, base), var) () ->
                ignore
                  (G.insert_before t vertex (Load { var; base; dir; addr }));
                ignore (G.insert_before t succ (Forget var)))
              loads ())
    | Branch { test; target; fallthrough } -> (
        match extract_loads [] test with
        | [], _ -> ()
        | loads, test ->
            add_node t vertex (Branch { test; target; fallthrough });
            List.fold_right
              (fun ((_, dir, addr, base), var) () ->
                ignore
                  (G.insert_before t vertex (Load { var; base; dir; addr }));
                ignore (G.insert_before t target (Forget var));
                ignore (G.insert_before t fallthrough (Forget var)))
              loads ())
    | Terminator (Jump { target; tag }) -> (
        match extract_loads [] target with
        | [], _ -> ()
        | loads, target ->
            add_node t vertex (Terminator (Jump { target; tag }));
            List.fold_right
              (fun ((_, dir, addr, base), var) () ->
                ignore
                  (G.insert_before t vertex (Load { var; base; dir; addr })))
              loads ())
    | Goto _ | Terminator _ -> ()

  let process t =
    G.iter_new_vertex (define_load t) t;
    Queue.iter (fun f -> f t) process_callback;
    analyze t;
    t.n <- I.Htbl.length t.nodes

  let create ?(volatile = false) ?hooks ~task base reader size =
    let vsize = size / 6 in
    let t =
      {
        n = 0;
        nodes = I.Htbl.create size;
        preds = I.Htbl.create size;
        entries = Virtual_address.Htbl.create vsize;
        exits = I.Set.empty;
        base;
        reader;
        size;
        volatile;
        last = None;
        sinks = I.Set.empty;
        killset = I.Htbl.create size;
        task;
        fibers = I.Htbl.create (size / 15);
      }
    in
    let todo = ref Virtual_address.Map.empty in
    Option.iter
      (fun (hooks, env) ->
        Virtual_address.Map.iter
          (fun addr hooks ->
            List.iter
              (fun (anchor, script) ->
                add_hook t todo addr anchor script env true)
              hooks)
          hooks)
      hooks;
    disasm t todo;
    process t;
    t

  let raw_fiber (node : node) : [ `All ] Fiber.t =
    match node with
    | Fallthrough { kind = Nop; _ } -> Halt
    | Fallthrough { kind = Debug msg; _ } -> Debug { msg; succ = Halt }
    | Fallthrough { kind = Print output; _ } -> Print { output; succ = Halt }
    | Fallthrough { kind = Hook { addr; _ }; _ } ->
        Step { addr; n = 0; succ = Halt }
    | Fallthrough { kind = Instruction inst; _ } ->
        Step { addr = Instruction.address inst; n = 1; succ = Halt }
    | Fallthrough { kind = Assign { var; rval }; _ } ->
        Assign { var; rval; succ = Halt }
    | Fallthrough { kind = Clobber var; _ } -> Clobber { var; succ = Halt }
    | Fallthrough { kind = Forget _; _ } -> Halt
    | Fallthrough { kind = Load { var; base; dir; addr }; _ } ->
        Load { var; base; dir; addr; succ = Halt }
    | Fallthrough { kind = Store { base; dir; addr; rval }; _ } ->
        Store { base; dir; addr; rval; succ = Halt }
    | Fallthrough { kind = Symbolize var; _ } -> Symbolize { var; succ = Halt }
    | Fallthrough { kind = Assume test; _ } -> Assume { test; succ = Halt }
    | Fallthrough { kind = Assert test; _ } -> Assert { test; succ = Halt }
    | Fallthrough { kind = Enumerate { tid; enum; format; n }; _ } ->
        Probe
          {
            kind = Enumerate { id = tid; enum; format; n; k = 0; values = [] };
            succ = Halt;
          }
    | Fallthrough { kind = Reach { tid; n; guard; actions }; _ } ->
        Probe { kind = Reach { id = tid; n; guard; actions }; succ = Halt }
    | Fallthrough { kind = Builtin f; _ } ->
        Builtin { f = resolve_builtin f !builtin_callback; succ = Halt }
    | Goto { succ = Some _; _ } -> Halt
    | Goto { target; succ = None; _ } -> Goto target
    | Branch { test; _ } -> Branch { test; taken = Halt; fallthrough = Halt }
    | Terminator (Jump { target; _ }) -> Jump target
    | Terminator Halt -> Halt
    | Terminator Cut -> Cut
    | Terminator (Die msg) -> Die msg

  let debug_level = Options.Logger.get_debug_level ()

  let decorate_fiber =
    if debug_level >= 40 then fun node ->
      let succ = raw_fiber node in
      (Fiber.Debug { msg = Format.asprintf "%a" pp_node node; succ }, succ)
    else fun node ->
      let fiber = raw_fiber node in
      (fiber, fiber)

  let make_label =
    if debug_level >= 2 then (fun node pred ->
      let debug =
        Fiber.Debug { msg = Format.asprintf "%a" pp_node node; succ = Halt }
      in
      Fiber.relink ~pred debug;
      debug)
    else fun _ pred -> pred

  let rec forward t vertex =
    match G.node t vertex with
    | Fallthrough { kind = Nop | Forget _; succ } | Goto { succ = Some succ; _ }
      ->
        forward t succ
    | Fallthrough
        {
          kind =
            Assign { var; _ } | Clobber var | Load { var; _ } | Symbolize var;
          succ;
        }
      when is_deadstore t var succ ->
        forward t succ
    | _ -> vertex

  let link t todo reloc pred taken vertex =
    let vertex = forward t vertex in
    try Fiber.relink ~taken ~pred (I.Htbl.find t.fibers vertex)
    with Not_found ->
      Queue.push vertex todo;
      Queue.push (pred, taken, vertex) reloc

  let commit_addr addr n pred =
    if n > 0 then (
      let step = Fiber.Step { addr; n; succ = Halt } in
      Fiber.relink ~pred
        (if debug_level >= 39 then
           Fiber.Debug { msg = Format.sprintf "step %d" n; succ = step }
         else step);
      step)
    else pred

  let commit_vars ?var ?deps vars pred =
    let vars, others =
      match deps with
      | None -> (vars, Var.Map.empty)
      | Some set ->
          let f var _ = Var.Set.mem var set in
          let f =
            match var with
            | None -> f
            | Some var' ->
                fun var rval ->
                  Option.fold ~none:false ~some:(Var.appears_in var') rval
                  || f var rval
          in
          Var.Map.partition f vars
    in
    ( others,
      Var.Map.fold
        (fun var value pred ->
          let kind =
            match value with
            | None -> Clobber var
            | Some rval -> Assign { var; rval }
          in
          let head, tail = decorate_fiber (Fallthrough { kind; succ = -1 }) in
          Fiber.relink ~pred head;
          tail)
        vars pred )

  let fallthrough_var = function
    | Assign { var; _ }
    | Load { var; _ }
    | Clobber var
    | Forget var
    | Symbolize var ->
        Some var
    | Nop | Debug _ | Instruction _ | Hook _ | Assume _ | Assert _ | Enumerate _
    | Store _ | Print _ | Reach _ | Builtin _ ->
        None

  let fallthrough_deps = function
    | Nop | Debug _ | Instruction _ | Hook _ | Clobber _ | Forget _
    | Symbolize _ ->
        Some Var.Set.empty
    | Assign { rval = e; _ }
    | Load { addr = e; _ }
    | Assume e
    | Assert e
    | Enumerate { enum = e; _ } ->
        Some (Var.collect e Var.Set.empty)
    | Store { addr; rval; _ } ->
        Some (Var.collect addr (Var.collect rval Var.Set.empty))
    | Print _ | Reach _ | Builtin _ -> None

  let commit addr n vars pred = commit_addr addr n (snd (commit_vars vars pred))

  let rec line t todo reloc addr n vars pred vertex =
    try
      let fiber = I.Htbl.find t.fibers vertex in
      Fiber.relink ~pred:(commit addr n vars pred) fiber
    with Not_found -> (
      match G.pred t vertex with
      | _ :: _ :: _ ->
          Queue.push vertex todo;
          Queue.push (commit addr n vars pred, false, vertex) reloc
      | _ -> baseline t todo reloc addr n vars pred vertex)

  and baseline t todo reloc addr n vars pred vertex =
    let node = G.node t vertex in
    match node with
    | Fallthrough { kind = Nop | Forget _; succ } | Goto { succ = Some succ; _ }
      ->
        line t todo reloc addr n vars pred succ
    | Fallthrough { kind = Instruction inst; succ } ->
        let pred = make_label node pred in
        line t todo reloc (Instruction.address inst) (n + 1) vars pred succ
    | Fallthrough { kind = Hook { addr; _ }; succ } ->
        let pred = make_label node pred in
        let step = Fiber.Step { addr; n; succ = Halt } in
        Fiber.relink ~pred step;
        line t todo reloc addr 0 vars step succ
    | Fallthrough
        {
          kind =
            Assign { var; _ } | Clobber var | Load { var; _ } | Symbolize var;
          succ;
        }
      when is_deadstore t var succ ->
        line t todo reloc addr n vars pred succ
    | Fallthrough { kind = Assign { var; rval = Var var' }; succ }
      when Var.Map.mem var' vars && is_deadstore t var' succ ->
        let value = Var.Map.find var' vars in
        let vars = Var.Map.remove var' vars in
        let vars, pred = commit_vars ~var ~deps:Var.Set.empty vars pred in
        line t todo reloc addr n (Var.Map.add var value vars) pred succ
    | Fallthrough { kind = Assign { var; rval }; succ } ->
        let vars, pred =
          commit_vars ~var ~deps:(Var.collect rval Var.Set.empty) vars pred
        in
        line t todo reloc addr n (Var.Map.add var (Some rval) vars) pred succ
    | Fallthrough { kind = Clobber var; succ } ->
        let vars, pred = commit_vars ~var vars pred in
        line t todo reloc addr n (Var.Map.add var None vars) pred succ
    | Fallthrough
        {
          kind =
            (Print _ | Assume _ | Assert _ | Enumerate _ | Reach _ | Builtin _)
            as fallthrough;
          succ;
        } ->
        let vars, pred =
          commit_vars ?deps:(fallthrough_deps fallthrough) vars pred
        in
        let head, tail = decorate_fiber node in
        Fiber.relink ~pred:(commit_addr addr n pred) head;
        line t todo reloc addr 0 vars tail succ
    | Fallthrough { kind; succ } ->
        let vars, pred =
          commit_vars ?var:(fallthrough_var kind) ?deps:(fallthrough_deps kind)
            vars pred
        in
        let head, tail = decorate_fiber node in
        Fiber.relink ~pred head;
        line t todo reloc addr n vars tail succ
    | Goto { succ = None; _ } | Terminator _ ->
        Fiber.relink ~pred:(commit addr n vars pred) (fst (decorate_fiber node))
    | Branch { target; fallthrough; _ } ->
        let head, tail = decorate_fiber node in
        Fiber.relink ~pred:(commit addr n vars pred) head;
        link t todo reloc tail true target;
        link t todo reloc tail false fallthrough

  let rec closure t todo reloc =
    if Queue.is_empty todo then
      Queue.iter
        (fun (pred, taken, target) ->
          Fiber.relink ~taken ~pred (I.Htbl.find t.fibers (forward t target)))
        reloc
    else
      let vertex = Queue.pop todo in
      let vertex = forward t vertex in
      if I.Htbl.mem t.fibers vertex then closure t todo reloc;
      let placeholder : [ `Assume ] Fiber.t =
        Assume { test = Expr.one; succ = Halt }
      in
      baseline t todo reloc Virtual_address.zero 0 Var.Map.empty
        (let (Assume _ as head) = placeholder in
         head)
        vertex;
      let (Assume { succ; _ }) = placeholder in
      I.Htbl.add t.fibers vertex succ;
      closure t todo reloc

  let rec get t addr =
    match Virtual_address.Htbl.find t.entries addr with
    | exception Not_found ->
        disasm t (ref (Virtual_address.Map.singleton addr []));
        process t;
        get t addr
    | vertex -> (
        let vertex = forward t vertex in
        try I.Htbl.find t.fibers vertex
        with Not_found ->
          let todo = Queue.create () in
          Queue.add vertex todo;
          closure t todo (Queue.create ());
          I.Htbl.find t.fibers vertex)

  let single ?hooks ~task addr reader size =
    let hooks =
      Option.map
        (fun (hooks, env) -> (Virtual_address.Map.singleton addr hooks, env))
        hooks
    in
    let t = create ~volatile:true ?hooks ~task addr reader size in
    let fiber = get t addr in
    (fiber, t.last)

  let script ~task addr ?(fallthrough = false) script env =
    let t =
      {
        n = 0;
        nodes = I.Htbl.create 16;
        preds = I.Htbl.create 16;
        entries = Virtual_address.Htbl.create 1;
        exits = I.Set.empty;
        base = addr;
        reader = Lreader.of_bytes "";
        size = 0;
        volatile = true;
        last = None;
        sinks = I.Set.empty;
        killset = I.Htbl.create 16;
        task;
        fibers = I.Htbl.create 1;
      }
    in
    add_node t 0
      (Fallthrough { kind = Hook { addr; info = "anonymous" }; succ = 1 });
    Virtual_address.Htbl.add t.entries addr 0;
    add_script t (ref Virtual_address.Map.empty) addr script env fallthrough;
    process t;
    get t addr
end
