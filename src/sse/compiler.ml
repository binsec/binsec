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

open Types
module IntTbl = Basic_types.Integers.Int.Htbl
module Var = Dba_types.Var
module Expr = Dba_types.Expr

type trace = No | Assembly | Ir

type 'a primitive = 'a Types.primitive =
  | Unknown
  | Apply of ('a -> unit)
  | Call of ('a -> 'a Types.continuation)

(** Information to be used by optimization. *)
type 'a knowledge =
  | May_read : Var.Set.t option knowledge
  | May_write : Var.Set.t option knowledge

type nonrec 'a fiber = ([ `All ], 'a) fiber

module type ASSEMBLER = sig
  type 'a t

  val empty : (module Types.PATH with type t = 'a) -> 'a t
  val assign : 'a t -> Dba.Var.t -> Dba.Expr.t -> unit
  val clobber : 'a t -> Dba.Var.t -> unit
  val symbolize : 'a t -> Dba.Var.t -> unit
  val forget : 'a t -> Dba.Var.t -> unit

  val load :
    'a t ->
    Dba.Var.t ->
    string option ->
    Machine.endianness ->
    Dba.Expr.t ->
    unit

  val store :
    'a t ->
    string option ->
    Machine.endianness ->
    addr:Dba.Expr.t ->
    Dba.Expr.t ->
    unit

  val assume : 'a t -> Dba.Expr.t -> unit
  val check : 'a t -> Dba.Expr.t -> unit

  val apply :
    'a t ->
    ?input:Dba_types.Var.Set.t ->
    ?output:Dba_types.Var.Set.t ->
    ('a -> unit) ->
    unit

  val commit :
    'a t -> pred:([ `All ], 'a) Types.fiber -> ([ `All ], 'a) Types.fiber
end

type 'a config = {
  debug : trace;
  echo : 'a -> string -> unit;
  step : 'a -> Virtual_address.t -> int -> unit;
  path : (module PATH with type t = 'a);
  assembler : (module ASSEMBLER);
  mutable builtin_callbacks : (Ir.builtin -> 'a primitive) list;
  mutable may_read_callbacks : (Ir.builtin -> Var.Set.t option option) list;
  mutable may_write_callbacks : (Ir.builtin -> Var.Set.t option option) list;
  mutable annotation_printer :
    (Format.formatter -> Virtual_address.t -> unit) option;
}

let invalid_successor : 'a fiber =
  Tail_call (fun _ -> Signal (Error "invalid successor"))

let rec resolve_builtin :
    Ir.builtin -> (Ir.builtin -> 'a primitive) list -> 'a primitive =
 fun builtin callbacks ->
  match callbacks with
  | [] -> Unknown
  | f :: callbacks -> (
      match f builtin with
      | Unknown -> resolve_builtin builtin callbacks
      | prim -> prim)

let rec resolve_knowledge :
    Ir.builtin -> (Ir.builtin -> 'a option option) list -> 'a option =
 fun builtin callbacks ->
  match callbacks with
  | [] -> None
  | f :: callbacks -> (
      match f builtin with
      | None -> resolve_knowledge builtin callbacks
      | Some knowledge -> knowledge)

let fallthrough_to_raw_fiber :
    (Ir.builtin -> 'a primitive) list -> Ir.fallthrough -> 'a fiber =
 fun builtin_callbacks kind ->
  match kind with
  | Nop | Goto _ -> invalid_successor
  | Hook { addr; _ } -> Step { addr; n = 0; succ = invalid_successor }
  | Instruction inst ->
      Step { addr = Instruction.address inst; n = 1; succ = invalid_successor }
  | Assign { var; rval } -> Assign { var; rval; succ = invalid_successor }
  | Clobber var -> Clobber { var; succ = invalid_successor }
  | Forget _ -> invalid_successor
  | Load { var; base; dir; addr } ->
      Load { var; base; dir; addr; succ = invalid_successor }
  | Store { base; dir; addr; rval } ->
      Store { base; dir; addr; rval; succ = invalid_successor }
  | Symbolize var -> Symbolize { var; succ = invalid_successor }
  | Assume test -> Assume { test; succ = invalid_successor }
  | Assert test -> Assert { test; succ = invalid_successor }
  | Builtin f -> (
      match resolve_builtin f builtin_callbacks with
      | Unknown ->
          let msg = Format.asprintf "no handler for %a" Ir.pp_builtin f in
          Tail_call (fun _ -> Signal (Error msg))
      | Apply f -> Apply { f; succ = invalid_successor }
      | Call f -> Call { f; succ = invalid_successor })

let node_to_raw_fiber : (Ir.builtin -> 'a primitive) list -> Ir.node -> 'a fiber
    =
 fun builtin_callbacks node ->
  match node with
  | Fallthrough { kind; _ } -> fallthrough_to_raw_fiber builtin_callbacks kind
  | Branch { test; _ } ->
      Branch
        { test; taken = invalid_successor; fallthrough = invalid_successor }
  | Terminator { kind = Goto { target; _ }; _ } -> Goto target
  | Terminator { kind = Jump { target; _ }; _ } -> Jump target
  | Terminator { kind = Halt; _ } ->
      Call { f = Fun.const (Signal Halt); succ = invalid_successor }
  | Terminator { kind = Cut; _ } ->
      Call { f = Fun.const (Signal Cut); succ = invalid_successor }
  | Terminator { kind = Die msg; _ } ->
      Call { f = Fun.const (Signal (Error msg)); succ = invalid_successor }
  | Terminator { kind = Builtin f; _ } -> (
      match resolve_builtin f builtin_callbacks with
      | Unknown ->
          let msg = Format.asprintf "no handler for %a" Ir.pp_builtin f in
          Tail_call (fun _ -> Signal (Error msg))
      | Apply f -> Apply { f; succ = invalid_successor }
      | Call f -> Tail_call f)

let make_config :
    ?debug:trace ->
    echo:('a -> string -> unit) ->
    step:('a -> Virtual_address.t -> int -> unit) ->
    (module PATH with type t = 'a) ->
    (module ASSEMBLER) ->
    'a config =
 fun ?(debug = No) ~echo ~step path assembler ->
  {
    debug;
    echo;
    step;
    path;
    assembler;
    builtin_callbacks = [];
    may_read_callbacks = [];
    may_write_callbacks = [];
    annotation_printer = None;
  }

let register_builtin_callback :
    'a config -> (Ir.builtin -> 'a primitive) -> unit =
 fun config callback ->
  config.builtin_callbacks <- callback :: config.builtin_callbacks

let register_knowledge :
    type b. 'a config -> b knowledge -> (Ir.builtin -> b option) -> unit =
 fun config info callback ->
  match info with
  | May_read ->
      config.may_read_callbacks <- callback :: config.may_read_callbacks
  | May_write ->
      config.may_write_callbacks <- callback :: config.may_write_callbacks

let set_annotation_printer :
    'a config -> (Format.formatter -> Virtual_address.t -> unit) option -> unit
    =
 fun config annotation_printer ->
  config.annotation_printer <- annotation_printer

let resolve_builtin : 'a config -> Ir.builtin -> 'a primitive =
 fun { builtin_callbacks; _ } builtin ->
  resolve_builtin builtin builtin_callbacks

let relink : ?taken:bool -> pred:'a fiber -> 'a fiber -> unit =
 fun ?(taken = false) ~pred succ ->
  match pred with
  | Step t -> t.succ <- succ
  | Assign t -> t.succ <- succ
  | Clobber t -> t.succ <- succ
  | Load t -> t.succ <- succ
  | Store t -> t.succ <- succ
  | Symbolize t -> t.succ <- succ
  | Apply t -> t.succ <- succ
  | Assume t -> t.succ <- succ
  | Assert t -> t.succ <- succ
  | Branch t when taken -> t.taken <- succ
  | Branch t -> t.fallthrough <- succ
  | Call t -> t.succ <- succ
  | Goto _ | Jump _ | Tail_call _ -> ()

let _succ : 'a fiber -> 'a fiber =
 fun node ->
  match node with
  | Step { succ; _ }
  | Assign { succ; _ }
  | Clobber { succ; _ }
  | Load { succ; _ }
  | Store { succ; _ }
  | Symbolize { succ; _ }
  | Apply { succ; _ }
  | Assume { succ; _ }
  | Assert { succ; _ }
  | Call { succ; _ } ->
      succ
  | Branch _ | Goto _ | Jump _ | Tail_call _ -> raise (Invalid_argument "succ")

let pp_array : Format.formatter -> string option -> unit =
  Format.pp_print_option
    ~none:(fun ppf () -> Format.pp_print_char ppf '@')
    Format.pp_print_string

let pp_endianness : Format.formatter -> Machine.endianness -> unit =
 fun ppf dir ->
  Format.pp_print_char ppf
    (match dir with LittleEndian -> 'L' | BigEndian -> 'B')

let pp_fiber : type a. Format.formatter -> (a, 'b) Types.fiber -> unit =
 fun ppf k ->
  match k with
  | Step _ -> Format.pp_print_string ppf "Step"
  | Assign { var = { name; _ }; rval; _ } ->
      Format.fprintf ppf "Assign (%s := %a)" name Dba_printer.Ascii.pp_expr rval
  | Clobber { var = { name; _ }; _ } -> Format.fprintf ppf "Clobber %s" name
  | Load { var = { name; _ }; base; addr; dir; _ } ->
      Format.fprintf ppf "Load (%s := %a[%a]%a)" name pp_array base
        Dba_printer.Ascii.pp_expr addr pp_endianness dir
  | Store { base; addr; dir; rval; _ } ->
      Format.fprintf ppf "Store (%a[%a]%a := %a)" pp_array base
        Dba_printer.Ascii.pp_expr addr pp_endianness dir
        Dba_printer.Ascii.pp_expr rval
  | Symbolize { var = { name; _ }; _ } -> Format.fprintf ppf "Symbolize %s" name
  | Apply _ -> Format.pp_print_string ppf "Apply"
  | Assume { test; _ } ->
      Format.fprintf ppf "Assume %a" Dba_printer.Ascii.pp_expr test
  | Assert { test; _ } ->
      Format.fprintf ppf "Check %a" Dba_printer.Ascii.pp_expr test
  | Branch { test; _ } ->
      Format.fprintf ppf "Ite %a" Dba_printer.Ascii.pp_expr test
  | Goto target -> Format.fprintf ppf "Goto %a" Virtual_address.pp target
  | Jump target -> Format.fprintf ppf "Jump %a" Dba_printer.Ascii.pp_expr target
  | Call _ -> Format.pp_print_string ppf "Call"
  | Tail_call _ -> Format.pp_print_string ppf "Tail_call"

module Straight : ASSEMBLER = struct
  type 'a queue = None | Some of { head : 'a fiber; mutable tail : 'a fiber }
  type 'a t = 'a queue ref

  let push : 'a t -> 'a fiber -> unit =
   fun state fiber ->
    Logger.debug ~level:4 "+ %a" pp_fiber fiber;
    match !state with
    | None -> state := Some { head = fiber; tail = fiber }
    | Some q ->
        relink ~pred:q.tail fiber;
        q.tail <- fiber

  let empty : (module PATH with type t = 'a) -> 'a t = fun _ -> ref None

  let assign : 'a t -> Dba.Var.t -> Dba.Expr.t -> unit =
   fun state var rval ->
    push state (Assign { var; rval; succ = invalid_successor })

  let clobber : 'a t -> Dba.Var.t -> unit =
   fun state var -> push state (Clobber { var; succ = invalid_successor })

  let symbolize : 'a t -> Dba.Var.t -> unit =
   fun state var -> push state (Symbolize { var; succ = invalid_successor })

  let forget : 'a t -> Dba.Var.t -> unit = fun _ _ -> ()

  let load :
      'a t ->
      Dba.Var.t ->
      string option ->
      Machine.endianness ->
      Dba.Expr.t ->
      unit =
   fun state var base dir addr ->
    push state (Load { var; base; dir; addr; succ = invalid_successor })

  let store :
      'a t ->
      string option ->
      Machine.endianness ->
      addr:Dba.Expr.t ->
      Dba.Expr.t ->
      unit =
   fun state base dir ~addr rval ->
    push state (Store { base; dir; addr; rval; succ = invalid_successor })

  let assume : 'a t -> Dba.Expr.t -> unit =
   fun state test -> push state (Assume { test; succ = invalid_successor })

  let check : 'a t -> Dba.Expr.t -> unit =
   fun state test -> push state (Assert { test; succ = invalid_successor })

  let apply :
      'a t -> ?input:Var.Set.t -> ?output:Var.Set.t -> ('a -> unit) -> unit =
   fun state ?input:_ ?output:_ f ->
    push state (Apply { f; succ = invalid_successor })

  let commit : 'a t -> pred:'a fiber -> 'a fiber =
   fun state ~pred ->
    match !state with
    | None -> pred
    | Some { head; tail } ->
        relink ~pred head;
        state := None;
        tail
end

module Default : sig
  include ASSEMBLER

  val fprint_graph : Format.formatter -> 'a t -> unit
end = struct
  module DGraph = Graph.Imperative.Digraph.Concrete (Basic_types.Integers.Int)
  module IntTbl = Basic_types.Integers.Int.Htbl
  module IntSet = Basic_types.Integers.Int.Set

  type 'a t = {
    mutable sync : IntSet.t;
    ordering : DGraph.t;
    nodes : 'a fiber IntTbl.t;
    definitions : (int * bool) Var.Htbl.t;
    uses : IntSet.t Var.Htbl.t;
  }

  let empty : type a. (module PATH with type t = a) -> a t =
   fun _ ->
    {
      sync = IntSet.empty;
      ordering = DGraph.create ~size:16 ();
      nodes = IntTbl.create 16;
      definitions = Var.Htbl.create 16;
      uses = Var.Htbl.create 16;
    }

  let push : 'a fiber IntTbl.t -> 'a fiber -> int =
   fun nodes node ->
    let n = IntTbl.length nodes in
    IntTbl.add nodes n node;
    n

  let get_dependencies : Dba.Expr.t -> Var.Set.t =
   fun exp -> Dba_types.Expr.collect_variables exp Var.Set.empty

  let add_use : 'a t -> int -> Var.t -> unit =
   fun state n var ->
    Var.Htbl.replace state.uses var
      (IntSet.add n
         (try Var.Htbl.find state.uses var with Not_found -> IntSet.empty));
    match Var.Htbl.find state.definitions var with
    | exception Not_found ->
        if not (IntSet.is_empty state.sync) then
          DGraph.add_edge state.ordering (IntSet.max_elt state.sync) n
    | d, _ -> DGraph.add_edge state.ordering d n

  and remove_use : 'a t -> int -> Var.t -> unit =
   fun state i var ->
    Var.Htbl.replace state.uses var
      (IntSet.remove i
         (try Var.Htbl.find state.uses var with Not_found -> IntSet.empty))

  let havoc : 'a t -> int -> Var.t -> unit =
   fun state n var ->
    (match Var.Htbl.find state.uses var with
    | exception Not_found ->
        if not (IntSet.is_empty state.sync) then
          DGraph.add_edge state.ordering (IntSet.max_elt state.sync) n
    | u ->
        IntSet.iter (fun i -> DGraph.add_edge state.ordering i n) u;
        Var.Htbl.remove state.uses var);
    match Var.Htbl.find state.definitions var with
    | exception Not_found -> ()
    | i, _ -> (
        match IntTbl.find state.nodes i with
        | Assign { rval = exp; _ } | Load { addr = exp; _ } ->
            Var.Set.iter
              (fun var -> remove_use state i var)
              (get_dependencies exp)
        | _ -> ())

  let use : 'a t -> int -> Var.Set.t -> unit =
   fun state n set -> Var.Set.iter (fun var -> add_use state n var) set

  let assign : 'a t -> Dba.Var.t -> Dba.Expr.t -> unit =
   fun state var rval ->
    let n = push state.nodes (Assign { var; rval; succ = invalid_successor }) in
    havoc state n var;
    use state n (get_dependencies rval);
    Var.Htbl.replace state.definitions var (n, true)

  let clobber : 'a t -> Dba.Var.t -> unit =
   fun state var ->
    let n = push state.nodes (Clobber { var; succ = invalid_successor }) in
    havoc state n var;
    Var.Htbl.replace state.definitions var (n, true)

  let symbolize : 'a t -> Dba.Var.t -> unit =
   fun state var ->
    let n = push state.nodes (Symbolize { var; succ = invalid_successor }) in
    havoc state n var;
    Var.Htbl.replace state.definitions var (n, true)

  let forget : 'a t -> Dba.Var.t -> unit =
   fun state var ->
    match Var.Htbl.find state.definitions var with
    | exception Not_found -> ()
    | i, _ -> Var.Htbl.replace state.definitions var (i, false)

  let load :
      'a t ->
      Dba.Var.t ->
      string option ->
      Machine.endianness ->
      Dba.Expr.t ->
      unit =
   fun state var base dir addr ->
    let n =
      push state.nodes (Load { var; base; dir; addr; succ = invalid_successor })
    in
    havoc state n var;
    use state n (get_dependencies addr);
    Var.Htbl.replace state.definitions var (n, true)

  let store :
      'a t ->
      string option ->
      Machine.endianness ->
      addr:Dba.Expr.t ->
      Dba.Expr.t ->
      unit =
   fun state base dir ~addr rval ->
    use state
      (push state.nodes
         (Store { base; dir; addr; rval; succ = invalid_successor }))
      (Dba_types.Expr.collect_variables rval
         (Dba_types.Expr.collect_variables addr Var.Set.empty))

  let assume : 'a t -> Dba.Expr.t -> unit =
   fun state test ->
    use state
      (push state.nodes (Assume { test; succ = invalid_successor }))
      (get_dependencies test)

  let check : 'a t -> Dba.Expr.t -> unit =
   fun state test ->
    use state
      (push state.nodes (Assert { test; succ = invalid_successor }))
      (get_dependencies test)

  let apply :
      'a t -> ?input:Var.Set.t -> ?output:Var.Set.t -> ('a -> unit) -> unit =
   fun state ?input ?output f ->
    let n = push state.nodes (Apply { f; succ = invalid_successor }) in
    state.sync <- IntSet.add n state.sync;
    match (input, output) with
    | None, _ | _, None ->
        Var.Htbl.iter
          (fun _ (i, alive) -> if alive then DGraph.add_edge state.ordering i n)
          state.definitions;
        Var.Htbl.clear state.definitions;
        Var.Htbl.clear state.uses
    | Some input, Some output ->
        use state n input;
        Var.Set.iter
          (fun var ->
            havoc state n var;
            Var.Htbl.replace state.definitions var (n, true))
          output

  let rec flush : 'a fiber IntTbl.t -> n:int -> int -> pred:'a fiber -> 'a fiber
      =
   fun nodes ~n i ~pred ->
    if i = n then pred
    else
      match IntTbl.find nodes i with
      | exception Not_found -> flush nodes ~n (i + 1) ~pred
      | node ->
          Logger.debug ~level:4 "+ %a" pp_fiber node;
          relink ~pred node;
          flush nodes ~n (i + 1) ~pred:node

  let substitute : Var.t -> Dba.Expr.t -> Dba.Expr.t -> Dba.Expr.t =
   fun tmp value exp ->
    Dba_types.Expr.substitute (Var.Map.singleton tmp value) exp

  let replace : 'a fiber IntTbl.t -> int -> Var.t -> Dba.Expr.t -> unit =
   fun nodes x tmp value ->
    match IntTbl.find nodes x with
    | Assign { var; rval; _ } ->
        IntTbl.replace nodes x
          (Assign
             { var; rval = substitute tmp value rval; succ = invalid_successor })
    | Load { var; base; addr; dir; _ } ->
        IntTbl.replace nodes x
          (Load
             {
               var;
               base;
               addr = substitute tmp value addr;
               dir;
               succ = invalid_successor;
             })
    | Store { base; addr; dir; rval; _ } ->
        IntTbl.replace nodes x
          (Store
             {
               base;
               addr = substitute tmp value addr;
               dir;
               rval = substitute tmp value rval;
               succ = invalid_successor;
             })
    | Assume { test; _ } ->
        IntTbl.replace nodes x
          (Assume { test = substitute tmp value test; succ = invalid_successor })
    | Assert { test; _ } ->
        IntTbl.replace nodes x
          (Assert { test = substitute tmp value test; succ = invalid_successor })
    | _ -> ()

  let forward : 'a t -> int -> Var.t -> Dba.Expr.t -> unit =
   fun { nodes; ordering; _ } i tmp value ->
    DGraph.iter_succ (fun x -> replace nodes x tmp value) ordering i;
    IntTbl.remove nodes i

  let fprint_graph : Format.formatter -> 'a t -> unit =
   fun ppf state ->
    let module Dot = Graph.Graphviz.Dot (struct
      include DGraph

      let graph_attributes _ = []
      let default_vertex_attributes _ = [ `Shape `Box ]

      let pp_vertex ppf i =
        try pp_fiber ppf (IntTbl.find state.nodes i)
        with Not_found -> Format.pp_print_int ppf i

      let vertex_name v = Format.asprintf "\"%a\"" pp_vertex v
      let vertex_attributes _v = []
      let get_subgraph _ = None
      let default_edge_attributes _ = []
      let edge_attributes _ = []
    end) in
    Dot.fprint_graph ppf state.ordering

  let last_successor : DGraph.t -> int -> int =
   fun ordering i -> DGraph.fold_succ max ordering i i

  let commit : 'a t -> pred:'a fiber -> 'a fiber =
   fun state ~pred ->
    let ordering = state.ordering and nodes = state.nodes in
    let n = IntTbl.length nodes in
    let sync = IntSet.add n state.sync in
    Var.Htbl.iter
      (fun _ (i, alive) -> if alive then DGraph.add_edge ordering i n)
      state.definitions;
    for i = 0 to n - 1 do
      match IntTbl.find nodes i with
      | Assign { var; rval = Cst _ as value; _ }
        when not (IntSet.exists (DGraph.mem_edge ordering i) sync) ->
          forward state i var value
      | Assign { var; rval = Var var'; _ } when Var.equal var var' ->
          IntTbl.remove nodes i
      | Assign { var; rval; _ }
        when DGraph.out_degree ordering i = 1
             && not (IntSet.exists (DGraph.mem_edge ordering i) sync) ->
          forward state i var rval
      | Assign { var; rval; _ }
        when not (IntSet.exists (DGraph.mem_edge ordering i) sync) -> (
          let last = last_successor ordering i in
          match IntTbl.find nodes (last_successor ordering i) with
          | Assign { var = var'; rval = Var var''; _ }
            when Var.equal var var'' && DGraph.in_degree ordering last = 1 ->
              DGraph.iter_succ
                (fun j -> replace nodes j var (Dba.Expr.v var'))
                ordering i;
              IntTbl.replace nodes i
                (Assign { var = var'; rval; succ = invalid_successor })
          | _ -> ())
      | Load { var; base; dir; addr; _ }
        when not (IntSet.exists (DGraph.mem_edge ordering i) sync) -> (
          let last = last_successor ordering i in
          match IntTbl.find nodes (last_successor ordering i) with
          | Assign { var = var'; rval = Var _; _ }
            when DGraph.in_degree ordering last = 1 ->
              DGraph.iter_succ
                (fun j -> replace nodes j var (Dba.Expr.v var'))
                ordering i;
              IntTbl.replace nodes i
                (Load { var = var'; base; dir; addr; succ = invalid_successor })
          | _ -> ())
      | _ -> ()
    done;
    flush nodes ~n 0 ~pred
end

let () = ignore Default.fprint_graph

module Cse : ASSEMBLER = struct
  module type EVAL = sig
    type path
    type value

    val zero : value
    val eval : path -> value array -> Cse.opcode -> value
  end

  module X (P : PATH) : EVAL with type path = P.t and type value = P.value =
  struct
    type path = P.t
    type value = P.value

    let zero : value = P.Value.zero

    let rec eval : type a. path -> value array -> a Cse.node -> value =
     fun path values opcode ->
      match opcode with
      | Constant bv -> P.Value.constant bv
      | Value idx -> Array.get values idx
      | Variable var -> P.lookup path var
      | Unary (op, x) -> P.Value.unary op (eval path values x)
      | Binary (op, x, y) ->
          P.Value.binary op (eval path values x) (eval path values y)
      | Ite (c, t, e) ->
          P.Value.ite (eval path values c) (eval path values t)
            (eval path values e)
      | Load (base, addr, dir, len) ->
          P.read_v path base ~addr:(eval path values addr) len dir
      | Store (base, addr, dir, rval) ->
          let value = eval path values rval in
          P.store_v path base ~addr:(eval path values addr) value dir;
          value
      | Assign (var, rval) ->
          P.assign_v path var (eval path values rval);
          P.lookup path var
      | Clobber var ->
          P.clobber path var;
          P.lookup path var
      | Symbolize var ->
          P.symbolize path var;
          P.lookup path var
  end

  type 'a t = {
    x : (module EVAL with type path = 'a);
    mutable env : Cse.Env.t;
    seq : 'a Straight.t;
  }

  let empty : type a. (module PATH with type t = a) -> a t =
   fun path ->
    {
      x = (module X ((val path)));
      env = Cse.Env.empty;
      seq = Straight.empty path;
    }

  let assign : 'a t -> Dba.Var.t -> Dba.Expr.t -> unit =
   fun state var rval -> state.env <- Cse.Env.assign var rval state.env

  let clobber : 'a t -> Dba.Var.t -> unit =
   fun state var -> state.env <- Cse.Env.clobber var state.env

  let symbolize : 'a t -> Dba.Var.t -> unit =
   fun state var -> state.env <- Cse.Env.symbolize var state.env

  let forget : 'a t -> Dba.Var.t -> unit =
   fun state var -> state.env <- Cse.Env.forget var state.env

  let load :
      'a t ->
      Dba.Var.t ->
      string option ->
      Machine.endianness ->
      Dba.Expr.t ->
      unit =
   fun state var base dir addr ->
    state.env <- Cse.Env.load var base dir addr state.env

  let store :
      'a t ->
      string option ->
      Machine.endianness ->
      addr:Dba.Expr.t ->
      Dba.Expr.t ->
      unit =
   fun state base dir ~addr rval ->
    state.env <- Cse.Env.store base dir ~addr rval state.env

  let exec :
      type a. (module EVAL with type path = a) -> Cse.opcode array -> a -> unit
      =
   fun x opcodes path ->
    let module E = (val x) in
    let values = Array.make (Array.length opcodes) E.zero in
    (* Logger.info "> start"; *)
    Array.iteri
      (fun i opcode ->
        (* Logger.info "+ %a" Cse.pp_opcode opcode; *)
        Array.set values i (E.eval path values opcode))
      opcodes

  let flush : ?input:Var.Set.t -> ?output:Var.Set.t -> 'a t -> unit =
   fun ?input ?output state ->
    if not (Cse.Env.is_empty state.env) then
      match (input, output) with
      | Some input, Some output ->
          let env, opcodes =
            Cse.partial_commit state.env (Var.Set.union input output)
          in
          state.env <- Var.Set.fold Cse.Env.forget output env;
          if Array.length opcodes <> 0 then
            Straight.apply state.seq (exec state.x opcodes)
      | None, _ | _, None ->
          Straight.apply state.seq (exec state.x (Cse.commit state.env));
          state.env <- Cse.Env.empty

  let assume : 'a t -> Dba.Expr.t -> unit =
   fun state test ->
    flush
      ~input:(Dba_types.Expr.collect_variables test Var.Set.empty)
      ~output:Var.Set.empty state;
    Straight.assume state.seq test

  let check : 'a t -> Dba.Expr.t -> unit =
   fun state test ->
    flush
      ~input:(Dba_types.Expr.collect_variables test Var.Set.empty)
      ~output:Var.Set.empty state;
    Straight.check state.seq test

  let apply :
      'a t -> ?input:Var.Set.t -> ?output:Var.Set.t -> ('a -> unit) -> unit =
   fun state ?input ?output f ->
    flush ?input ?output state;
    Straight.apply state.seq ?input ?output f

  let commit : 'a t -> pred:'a fiber -> 'a fiber =
   fun state ~pred ->
    flush state;
    Straight.commit state.seq ~pred
end

type 'a cache = 'a fiber IntTbl.t

type 'a t = {
  config : 'a config;
  graph : Ir.View.t;
  killset : Ir.View.vertex -> Dba_types.Var.Set.t;
  fibers : 'a cache;
}

let rec forward : 'a t -> Ir.View.vertex -> Ir.View.vertex =
 fun ({ graph; killset; _ } as env) vertex ->
  match Ir.View.node graph vertex with
  | Fallthrough { kind = Nop | Forget _; succ; _ }
  | Fallthrough { kind = Goto _; succ; _ } ->
      forward env succ
  | Fallthrough
      {
        kind = Assign { var; _ } | Clobber var | Load { var; _ } | Symbolize var;
        succ;
        _;
      }
    when Var.Set.mem var (killset succ) ->
      forward env succ
  | _ -> vertex

let link :
    'a t ->
    Ir.View.vertex Queue.t ->
    ('a fiber * bool * int) Queue.t ->
    'a fiber ->
    bool ->
    Ir.View.vertex ->
    unit =
 fun env todo reloc pred taken vertex ->
  let vertex = forward env vertex in
  try relink ~taken ~pred (IntTbl.find env.fibers vertex)
  with Not_found ->
    Queue.push vertex todo;
    Queue.push (pred, taken, vertex) reloc

let echo : 'a config -> string -> pred:'a fiber -> 'a fiber =
 fun { echo; _ } msg ~pred ->
  let debug : 'a fiber =
    Apply { f = (fun path -> echo path msg); succ = invalid_successor }
  in
  relink ~pred debug;
  debug

let decorate_fallthrough :
    'a config -> Ir.fallthrough -> pred:'a fiber -> 'a fiber =
 fun ({ debug; annotation_printer; _ } as config) kind ~pred ->
  match (debug, kind) with
  | No, _ -> pred
  | (Assembly | Ir), Instruction inst ->
      echo config
        (match annotation_printer with
        | Some pp ->
            Format.asprintf "%a %-25s%a" Virtual_address.pp
              (Instruction.address inst)
              (Mnemonic.to_string (Instruction.mnemonic inst))
              pp (Instruction.address inst)
        | None ->
            Format.asprintf "%a %a" Virtual_address.pp
              (Instruction.address inst) Mnemonic.pp
              (Instruction.mnemonic inst))
        ~pred
  | (Assembly | Ir), Hook { addr; info } ->
      echo config
        (match annotation_printer with
        | Some pp ->
            Format.asprintf "%a %-25s%a" Virtual_address.pp addr info pp addr
        | None -> Format.asprintf "%a %s" Virtual_address.pp addr info)
        ~pred
  | Assembly, _ -> pred
  | Ir, _ -> echo config (Format.asprintf "%a" Ir.pp_opcode kind) ~pred

let decorate_node : 'a config -> Ir.node -> pred:'a fiber -> 'a fiber =
 fun ({ debug; _ } as config) node ~pred ->
  match (debug, node) with
  | _, Fallthrough { kind; _ } -> decorate_fallthrough config kind ~pred
  | Ir, _ -> echo config (Format.asprintf "%a" Ir.pp_node node) ~pred
  | (Assembly | No), _ -> pred

let commit_addr :
    'a config -> Virtual_address.t -> int -> pred:'a fiber -> 'a fiber =
 fun { echo; debug; _ } addr n ~pred ->
  if n = 0 then pred
  else
    let step = Step { addr; n; succ = invalid_successor } in
    relink ~pred
      (match debug with
      | Ir ->
          let decoration = Format.sprintf "step %d" n in
          Apply { f = (fun path -> echo path decoration); succ = step }
      | Assembly | No -> step);
    step

module X (As : ASSEMBLER) = struct
  type 'a state = 'a As.t

  let commit :
      'a t ->
      'a state ->
      Virtual_address.t ->
      int ->
      Ir.View.vertex ->
      pred:'a fiber ->
      'a fiber =
   fun { config; killset; _ } state addr n vertex ~pred ->
    Var.Set.iter (fun var -> As.forget state var) (killset vertex);
    As.commit state ~pred:(commit_addr config addr n ~pred)

  type continuation =
    | Continue
    | Shift of Virtual_address.t * int
    | Builtin of Ir.builtin
    | Skip

  let step :
      'a t ->
      'a state ->
      Virtual_address.t ->
      int ->
      Ir.fallthrough ->
      Ir.View.vertex ->
      continuation =
   fun { config; killset; _ } state addr n kind succ ->
    match kind with
    | Nop | Goto _ -> Skip
    | Forget var ->
        As.forget state var;
        Skip
    | Instruction inst -> Shift (Instruction.address inst, n + 1)
    | Hook { addr; _ } ->
        As.apply ~input:Var.Set.empty ~output:Var.Set.empty state (fun path ->
            config.step path addr n);
        Shift (addr, 0)
    | (Assign { var; _ } | Clobber var | Load { var; _ } | Symbolize var)
      when Var.Set.mem var (killset succ) ->
        Skip
    | Assign { var; rval } ->
        As.assign state var rval;
        Continue
    | Clobber var ->
        As.clobber state var;
        Continue
    | Symbolize var ->
        As.symbolize state var;
        Continue
    | Load { var; base; dir; addr = ptr } ->
        As.load state var base dir ptr;
        Continue
    | Store { base; dir; addr = ptr; rval } ->
        As.store state base dir ~addr:ptr rval;
        Continue
    | Assume test | Assert test ->
        if n > 0 then
          As.apply ~input:Var.Set.empty ~output:Var.Set.empty state (fun path ->
              config.step path addr n);
        (match kind with Assume _ -> As.assume | _ -> As.check) state test;
        Shift (addr, 0)
    | Builtin builtin -> Builtin builtin

  let rec line :
      'a t ->
      Ir.View.vertex Queue.t ->
      ('a fiber * bool * Ir.View.vertex) Queue.t ->
      'a state ->
      Virtual_address.t ->
      int ->
      Ir.View.vertex ->
      pred:'a fiber ->
      unit =
   fun ({ graph; fibers; _ } as env) todo reloc state addr n vertex ~pred ->
    match IntTbl.find fibers vertex with
    | fiber -> relink ~pred:(commit env state addr n vertex ~pred) fiber
    | exception Not_found -> (
        match Ir.View.pred graph vertex with
        | _ :: _ :: _ ->
            Queue.push vertex todo;
            Queue.push
              (commit env state addr n vertex ~pred, false, vertex)
              reloc
        | _ -> baseline env todo reloc state addr n vertex ~pred)

  and baseline :
      'a t ->
      Ir.View.vertex Queue.t ->
      ('a fiber * bool * Ir.View.vertex) Queue.t ->
      'a state ->
      Virtual_address.t ->
      int ->
      Ir.View.vertex ->
      pred:'a fiber ->
      unit =
   fun ({ config; graph; fibers; _ } as env) todo reloc state addr n vertex
       ~pred ->
    let node = Ir.View.node graph vertex in
    match node with
    | Terminator _ ->
        relink
          ~pred:
            (decorate_node config node
               ~pred:(commit env state addr n vertex ~pred))
          (node_to_raw_fiber config.builtin_callbacks node)
    | Branch { test; target; fallthrough; _ } ->
        let branch =
          Branch
            { test; taken = invalid_successor; fallthrough = invalid_successor }
        in
        relink
          ~pred:
            (decorate_node config node
               ~pred:(commit env state addr n vertex ~pred))
          branch;
        link env todo reloc branch true target;
        link env todo reloc branch false fallthrough
    | Fallthrough { kind; succ; _ } -> (
        match step env state addr n kind succ with
        | Skip -> line env todo reloc state addr n succ ~pred
        | Continue ->
            line env todo reloc state addr n succ
              ~pred:(decorate_fallthrough config kind ~pred)
        | Shift (addr, n) ->
            line env todo reloc state addr n succ
              ~pred:(decorate_fallthrough config kind ~pred)
        | Builtin builtin -> (
            match resolve_builtin config builtin with
            | Unknown ->
                let msg =
                  Format.asprintf "no handler for %a" Ir.pp_builtin builtin
                in
                relink
                  ~pred:
                    (decorate_fallthrough config kind
                       ~pred:(commit env state addr n vertex ~pred))
                  (Tail_call (fun _ -> Signal (Error msg)))
            | Apply f ->
                if n > 0 then
                  As.apply ~input:Var.Set.empty ~output:Var.Set.empty state
                    (fun path -> config.step path addr n);
                As.apply
                  ?input:(resolve_knowledge builtin config.may_read_callbacks)
                  ?output:(resolve_knowledge builtin config.may_write_callbacks)
                  state f;
                line env todo reloc state addr 0 succ
                  ~pred:(decorate_fallthrough config kind ~pred)
            | Call f ->
                let pred = commit env state addr n vertex ~pred
                and call : 'a fiber = Call { f; succ = invalid_successor } in
                let head = decorate_fallthrough config kind ~pred in
                relink ~pred:head call;
                IntTbl.add fibers vertex (if head == pred then call else head);
                line env todo reloc (As.empty config.path) addr 0 succ
                  ~pred:call))
end

let assemble :
    type a.
    a t ->
    int Queue.t ->
    (a fiber * bool * int) Queue.t ->
    a fiber ->
    Ir.View.vertex ->
    unit =
 fun ({ config = { path; assembler; _ }; _ } as env) todo reloc pred vertex ->
  let module As = (val assembler : ASSEMBLER) in
  let module X = X (As) in
  X.baseline env todo reloc (As.empty path) Virtual_address.zero 0 vertex ~pred

let rec closure : 'a t -> int Queue.t -> ('a fiber * bool * int) Queue.t -> unit
    =
 fun ({ fibers; _ } as env) todo reloc ->
  if Queue.is_empty todo then
    Queue.iter
      (fun (pred, taken, target) ->
        relink ~taken ~pred (IntTbl.find fibers (forward env target)))
      reloc
  else
    let vertex = Queue.pop todo in
    let vertex = forward env vertex in
    if IntTbl.mem fibers vertex then closure env todo reloc;
    let placeholder : ([ `Assume ], 'a) Types.fiber =
      Assume { test = Dba.Expr.one; succ = invalid_successor }
    in
    assemble env todo reloc
      (match placeholder with Assume _ as head -> head)
      vertex;
    let (Assume { succ; _ }) = placeholder in
    IntTbl.add fibers vertex succ;
    closure env todo reloc

let create :
    'a config ->
    ?killset:(Ir.View.vertex -> Var.Set.t) ->
    ?fibers:'a cache ->
    Ir.View.t ->
    'a t =
 fun ({ may_read_callbacks; _ } as config) ?killset ?fibers graph ->
  let killset =
    match killset with
    | Some f -> f
    | None ->
        let set = IntTbl.create (Ir.View.nb_vertex graph) in
        Ir.Killset.analyze
          ~may_read:(fun builtin ->
            resolve_knowledge builtin may_read_callbacks)
          ~must_write:(Fun.const Var.Set.empty) graph set;
        IntTbl.find set
  and fibers =
    match fibers with Some cache -> cache | None -> IntTbl.create 16
  in
  { config; graph; killset; fibers }

let get : 'a t -> Ir.View.vertex -> 'a fiber =
 fun ({ fibers; _ } as env) vertex ->
  let vertex = forward env vertex in
  try IntTbl.find fibers vertex
  with Not_found ->
    let todo = Queue.create () in
    Queue.add vertex todo;
    closure env todo (Queue.create ());
    IntTbl.find fibers vertex
