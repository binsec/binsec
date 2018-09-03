(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2018                                               *)
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

open Format
open Sse_options

module LocalAnalysis = struct
  open Dba_types
  module Logger = Sse_options.Logger
  type t = {
    cfg : Sse_graph.G.t;
    (* packing *)
    (* mergepoints : int Caddress.Map.t;
     * mergeedges : Sse_pack.EdgeSet.t;
     * invalidatingedges : Sse_pack.EdgeSet.t;
     * id : int;
     * (\* pruning *\) *)
    distances_to_goals : Caddress.t -> Sse_prune.Distance.t;
  }

  let empty depth address =
    Logger.debug ~level:3 "Updating cfg at %a" Virtual_address.pp address;
    let cfg = Sse_graph.G.create 257 in
    Sse_graph.populate_from cfg address depth;
    let distances_to_goals = fun _ -> Sse_prune.Distance.Finite 0 in
    { cfg; distances_to_goals; }

  let update_dist_to_goals depth address analysis =
    Logger.debug ~level:3 "Updating reachability at %a" Virtual_address.pp address;
    let distances_to_goals =
      Sse_prune.get_distances_to_goals analysis.cfg address depth
    in
    { analysis with distances_to_goals }


  let create depth addr =
    let vaddr = Dba_types.Caddress.to_virtual_address addr in
    empty depth vaddr |> update_dist_to_goals depth vaddr

  let update analysis depth dst =
    if Sse_graph.G.mem_vertex_a analysis.cfg dst = None then
      create depth dst
    else analysis

end

module Path = struct
  type t = Dba_types.Statement.t Sequence.t
  let empty = Sequence.empty
  let extend i p = Sequence.push_front i p

  let virtual_address stmt =
    let open Dba_types in
    Statement.location stmt |> Caddress.to_virtual_address

  let pp_as_address_trace ppf p =
    match Sequence.peek_front p with
    | None -> ()
    | Some stmt ->
      let last_vaddres = ref (virtual_address stmt) in
      pp_open_vbox ppf 0;
      Sequence.iter_forward
        (fun i ->
           let v = virtual_address i in
           if v <> !last_vaddres then
             fprintf ppf "@[<h>%a@]@ " Virtual_address.pp v;
           last_vaddres := v) p;
      pp_close_box ppf ()


  let pp_address_trace_to_file p =
    if Sse_options.AddressTraceFile.is_set () then
      let filename = Sse_options.AddressTraceFile.get () in
      Print_utils.pp_to_file ~filename pp_as_address_trace p
end

let decode vaddress = Disasm_core.decode vaddress |> fst

module Path_state = struct

  type t  = {
    id : int ;
    depth : int; (** Current depth of traversal *)
    solver_calls : int;
    path : Path.t option;
    symbolic_state : Sse_symbolic.State.t;
    instruction : Instruction.t;
    block_index : int;
    infos : LocalAnalysis.t;
    address_counters : Sse_options.Address_counter.t Virtual_address.Map.t;
  }

  let gen_id = ref (-1)

  let id st = st.id
  let symbolic_state st = st.symbolic_state
  let block_index st = st.block_index
  let counter vaddr st =
    match Virtual_address.Map.find vaddr st.address_counters with
    | c -> Some c
    | exception Not_found -> None


  let set_counter vaddr c st =
    { st with address_counters =
                Virtual_address.Map.add vaddr c st.address_counters }

  let paths_created () = !gen_id

  let solver_calls p = p.solver_calls
  let incr_solver_calls p = { p with solver_calls = p.solver_calls + 1; }
  let reset_solver_calls p = { p with solver_calls = 0; }

  let dba_instruction st =
    let block = st.instruction.Instruction.dba_block in
    Dhunk.inst block st.block_index |> Utils.unsafe_get_opt

  let set_instruction instruction st =
    if instruction <> st.instruction then
    Logger.debug ~level:2 "@%a"
      Virtual_address.pp instruction.Instruction.address;
    { st with instruction }

  let set_block_index block_index st = { st with block_index }

  let set_symbolic_state symbolic_state st = { st with symbolic_state }

  let set_address_counters address_counters st = { st with address_counters }

  let add_assertion cond st =
    let open Sse_symbolic.State in
    let open Sse_symbolic in
    let open Formula in
    let current_pc = get_path_constraint st.symbolic_state in
    let pc = mk_bl_and current_pc cond in
    let symbolic_state =
      assign st.symbolic_state path_constraint_name bl_sort (mk_bl_term pc) in
    { st with symbolic_state }

  let update_symbolic_state name var_type value st =
    let symbolic_state =
      Sse_symbolic.State.assign st.symbolic_state name var_type value in
    { st with symbolic_state }

  let with_init_mem_at ~addr ~size path_state =
    let symbolic_state =
      Sse_symbolic.State.init_mem_at path_state.symbolic_state ~addr ~size in
    { path_state with symbolic_state }

  exception Found
  let address_belongs_to_init ~addr path_state =
    try
      let open Sse_symbolic.State in
      S.iter
        (fun kaddr vsize ->
           if
             Int64.compare addr kaddr <= 0 &&
             let end_addr = Int64.(add kaddr (of_int vsize)) in
             Int64.compare kaddr end_addr < 0
           then raise Found
        ) path_state.symbolic_state.initialisation;
      false
    with Found -> true

  let virtual_address st =
    let open Instruction in
    st.instruction.address

  let location st =
    let caddress =
      virtual_address st |> Dba_types.Caddress.of_virtual_address in
    Dba_types.Caddress.reid caddress st.block_index

  let current_statement st =
    dba_instruction st
    |> Dba_types.Statement.create (location st)

  let goto address st =
    let vaddr = Dba_types.Caddress.to_virtual_address address in
    let instruction =
      if Virtual_address.compare vaddr (virtual_address st) <> 0 then
        decode vaddr
      else
        st.instruction
    in
    let st = set_instruction instruction st |> set_block_index address.Dba.id in
    let statement = current_statement st in
    let path = match st.path with
      | None -> None
      | Some x -> Some (Path.extend statement x)
    in
    let depth = st.depth + 1 in
    let infos =
      LocalAnalysis.update st.infos (MaxDepth.get() - depth) address in
    { st with path; depth; infos; }

  let goto_vaddr address st =
    goto (Dba_types.Caddress.of_virtual_address address) st

  let set_block_index idx st =
    goto (Dba_types.Caddress.reid (location st) idx) st

  let pp_loc ppf st =
    let dba_instruction = dba_instruction st in
    let vaddress = virtual_address st in
    fprintf ppf "@[<hov>(%a, %d)@ :@ @[%a@]@]"
      Virtual_address.pp vaddress
      st.block_index
      Dba_printer.Ascii.pp_instruction dba_instruction

  let pp_path ps = match ps.path with
    | None -> ()
    | Some x -> Path.pp_address_trace_to_file x

  let leads_to_goal st =
    let open Sse_prune in
    let distance = st.infos.LocalAnalysis.distances_to_goals (location st) in
    let max_depth = Distance.Finite (Sse_options.MaxDepth.get ()) in
    let res = Distance.(lt (add distance (Finite st.depth)) max_depth) in
    Logger.debug ~level:3
      "@[<hov>At %a@ (depth=%d) distance to goal=%a@ : useful branch=%B@]"
      pp_loc st st.depth Distance.pp distance res;
    res

  let do_optimization ps fm =
    let open Sse_symbolic in
    let keep = State.get_path_variables ps.symbolic_state in
    Formula_transformation.optimize_from_options ~keep fm

  let prepare_solver_in_state ps solver =
    let open Sse_symbolic in
    let append en = Solver.Session.put_entry solver en in
    Formula.push_front_assert
      (State.get_path_constraint ps.symbolic_state)
      (State.get_entries ps.symbolic_state)
    |> do_optimization ps
    |> Formula.iter_forward append

  let create
      ?(depth=0)
      ?(address_counters=Virtual_address.Map.empty)
      ?(block_index=0) symbolic_state instruction =
    assert(
      block_index >= 0 &&
      block_index <=
      Dhunk.length instruction.Instruction.dba_block);
    let infos =
      Instruction.get_caddress instruction
      |> LocalAnalysis.create (MaxDepth.get() - depth)
    in
    incr gen_id;
    { id = !gen_id; address_counters;
      depth; infos; path = Some Path.empty; block_index; symbolic_state; instruction;
      solver_calls = 0; (* At path creation we have never called a solver *)
     }

    let branch p =
      incr gen_id;
      { p with id = !gen_id }

end

(* Both the stack and the queue below are functional implementations of these
   data structures
 *)

module type WORKLIST = sig
  type t
  val push : Path_state.t -> t -> t
  val pop  : t -> Path_state.t * t
  val singleton : Path_state.t -> t
  val length : t -> int
  val is_empty : t -> bool
  val empty : t
end

module Stack:WORKLIST = Fstack.Make(Path_state)

module Queue:WORKLIST = struct
  type t = Path_state.t Sequence.t

  let length = Sequence.length
  let is_empty q = Sequence.length q = 0
  let empty = Sequence.empty
  let push p q = Sequence.push_back p q
  let pop q =
    match Sequence.peek_front q with
    | None -> raise Not_found
    | Some v ->
       match Sequence.pop_front q with
       | None -> assert false
       | Some seq -> v, seq

  let singleton p = push p empty
end

module Random_heap: WORKLIST = struct
  (* This is actually a fairly classical heap.
     The priority added to the date is just generated at random.
   *)

  module T = struct
    type t = {
        priority : int;
        state : Path_state.t
      }

    let compare t1 t2 = Pervasives.compare t1.priority t2.priority

    let create ~priority ~state = {priority; state;}
  end

  module H = Worklist.Make(T)

  type t = H.t

  let gen_priority () = Utils.random_max_int ()

  let length = H.length
  let is_empty = H.is_empty
  let empty = H.empty

  let push p h =
    let priority = gen_priority () in
    H.add (T.create ~priority ~state:p) h

  let pop h =
    let e, h' = H.pop h in
    e.T.state, h'

  let singleton p = push p empty

end

module G(W:WORKLIST) = struct
  type goal = {
      mutable todo : int;
      actions : Action.t Virtual_address.Htbl.t;
    }

  module Bv_set = struct
    (* one could use Set.Make(Bitvector) but lists are simpler
     * and might even be faster
     *)
    type t = Bitvector.t list

    let union l1 l2 = l1 @ l2 |> List.sort_uniq Bitvector.compare

    let cardinal = List.length
  end

  type t = {
    worklist : W.t;
    goals : goal;
    enumerations : Bv_set.t Virtual_address.Htbl.t;
  }

  module Goals = struct
    let at address e =
      match Virtual_address.Htbl.find e.goals.actions address with
      | goals -> Some goals
      | exception Not_found -> None

    let count e = e.goals.todo

    let remove address e =
    Virtual_address.Htbl.remove e.goals.actions address;
    e.goals.todo <- e.goals.todo - 1

    (* [todo] is not updated since we are supposedly just keeping the same number
       of todos
     *)
    let update address g e =
      Virtual_address.Htbl.replace e.goals.actions address g

    let has e =
      let ngoals = count e in
      ngoals <> 0

    (* Initialize goal table from cli specification *)
    let init () =
      let todo = ref 0 in
      let gs = Goals.get () in
      let len = List.length gs in
      (* Add one more slot to the hashtables if needed later on *)
      let h = Virtual_address.Htbl.create (len + 1) in

      let add_actions gs =
        List.iter
          (fun g ->
            let open Action in
            (match Action.goal g with
             | Choice _
             | Cut
             | Restrict _ -> ()
             | Reach _
             | Enumerate _ -> incr todo);
            Logger.debug ~level:2
              "Add action %a" Action.pp g;
            Virtual_address.Htbl.add h g.address g) gs in
      add_actions gs;
      let open Basic_types in
      let add_action action int_set =
        Int.Set.iter
          (fun n ->
            let vaddr = Virtual_address.create n in
            let action = action vaddr in
            Logger.debug ~level:2
              "Add action %a" Action.pp action;
            Virtual_address.Htbl.add h vaddr action) int_set in
      add_action
        (fun vaddr -> incr todo; Action.reach vaddr) (GoalAddresses.get ());
      add_action Action.cut (AvoidAddresses.get ());
      { todo = !todo; actions = h }

    module Enumeration = struct

      let record va bvs g =
        let enums = g.enumerations in
        let log =
          Logger.debug ~level:3
            "@[<h>Enumeration: recorded %d new values %@ %a@]"
        in
        match Virtual_address.Htbl.find enums va with
        | vs ->
           let n0 = Bv_set.cardinal vs in
           let v' = Bv_set.union bvs vs in
           let n1 = Bv_set.cardinal v' in
           log (n1 - n0) Virtual_address.pp va;
           Virtual_address.Htbl.replace enums va v'
        | exception Not_found ->
           log (Bv_set.cardinal bvs) Virtual_address.pp va;
           Virtual_address.Htbl.replace enums va bvs

      let count va g =
        match Virtual_address.Htbl.find g.enumerations va with
        | v -> Bv_set.cardinal v
        | exception Not_found -> 0

      let get va g =
        match Virtual_address.Htbl.find g.enumerations va with
        | vs -> vs
        | exception Not_found -> []

    end
  end

  module Path = struct
    exception Empty_worklist

    let choose e =
      if W.is_empty e.worklist then begin
          Logger.warning "Empty worklist: we're done ...";
          raise Empty_worklist
        end
      else
        let path_state, worklist = W.pop e.worklist in
        Logger.debug "Selecting path : %d" (Path_state.id path_state);
        { e with worklist }, path_state

    let add path_state e =
      let worklist = W.push path_state e.worklist in
      { e with worklist }
  end

  let from_address ~initialize_fun ~entrypoint =
    let level = 4 in
    let initial_instruction = decode entrypoint in
    Logger.debug ~level "Creating symbolic store ...";
    let symbolic_store = Sse_symbolic.Store.create () in
    let symbolic_state = Sse_symbolic.State.create symbolic_store in
    Logger.debug ~level "Creating initial path state ...";
    let initial_path_state =
      Path_state.create symbolic_state initial_instruction |> initialize_fun in
    Logger.debug ~level "Creating worklist ...";
    let worklist = W.singleton initial_path_state in
    Logger.debug ~level "@[<h>Initializing SSE goals ...@]";
    let goals = Goals.init () in
    let enumerations = Virtual_address.Htbl.create 7 in
    { worklist; goals; enumerations; }


  let wl_size path_state = W.length path_state.worklist

end

module type GLOBAL_ENV = sig
  type t

  val wl_size : t -> int

  module Goals : sig
    val at : Virtual_address.t -> t -> Action.t option
    val has : t -> bool
    val update : Virtual_address.t -> Action.t -> t -> unit
    val remove : Virtual_address.t -> t -> unit

    module Enumeration : sig
      val record: Virtual_address.t -> Bitvector.t list -> t -> unit
      val count : Virtual_address.t -> t -> int
      val get   : Virtual_address.t -> t -> Bitvector.t list
    end

  end


  (** {3 Constructor }*)
  val from_address :
    initialize_fun:(Path_state.t -> Path_state.t) ->
    entrypoint:Virtual_address.t -> t

  module Path : sig
    exception Empty_worklist

    val choose : t -> t * Path_state.t
    (** [choose_path e] pops a new path [p] from environment [e],
      and returns both the path and the environment without this path.

      @raise Empty_worklist *)

    val add : Path_state.t -> t -> t
    (** [add_path p e] register path [p] in the worlist
        of environment [e].
     *)
  end

end

module Dfs_global:GLOBAL_ENV = G(Stack)

module Bfs_global:GLOBAL_ENV = G(Queue)

module Nurs_global:GLOBAL_ENV = G(Random_heap)
