(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2021                                               *)
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

module Pragma = struct
  type t = Start_from of Dba.Expr.t | Load_sections of string list
end

module Script = struct
  type t =
    | Init of Parse_helpers.Initialization.t
    | Goal of Directive.t
    | Stub of Dba.Expr.t list * Dhunk.t
    | Pragma of Pragma.t
end

module C = struct
  include Instr_cfg.Make (struct
    include Basic_types.Int

    let hash i = i

    let equal = ( == )
  end)
end

module Path_state (S : Smt_solver.Solver) = struct
  module State = Senv.State (S)

  type t = {
    id : int;
    (* Unique identifier for the path *)
    depth : int;
    (* Current depth of traversal *)
    solver_calls : int;
    path : Virtual_address.t list;
    (* Sequence of virtual addresses for this path *)
    symbolic_state : State.t;
    (* Current symbolic state *)
    instruction : Instruction.t;
    (* Current instruction *)
    block_index : int;
    (* Current index into DBA block of current
       instruction *)
    next_addr : Virtual_address.t option;
    (* Next address to decode *)
    (* How many times we can pass at this address before cut *)
    address_counters : Sse_options.Address_counter.t Virtual_address.Map.t;
  }

  let gen_id = ref (-1)

  let id st = st.id

  let depth st = st.depth

  let symbolic_state st = st.symbolic_state

  let block_index st = st.block_index

  let inst ps = ps.instruction

  let next_address ps = ps.next_addr

  let counter vaddr st =
    match Virtual_address.Map.find vaddr st.address_counters with
    | c -> Some c
    | exception Not_found -> None

  let set_counter vaddr c st =
    {
      st with
      address_counters = Virtual_address.Map.add vaddr c st.address_counters;
    }

  let paths_created () = !gen_id

  let solver_calls p = p.solver_calls

  let incr_solver_calls p = { p with solver_calls = p.solver_calls + 1 }

  let reset_solver_calls p = { p with solver_calls = 0 }

  let dba_instruction st =
    let block = st.instruction.Instruction.dba_block in
    Dhunk.inst block st.block_index |> Utils.unsafe_get_opt

  let set_block_index block_index st = { st with block_index }

  let set_instruction instruction st =
    {
      st with
      block_index = 0;
      instruction;
      depth = st.depth + 1;
      next_addr = None;
      path = Instruction.address instruction :: st.path;
    }

  let set_next_address addr st = { st with next_addr = Some addr }

  let set_symbolic_state symbolic_state st = { st with symbolic_state }

  let set_address_counters address_counters st = { st with address_counters }

  let with_init_mem_at ~addr ~size path_state =
    let symbolic_state =
      let value = Bitvector.value_of addr in
      let bvsize = Kernel_options.Machine.word_size () in
      let addr = Bitvector.create value bvsize in
      State.load_from ~addr size path_state.symbolic_state
    in
    { path_state with symbolic_state }

  let virtual_address st =
    let open Instruction in
    st.instruction.address

  let location st =
    let caddress =
      virtual_address st |> Dba_types.Caddress.of_virtual_address
    in
    Dba_types.Caddress.reid caddress st.block_index

  let current_statement st =
    dba_instruction st |> Dba_types.Statement.create (location st)

  let pp_loc ppf st =
    let dba_instruction = dba_instruction st in
    let vaddress = virtual_address st in
    fprintf ppf "@[<hov>(%a, %d)@ :@ @[%a@]@]" Virtual_address.pp vaddress
      st.block_index Dba_printer.Ascii.pp_instruction dba_instruction

  let pp_path ppf ps =
    Format.pp_open_vbox ppf 0;
    List.iter
      (fun v ->
        Virtual_address.pp ppf v;
        Format.pp_print_space ppf ())
      (List.rev ps.path);
    Format.pp_close_box ppf ()

  let is_depth_ok ps =
    let max_depth = Sse_options.MaxDepth.get () in
    ps.depth < max_depth

  let may_lead_to_goal = is_depth_ok

  (* One might elements from the CFG here *)

  let create ?(depth = 0) ?(address_counters = Virtual_address.Map.empty)
      ?(block_index = 0) symbolic_state instruction =
    assert (
      block_index >= 0
      && block_index <= Dhunk.length instruction.Instruction.dba_block);
    incr gen_id;
    {
      id = !gen_id;
      address_counters;
      depth;
      path = [];
      block_index;
      symbolic_state;
      instruction;
      next_addr = None;
      solver_calls = 0 (* At path creation we have never called a solver *);
    }

  let branch p =
    incr gen_id;
    { p with id = !gen_id }
end

(* Both the stack and the queue below are functional implementations of these
   data structures
*)

module type WORKLIST = sig
  type elt

  type t

  val push : elt -> t -> t

  val pop : t -> elt * t

  val singleton : elt -> t

  val length : t -> int

  val is_empty : t -> bool

  val empty : t
end

module type WORKLIST_FACTORY = functor (E : Sigs.ANY) ->
  WORKLIST with type elt := E.t

module Dfs (E : Sigs.ANY) : WORKLIST with type elt := E.t = struct
  type t = E.t list

  let empty = []

  let is_empty = function [] -> true | _ -> false

  let push e w = e :: w

  let singleton e = [ e ]

  let pop = function e :: w -> (e, w) | [] -> raise Not_found

  let length = List.length
end

module Bfs (E : Sigs.ANY) : WORKLIST with type elt := E.t = struct
  type t = E.t Sequence.t

  let length = Sequence.length

  let is_empty q = Sequence.length q = 0

  let empty = Sequence.empty

  let push p q = Sequence.push_back p q

  let pop q =
    match Sequence.peek_front q with
    | None -> raise Not_found
    | Some v -> (
        match Sequence.pop_front q with
        | None -> assert false
        | Some seq -> (v, seq))

  let singleton p = push p empty
end

module Nurs (E : Sigs.ANY) : WORKLIST with type elt := E.t = struct
  (* This is actually a fairly classical heap.
     The priority added to the date is just generated at random.
  *)

  module T = struct
    type t = { priority : int; state : E.t }

    let compare t1 t2 = compare t1.priority t2.priority

    let create ~priority ~state = { priority; state }
  end

  module H = Worklist.Make (T)

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
    (e.T.state, h')

  let singleton p = push p empty
end
