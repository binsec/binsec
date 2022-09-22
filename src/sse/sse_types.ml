(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2022                                               *)
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

exception Unknown

type 'a test = True of 'a | False of 'a | Both of { t : 'a; f : 'a }

module type STATE = sig
  type t
  (** Symbolic state *)

  val empty : unit -> t

  val assume : Dba.Expr.t -> t -> t option

  val test : Dba.Expr.t -> t -> t test

  val split_on :
    Dba.Expr.t ->
    ?n:int ->
    ?except:Bitvector.t list ->
    t ->
    (Bitvector.t * t) list

  val fresh : string -> int -> t -> t

  val assign : string -> Dba.Expr.t -> t -> t

  val write : addr:Dba.Expr.t -> Dba.Expr.t -> Machine.endianness -> t -> t

  val memcpy : addr:Bitvector.t -> int -> Loader_buf.t -> t -> t

  val pp : Format.formatter -> t -> unit

  val pp_smt :
    ?slice:(Dba.Expr.t * string) list -> Format.formatter -> t -> unit

  val as_ascii : string -> t -> string
end

module type EXPLORATION_STATISTICS = sig
  val get_paths : unit -> int

  val get_completed_paths : unit -> int

  val get_unknown_paths : unit -> int

  val get_total_asserts : unit -> int

  val get_failed_asserts : unit -> int

  val get_branches : unit -> int

  val get_max_depth : unit -> int

  val get_instructions : unit -> int

  val get_unique_insts : unit -> int

  val get_time : unit -> float
end

module type QUERY_STATISTICS = sig
  module Preprocess : sig
    val get_sat : unit -> int

    val get_unsat : unit -> int

    val get_const : unit -> int

    val incr_sat : unit -> unit

    val incr_unsat : unit -> unit

    val incr_const : unit -> unit

    val pp : Format.formatter -> unit -> unit

    val to_toml : unit -> Toml.Types.table
  end

  module Solver : sig
    val get_sat : unit -> int

    val get_unsat : unit -> int

    val get_err : unit -> int

    val get_time : unit -> float

    val incr_sat : unit -> unit

    val incr_unsat : unit -> unit

    val incr_err : unit -> unit

    val start_timer : unit -> unit

    val stop_timer : unit -> unit

    val pp : Format.formatter -> unit -> unit

    val to_toml : unit -> Toml.Types.table
  end
end

module type STATE_FACTORY = functor (QS : QUERY_STATISTICS) -> STATE

module Pragma = struct
  type t =
    | Start_from of Dba.Expr.t * Dhunk.t
    | Start_from_core of Dhunk.t
    | Load_sections of string list
    | Reach_all
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

module Path_state (S : STATE) = struct
  type t = {
    id : int;
    (* Unique identifier for the path *)
    depth : int;
    (* Current depth of traversal *)
    solver_calls : int;
    path : Virtual_address.t list;
    (* Sequence of virtual addresses for this path *)
    symbolic_state : S.t;
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
