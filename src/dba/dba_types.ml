(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2019                                               *)
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

open Dba

module Logger = Logger.Make(struct let name = "dba" end)

type instruction_sequence = (Dba.address *  Dba.Instr.t) list

let malloc_id = ref 0

module Caddress = struct
  module X = struct
    type t = Dba.address
    let compare a1 a2 =
      let c = Virtual_address.compare a1.base a2.base in
      if c = 0 then a1.id - a2.id else c
  end

  include Basic_types.Collection_make.Default(X)

  let compare = X.compare

  let create base id =
    assert (id >= 0);
    { base; id; }

  let rebase a base = create base a.id
  let reid a id  = create a.base id

  let block_start bv = create bv 0

  let base_value addr = addr.base

  let equal caddr1 caddr2 = compare caddr1 caddr2 = 0

  let pp_base ppf v = Format.fprintf ppf "%a" Virtual_address.pp v.base

  let add_int a n =
    rebase a (Virtual_address.add_int n a.base)

  let add_id a n = reid a (n + a.id)

  let block_start_of_int n =
    block_start (Virtual_address.create n)

  let default_init = ref (block_start_of_int 0)

  let to_virtual_address caddr =
    base_value caddr

  let of_virtual_address n =
    block_start n
end


module Call_stack = struct
  exception Not_equal_stacks of int

  type t = (Caddress.t * Caddress.t) list

  let compare stack1 stack2 =
    let len1 = List.length stack1 and len2 = List.length stack2 in
    if len1 <> len2 then len1 - len2
    else
      try
        List.iter2 (fun (a1, a2) (aa1, aa2)->
            if Caddress.compare a1 aa1 = 0
            then
              if Caddress.compare a2 aa2 = 0
              then ()
              else raise (Not_equal_stacks (Caddress.compare a2 aa2))
            else raise (Not_equal_stacks (Caddress.compare a1 aa1))
          ) stack1 stack2;
        0
      with
      | Not_equal_stacks c -> c
      | Invalid_argument _ -> failwith "not equal lists in compare_stacks!"


  let _equal stack1 stack2 = compare stack1 stack2 = 0

  let rec pp fmt = function
    | [] -> Format.fprintf fmt "main"
    | (_caller, callee) :: stack ->
      Format.fprintf fmt "%a %a"
        pp stack Dba_printer.Ascii.pp_code_address callee
end

(* Region *)
module ComparableRegion = struct
  type t = Dba.region
  let compare r1 r2 =
    match r1, r2 with
    | `Constant, `Constant -> 0
    | `Stack, `Stack -> 0
    | `Malloc ((id1, _), _), `Malloc ((id2, _), _) -> Pervasives.compare id1 id2
    | `Constant, _ -> 1
    | `Stack, `Constant -> -1
    | `Stack, _ -> 1
    | `Malloc _, _ -> -1

end

module Region = struct
  include Basic_types.Collection_make.Default(ComparableRegion)

  let malloc _sz =
    let a = Caddress.block_start (Virtual_address.create (-1)) in
    `Malloc ((-1, a), Bigint.zero_big_int)

  let pp ppf = function
    | `Constant -> Format.fprintf ppf "constant"
    | `Malloc _ -> Format.fprintf ppf "heap"
    | `Stack    -> Format.fprintf ppf "stack"
end

(* Rights *)
module Rights = struct
  type action = R | W | X
  include Map.Make (
    struct
      type t = action * Dba.region
      let compare (a1, b1) (a2, b2) =
        let i = Region.compare b1 b2 in
        if i = 0 then Pervasives.compare a1 a2
        else i
    end
    )

  let find_right right v m = find (right, v) m
  let find_read_right v m  = find_right R v m
  let find_write_right v m = find_right W v m
  let find_exec_right v m  = find_right X v m
end


module Expr:  sig
  include Sigs.PRINTABLE with type t := Dba.Expr.t

  type t = Dba.Expr.t
  val var : string -> Size.Bit.t -> Dba.VarTag.t -> t
  val flag : ?bits:Size.Bit.t -> string -> t

  val temporary : string -> Size.Bit.t -> t

  val sext : t -> Size.Bit.t -> t
  val uext : t -> Size.Bit.t -> t

  val bool_false : t
  val bool_true : t

  val temp : Size.Bit.t -> t
  val is_symbolic : t -> bool
  val of_lvalue : Dba.LValue.t -> t
  val is_zero : t -> bool
  val is_one : t -> bool
  val is_max : t -> bool

  val variables : t -> Basic_types.String.Set.t
  val temporaries : t -> Basic_types.String.Set.t
end = struct
  type t = Dba.Expr.t

  let var name nbits tag =
    let sz = Size.Bit.to_int nbits in
    Dba.Expr.var name sz ~tag

  let flag ?(bits=Size.Bit.bits1) flagname =
    var flagname bits (Dba.VarTag.flag Flag.unspecified)

  let temporary tempname nbits =
    let size = Size.Bit.to_int nbits in
    Dba.Expr.temporary ~size tempname
  ;;

  let pp = Dba_printer.Ascii.pp_bl_term

  let sext e bitsize = Dba.Expr.sext (Size.Bit.to_int bitsize) e
  let uext e bitsize = Dba.Expr.uext (Size.Bit.to_int bitsize) e

  let temp size =
    let name = Format.asprintf "temp%a" Size.Bit.pp size in
    temporary name size

  let bool_true = Dba.Expr.one
  let bool_false = Dba.Expr.zero

  open! Dba
  let rec is_symbolic = function
    | Expr.Var _
    | Expr.Load _ -> true
    | Expr.Unary(_, e) -> is_symbolic e
    | Expr.Binary(_, e1, e2) ->
      is_symbolic e1 || is_symbolic e2
    | Expr.Ite(c, e1, e2) ->
      is_symbolic c || is_symbolic e1 || is_symbolic e2
    | _ -> false

  let of_lvalue = function
    | LValue.Var {name; size; info = tag} -> Expr.var name size ~tag
    | LValue.Restrict (v, {Interval.lo=o1; Interval.hi=o2}) ->
      Expr.restrict o1 o2 (Expr.var v.name v.size ~tag:v.info)
    | LValue.Store (sz, endiannness, e) ->
       let bysz = Size.Byte.create sz in
       Expr.load bysz endiannness e

  let is_zero = function
    | Expr.Cst (`Constant, bv) -> Bitvector.is_zeros bv
    | _ -> false

  let is_one = function
    | Expr.Cst (_, bv) -> Bitvector.is_one bv
    | _ -> false

  let is_max = function
    | Dba.Expr.Cst (`Constant, bv) -> Bitvector.is_max_ubv bv
    | _ -> false


  let rec variables = function
    | Dba.Expr.Cst _ -> Basic_types.String.Set.empty
    | Dba.Expr.Unary(_, e)
    | Dba.Expr.Load(_, _, e)  -> variables e
    | Dba.Expr.Var { name; _ } -> Basic_types.String.Set.singleton name
    | Dba.Expr.Binary(_, e1, e2)
    | Dba.Expr.Ite(_, e1, e2) ->
      Basic_types.String.Set.union (variables e1) (variables e2)

  let rec temporaries = function
    | Dba.Expr.Cst _ -> Basic_types.String.Set.empty
    | Dba.Expr.Unary(_, e)
    | Dba.Expr.Load(_, _, e) -> temporaries e
    | Dba.Expr.Var { name; info = Dba.VarTag.Temp; _ } ->
      Basic_types.String.Set.singleton name
    | Dba.Expr.Var _ -> Basic_types.String.Set.empty
    | Dba.Expr.Binary(_, e1, e2)
    | Dba.Expr.Ite(_, e1, e2) ->
      Basic_types.String.Set.union (temporaries e1) (temporaries e2)

end


module LValue = struct
  type t = LValue.t

  module Map = Map.Make (
    struct
      type t = Dba.LValue.t
      let compare  = Pervasives.compare
    end
    )

  let bitsize lval =
    let sz =
      match lval with
      | LValue.Var v -> v.size
      | LValue.Store(size, _, _) -> 8 * size
      | LValue.Restrict(_,  {Interval.lo; Interval.hi}) -> hi - lo + 1
    in Size.Bit.create sz

  let unsafe_bitsize lval = bitsize lval |> Size.Bit.to_int

  let _pp = Dba_printer.Ascii.pp_lhs

  let name_of = function
    | LValue.Var { name; _}
    | LValue.Restrict ({name; _}, _) -> Some name
    | LValue.Store _ -> None

  let is_temporary = function
    | LValue.Var { info = VarTag.Temp; _ } -> true
    | LValue.Var _
    | LValue.Restrict _
    | LValue.Store _ -> false

  let is_flag = function
    | LValue.Var {info = VarTag.Flag _; _} -> true
    | LValue.Var _
    | LValue.Restrict _
    | LValue.Store _ -> false

  let variables = function
    | LValue.Var { name; _ }
    | LValue.Restrict ({name; _},  _) -> Basic_types.String.Set.singleton name
    | LValue.Store (_, _, e) -> Expr.variables e

  let temporaries = function
    | LValue.Var { name; info = VarTag.Temp; _ } ->
      Basic_types.String.Set.singleton name
    | LValue.Var _
    | LValue.Restrict _ -> Basic_types.String.Set.empty
    (* Restrict cannot be applied to temporaries : check that! *)
    | LValue.Store (_, _, e) -> Expr.temporaries e

end


module ComparableAddressStack = struct
  type t = Caddress.t * Call_stack.t * int
  let compare (a1, stack1, loop1) (a2, stack2, loop2) =
    let c = Call_stack.compare stack1 stack2 in
    if c = 0
    then
      let c' = Caddress.compare a1 a2 in
      if c' = 0
      then Pervasives.compare loop1 loop2
      else c'
    else c
end

module AddressStack = struct
  include Basic_types.Collection_make.Default(ComparableAddressStack)
  let pp fmt (caddr, call_stack, n) =
    Format.fprintf fmt "@[<hov 2>%a@,<%a>@,(%d)@]"
      Dba_printer.Ascii.pp_code_address caddr
      Call_stack.pp call_stack
      n
end


type 'a dbainstrmap = (Dba.Instr.t * 'a option) Caddress.Map.t


module Declarations = struct
  type t = (Dba.size * Dba.VarTag.t) Basic_types.String.Map.t

  open Basic_types.String.Map
  let of_list declarations =
    List.fold_left
      (fun smap (id, size, opttags) -> add id (size, opttags) smap)
      empty declarations
end


module Jump_target = struct

  let outer_jumps = function
    | JInner _ -> Virtual_address.Set.empty
    | JOuter a -> Virtual_address.Set.singleton (Caddress.to_virtual_address a)
end


type ('a, 'b) defuse = {
  defs: 'a;
  uses: 'b;
}

module Instruction = struct
  type t = Dba.Instr.t

  let set_successor i id =
    let open Dba.Instr in
    match i with
    | Assign (lv, e, _) -> assign lv e id
    | If (c, jt, _) -> ite c jt id
    | Assert (c, _) -> _assert c id
    | Assume (c, _) -> assume c id
    | NondetAssume (lvs, c, _) -> non_deterministic_assume lvs c id
    | Nondet (lv, region, _) -> non_deterministic lv ~region id
    | Undef (lv, _) -> undefined lv id
    | Malloc (lv, e, _) -> malloc lv e id
    | Free (e, _) -> free e id
    | Print (ds, _) -> print ds id
    | Stop _
    | SJump _
    | DJump _ -> i


  let generic_reset_successors ~p ~f instr =
    let new_id id = if p id then f id else id in
    let new_jt = function
      | JOuter _ as jt -> jt
      | JInner id -> Dba.Jump_target.inner (new_id id)
    in
    let open Dba.Instr in
    match instr with
    | Assign (lv, e, id) -> assign lv e (new_id id)
    | If (c, jt, id) -> ite c (new_jt jt) (new_id id)
    | Assert (c, id) -> _assert c (new_id id)
    | Assume (c, id) -> assume c (new_id id)
    | NondetAssume (lvs, c, id) -> non_deterministic_assume lvs c (new_id id)
    | Nondet (lv, region, id) -> non_deterministic lv ~region (new_id id)
    | Undef (lv, id) -> undefined lv (new_id id)
    | Malloc (lv, e, id) -> malloc lv e (new_id id)
    | Free (e, id) -> free e (new_id id)
    | Print (ds, id) -> print ds (new_id id)
    | SJump (jt, tag) -> static_jump ?tag (new_jt jt)
    | Stop _
    | DJump _ -> instr


  let reset_successor ~src_id ~dst_id instr =
    generic_reset_successors ~p:((=) src_id) ~f:(fun _ -> dst_id) instr


  let successors = function
    | Instr.Assign (_, _, i)
    | Instr.Assert (_, i)
    | Instr.Assume (_, i)
    | Instr.NondetAssume (_, _, i)
    | Instr.Nondet (_, _, i)
    | Instr.Undef (_, i)
    | Instr.Malloc (_, _, i)
    | Instr.Free (_, i)
    | Instr.Print (_, i)  -> [Dba.Jump_target.inner i]
    | Instr.Stop _ -> []
    | Instr.SJump (jt, _) -> [jt]
    | Instr.If (_, jt, i) -> [jt; Dba.Jump_target.inner i]
    | Instr.DJump _ -> []


  let no_defs uses = { defs = Basic_types.String.Set.empty; uses}

  let variables = function
    | Instr.Malloc (lv, e, _)
    | Instr.Assign (lv, e, _) ->
      { defs = LValue.variables lv;
        uses = Expr.variables e;}
    | Instr.Print _
    | Instr.Stop _
    | Instr.SJump _ -> no_defs Basic_types.String.Set.empty

    | Instr.Free (e, _)
    | Instr.DJump (e, _) -> no_defs (Expr.variables e)
    | Instr.If (c, _, _)
    | Instr.Assert (c, _)
    | Instr.Assume (c, _) -> no_defs (Expr.variables c)
    | Instr.NondetAssume (lvals, c, _) ->
      { defs =
          List.fold_left
            (fun s lv -> Basic_types.String.Set.union s (LValue.variables lv))
            Basic_types.String.Set.empty lvals;
        uses = Expr.variables c;
      }
    | Instr.Nondet (lval, _, _)
    | Instr.Undef (lval, _) ->
      { defs = LValue.variables lval; uses = Basic_types.String.Set.empty; }


  let temporaries = function
    | Instr.Malloc (lv, e, _)
    | Instr.Assign (lv, e, _) ->
      { defs = LValue.temporaries lv;
        uses = Expr.temporaries e;
      }
    | Instr.Print _
    | Instr.Stop _
    | Instr.SJump _ -> { defs = Basic_types.String.Set.empty;
                         uses = Basic_types.String.Set.empty; }
    | Instr.Free (e, _)
    | Instr.DJump (e, _) ->
      { defs = Basic_types.String.Set.empty;
        uses = Expr.temporaries e; }
    | Instr.If (c, _, _)
    | Instr.Assert (c, _)
    | Instr.Assume (c, _) ->
      { defs = Basic_types.String.Set.empty;
        uses = Expr.temporaries c; }
    | Instr.NondetAssume (lvals, c, _) ->
      { defs =
          List.fold_left
            (fun s lv -> Basic_types.String.Set.union s (LValue.temporaries lv))
            Basic_types.String.Set.empty
            lvals;
        uses = Expr.temporaries c; }
    | Instr.Nondet (lval, _, _)
    | Instr.Undef (lval, _) -> { defs = LValue.temporaries lval;
                                 uses = Basic_types.String.Set.empty; }


  let outer_jumps instr =
    match instr with
    | Instr.Assign _
    | Instr.Stop _
    | Instr.Assert _
    | Instr.Assume _
    | Instr.NondetAssume _
    | Instr.Nondet _
    | Instr.Undef _
    | Instr.Free _
    | Instr.Malloc _
    | Instr.DJump (_, (Some Return | None))
    | Instr.Print _ -> Virtual_address.Set.empty
    | Instr.DJump (_, Some Call a) ->
      Virtual_address.Set.singleton (Caddress.to_virtual_address a)
    | Instr.SJump (jt, Some Call a) ->
      Virtual_address.Set.add
        (Caddress.to_virtual_address a) (Jump_target.outer_jumps jt)
    | Instr.SJump (jt, (Some Return  | None))
    | Instr.If (_, jt, _) ->  Jump_target.outer_jumps jt

  let is_call = function
    | Instr.SJump (_, Some Call _)
    | Instr.DJump (_, Some Call _) -> true
    | _ -> false

  let is_return = function
    | Instr.SJump (_, Some Return)
    (* Returns are actually encoded mostly that way *)
    | Instr.DJump (_, Some Return)
      ->  true
    | _ -> false
end

module Statement = struct
  type t = {
    location : Caddress.t;
    instruction : Instr.t
  }

  let create a i = { location = a; instruction = i }
  let location li = li.location
  let instruction li = li.instruction

  let set_instruction li instruction = { li with instruction }
  let set_location li location = { li with location }

  let pp ppf li =
    Format.fprintf ppf "%a: %a"
      Dba_printer.Ascii.pp_code_address li.location
      Dba_printer.Ascii.pp_instruction li.instruction
end



type read_perm = Read of bool
type write_perm = Write of bool
type exec_perm =  Exec of bool
type permissions = Dba.Expr.t * (read_perm * write_perm * exec_perm)

type 'a program = {
  start_address : Dba.address;
  declarations : Declarations.t;
  permissions: permissions list Region.Map.t * Dba.Expr.t Rights.t;
  initializations : Dba.Instr.t list;
  instructions : 'a dbainstrmap;
}
