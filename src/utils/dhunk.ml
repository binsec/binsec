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

(* A block is a small CFG consisting of DBAs *)
type t = {
  instructions : Dba.Instr.t array;
  predecessors : int list array;
  exits : int list;
}

module Share = Weak.Make (struct
  include Dba.Instr

  let equal = ( = )
  let hash = Hashtbl.hash
end)

let share = Share.create 128

module G : sig
  include Graph.Sig.G with type t = t and type V.t = int

  val empty : t
  val singleton : Dba.Instr.t -> t
  val init : int -> (int -> Dba.Instr.t) -> t
end = struct
  type nonrec t = t

  let empty = { instructions = [||]; predecessors = [||]; exits = [] }

  let singleton instr =
    let instr = Share.merge share instr in
    match instr with
    | Dba.Instr.DJump _ | Dba.Instr.SJump (JOuter _, _) | Dba.Instr.Stop _ ->
        { instructions = [| instr |]; predecessors = [| [] |]; exits = [ 0 ] }
    | Dba.Instr.Assert (Cst bv, 0) when Bitvector.is_zero bv ->
        {
          instructions = [| instr |];
          predecessors = [| [ 0 ] |];
          exits = [ 0 ];
        }
    | Dba.Instr.Assert _ | Dba.Instr.Assign _ | Dba.Instr.Assume _
    | Dba.Instr.If _ | Dba.Instr.Nondet _ | Dba.Instr.SJump _
    | Dba.Instr.Undef _ ->
        raise (Invalid_argument "Instruction is not a terminator")

  let init n f =
    let exits = ref [] in
    let predecessors = Array.make n [] in
    let instructions =
      Array.init n (fun i ->
          let instr = Share.merge share (f i) in
          match instr with
          | Dba.Instr.DJump _ | Dba.Instr.SJump (JOuter _, _) | Dba.Instr.Stop _
            ->
              exits := i :: !exits;
              instr
          | Dba.Instr.If (_, JOuter _, fallthrough) ->
              exits := i :: !exits;
              predecessors.(fallthrough) <- i :: predecessors.(fallthrough);
              instr
          | Dba.Instr.If (_, JInner target, fallthrough) ->
              predecessors.(target) <- i :: predecessors.(target);
              predecessors.(fallthrough) <- i :: predecessors.(fallthrough);
              instr
          | Dba.Instr.Assert (_, fallthrough)
          | Dba.Instr.Assign (_, _, fallthrough)
          | Dba.Instr.Assume (_, fallthrough)
          | Dba.Instr.Nondet (_, fallthrough)
          | Dba.Instr.SJump (JInner fallthrough, _)
          | Dba.Instr.Undef (_, fallthrough) ->
              predecessors.(fallthrough) <- i :: predecessors.(fallthrough);
              instr)
    in
    { instructions; predecessors; exits = !exits }

  module V : Graph.Sig.VERTEX with type t = int = struct
    type t = int

    let compare a b = a - b
    let equal a b = a == b
    let hash = Fun.id

    type label = t

    let create = Fun.id
    let label = Fun.id
  end

  type vertex = V.t

  module E : Graph.Sig.EDGE with type t = V.t * V.t and type vertex = V.t =
  struct
    type t = V.t * V.t

    let compare = compare

    type vertex = V.t

    let src = fst
    let dst = snd

    type label = unit

    let create src () dst = (src, dst)
    let label _ = ()
  end

  type edge = E.t

  let is_directed = true
  let is_empty = function { instructions = [||]; _ } -> true | _ -> false
  let nb_vertex { instructions; _ } = Array.length instructions

  let nb_edges { predecessors; _ } =
    Array.fold_left
      (fun edges preds -> edges + List.length preds)
      0 predecessors

  let out_degree { instructions; _ } v =
    match instructions.(v) with
    | Dba.Instr.If (_, JInner _, _) -> 2
    | Dba.Instr.DJump _ | Dba.Instr.SJump (JOuter _, _) | Dba.Instr.Stop _ -> 0
    | Dba.Instr.Assert _ | Dba.Instr.Assign _ | Dba.Instr.Assume _
    | Dba.Instr.If _ | Dba.Instr.Nondet _ | Dba.Instr.SJump _
    | Dba.Instr.Undef _ ->
        1

  let in_degree { predecessors; _ } v = List.length predecessors.(v)
  let mem_vertex { instructions; _ } v = v < Array.length instructions

  let mem_edge { instructions; _ } src dst =
    match instructions.(src) with
    | exception Invalid_argument _ -> false
    | Dba.Instr.DJump _ | Dba.Instr.SJump (JOuter _, _) | Dba.Instr.Stop _ ->
        false
    | Dba.Instr.If (_, JInner target, fallthrough) ->
        dst = target || dst = fallthrough
    | Dba.Instr.Assert (_, fallthrough)
    | Dba.Instr.Assign (_, _, fallthrough)
    | Dba.Instr.Assume (_, fallthrough)
    | Dba.Instr.If (_, _, fallthrough)
    | Dba.Instr.Nondet (_, fallthrough)
    | Dba.Instr.SJump (JInner fallthrough, _)
    | Dba.Instr.Undef (_, fallthrough) ->
        dst = fallthrough

  let mem_edge_e t (src, dst) = mem_edge t src dst

  let find_edge t src dst =
    if mem_edge t src dst then (src, dst) else raise Not_found

  let find_all_edges t src dst =
    if mem_edge t src dst then [ (src, dst) ] else []

  let succ { instructions; _ } v =
    match instructions.(v) with
    | Dba.Instr.DJump _ | Dba.Instr.SJump (JOuter _, _) | Dba.Instr.Stop _ -> []
    | Dba.Instr.If (_, JInner target, fallthrough) -> [ target; fallthrough ]
    | Dba.Instr.Assert (_, fallthrough)
    | Dba.Instr.Assign (_, _, fallthrough)
    | Dba.Instr.Assume (_, fallthrough)
    | Dba.Instr.If (_, _, fallthrough)
    | Dba.Instr.Nondet (_, fallthrough)
    | Dba.Instr.SJump (JInner fallthrough, _)
    | Dba.Instr.Undef (_, fallthrough) ->
        [ fallthrough ]

  let pred { predecessors; _ } v = predecessors.(v)
  let succ_e t v = List.map (fun dst -> (v, dst)) (succ t v)
  let pred_e t v = List.map (fun src -> (src, v)) (pred t v)

  let iter_vertex f { instructions; _ } =
    for i = 0 to Array.length instructions - 1 do
      f i
    done

  let fold_vertex =
    let rec loop f n i x = if i < n then loop f n (i + 1) (f i x) else x in
    fun f { instructions; _ } x -> loop f (Array.length instructions) 0 x

  let iter_edges f { instructions; _ } =
    for i = 0 to Array.length instructions - 1 do
      match instructions.(i) with
      | Dba.Instr.DJump _ | Dba.Instr.SJump (JOuter _, _) | Dba.Instr.Stop _ ->
          ()
      | Dba.Instr.If (_, JInner target, fallthrough) ->
          f i target;
          f i fallthrough
      | Dba.Instr.Assert (_, fallthrough)
      | Dba.Instr.Assign (_, _, fallthrough)
      | Dba.Instr.Assume (_, fallthrough)
      | Dba.Instr.If (_, _, fallthrough)
      | Dba.Instr.Nondet (_, fallthrough)
      | Dba.Instr.SJump (JInner fallthrough, _)
      | Dba.Instr.Undef (_, fallthrough) ->
          f i fallthrough
    done

  let fold_edges =
    let rec loop instructions f n i x =
      if i < n then
        loop instructions f n (i + 1)
          (match instructions.(i) with
          | Dba.Instr.DJump _ | Dba.Instr.SJump (JOuter _, _) | Dba.Instr.Stop _
            ->
              x
          | Dba.Instr.If (_, JInner target, fallthrough) ->
              f i target (f i fallthrough x)
          | Dba.Instr.Assert (_, fallthrough)
          | Dba.Instr.Assign (_, _, fallthrough)
          | Dba.Instr.Assume (_, fallthrough)
          | Dba.Instr.If (_, _, fallthrough)
          | Dba.Instr.Nondet (_, fallthrough)
          | Dba.Instr.SJump (JInner fallthrough, _)
          | Dba.Instr.Undef (_, fallthrough) ->
              f i fallthrough x)
      else x
    in
    fun f { instructions; _ } x ->
      loop instructions f (Array.length instructions) 0 x

  let iter_edges_e f t = iter_edges (fun src dst -> f (src, dst)) t
  let fold_edges_e f t x = fold_edges (fun src dst x -> f (src, dst) x) t x

  let map_vertex f { instructions; _ } =
    let size = Array.length instructions in
    let transient = Array.make size (Dba.Instr.stop (Some Dba.KO)) in
    for i = 0 to size - 1 do
      transient.(f i) <-
        Share.merge share
          (Dba_types.Instruction.reloc ~inner:f instructions.(i))
    done;
    init size (fun i -> transient.(i))

  let iter_succ f { instructions; _ } v =
    match instructions.(v) with
    | Dba.Instr.DJump _ | Dba.Instr.SJump (JOuter _, _) | Dba.Instr.Stop _ -> ()
    | Dba.Instr.If (_, JInner target, fallthrough) ->
        f target;
        f fallthrough
    | Dba.Instr.Assert (_, fallthrough)
    | Dba.Instr.Assign (_, _, fallthrough)
    | Dba.Instr.Assume (_, fallthrough)
    | Dba.Instr.If (_, _, fallthrough)
    | Dba.Instr.Nondet (_, fallthrough)
    | Dba.Instr.SJump (JInner fallthrough, _)
    | Dba.Instr.Undef (_, fallthrough) ->
        f fallthrough

  let iter_pred f { predecessors; _ } v = List.iter f predecessors.(v)

  let fold_succ f { instructions; _ } v x =
    match instructions.(v) with
    | Dba.Instr.DJump _ | Dba.Instr.SJump (JOuter _, _) | Dba.Instr.Stop _ -> x
    | Dba.Instr.If (_, JInner target, fallthrough) -> f target (f fallthrough x)
    | Dba.Instr.Assert (_, fallthrough)
    | Dba.Instr.Assign (_, _, fallthrough)
    | Dba.Instr.Assume (_, fallthrough)
    | Dba.Instr.If (_, _, fallthrough)
    | Dba.Instr.Nondet (_, fallthrough)
    | Dba.Instr.SJump (JInner fallthrough, _)
    | Dba.Instr.Undef (_, fallthrough) ->
        f fallthrough x

  let fold_pred f { predecessors; _ } v x =
    List.fold_left (fun x src -> f src x) x predecessors.(v)

  let iter_succ_e f { instructions; _ } v =
    match instructions.(v) with
    | Dba.Instr.DJump _ | Dba.Instr.SJump (JOuter _, _) | Dba.Instr.Stop _ -> ()
    | Dba.Instr.If (_, JInner target, fallthrough) ->
        f (v, target);
        f (v, fallthrough)
    | Dba.Instr.Assert (_, fallthrough)
    | Dba.Instr.Assign (_, _, fallthrough)
    | Dba.Instr.Assume (_, fallthrough)
    | Dba.Instr.If (_, _, fallthrough)
    | Dba.Instr.Nondet (_, fallthrough)
    | Dba.Instr.SJump (JInner fallthrough, _)
    | Dba.Instr.Undef (_, fallthrough) ->
        f (v, fallthrough)

  let fold_succ_e f { instructions; _ } v x =
    match instructions.(v) with
    | Dba.Instr.DJump _ | Dba.Instr.SJump (JOuter _, _) | Dba.Instr.Stop _ -> x
    | Dba.Instr.If (_, JInner target, fallthrough) ->
        f (v, target) (f (v, fallthrough) x)
    | Dba.Instr.Assert (_, fallthrough)
    | Dba.Instr.Assign (_, _, fallthrough)
    | Dba.Instr.Assume (_, fallthrough)
    | Dba.Instr.If (_, _, fallthrough)
    | Dba.Instr.Nondet (_, fallthrough)
    | Dba.Instr.SJump (JInner fallthrough, _)
    | Dba.Instr.Undef (_, fallthrough) ->
        f (v, fallthrough) x

  let iter_pred_e f { predecessors; _ } v =
    List.iter (fun src -> f (src, v)) predecessors.(v)

  let fold_pred_e f { predecessors; _ } v x =
    List.fold_left (fun x src -> f (src, v) x) x predecessors.(v)
end

module DI = Dba_types.Instruction

let stop_ok = Share.merge share (Dba.Instr.stop (Some Dba.OK))

(* pred and succ are aliases *)
let pred = G.pred
let succ = G.succ
let empty = G.empty
let stop = G.singleton stop_ok
let inst_exn { instructions; _ } n = instructions.(n)
let inst g n = try Some (inst_exn g n) with Invalid_argument _ -> None
let init = G.init
let singleton = G.singleton

let goto vaddr =
  G.singleton (Share.merge share (Dba.Instr.static_outer_jump vaddr))

let unlink { instructions; predecessors; _ } i =
  let n =
    match Array.get instructions i with
    | DJump _ | SJump (JOuter _, _) | Stop _ | If _ ->
        raise (Invalid_argument "unlink")
    | Assert (_, n)
    | Assume (_, n)
    | Assign (_, _, n)
    | Nondet (_, n)
    | SJump (JInner n, _)
    | Undef (_, n) ->
        n
  in
  if i = 0 then
    Array.set instructions 0 (Share.merge share (Dba.Instr.static_inner_jump n))
  else
    let inner j = if j = i then n else j in
    Array.set predecessors n
      (List.fold_left
         (fun ps p ->
           Array.set instructions p
             (Dba_types.Instruction.reloc ~inner (Array.get instructions p));
           p :: ps)
         (List.filter (( <> ) i) (Array.get predecessors n))
         (Array.get predecessors i));
    Array.set predecessors i [];
    Array.set instructions i stop_ok

let length = G.nb_vertex
let is_empty = G.is_empty
let start _ = 0
let exits { exits; _ } = exits
let beginning_inst g = inst_exn g 0
let fold f acc { instructions; _ } = Array.fold_left f acc instructions

let of_list l =
  let a = Array.of_list l in
  init (Array.length a) (fun i -> a.(i))

let of_labelled_list l =
  let a = Array.of_list l in
  init (Array.length a) (fun i ->
      let j, instr = a.(i) in
      assert (i == j);
      instr)

let flatten { instructions; _ } =
  Array_utils.fold_righti
    (fun i list instr -> (i, instr) :: list)
    [] instructions

let to_list { instructions; _ } = Array.to_list instructions
let iteri ~f { instructions; _ } = Array.iteri f instructions
let iter ~f g = iteri ~f:(fun _ inst -> f inst) g

let copy { instructions; predecessors; exits } =
  {
    instructions = Array.copy instructions;
    predecessors = Array.copy predecessors;
    exits;
  }

let mapi ~f { instructions; _ } =
  init (Array.length instructions) (fun i -> f i instructions.(i))

exception Done

let for_all p g =
  try
    iter ~f:(fun i -> if not (p i) then raise_notrace Done) g;
    true
  with Done -> false

let pp ppf t =
  let open Format in
  fprintf ppf "@[<v 0>";
  flatten t
  |> List.iter (fun (addr, instr) ->
         fprintf ppf "@[<h>%2d: %a@]@ " addr
           (Dba_printer.Ascii.pp_instruction_maybe_goto ~current_id:addr)
           instr);
  fprintf ppf "@]"

let to_stmts t (address : Virtual_address.t) =
  let base = Dba_types.Caddress.block_start_of_int (address :> int) in
  let l = to_list t in
  List.mapi
    (fun i e -> Dba_types.Statement.create (Dba_types.Caddress.reid base i) e)
    l

let no_inner_reference instr = function
  | Dba.JOuter _ -> true
  | Dba.JInner _ as jt -> not (List.mem jt (DI.successors instr))

let _no_block_inner_references t n =
  let jt = Dba.Jump_target.inner n in
  for_all (fun instr -> no_inner_reference instr jt) t

let outer_jumps =
  fold
    (fun hwset instr ->
      let jset = DI.outer_jumps instr in
      Virtual_address.Set.union hwset jset)
    Virtual_address.Set.empty

let callees =
  fold
    (fun hwset dinstr ->
      let open Dba in
      match dinstr with
      | Instr.SJump (JOuter dst, Call _) ->
          (* Only this pattern marks a call instruction of which we know the
               target *)
          let a = Dba_types.Caddress.to_virtual_address dst in
          Virtual_address.Set.add a hwset
      | _ -> hwset)
    Virtual_address.Set.empty

module Var = struct
  include Dba.Var

  let compare (t : t) (t' : t) = t.id - t'.id

  module Set = struct
    include Set.Make (struct
      type nonrec t = t

      let compare = compare
    end)

    let _pp ppf t =
      if is_empty t then Format.pp_print_string ppf "{}"
      else
        let v = choose t in
        let t = remove v t in
        Format.pp_print_string ppf "{ ";
        Format.pp_print_string ppf v.name;
        iter
          (fun v ->
            Format.pp_print_string ppf ", ";
            Format.pp_print_string ppf v.name)
          t;
        Format.pp_print_string ppf " }"
  end

  let rec collect (e : Dba.Expr.t) (d : Set.t) : Set.t =
    match e with
    | Cst _ -> d
    | Var v -> Set.add v d
    | Load (_, _, e, _) | Unary (_, e) -> collect e d
    | Binary (_, e, e') -> collect e (collect e' d)
    | Ite (e, e', e'') -> collect e (collect e' (collect e'' d))

  let rec contains (v : t) (e : Dba.Expr.t) : bool =
    match e with
    | Cst _ -> false
    | Var v' -> equal v v'
    | Load (_, _, e, _) | Unary (_, e) -> contains v e
    | Binary (_, e, e') -> contains v e || contains v e'
    | Ite (e, e', e'') -> contains v e || contains v e' || contains v e''

  module Map = Map.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end

module Int = Basic_types.Int
module Leader = Graph.Leaderlist.Make (G)

let optimize ?(inplace = false) t =
  let t = if inplace then t else copy t in
  let outs =
    Var.Set.filter
      (fun (var : Var.t) -> var.info <> Var.Tag.Temp)
      (fold
         (fun d (i : DI.t) ->
           match i with
           | Assign ((Var var | Restrict (var, _)), exp, _) ->
               Var.(collect exp (Set.add var d))
           | Assign (Store (_, _, addr, _), exp, _) ->
               Var.(collect addr (collect exp d))
           | Undef ((Var var | Restrict (var, _)), _)
           | Nondet ((Var var | Restrict (var, _)), _) ->
               Var.Set.add var d
           | Undef (Store (_, _, addr, _), _) | Nondet (Store (_, _, addr, _), _)
             ->
               Var.collect addr d
           | Assume (exp, _) | Assert (exp, _) | If (exp, _, _) | DJump (exp, _)
             ->
               Var.collect exp d
           | SJump _ | Stop _ -> d)
         Var.Set.empty t)
  in
  let module Analyze = struct
    type data = Var.Set.t
    type edge = G.E.t
    type vertex = G.E.vertex
    type g = G.t

    let direction = Graph.Fixpoint.Backward
    let join = Var.Set.union
    let equal = Var.Set.equal

    let analyze e d =
      match inst_exn t (G.E.dst e) with
      | Assign (Var var, exp, _) ->
          if Var.Set.mem var d then Var.collect exp (Var.Set.remove var d)
          else d
      | Assign (Restrict (var, _), exp, _) ->
          if Var.Set.mem var d then Var.collect exp d else d
      | Assign (Store (_, _, addr, _), exp, _) ->
          Var.(collect addr (collect exp d))
      | Undef (Var var, _) | Nondet (Var var, _) -> Var.Set.remove var d
      | Undef (Restrict _, _) | Nondet (Restrict _, _) -> d
      | Undef (Store (_, _, addr, _), _) | Nondet (Store (_, _, addr, _), _) ->
          Var.collect addr d
      | Assume (exp, _) | Assert (exp, _) | If (exp, _, _) | DJump (exp, _) ->
          Var.collect exp d
      | SJump _ | Stop _ -> d
  end in
  let module Liveness = Graph.Fixpoint.Make (G) (Analyze) in
  let liveness =
    Liveness.analyze
      (fun i ->
        match inst_exn t i with
        | DJump _ | SJump (JOuter _, _) | Stop _ | If (_, JOuter _, _) -> outs
        | Assign _ | Undef _ | Nondet _ | Assume _ | Assert _ | If _ | SJump _
          ->
            Var.Set.empty)
      t
  in
  iteri
    ~f:(fun i (k : DI.t) ->
      match k with
      | Assign ((Var v | Restrict (v, _)), _, _)
      | Undef ((Var v | Restrict (v, _)), _)
      | Nondet ((Var v | Restrict (v, _)), _)
        when Var.Set.mem v (liveness i) = false ->
          unlink t i
      | _ -> ())
    t;
  let rec associate o u i (e : Dba.Expr.t) =
    match e with
    | Cst _ -> u
    | Var v -> (
        try
          let j = Var.Map.find v o in
          if Int.Map.mem j u then Int.Map.add j None u
          else Int.Map.add j (Some i) u
        with Not_found -> u)
    | Load (_, _, e, _) | Unary (_, e) -> associate o u i e
    | Binary (_, e, e') -> associate o (associate o u i e) i e'
    | Ite (e, e', e'') ->
        associate o (associate o (associate o u i e) i e') i e''
  in
  List.iter
    (fun block ->
      let last = ref 0 in
      let origin, use =
        List.fold_left
          (fun (o, u) i ->
            last := i;
            match inst_exn t i with
            | Assign (Var var, exp, _) ->
                (Var.Map.add var i o, associate o u i exp)
            | Assign (Restrict (var, _), exp, _) ->
                let o = Var.Map.remove var o
                and u =
                  try
                    let j = Var.Map.find var o in
                    Int.Map.add j None u
                  with Not_found -> u
                in
                (o, Int.Map.add i None (associate o u i exp))
            | Assign (Store (_, _, addr, _), exp, _) ->
                (o, associate o (associate o u i addr) i exp)
            | Undef (Var var, _) | Nondet (Var var, _) ->
                (Var.Map.remove var o, u)
            | Undef (Restrict (var, _), _) | Nondet (Restrict (var, _), _) ->
                ( Var.Map.remove var o,
                  try
                    let j = Var.Map.find var o in
                    Int.Map.add j None u
                  with Not_found -> u )
            | Undef (Store (_, _, addr, _), _)
            | Nondet (Store (_, _, addr, _), _) ->
                (o, associate o u i addr)
            | Assume (exp, _) | Assert (exp, _) | If (exp, _, _) | DJump (exp, _)
              ->
                (o, associate o u i exp)
            | SJump _ | Stop _ -> (o, u))
          (Var.Map.empty, Int.Map.empty)
          block
      in
      let use =
        Var.Set.fold
          (fun v u ->
            try
              let j = Var.Map.find v origin in
              Int.Map.add j None u
            with Not_found -> u)
          (liveness !last) use
      in
      let rec subs_exp var value (e : Dba.Expr.t) =
        match e with
        | Cst _ -> e
        | Var var' -> if Var.equal var var' then value else e
        | Load (sz, en, e, array) ->
            Dba.Expr.load (Size.Byte.create sz) en (subs_exp var value e) ?array
        | Unary (f, e) -> Dba.Expr.unary f (subs_exp var value e)
        | Binary (f, e, e') ->
            Dba.Expr.binary f (subs_exp var value e) (subs_exp var value e')
        | Ite (e, e', e'') ->
            Dba.Expr.ite (subs_exp var value e) (subs_exp var value e')
              (subs_exp var value e'')
      in
      let subs_lval var value (lv : Dba.LValue.t) =
        match lv with
        | Var _ -> lv
        | Restrict _ -> lv
        | Store (sz, en, addr, array) ->
            Dba.LValue.store (Size.Byte.create sz) en (subs_exp var value addr)
              ?array
      in
      let rec contains_mem (e : Dba.Expr.t) =
        match e with
        | Cst _ -> false
        | Var _ -> false
        | Load _ -> true
        | Unary (_, e) -> contains_mem e
        | Binary (_, e, e') -> contains_mem e || contains_mem e'
        | Ite (e, e', e'') ->
            contains_mem e || contains_mem e' || contains_mem e''
      in
      Int.Map.iter
        (fun o u ->
          match u with
          | None -> ()
          | Some i -> (
              match inst_exn t o with
              | Assign (Var var, exp, n) ->
                  let mem_barrier = contains_mem exp in
                  let rec inline n =
                    if n = i then (
                      Array.set t.instructions i
                        (match inst_exn t n with
                        | Assign (lv, rv, n) ->
                            Dba.Instr.assign (subs_lval var exp lv)
                              (subs_exp var exp rv) n
                        | Undef (lv, n) ->
                            Dba.Instr.undefined (subs_lval var exp lv) n
                        | Nondet (lv, n) ->
                            Dba.Instr.non_deterministic (subs_lval var exp lv) n
                        | Assert (test, n) ->
                            Dba.Instr._assert (subs_exp var exp test) n
                        | Assume (test, n) ->
                            Dba.Instr.assume (subs_exp var exp test) n
                        | If (test, branch, n) ->
                            Dba.Instr.ite (subs_exp var exp test) branch n
                        | DJump (target, tag) ->
                            Dba.Instr.dynamic_jump (subs_exp var exp target)
                              ~tag
                        | SJump _ | Stop _ -> assert false);
                      unlink t o)
                    else
                      match inst_exn t n with
                      | DJump _ | SJump (JOuter _, _) | Stop _ | If _ ->
                          assert false
                      | Assign ((Var var | Restrict (var, _)), _, n)
                      | Nondet ((Var var | Restrict (var, _)), n)
                      | Undef ((Var var | Restrict (var, _)), n) ->
                          if Var.contains var exp then () else inline n
                      | Assign (Store _, _, n)
                      | Nondet (Store _, n)
                      | Undef (Store _, n) ->
                          if not mem_barrier then inline n
                      | Assert (_, n) | Assume (_, n) | SJump (JInner n, _) ->
                          inline n
                  in
                  inline n
              | _ -> assert false))
        use)
    (Leader.leader_lists t 0);
  t

module Logger = Logger.Make (struct
  let name = "hunk"
end)

(* TODO: take possible failures into account *)
let export_to_file g =
  let filename = Filename.temp_file "dba" ".dot" in
  Logger.debug ~level:4 "Exporting graph to file %s" filename;
  let oc = open_out_bin filename in
  let module C_dot = struct
    include G

    let graph_attributes _g = []
    let default_vertex_attributes _ = [ `Shape `Box ]

    let vertex_name v =
      Format.asprintf "\"%d: %a\"" v Dba_printer.Ascii.pp_instruction
        (inst_exn g v)

    let vertex_attributes _v = []
    let get_subgraph _ = None
    let default_edge_attributes _ = []
    let edge_attributes _ = []
  end in
  let module D = Graph.Graphviz.Dot (C_dot) in
  D.output_graph oc g;
  close_out oc;
  filename

let view ~viewer filename =
  let svg_filename = Filename.chop_extension filename ^ ".svg" in
  Logger.debug ~level:4 "Exporting graph to SVG file %s" svg_filename;
  let cmd = Printf.sprintf "dot -T svg %s > %s" filename svg_filename in
  ignore (Sys.command cmd);
  let view_cmd = Printf.sprintf "%s %s" viewer svg_filename in
  ignore (Sys.command view_cmd)

let export_and_view ?(cmd = "firefox") g = export_to_file g |> view ~viewer:cmd

(* [is_return] actually checks if the graph is a linear suite of instruction
   terminated by a return jump.
*)
let is_return g =
  let rec aux node =
    let dinst = inst_exn g node in
    DI.is_return dinst || match G.succ g node with [ v ] -> aux v | _ -> false
  in
  (not (G.is_empty g)) && aux 0

let has_indirect_jump { instructions; exits; _ } =
  List.exists
    (fun i ->
      match instructions.(i) with Dba.Instr.DJump _ -> true | _ -> false)
    exits

module Check = struct
  let inner_jump_inside_bound t label = label >= 0 && label < length t

  let get_inner_jumps =
    let open Dba in
    let aux acc = function
      | Instr.SJump (JInner id, _) | Instr.If (_, JInner id, _) -> id :: acc
      | Instr.If _ | Instr.SJump _ | Instr.DJump _ | Instr.Assign _
      | Instr.Stop _ | Instr.Assert _ | Instr.Assume _ | Instr.Nondet _
      | Instr.Undef _ ->
          acc
    in
    fold aux []

  let has_inbound_inner_jumps t =
    get_inner_jumps t |> List.for_all (inner_jump_inside_bound t)

  exception Undeclared_Variable of string * Dba.Instr.t

  let no_undeclared_variables decls t =
    let no_undeclared_at_instr i =
      let du = DI.variables i in
      let vset =
        let open Dba_types in
        Basic_types.String.Set.union du.uses du.defs
      in
      try
        Basic_types.String.Set.iter
          (fun vname ->
            if not (Basic_types.String.Map.mem vname decls) then
              raise (Undeclared_Variable (vname, i)))
          vset;
        true
      with Undeclared_Variable (vname, instr) ->
        Logger.fatal "Undeclared variable %s at instruction %a" vname
          Dba_printer.Ascii.pp_instruction instr
    in
    for_all no_undeclared_at_instr t

  exception Temporaries_undefined of Basic_types.String.Set.t * Dba.Instr.t

  let no_temporary_leak g =
    let module Strg = Basic_types.String in
    let start = 0 in
    let init v = (v = start, (DI.temporaries (inst_exn g v)).Dba_types.defs) in
    let module N =
      Graph.Fixpoint.Make
        (G)
        (struct
          type g = G.t
          type edge = G.E.t
          type vertex = G.V.t

          (* fact = reachable flag * defined temporaries *)
          type data = bool * Basic_types.String.Set.t

          let direction = Graph.Fixpoint.Forward
          let join (r, s) (r', s') = (r || r', Basic_types.String.Set.union s s')

          let equal (r, s) (r', s') =
            r = r' && Basic_types.String.Set.equal s s'

          let analyze e (r, s) =
            let src = G.E.src e in
            ( r,
              Basic_types.String.Set.union
                (DI.temporaries (inst_exn g src)).Dba_types.defs s )
        end)
    in
    let f = N.analyze init g in
    Logger.debug ~level:6 "@[<v 0>%a@]"
      (fun ppf g ->
        G.iter_vertex
          (fun v ->
            Format.fprintf ppf "%d: %a [%a]@ " v
              Dba_printer.Ascii.pp_instruction (inst_exn g v)
              (fun ppf s ->
                Strg.Set.iter (fun name -> Format.fprintf ppf "%s; " name) s)
              (snd (f v)))
          g)
      g;
    try
      Array.iteri
        (fun v inst ->
          let reachable, defined = f v in
          if not reachable then g.instructions.(v) <- stop.instructions.(0)
          else
            let du = DI.temporaries inst in
            let undefined_temporaries =
              Strg.Set.diff du.Dba_types.uses defined
            in
            if not (Strg.Set.is_empty undefined_temporaries) then
              raise (Temporaries_undefined (undefined_temporaries, inst)))
        g.instructions;
      true
    with Temporaries_undefined (tset, instr) ->
      export_and_view g;
      Logger.fatal
        "@[<h>Temporaries %a were previously undefined but used at instruction \
         %a@]"
        (fun ppf set ->
          Basic_types.String.Set.iter
            (fun s -> Format.fprintf ppf "%s;@ " s)
            set)
        tset Dba_printer.Ascii.pp_instruction instr
end

type conditional = {
  condition : Dba.Expr.t;
  consequent : Virtual_address.t;
  alternative : Virtual_address.t;
}

let conditional g =
  if length g <> 2 then None
  else
    match beginning_inst g with
    | Dba.Instr.If (condition, Dba.JOuter consequent, fallthrough) -> (
        match inst_exn g fallthrough with
        | Dba.Instr.SJump (Dba.JOuter alternative, _) ->
            let open Dba_types.Caddress in
            Some
              {
                condition;
                consequent = to_virtual_address consequent;
                alternative = to_virtual_address alternative;
              }
        | _ -> None)
    | _ -> None

module Constant_propagation = struct
  open Dba

  module Env = struct
    include Basic_types.String.Map

    let eq = ( = )
    (* Maybe this is not the right equality for region * Bv.t type *)

    (* Test if env1 contains env2 *)
    let contains env1 env2 =
      let mem vname cst =
        match find vname env1 with
        | v -> eq v cst
        | exception Not_found -> false
      in
      for_all mem env2

    let add vname cst env =
      match find vname env with
      | v -> if eq v cst then env else remove vname env
      | exception Not_found -> add vname cst env
  end

  let rec eval_expr env = function
    | Dba.Expr.Var v as e -> (
        match Basic_types.String.Map.find v.name env with
        | bv -> Expr.constant bv
        | exception Not_found -> e)
    | Dba.Expr.Load (sz, en, e, array) ->
        let sz = Size.Byte.create sz in
        Expr.load sz en (eval_expr env e) ?array
    | Dba.Expr.Cst _ as e -> e
    | Dba.Expr.Unary (uop, e) -> Expr.unary uop (eval_expr env e)
    | Dba.Expr.Binary (bop, e1, e2) ->
        Expr.binary bop (eval_expr env e1) (eval_expr env e2)
    | Dba.Expr.Ite (c, e1, e2) ->
        Expr.ite (eval_expr env c) (eval_expr env e1) (eval_expr env e2)

  let eval_instruction penv i =
    match i with
    | Dba.Instr.Assign (lv, e, id) -> Instr.assign lv (eval_expr penv e) id
    | Dba.Instr.DJump (e, tag) -> Instr.dynamic_jump ~tag (eval_expr penv e)
    | Dba.Instr.If (c, jt, id) -> Instr.ite (eval_expr penv c) jt id
    | Dba.Instr.Assert (c, id) -> Instr._assert (eval_expr penv c) id
    | Dba.Instr.Assume (c, id) -> Instr.assume (eval_expr penv c) id
    | ( Dba.Instr.Undef _ | Dba.Instr.Nondet _ | Dba.Instr.Stop _
      | Dba.Instr.SJump _ ) as instr ->
        instr

  let gather_propagations ?(env = Env.empty) block =
    (* All elements are initialized at None *)
    let envs = to_list block |> List.map (fun _ -> None) |> Array.of_list in
    let should_propagate env id =
      match envs.(id) with
      | None -> true (* this index was never visited *)
      | Some e -> not (Env.contains env e)
    in
    let mark_env env idx = envs.(idx) <- Some env in
    let remove lval env =
      match Dba_types.LValue.name_of lval with
      | Some vname -> Basic_types.String.Map.remove vname env
      | None -> env
    in
    let rec loop env idx =
      if should_propagate env idx then (
        mark_env env idx;
        match inst block idx with
        | None -> env
        | Some i -> (
            match i with
            | Dba.Instr.Assign (Dba.LValue.Var { name; _ }, Dba.Expr.Cst v, idx')
              ->
                loop (Env.add name v env) idx'
            | Dba.Instr.If (_, Dba.JInner idx1, idx2) ->
                loop (loop env idx1) idx2
            | Dba.Instr.Nondet (lv, id) -> loop (remove lv env) id
            | Dba.Instr.Assert (_, id)
            | Dba.Instr.Assume (_, id)
            | Dba.Instr.Undef (_, id)
            | Dba.Instr.Assign (_, _, id)
            | Dba.Instr.SJump (Dba.JInner id, _)
            | Dba.Instr.If (_, Dba.JOuter _, id) ->
                loop env id
            | Dba.Instr.SJump (Dba.JOuter _, _)
            | Dba.Instr.DJump _ | Dba.Instr.Stop _ ->
                env))
      else env
    in
    ignore (loop env (start block));
    envs

  let do_propagations block propagation_envs =
    mapi
      ~f:(fun i instruction ->
        match propagation_envs.(i) with
        | None -> instruction
        | Some env ->
            if Basic_types.String.Map.is_empty env then instruction
            else eval_instruction env instruction)
      block

  let eval block =
    Logger.debug ~level:5 "@[<v 0>Prepropagation@ %a@]" pp block;
    let b = gather_propagations block |> do_propagations block in
    Logger.debug ~level:5 "@[<v 0>Post-propagation@ %a@]" pp b;
    b
end

let constant_propagation = Constant_propagation.eval

module DC_elimination = struct
  module M = Basic_types.Int.Map
  module S = Basic_types.Int.Set

  let fetch target src p =
    let alias = try src :: M.find src p with Not_found -> [ src ] in
    try M.add target (List.append alias (M.find target p)) p
    with Not_found -> M.add target alias p

  let eval block =
    let rec collect b n r m p w =
      match S.min_elt w with
      | exception Not_found -> (n, r, m)
      | i when M.mem i m -> collect b n r m p (S.remove i w)
      | i -> (
          match inst_exn b i with
          | Dba.Instr.SJump (Dba.JInner goto, _) ->
              collect b n r m (fetch goto i p) S.(add goto (remove i w))
          | inst ->
              let m =
                try List.fold_left (fun m i -> M.add i n m) m (M.find i p)
                with Not_found -> m
              in
              collect b (n + 1) (M.add n i r) (M.add i n m) p
                (List.fold_left
                   (fun w -> function
                     | Dba.JOuter _ -> w
                     | Dba.JInner id -> S.add id w)
                   (S.remove i w)
                   (Dba_types.Instruction.successors inst)))
    in
    let n, r, m = collect block 0 M.empty M.empty M.empty (S.singleton 0) in
    let inner i = try M.find i m with Not_found -> i in
    init n (fun i ->
        Dba_types.Instruction.reloc ~inner (inst_exn block (M.find i r)))
end

let dead_code_elimination = DC_elimination.eval
