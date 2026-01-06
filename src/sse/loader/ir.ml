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

module StrTbl = Basic_types.String.Htbl

module IntTbl = Hmap.Make (struct
  type t = int

  let hash = Fun.id
  let compare = ( - )
end)

module IntSet = Basic_types.Integers.Int.Set

module Var = struct
  module Tag = Dba.Var.Tag
  include Dba_types.Var

  let collect = Dba_types.Expr.collect_variables
end

type builtin = ..
type builtin += Inline of Dhunk.t | EndOfHook

let (pp_builtin, register_builtin_printer) :
    (Format.formatter -> builtin -> unit)
    * ((Format.formatter -> builtin -> bool) -> unit) =
  let rec pp_builtin ppf x printers =
    match printers with
    | [] -> Format.pp_print_string ppf "unknown builtin"
    | printer :: printers ->
        if not (printer ppf x) then pp_builtin ppf x printers
  in
  let builtin_printers =
    ref
      [
        (fun ppf builtin ->
          match builtin with
          | EndOfHook ->
              Format.pp_print_string ppf "end of hook";
              true
          | _ -> false);
      ]
  in
  ( (fun ppf x -> pp_builtin ppf x !builtin_printers),
    fun printer -> builtin_printers := printer :: !builtin_printers )

type 'a opcode =
  | Nop : [ `Fallthrough ] opcode
  | Instruction : Instruction.t -> [< `Label | `Fallthrough ] opcode
  | Hook : {
      addr : Virtual_address.t;
      info : string;
    }
      -> [< `Label | `Fallthrough ] opcode
  | Assign : { var : Dba.Var.t; rval : Dba.Expr.t } -> [ `Fallthrough ] opcode
  | Clobber : Dba.Var.t -> [ `Fallthrough ] opcode
  | Forget : Dba.Var.t -> [ `Fallthrough ] opcode
  | Load : {
      var : Dba.Var.t;
      base : string option;
      dir : Machine.endianness;
      addr : Dba.Expr.t;
    }
      -> [ `Fallthrough ] opcode
  | Store : {
      base : string option;
      dir : Machine.endianness;
      addr : Dba.Expr.t;
      rval : Dba.Expr.t;
    }
      -> [ `Fallthrough ] opcode
  | Symbolize : Dba.Var.t -> [ `Fallthrough ] opcode
  | Assume : Dba.Expr.t -> [ `Fallthrough ] opcode
  | Assert : Dba.Expr.t -> [ `Fallthrough ] opcode
  | Builtin : builtin -> [< `Fallthrough | `Terminator ] opcode
  | Goto : {
      target : Virtual_address.t;
      tag : Dba.tag;
    }
      -> [< `Fallthrough | `Terminator ] opcode
  | Jump : { target : Dba.Expr.t; tag : Dba.tag } -> [ `Terminator ] opcode
  | Halt : [ `Terminator ] opcode
  | Cut : [ `Terminator ] opcode
  | Die : string -> [ `Terminator ] opcode

and label = [ `Label ] opcode
and fallthrough = [ `Fallthrough ] opcode
and terminator = [ `Terminator ] opcode

let pp_opcode : type a. Format.formatter -> a opcode -> unit =
 fun ppf fallthrough ->
  match fallthrough with
  | Nop -> Format.pp_print_string ppf "nop"
  | Instruction inst ->
      Format.fprintf ppf "%a %a" Virtual_address.pp (Instruction.address inst)
        Mnemonic.pp
        (Instruction.mnemonic inst)
  | Hook { addr; info } ->
      Format.fprintf ppf "%a %s" Virtual_address.pp addr info
  | Assign { var = { name; _ }; rval } ->
      Format.fprintf ppf "%s := %a" name Dba_printer.Ascii.pp_bl_term rval
  | Clobber { name; _ } -> Format.fprintf ppf "%s := undef" name
  | Forget { name; _ } -> Format.fprintf ppf "%s := undef" name
  | Load { var = { name; size; _ }; base; dir; addr } ->
      Format.fprintf ppf "%s := %s[%a%s, %d]" name
        (Option.fold ~none:"@" ~some:Fun.id base)
        Dba_printer.Ascii.pp_bl_term addr
        (match dir with LittleEndian -> "" | BigEndian -> ", ->")
        (size / 8)
  | Store { base; dir; addr; rval } ->
      Format.fprintf ppf "%s[%a%s, %d] := %a"
        (Option.fold ~none:"@" ~some:Fun.id base)
        Dba_printer.Ascii.pp_bl_term addr
        (match dir with LittleEndian -> "" | BigEndian -> ", ->")
        (Dba.Expr.size_of rval / 8)
        Dba_printer.Ascii.pp_bl_term rval
  | Symbolize { name; _ } -> Format.fprintf ppf "%s := nondet" name
  | Assume test ->
      Format.fprintf ppf "assume %a" Dba_printer.Ascii.pp_bl_term test
  | Assert test ->
      Format.fprintf ppf "assert %a" Dba_printer.Ascii.pp_bl_term test
  | Goto { target; _ } ->
      Format.fprintf ppf "jump at %a" Virtual_address.pp target
  | Jump { target; _ } ->
      Format.fprintf ppf "jump at %a" Dba_printer.Ascii.pp_bl_term target
  | Halt -> Format.pp_print_string ppf "halt"
  | Cut -> Format.pp_print_string ppf "cut"
  | Die msg -> Format.fprintf ppf "die(%S)" msg
  | Builtin x -> pp_builtin ppf x

type node =
  | Fallthrough of { label : label; kind : fallthrough; succ : int }
  | Branch of {
      label : label;
      test : Dba.Expr.t;
      target : int;
      fallthrough : int;
    }
  | Terminator of { label : label; kind : terminator }

let pp_node ppf node =
  match node with
  | Fallthrough { kind; _ } -> pp_opcode ppf kind
  | Branch { test; target; fallthrough; _ } ->
      Format.fprintf ppf "if %a then goto %d else goto %d"
        Dba_printer.Ascii.pp_bl_term test target fallthrough
  | Terminator { kind; _ } -> pp_opcode ppf kind

let label_of : node -> label = function
  | Fallthrough { label; _ } | Branch { label; _ } | Terminator { label; _ } ->
      label

type stmt =
  | Nop
  | Label of string  (** [label]: *)
  | Opcode of fallthrough
  | If of Dba.Expr.t * string  (** if [rval] then goto [label] *)
  | Goto of string  (** goto [label] *)
  | End of terminator

module type GRAPH = sig
  include Graph.Sig.G with type V.t = int and type E.t = int * bool * int

  val node : t -> vertex -> node
  val fold_entries : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_exits : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
  val iter_entries : (vertex -> unit) -> t -> unit
  val iter_exits : (vertex -> unit) -> t -> unit
end

module type INSTRUMENT = sig
  type t
  type vertex

  val insert_before : t -> vertex -> ?label:label -> fallthrough -> unit
  val insert_before_v : t -> vertex -> ?label:label -> fallthrough -> vertex

  val insert_list_before :
    t -> vertex -> ?label:label -> fallthrough list -> unit

  val insert_list_before_v :
    t -> vertex -> ?label:label -> fallthrough list -> vertex
end

module Graph = struct
  type t = {
    mutable n : int;
    mutable nodes : node IntTbl.t;
    mutable preds : int list IntTbl.t;
    mutable entries : IntSet.t;
    mutable exits : IntSet.t;
  }

  let length : t -> int = fun { n; _ } -> n

  type vertex = int

  let add_node : t -> vertex -> node -> unit =
   fun t vertex node ->
    (match IntTbl.find vertex t.nodes with
    | exception Not_found -> t.n <- t.n + 1
    | Fallthrough { succ; _ } ->
        t.preds <-
          IntTbl.add succ
            (List.filter (( != ) vertex) (IntTbl.find succ t.preds))
            t.preds
    | Branch { target; fallthrough; _ } ->
        t.preds <-
          IntTbl.add target
            (List.filter (( != ) vertex) (IntTbl.find target t.preds))
            t.preds;
        t.preds <-
          IntTbl.add fallthrough
            (List.filter (( != ) vertex) (IntTbl.find fallthrough t.preds))
            t.preds
    | Terminator _ -> t.exits <- IntSet.remove vertex t.exits);
    t.nodes <- IntTbl.add vertex node t.nodes;
    if not (IntTbl.mem vertex t.preds) then
      t.preds <- IntTbl.add vertex [] t.preds;
    match node with
    | Fallthrough { succ; _ } ->
        t.preds <-
          IntTbl.add succ
            (vertex :: (try IntTbl.find succ t.preds with Not_found -> []))
            t.preds
    | Branch { target; fallthrough; _ } ->
        t.preds <-
          IntTbl.add target
            (vertex :: (try IntTbl.find target t.preds with Not_found -> []))
            t.preds;
        t.preds <-
          IntTbl.add fallthrough
            (vertex
            :: (try IntTbl.find fallthrough t.preds with Not_found -> []))
            t.preds
    | Terminator _ -> t.exits <- IntSet.add vertex t.exits

  let node { nodes; _ } vertex = IntTbl.find vertex nodes

  module V : Graph.Sig.VERTEX with type t = int = struct
    type t = vertex

    let compare = ( - )
    let equal = ( == )
    let hash = Fun.id

    type label = t

    let create = Fun.id
    let label = Fun.id
  end

  module E :
    Graph.Sig.EDGE with type t = V.t * bool * V.t and type vertex = V.t = struct
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
  let is_empty t = length t = 0
  let nb_vertex = length

  let nb_edges { preds; _ } =
    IntTbl.fold (fun _ preds n -> n + List.length preds) preds 0

  let out_degree { nodes; _ } vertex =
    match IntTbl.find vertex nodes with
    | Terminator _ -> 0
    | Fallthrough _ -> 1
    | Branch _ -> 2

  let in_degree { preds; _ } vertex = List.length (IntTbl.find vertex preds)
  let mem_vertex { nodes; _ } vertex = IntTbl.mem vertex nodes

  let mem_edge { nodes; _ } src dst =
    match IntTbl.find src nodes with
    | Fallthrough { succ; _ } -> succ = dst
    | Branch { target; fallthrough; _ } -> target = dst || fallthrough = dst
    | Terminator _ -> false

  let mem_edge_e { nodes; _ } (src, branch, dst) =
    match IntTbl.find src nodes with
    | Fallthrough { succ; _ } -> succ = dst
    | Branch { target; fallthrough; _ } ->
        (branch && target = dst) || ((not branch) && fallthrough = dst)
    | Terminator _ -> false

  let find_edge { nodes; _ } src dst =
    match IntTbl.find src nodes with
    | (Fallthrough { succ; _ } | Branch { fallthrough = succ; _ })
      when succ = dst ->
        (src, false, dst)
    | Branch { target; _ } when target = dst -> (src, true, dst)
    | Fallthrough _ | Branch _ | Terminator _ -> raise Not_found

  let find_all_edges t src dst =
    try [ find_edge t src dst ] with Not_found -> []

  let succ { nodes; _ } vertex =
    match IntTbl.find vertex nodes with
    | Fallthrough { succ; _ } -> [ succ ]
    | Branch { target; fallthrough; _ } -> [ target; fallthrough ]
    | Terminator _ -> []

  let pred { preds; _ } vertex = IntTbl.find vertex preds

  let succ_e { nodes; _ } vertex =
    match IntTbl.find vertex nodes with
    | Fallthrough { succ; _ } -> [ (vertex, false, succ) ]
    | Branch { target; fallthrough; _ } ->
        [ (vertex, true, target); (vertex, false, fallthrough) ]
    | Terminator _ -> []

  let pred_e t vertex =
    List.map (fun src -> find_edge t src vertex) (pred t vertex)

  let iter_vertex f t =
    let last = length t - 1 in
    for i = 0 to last do
      f i
    done

  let fold_entries : (vertex -> 'a -> 'a) -> t -> 'a -> 'a =
   fun f { entries; _ } a -> IntSet.fold f entries a

  let fold_exits : (vertex -> 'a -> 'a) -> t -> 'a -> 'a =
   fun f { exits; _ } a -> IntSet.fold f exits a

  let iter_entries : (vertex -> unit) -> t -> unit =
   fun f { entries; _ } -> IntSet.iter f entries

  let iter_exits : (vertex -> unit) -> t -> unit =
   fun f { exits; _ } -> IntSet.iter f exits

  let fold_vertex f { nodes; _ } data =
    IntTbl.fold (fun vertex _ -> f vertex) nodes data

  let iter_edges f { nodes; _ } =
    IntTbl.iter
      (fun vertex node ->
        match node with
        | Fallthrough { succ; _ } -> f vertex succ
        | Branch { target; fallthrough; _ } ->
            f vertex target;
            f vertex fallthrough
        | Terminator _ -> ())
      nodes

  let fold_edges f { nodes; _ } data =
    IntTbl.fold
      (fun vertex node data ->
        match node with
        | Fallthrough { succ; _ } -> f vertex succ data
        | Branch { target; fallthrough; _ } ->
            f vertex target (f vertex fallthrough data)
        | Terminator _ -> data)
      nodes data

  let iter_edges_e f { nodes; _ } =
    IntTbl.iter
      (fun vertex node ->
        match node with
        | Fallthrough { succ; _ } -> f (vertex, false, succ)
        | Branch { target; fallthrough; _ } ->
            f (vertex, true, target);
            f (vertex, false, fallthrough)
        | Terminator _ -> ())
      nodes

  let fold_edges_e f { nodes; _ } data =
    IntTbl.fold
      (fun vertex node data ->
        match node with
        | Fallthrough { succ; _ } -> f (vertex, false, succ) data
        | Branch { target; fallthrough; _ } ->
            f (vertex, true, target) (f (vertex, false, fallthrough) data)
        | Terminator _ -> data)
      nodes data

  let shuffle f node =
    match node with
    | Fallthrough { label; kind; succ } ->
        Fallthrough { label; kind; succ = f succ }
    | Branch { label; test; target; fallthrough } ->
        Branch { label; test; target = f target; fallthrough = f fallthrough }
    | Terminator _ -> node

  let map_vertex f r =
    {
      n = r.n;
      nodes =
        IntTbl.fold
          (fun vertex node nodes ->
            IntTbl.add (f vertex) (shuffle f node) nodes)
          r.nodes IntTbl.empty;
      preds =
        IntTbl.fold
          (fun vertex pred preds ->
            IntTbl.add (f vertex) (List.map f pred) preds)
          r.preds IntTbl.empty;
      exits = IntSet.map f r.exits;
      entries = IntSet.map f r.entries;
    }

  let iter_succ f { nodes; _ } vertex =
    match IntTbl.find vertex nodes with
    | Fallthrough { succ; _ } -> f succ
    | Branch { target; fallthrough; _ } ->
        f target;
        f fallthrough
    | Terminator _ -> ()

  let iter_pred f { preds; _ } vertex = List.iter f (IntTbl.find vertex preds)

  let fold_succ f { nodes; _ } vertex data =
    match IntTbl.find vertex nodes with
    | Fallthrough { succ; _ } -> f succ data
    | Branch { target; fallthrough; _ } -> f target (f fallthrough data)
    | Terminator _ -> data

  let fold_pred f { preds; _ } vertex data =
    List.fold_left (Fun.flip f) data (IntTbl.find vertex preds)

  let iter_succ_e f { nodes; _ } vertex =
    match IntTbl.find vertex nodes with
    | Fallthrough { succ; _ } -> f (vertex, false, succ)
    | Branch { target; fallthrough; _ } ->
        f (vertex, true, target);
        f (vertex, false, fallthrough)
    | Terminator _ -> ()

  let iter_pred_e f t vertex =
    List.iter
      (fun src -> f (find_edge t src vertex))
      (IntTbl.find vertex t.preds)

  let fold_succ_e f { nodes; _ } vertex data =
    match IntTbl.find vertex nodes with
    | Fallthrough { succ; _ } -> f (vertex, false, succ) data
    | Branch { target; fallthrough; _ } ->
        f (vertex, true, target) (f (vertex, false, fallthrough) data)
    | Terminator _ -> data

  let fold_pred_e f t vertex data =
    List.fold_left
      (fun data src -> f (find_edge t src vertex) data)
      data
      (IntTbl.find vertex t.preds)

  let insert_before_v : t -> vertex -> ?label:label -> fallthrough -> vertex =
   fun t vertex ?label kind ->
    let cur = length t
    and label =
      match label with None -> label_of (node t vertex) | Some label -> label
    in
    iter_pred
      (fun pred ->
        add_node t pred
          (shuffle (fun i -> if i = vertex then cur else i) (node t pred)))
      t vertex;
    add_node t cur (Fallthrough { label; kind; succ = vertex });
    cur

  let insert_before : t -> vertex -> ?label:label -> fallthrough -> unit =
   fun t vertex ?label kind -> ignore (insert_before_v t vertex ?label kind)

  let insert_list_before_v :
      t -> vertex -> ?label:label -> fallthrough list -> vertex =
   fun t vertex ?label -> function
    | [] -> vertex
    | kind :: tl ->
        let vertex' = insert_before_v t vertex ?label kind in
        List.iter (fun kind -> insert_before t vertex ?label kind) tl;
        vertex'

  let insert_list_before :
      t -> vertex -> ?label:label -> fallthrough list -> unit =
   fun t vertex ?label list ->
    ignore (insert_list_before_v t vertex ?label list)

  let extract_loads :
      ((vertex * Basic_types.endianness * Dba.Expr.t * string option) * Var.t)
      list ->
      Dba.Expr.t ->
      ((vertex * Basic_types.endianness * Dba.Expr.t * string option) * Var.t)
      list
      * Dba.Expr.t =
    let rec fold :
        ((vertex * Basic_types.endianness * Dba.Expr.t * string option) * Var.t)
        list ->
        Dba.Expr.t ->
        ((vertex * Basic_types.endianness * Dba.Expr.t * string option) * Var.t)
        list
        * Dba.Expr.t =
     fun m (e : Dba.Expr.t) ->
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
          ((k, v) :: m', Dba.Expr.v v)
      | Unary (o, x) ->
          let m', x' = fold m x in
          let e' = if x == x' then e else Dba.Expr.unary o x' in
          (m', e')
      | Binary (o, x, y) ->
          let m', x' = fold m x in
          let m', y' = fold m' y in
          let e' = if x == x' && y == y' then e else Dba.Expr.binary o x' y' in
          (m', e')
      | Ite (c, x, y) ->
          let m', c' = fold m c in
          let m', x' = fold m' x in
          let m', y' = fold m' y in
          let e' =
            if c == c' && x == x' && y == y' then e else Dba.Expr.ite c' x' y'
          in
          (m', e')
    in
    fold

  let define_loads :
      [ `Fallthrough ] opcode ->
      ((vertex * Basic_types.endianness * Dba.Expr.t * string option) * Var.t)
      list
      * [ `Fallthrough ] opcode =
   fun kind ->
    match kind with
    | Nop | Instruction _ | Hook _ | Clobber _ | Forget _ | Symbolize _
    | Builtin _ | Goto _ ->
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

  let define_load : t -> vertex -> unit =
   fun t vertex ->
    match node t vertex with
    | Fallthrough { label; kind; succ } -> (
        match define_loads kind with
        | [], kind' when kind == kind' -> ()
        | loads, kind ->
            add_node t vertex (Fallthrough { label; kind; succ });
            List.fold_right
              (fun ((_, dir, addr, base), var) () ->
                insert_before t vertex ~label (Load { var; base; dir; addr });
                insert_before t succ ~label (Forget var))
              loads ())
    | Branch { label; test; target; fallthrough } -> (
        match extract_loads [] test with
        | [], _ -> ()
        | loads, test ->
            add_node t vertex (Branch { label; test; target; fallthrough });
            List.fold_right
              (fun ((_, dir, addr, base), var) () ->
                insert_before t vertex ~label (Load { var; base; dir; addr });
                insert_before t target ~label (Forget var);
                insert_before t fallthrough ~label (Forget var))
              loads ())
    | Terminator { label; kind = Jump { target; tag } } -> (
        match extract_loads [] target with
        | [], _ -> ()
        | loads, target ->
            add_node t vertex
              (Terminator { label; kind = Jump { target; tag } });
            List.fold_right
              (fun ((_, dir, addr, base), var) () ->
                insert_before t vertex ~label (Load { var; base; dir; addr }))
              loads ())
    | Terminator _ -> ()

  let entropy = Printf.sprintf "%%entropy%%%d"

  let inline_dhunk : t -> label -> Dhunk.t -> Var.Set.t =
   fun t label hunk ->
    let vertex = length t in
    let next = ref (vertex + Dhunk.length hunk) in
    let temps = ref Var.Set.empty in
    Dhunk.iteri
      ~f:(fun i inst ->
        let cur = vertex + i in
        match inst with
        | Assign (Var var, rval, succ) ->
            if var.info = Var.Tag.Temp then temps := Var.Set.add var !temps;
            add_node t cur
              (Fallthrough
                 { label; kind = Assign { var; rval }; succ = vertex + succ })
        | Assign (Restrict (var, { hi; lo }), rval, succ) ->
            if var.info = Var.Tag.Temp then temps := Var.Set.add var !temps;
            add_node t cur
              (Fallthrough
                 {
                   label;
                   kind =
                     Assign
                       {
                         var;
                         rval = Dba_types.Expr.complement rval ~hi ~lo var;
                       };
                   succ = vertex + succ;
                 })
        | Assign (Store (_, dir, addr, base), rval, succ) ->
            add_node t cur
              (Fallthrough
                 {
                   label;
                   kind = Store { base; dir; addr; rval };
                   succ = vertex + succ;
                 })
        | Nondet (Var var, succ) | Undef (Var var, succ) ->
            if var.info = Var.Tag.Temp then temps := Var.Set.add var !temps;
            let kind =
              match inst with Nondet _ -> Symbolize var | _ -> Clobber var
            in
            add_node t cur (Fallthrough { label; kind; succ = vertex + succ })
        | Nondet (Restrict (var, { hi; lo }), succ)
        | Undef (Restrict (var, { hi; lo }), succ) ->
            if var.info = Var.Tag.Temp then temps := Var.Set.add var !temps;
            let size' = hi - lo + 1 in
            let name' = entropy size' in
            let var' = Dba.Var.temporary name' (Size.Bit.create size') in
            temps := Var.Set.add var' !temps;
            let rval =
              Dba_types.Expr.complement (Dba.Expr.v var') ~lo ~hi var
            in
            let succ' = !next in
            incr next;
            let kind =
              match inst with Nondet _ -> Symbolize var' | _ -> Clobber var'
            in
            add_node t cur (Fallthrough { label; kind; succ = succ' });
            add_node t succ'
              (Fallthrough
                 { label; kind = Assign { var; rval }; succ = vertex + succ })
        | Nondet (Store (bytes, dir, addr, base), succ)
        | Undef (Store (bytes, dir, addr, base), succ) ->
            let size' = 8 * bytes in
            let name' = entropy size' in
            let var' = Dba.Var.temporary name' (Size.Bit.create size') in
            let rval = Dba.Expr.v var' in
            let succ' = !next in
            incr next;
            let kind =
              match inst with Nondet _ -> Symbolize var' | _ -> Clobber var'
            in
            add_node t cur (Fallthrough { label; kind; succ = succ' });
            add_node t succ'
              (Fallthrough
                 {
                   label;
                   kind = Store { base; dir; addr; rval };
                   succ = vertex + succ;
                 })
        | Assume (test, succ) ->
            add_node t cur
              (Fallthrough { label; kind = Assume test; succ = vertex + succ })
        | Assert (test, succ) ->
            add_node t cur
              (Fallthrough { label; kind = Assert test; succ = vertex + succ })
        | If (test, JInner target, fallthrough) ->
            add_node t cur
              (Branch
                 {
                   label;
                   test;
                   target = vertex + target;
                   fallthrough = vertex + fallthrough;
                 })
        | If (test, JOuter { base = target; _ }, fallthrough) ->
            let succ = !next in
            incr next;
            add_node t succ
              (Terminator { label; kind = Goto { target; tag = Default } });
            add_node t cur
              (Branch
                 {
                   label;
                   test;
                   target = succ;
                   fallthrough = vertex + fallthrough;
                 })
        | DJump (target, tag) ->
            add_node t cur (Terminator { label; kind = Jump { target; tag } })
        | SJump (JOuter { base = target; _ }, tag) ->
            add_node t cur (Terminator { label; kind = Goto { target; tag } })
        | SJump (JInner succ, _) ->
            add_node t cur
              (Fallthrough { label; kind = Nop; succ = vertex + succ })
        | Stop (None | Some OK) ->
            add_node t cur (Terminator { label; kind = Halt })
        | Stop (Some KO) ->
            add_node t cur (Terminator { label; kind = Die "KO" })
        | Stop (Some (Unsupported msg)) ->
            let msg =
              match label with
              | Instruction ins ->
                  Format.asprintf "%a # %s" Binstream.pp
                    (Instruction.opcode ins) msg
              | Hook _ -> msg
            in
            add_node t cur (Terminator { label; kind = Die msg })
        | Stop (Some (Undecoded msg)) ->
            add_node t cur (Terminator { label; kind = Die msg }))
      hunk;
    !temps

  let inline_script : t -> label -> stmt list -> Var.Set.t =
   fun t label stmts ->
    let labels = StrTbl.create 16
    and tolink = StrTbl.create 16
    and temps = ref Var.Set.empty in
    List.iter
      (fun ins ->
        let vertex = length t in
        match ins with
        | Nop -> ()
        | Opcode kind ->
            (match kind with
            | Assign { var = { info = Temp; _ } as var; _ }
            | Load { var = { info = Temp; _ } as var; _ }
            | Symbolize ({ info = Temp; _ } as var)
            | Clobber ({ info = Temp; _ } as var) ->
                temps := Var.Set.add var !temps
            | _ -> ());
            add_node t vertex (Fallthrough { label; kind; succ = vertex + 1 })
        | Label name -> StrTbl.add labels name vertex
        | If (test, target) ->
            add_node t vertex (Terminator { label; kind = Halt });
            StrTbl.replace tolink target
              ((Some test, vertex)
              :: (try StrTbl.find tolink target with Not_found -> []))
        | Goto target ->
            add_node t vertex (Terminator { label; kind = Halt });
            StrTbl.replace tolink target
              ((None, vertex)
              :: (try StrTbl.find tolink target with Not_found -> []))
        | End (Jump { target = Cst bv; tag }) ->
            add_node t vertex
              (Terminator
                 {
                   label;
                   kind = Goto { target = Virtual_address.of_bitvector bv; tag };
                 })
        | End (Builtin (Inline hunk)) ->
            temps := Var.Set.union !temps (inline_dhunk t label hunk)
        | End kind -> add_node t vertex (Terminator { label; kind }))
      stmts;
    StrTbl.iter
      (fun target preds ->
        let target = StrTbl.find labels target in
        List.iter
          (fun (test, pred) ->
            match test with
            | None ->
                add_node t pred
                  (Fallthrough { label; kind = Nop; succ = target })
            | Some test ->
                add_node t pred
                  (Branch { label; test; target; fallthrough = pred + 1 }))
          preds)
      tolink;
    let vertex = length t in
    if stmts = [] || IntTbl.mem vertex t.preds then
      add_node t vertex (Terminator { label; kind = Builtin EndOfHook });
    !temps

  let copy : t -> t =
   fun { n; nodes; preds; entries; exits } ->
    IntTbl.freeze nodes;
    IntTbl.freeze preds;
    { n; nodes; preds; entries; exits }

  let empty : unit -> t =
   fun () ->
    {
      n = 0;
      nodes = IntTbl.empty;
      preds = IntTbl.empty;
      entries = IntSet.empty;
      exits = IntSet.empty;
    }

  let init : label -> (t -> Var.Set.t) -> t =
   fun label f ->
    let t = empty () in
    add_node t 0
      (Fallthrough
         {
           label;
           kind =
             (match label with
             | (Instruction _ as label) | (Hook _ as label) -> label);
           succ = 1;
         });
    t.entries <- IntSet.add 0 t.entries;
    let temps = f t in
    iter_vertex (define_load t) t;
    IntSet.iter
      (fun vertex ->
        let temps =
          match node t vertex with
          | Terminator { kind = Jump { target; _ }; _ } ->
              Var.Set.diff temps (Var.collect target Var.Set.empty)
          | _ -> temps
        in
        Var.Set.iter
          (fun var -> insert_before t vertex ~label (Forget var))
          temps)
      t.exits;
    t

  let of_instruction : Instruction.t -> t =
   fun ins ->
    let label = Instruction ins in
    init label (fun t -> inline_dhunk t label (Instruction.hunk ins))

  let of_script :
      Virtual_address.t -> string -> ?eoh:terminator -> stmt list -> t =
   fun addr info ?eoh stmts ->
    let label = Hook { addr; info } in
    let t = init label (fun t -> inline_script t label stmts) in
    match eoh with
    | None -> t
    | Some kind ->
        iter_exits
          (fun v ->
            match node t v with
            | Terminator { label; kind = Builtin EndOfHook } ->
                add_node t v (Terminator { label; kind })
            | _ -> ())
          t;
        t

  let replace_node : t -> vertex -> node -> unit =
   fun t vertex node ->
    if t.n <= vertex then raise (Invalid_argument "replace_node");
    add_node t vertex node

  let append_node : t -> node -> vertex =
   fun t node ->
    let vertex = length t in
    add_node t vertex node;
    vertex

  let append : t -> from:t -> vertex =
   fun t ~from ->
    let vertex = length t in
    let map_vertex = ( + ) vertex in
    iter_vertex
      (fun i -> add_node t (map_vertex i) (shuffle map_vertex (node from i)))
      from;
    t.entries <-
      IntSet.fold
        (fun i entries -> IntSet.add (map_vertex i) entries)
        from.entries t.entries;
    t.exits <-
      IntSet.fold
        (fun i entries -> IntSet.add (map_vertex i) entries)
        from.exits t.exits;
    vertex
end

module View = Graph

module Killset = struct
  module IntTbl = Basic_types.Integers.Int.Htbl

  type t = Var.Set.t IntTbl.t

  let is_deadstore : t -> Dba.Var.t -> View.vertex -> bool =
   fun killset var vertex ->
    try Var.Set.mem var (IntTbl.find killset vertex) with Not_found -> false

  let analyze_fallthrough :
      may_read:(builtin -> Var.Set.t option) ->
      must_write:(builtin -> Var.Set.t) ->
      fallthrough ->
      Var.Set.t ->
      Var.Set.t =
   fun ~may_read ~must_write kind killset ->
    match kind with
    | Nop | Instruction _ | Hook _ | Goto _ -> killset
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
    | Assume expr | Assert expr ->
        Var.Set.diff killset (Var.collect expr Var.Set.empty)
    | Builtin builtin -> (
        match may_read builtin with
        | None -> Var.Set.empty
        | Some readset ->
            Var.Set.diff (Var.Set.union killset (must_write builtin)) readset)

  let analyze_branch :
      Dba.Expr.t -> Var.Set.t option -> Var.Set.t option -> Var.Set.t =
   fun test target fallthrough ->
    match (target, fallthrough) with
    | None, None -> assert false
    | None, Some killset | Some killset, None ->
        Var.Set.diff killset (Var.collect test Var.Set.empty)
    | Some target, Some fallthrough ->
        let killset = Var.Set.inter target fallthrough in
        Var.Set.diff killset (Var.collect test Var.Set.empty)

  let rec closure :
      View.t ->
      may_read:(builtin -> Var.Set.t option) ->
      must_write:(builtin -> Var.Set.t) ->
      View.vertex Queue.t ->
      (View.vertex -> unit) ->
      t ->
      unit =
   fun t ~may_read ~must_write todo push acc ->
    if not (Queue.is_empty todo) then
      let vertex = Queue.pop todo in
      let killset =
        match View.node t vertex with
        | Fallthrough { kind; succ; _ } ->
            analyze_fallthrough ~may_read ~must_write kind
              (IntTbl.find acc succ)
        | Branch { test; target; fallthrough; _ } ->
            analyze_branch test
              (IntTbl.find_opt acc target)
              (IntTbl.find_opt acc fallthrough)
        | Terminator _ -> assert false
      in
      match IntTbl.find acc vertex with
      | old when Var.Set.equal old killset ->
          closure t ~may_read ~must_write todo push acc
      | (exception Not_found) | _ ->
          IntTbl.replace acc vertex killset;
          View.iter_pred push t vertex;
          closure t ~may_read ~must_write todo push acc

  let analyze :
      View.t ->
      may_read:(builtin -> Var.Set.t option) ->
      must_write:(builtin -> Var.Set.t) ->
      ?sink:IntSet.t ->
      t ->
      unit =
   fun t ~may_read ~must_write ?(sink = t.exits) killset ->
    let todo = Queue.create () in
    let push = Fun.flip Queue.push todo in
    IntSet.iter
      (fun vertex ->
        View.iter_pred push t vertex;
        match View.node t vertex with
        | Fallthrough { kind; succ; _ } ->
            IntTbl.add killset vertex
              (analyze_fallthrough ~may_read ~must_write kind
                 (IntTbl.find killset succ))
        | Branch { test; target; fallthrough; _ } ->
            IntTbl.add killset vertex
              (analyze_branch test
                 (IntTbl.find_opt killset target)
                 (IntTbl.find_opt killset fallthrough))
        | Terminator { kind = Builtin builtin; _ } ->
            IntTbl.add killset vertex
              (match may_read builtin with
              | None -> Var.Set.empty
              | Some readset -> Var.Set.diff (must_write builtin) readset)
        | Terminator _ -> IntTbl.add killset vertex Var.Set.empty)
      sink;
    closure t ~may_read ~must_write todo push killset
end
