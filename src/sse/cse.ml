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

module Source : sig
  type kind = Input | Clobber | Symbolic
  type t = Dba.Var.t * int * kind

  include Sigs.HASHABLE with type t := t
  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
end = struct
  module T = struct
    type kind = Input | Clobber | Symbolic
    type t = Dba.Var.t * int * kind

    let hash : t -> int = fun (_, id, _) -> id
    let equal x y = hash x = hash y
    let compare x y = hash x - hash y
  end

  include T
  module Set = Set.Make (T)
  module Map = Map.Make (T)
end

module rec Expr : (Term.S with type a := Source.t and type b := Layer.t) =
  Term.Make (Source) (Layer)

and Store : sig
  type t

  val empty : t
  val store : Bitvector.t -> Expr.t -> t -> t
  val select : (Z.t -> int -> Expr.t) -> Bitvector.t -> int -> t -> Expr.t
  val iter : (Z.t -> Expr.t -> unit) -> t -> unit
  val rev_iter : (Z.t -> Expr.t -> unit) -> t -> unit
end = struct
  module Chunk = struct
    type t = Expr.t

    let equal = Expr.is_equal
    let len t = Expr.sizeof t lsr 3
    let crop ~lo ~hi t = Expr.restrict ~lo:(lo lsl 3) ~hi:((hi lsl 3) + 7) t
    let concat = Expr.append
  end

  include Lmap.Make (Chunk)

  let store addr value t =
    let z = Bitvector.value_of addr and s = Chunk.len value in
    let u = Z.add z (Z.of_int s) in
    let n = Bitvector.size_of addr in
    if Z.numbits u > n && Z.popcount u > 1 then
      let o = Z.to_int (Z.extract u 0 n) in
      store z
        (Chunk.crop ~hi:(s - o - 1) ~lo:0 value)
        (store Z.zero (Chunk.crop ~hi:(s - 1) ~lo:(s - o) value) t)
    else store z value t

  let select f addr s t =
    let z = Bitvector.value_of addr in
    let u = Z.add z (Z.of_int s) in
    let n = Bitvector.size_of addr in
    if Z.numbits u > n && Z.popcount u > 1 then
      let o = Z.to_int (Z.extract u 0 n) in
      Chunk.concat (select f Z.zero o t) (select f z (s - o) t)
    else select f z s t
end

and Layer : sig
  type t =
    | Base of string option
    | Layer of {
        id : int;
        over : t;
        base : string option;
        addr : Expr.t;
        store : Store.t;
      }

  val base : t -> string option
  val write : read:bool -> addr:Expr.t -> Expr.t -> Expr.endianness -> t -> t
  val read : addr:Expr.t -> int -> Expr.endianness -> t -> Expr.t * bool

  include Sigs.HASHABLE with type t := t
end = struct
  type t =
    | Base of string option
    | Layer of {
        id : int;
        over : t;
        base : string option;
        addr : Expr.t;
        store : Store.t;
      }

  let id = ref 0
  let hash = function Base b -> Hashtbl.hash b | Layer { id; _ } -> id

  let compare t t' =
    match (t, t') with
    | Base b, Base b' -> compare b b'
    | Layer { id; _ }, Layer { id = id'; _ } -> id - id'
    | Base _, Layer _ -> -1
    | Layer _, Base _ -> 1

  let equal t t' = compare t t' = 0

  let bswap =
    let rec iter e i r =
      if i = 0 then r
      else
        iter e (i - 8) (Expr.append (Expr.restrict ~hi:(i - 1) ~lo:(i - 8) e) r)
    in
    fun e ->
      let size = Expr.sizeof e in
      assert (size land 0x7 = 0);
      iter e (size - 8) (Expr.restrict ~hi:(size - 1) ~lo:(size - 8) e)

  let rebase (addr : Expr.t) =
    match addr with
    | Cst bv -> (Expr.zeros (Bitvector.size_of bv), bv)
    | Binary { f = Plus; x; y = Cst bv; _ } -> (x, bv)
    | Binary { f = Minus; x; y = Cst bv; _ } -> (x, Bitvector.neg bv)
    | _ -> (addr, Bitvector.zeros (Expr.sizeof addr))

  let base = function Base base | Layer { base; _ } -> base

  let write addr value over =
    let addr, offset = rebase addr in
    incr id;
    Layer
      {
        id = !id;
        over;
        base = base over;
        addr;
        store = Store.store offset value Store.empty;
      }

  let write ~read ~addr value (dir : Expr.endianness) over =
    let value =
      match dir with LittleEndian -> value | BigEndian -> bswap value
    in
    match over with
    | Base _ -> write addr value over
    | Layer _ when read -> write addr value over
    | Layer { base = base'; addr = addr'; store = store'; over = over'; _ } -> (
        match Expr.sub addr addr' with
        | Expr.Cst bv ->
            let store = Store.store bv value store' in
            incr id;
            Layer { id = !id; over = over'; base = base'; addr = addr'; store }
        | _ -> write addr value over)

  let rec read ~addr bytes (dir : Expr.endianness) t =
    match t with
    | Base _ -> (Expr.load bytes dir addr t, true)
    | Layer { addr = addr'; store; over; _ } -> (
        match Expr.sub addr addr' with
        | Expr.Cst bv ->
            let miss i s =
              fst (read ~addr:(Expr.addz addr' i) s Expr.LittleEndian over)
            in
            let bytes = Store.select miss bv bytes store in
            let bytes =
              match dir with LittleEndian -> bytes | BigEndian -> bswap bytes
            in
            (bytes, false)
        | _ -> (Expr.load bytes dir addr t, true))
end

type var = ([ `Var ], Source.t, Layer.t) Expr.term

module StrMap = Basic_types.String.Map
module VarMap = Dba_types.Var.Map
module VarSet = Dba_types.Var.Set

let uop (e : Dba.Expr.t) (op : Dba.Unary_op.t) : Term.unary Term.operator =
  match op with
  | Not -> Not
  | UMinus -> Minus
  | Sext n -> Sext (n - Dba.Expr.size_of e)
  | Uext n -> Uext (n - Dba.Expr.size_of e)
  | Restrict interval -> Restrict interval

let bop (op : Dba.Binary_op.t) : Term.binary Term.operator =
  match op with
  | Plus -> Plus
  | Minus -> Minus
  | Mult -> Mul
  | DivU -> Udiv
  | DivS -> Sdiv
  | RemU -> Urem
  | RemS -> Srem
  | Eq -> Eq
  | Diff -> Diff
  | LeqU -> Ule
  | LtU -> Ult
  | GeqU -> Uge
  | GtU -> Ugt
  | LeqS -> Sle
  | LtS -> Slt
  | GeqS -> Sge
  | GtS -> Sgt
  | Xor -> Xor
  | And -> And
  | Or -> Or
  | Concat -> Concat
  | LShift -> Lsl
  | RShiftU -> Lsr
  | RShiftS -> Asr
  | LeftRotate -> Rol
  | RightRotate -> Ror

module Env = struct
  type t = {
    id : int;
    vars : Expr.t VarMap.t;
    layers : (Layer.t * bool) StrMap.t;
    rev_reads : Expr.t list;
    sources : var list VarMap.t;
  }

  let havoc var t kind =
    let history = try VarMap.find var t.sources with Not_found -> [] in
    let value = Expr.var var.name var.size (var, t.id, kind) in
    ( value,
      {
        t with
        id = t.id + 1;
        vars = VarMap.add var value t.vars;
        sources = VarMap.add var (Term.to_var_exn value :: history) t.sources;
      } )

  let is_empty { vars; layers; rev_reads; _ } =
    VarMap.is_empty vars && StrMap.is_empty layers && rev_reads = []

  let rec eval (e : Dba.Expr.t) t =
    match e with
    | Cst bv | Var { info = Symbol (_, (lazy bv)); _ } -> (Expr.constant bv, t)
    | Var var -> lookup var t
    | Load (len, dir, addr, base) -> load len dir addr base t
    | Unary (f, x) ->
        let x', t' = eval x t in
        (Expr.unary (uop x f) x', t')
    | Binary (f, x, y) ->
        let x', t' = eval x t in
        let y', t' = eval y t' in
        (Expr.binary (bop f) x' y', t')
    | Ite (c, r, e) ->
        let c', t' = eval c t in
        let r', t' = eval r t' in
        let e', t' = eval e t' in
        (Expr.ite c' r' e', t')

  and lookup var t =
    try (VarMap.find var t.vars, t) with Not_found -> havoc var t Input

  and load len dir addr base t =
    let name = Option.value ~default:"" base in
    let layer, read =
      try StrMap.find name t.layers with Not_found -> (Layer.Base base, true)
    in
    let addr, t' = eval addr t in
    let bytes, read' = Layer.read ~addr len dir layer in
    ( bytes,
      {
        t' with
        layers = StrMap.add name (layer, read || read') t'.layers;
        rev_reads = (if read' then bytes :: t'.rev_reads else t'.rev_reads);
      } )

  let empty =
    {
      id = 0;
      vars = VarMap.empty;
      layers = StrMap.empty;
      rev_reads = [];
      sources = VarMap.empty;
    }

  let assign var value t =
    let value', t' = eval value t in
    { t' with vars = VarMap.add var value' t'.vars }

  let clobber var t = snd (havoc var t Clobber)
  let symbolize var t = snd (havoc var t Symbolic)
  let forget var t = { t with vars = VarMap.remove var t.vars }

  let load (var : Dba.Var.t) base dir addr t =
    let bytes, t' = load (var.size / 8) dir addr base t in
    { t' with vars = VarMap.add var bytes t'.vars }

  let store base dir ~addr value t =
    let name = Option.value ~default:"" base in
    let layer, read =
      try StrMap.find name t.layers with Not_found -> (Layer.Base base, false)
    in
    let addr', t' = eval addr t in
    let value', t' = eval value t' in
    let layer' = Layer.write ~read ~addr:addr' value' dir layer in
    { t' with layers = StrMap.add name (layer', false) t'.layers }
end

type 'a operator = 'a Term.operator
and unary = Term.unary
and binary = Term.binary

type 'a node =
  | Constant : Bitvector.t -> [< `Value | `Opcode ] node
  | Value : int -> [< `Value | `Opcode ] node
  | Variable : Dba.Var.t -> [< `Value | `Opcode ] node
  | Unary : unary operator * [ `Value ] node -> [< `Value | `Opcode ] node
  | Binary :
      binary operator * [ `Value ] node * [ `Value ] node
      -> [< `Value | `Opcode ] node
  | Ite :
      [ `Value ] node * [ `Value ] node * [ `Value ] node
      -> [< `Value | `Opcode ] node
  | Load :
      string option * [ `Value ] node * Machine.endianness * int
      -> [ `Opcode ] node
  | Store :
      string option * [ `Value ] node * Machine.endianness * [ `Value ] node
      -> [ `Opcode ] node
  | Assign : Dba.Var.t * [ `Value ] node -> [ `Opcode ] node
  | Clobber : Dba.Var.t -> [ `Opcode ] node
  | Symbolize : Dba.Var.t -> [ `Opcode ] node

and value = [ `Value ] node
and opcode = [ `Opcode ] node

let rec pp_opcode : type a. Format.formatter -> a node -> unit =
 fun ppf opcode ->
  match opcode with
  | Constant bv -> Bitvector.pp_hex_or_bin ppf bv
  | Value i -> Format.fprintf ppf "Value[%d]" i
  | Variable { name; _ } -> Format.pp_print_string ppf name
  | Unary (op, x) -> Format.fprintf ppf "(%a %a)" Term.Op.pp op pp_opcode x
  | Binary (op, x, y) ->
      Format.fprintf ppf "(%a %a %a)" Term.Op.pp op pp_opcode x pp_opcode y
  | Ite (c, t, e) ->
      Format.fprintf ppf "(%a ? %a : %a)" pp_opcode c pp_opcode t pp_opcode e
  | Load (base, addr, dir, len) ->
      Format.fprintf ppf "%a[%a, %d]%c"
        (Format.pp_print_option
           ~none:(fun ppf () -> Format.pp_print_string ppf "@")
           Format.pp_print_string)
        base pp_opcode addr len
        (match dir with LittleEndian -> 'l' | BigEndian -> 'b')
  | Store (base, addr, dir, value) ->
      Format.fprintf ppf "%a[%a]%c := %a"
        (Format.pp_print_option
           ~none:(fun ppf () -> Format.pp_print_string ppf "@")
           Format.pp_print_string)
        base pp_opcode addr
        (match dir with LittleEndian -> 'l' | BigEndian -> 'b')
        pp_opcode value
  | Assign ({ name; _ }, value) ->
      Format.fprintf ppf "%s := %a" name pp_opcode value
  | Clobber { name; _ } -> Format.fprintf ppf "%s := undef" name
  | Symbolize { name; _ } -> Format.fprintf ppf "%s := nondet" name

module VarTbl = Dba_types.Var.Htbl

module BvTbl = Hashtbl.Make (struct
  type t = Expr.t

  let hash = Expr.hash
  let equal = Expr.is_equal
end)

module AxTbl = Hashtbl.Make (Layer)

type point = Bv of Expr.t | Ax of string option * Expr.t * Expr.t

type t = {
  mutable id : int;
  queue : point Queue.t;
  mutable sources : Expr.t Source.Map.t;
  locals : int BvTbl.t;
  layers : unit AxTbl.t;
}

let init () =
  {
    id = 0;
    queue = Queue.create ();
    sources = Source.Map.empty;
    locals = BvTbl.create 32;
    layers = AxTbl.create 4;
  }

let rec visit_bv env bv =
  match BvTbl.find env.locals bv with
  | -1 ->
      BvTbl.replace env.locals bv 0;
      env.id <- env.id + 1
  | _ -> ()
  | exception Not_found -> (
      match bv with
      | Var { label; _ } ->
          BvTbl.replace env.locals bv 0;
          env.id <- env.id + 1;
          env.sources <- Source.Map.add label bv env.sources
      | Load { addr; label; _ } ->
          visit_ax env label;
          visit_bv env addr;
          BvTbl.add env.locals bv 0;
          env.id <- env.id + 1;
          Queue.add (Bv bv) env.queue
      | Cst _ ->
          BvTbl.add env.locals bv (-1);
          Queue.add (Bv bv) env.queue
      | Unary { x; _ } ->
          visit_bv env x;
          BvTbl.add env.locals bv (-1);
          Queue.add (Bv bv) env.queue
      | Binary { x; y; _ } ->
          visit_bv env x;
          visit_bv env y;
          BvTbl.add env.locals bv (-1);
          Queue.add (Bv bv) env.queue
      | Ite { c; t = r; e; _ } ->
          visit_bv env c;
          visit_bv env r;
          visit_bv env e;
          BvTbl.add env.locals bv (-1);
          Queue.add (Bv bv) env.queue)

and visit_ax env ax =
  if not (AxTbl.mem env.layers ax) then (
    AxTbl.add env.layers ax ();
    match ax with
    | Base _ -> ()
    | Layer { base; addr; store; over; _ } ->
        visit_ax env over;
        Store.iter
          (fun offset value ->
            let addr = Expr.addz addr offset in
            visit_bv env addr;
            visit_bv env value;
            env.id <- env.id + 1;
            Queue.add (Ax (base, addr, value)) env.queue)
          store)

let rec mk_bv : t -> Expr.t -> value =
 fun env bv ->
  match BvTbl.find env.locals bv with
  | -1 | (exception Not_found) -> mk_bv_no_cons env bv
  | id -> Value id

and mk_bv_no_cons : t -> Expr.t -> value =
 fun env bv ->
  match mk_opcode env bv with
  | (Constant _ | Value _ | Variable _ | Unary _ | Binary _ | Ite _) as value ->
      value
  | Clobber _ | Symbolize _ | Load _ | Store _ | Assign _ -> assert false

and mk_opcode : t -> Expr.t -> opcode =
 fun env bv ->
  match bv with
  | Cst bv -> Constant bv
  | Var { label = var, _, Input; _ } -> Variable var
  | Var { label = var, _, Clobber; _ } -> Clobber var
  | Var { label = var, _, Symbolic; _ } -> Symbolize var
  | Load { addr; dir; len; label = Base base | Layer { base; _ }; _ } ->
      Load (base, mk_bv env addr, dir, len)
  | Unary { f; x; _ } -> Unary (f, mk_bv env x)
  | Binary { f; x; y; _ } -> Binary (f, mk_bv env x, mk_bv env y)
  | Ite { c; t; e; _ } -> Ite (mk_bv env c, mk_bv env t, mk_bv env e)

let commit : Env.t -> opcode array =
 fun body ->
  if Env.is_empty body then [||]
  else
    let env = init () in
    List.iter (visit_bv env) (List.rev body.rev_reads);
    StrMap.iter (fun _ (ax, _) -> visit_ax env ax) body.layers;
    VarMap.iter
      (fun var bv ->
        match (bv : Expr.t) with
        | Var { label = var', _, Input; _ } when Dba.Var.equal var var' -> ()
        | _ ->
            env.id <- env.id + 1;
            visit_bv env bv)
      body.vars;
    let opcodes = Array.make env.id (Constant Bitvector.zero) in
    let idx = ref 0 in
    Source.Map.iter
      (fun _ bv ->
        BvTbl.replace env.locals bv !idx;
        Array.set opcodes !idx (mk_opcode env bv);
        incr idx)
      env.sources;
    Queue.iter
      (function
        | Bv bv ->
            if BvTbl.find env.locals bv <> -1 then (
              BvTbl.replace env.locals bv !idx;
              Array.set opcodes !idx (mk_opcode env bv);
              incr idx)
        | Ax (base, addr, rval) ->
            Array.set opcodes !idx
              (Store (base, mk_bv env addr, LittleEndian, mk_bv env rval));
            incr idx)
      env.queue;
    VarMap.iter
      (fun var bv ->
        match (bv : Expr.t) with
        | Var { label = var', _, Input; _ } when Dba.Var.equal var var' -> ()
        | _ ->
            Array.set opcodes !idx (Assign (var, mk_bv env bv));
            incr idx)
      body.vars;
    opcodes

let rec closure : bool BvTbl.t -> Expr.t VarMap.t -> VarSet.t -> VarSet.t =
  let rec analyze : bool BvTbl.t -> Expr.t -> VarSet.t -> bool =
   fun tainted value slice ->
    try BvTbl.find tainted value
    with Not_found ->
      let taint =
        match value with
        | Cst _ -> false
        | Var { label = var, _, _; _ } -> VarSet.mem var slice
        | Load _ -> true
        | Unary { x; _ } -> analyze tainted x slice
        | Binary { x; y; _ } ->
            analyze tainted x slice || analyze tainted y slice
        | Ite { c; t; e; _ } ->
            analyze tainted c slice || analyze tainted t slice
            || analyze tainted e slice
      in
      BvTbl.add tainted value taint;
      taint
  in
  fun tainted vars slice ->
    let slice' =
      VarMap.fold
        (fun var value slice ->
          if analyze tainted value slice then VarSet.add var slice else slice)
        vars slice
    in
    if VarSet.equal slice slice' then slice
    else (
      BvTbl.filter_map_inplace
        (fun _ b -> if b then Some true else None)
        tainted;
      closure tainted vars slice')

let partial_commit : Env.t -> VarSet.t -> Env.t * opcode array =
 fun body slice ->
  if Env.is_empty body then (body, [||])
  else
    let slice = closure (BvTbl.create 64) body.vars slice in
    let vars, vars' =
      VarMap.partition (fun var _ -> VarSet.mem var slice) body.vars
    in
    ( { body with vars = vars'; layers = StrMap.empty; rev_reads = [] },
      commit { body with vars } )
