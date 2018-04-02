(**************************************************************************)
(*  This file is part of Binsec.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
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

type instruction_sequence = (Dba.address *  Dba.instruction) list

let malloc_id = ref 0

let get_endianness, set_endianness =
  let endianness = ref LittleEndian in
  (fun () -> !endianness),
  (fun e -> endianness := e)

module Caddress = struct
  module X = struct
    type t = Dba.address
    let compare a1 a2 =
      assert (Bitvector.size_of a1.base = Bitvector.size_of a2.base);
      let v1 = Bitvector.value_of a1.base in
      let v2 = Bitvector.value_of a2.base in
      let c = Bigint.compare_big_int v1 v2 in
      if c = 0 then a1.id - a2.id else c
  end

  include Basic_types.MapSetMaker(X)

  module Hashtbl = Hashtbl.Make(
    struct
      include X
      let equal x y = compare x y = 0
      let hash = Hashtbl.hash
    end
    )

  let compare = X.compare

  let create base id =
    assert (id >= 0);
    let sz = Bitvector.size_of base in
    let word_size = Machine.Word_size.get () in
    assert (sz <= word_size);
    (* Auto-extend smaller address to the expected word-size for this machine *)
    let base =
      if sz < word_size then Bitvector.extend base word_size else base in
    { base; id; }

  let rebase a base = create base a.id
  let reid a id  = create a.base id

  let block_start bv = create bv 0

  let base_value addr = Bitvector.value_of addr.base

  let equal caddr1 caddr2 = compare caddr1 caddr2 = 0

  let pp_base ppf v = Format.fprintf ppf "%a" Bitvector.pp_hex v.base

  let add_int a n =
    let bv =
      Bitvector.create (Bigint.big_int_of_int n) (Bitvector.size_of a.base) in
    rebase a (Bitvector.add a.base bv)

  let add_id a n = reid a (n + a.id)

  let block_start_of_int n =
    let v = Bigint.big_int_of_int n in
    block_start (Bitvector.create v (Machine.Word_size.get ()))

  let default_init = ref (block_start_of_int 0)
end


module Virtual_address = struct
  module V_comparable = struct
    type t = int
    let compare = Pervasives.compare
    let equal x y = compare x y = 0
    let hash x = x
  end

  include Basic_types.MapSetMaker(V_comparable)

  module Hashtbl = Hashtbl.Make(V_comparable)

  let create n = n
  let of_code_address caddr =
    Caddress.base_value caddr |> Bigint.int_of_big_int |> create

  let to_code_address n =
    Caddress.block_start @@
    Bitvector.create (Bigint.big_int_of_int n) (Machine.Word_size.get ())

  let to_int64 = Int64.of_int
  let of_int64 = Int64.to_int

  let of_bigint = Bigint.int_of_big_int
  let to_bigint = Bigint.big_int_of_int
  let of_bitvector bv = Bitvector.value_of bv |> of_bigint

  let add_int t n = create (t + n)

  let pp ppf = Format.fprintf ppf "0x%08x"
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
  include Basic_types.MapSetMaker(ComparableRegion)

  let malloc sz =
    let minus_one = Bigint.big_int_of_int (-1) in
    let a = Caddress.block_start (Bitvector.create minus_one sz) in
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
module Tag = struct
  type t = Dba.tag
  let equal t1 t2 = Pervasives.compare t1 t2 = 0
end

module BinaryOperator = struct

  type t = Dba.binary_op

  let invert = function
    | Dba.Eq   -> Diff
    | Dba.Diff -> Eq
    | Dba.LeqU -> GtU
    | Dba.LtU  -> GeqU
    | Dba.GeqU -> LtU
    | Dba.GtU  -> LeqU
    | Dba.LeqS -> GtS
    | Dba.LtS  -> GeqS
    | Dba.GeqS -> LtS
    | Dba.GtS  -> LeqS
    | _ -> failwith "BinaryOperator.invert"

  let has_inverse = function
    | Dba.Eq  | Dba.Diff | Dba.LeqU | Dba.LtU  | Dba.GeqU
    | Dba.GtU | Dba.LeqS | Dba.LtS  | Dba.GeqS | Dba.GtS -> true
    | _ -> false


end

module rec Condition: sig
  include Sigs.Eq with type t = Dba.cond
  val is_symbolic : t -> bool
  val cnot : t -> t
  val cand : t -> t -> t
  val cor  : t -> t -> t
  val creified : Expr.t -> t
  val variables : t -> Basic_types.String.Set.t
  val temporaries : t -> Basic_types.String.Set.t
end
= struct
  type t = Dba.cond

  let cnot = function
    | True -> False
    | False -> True
    | CondReif (ExprBinary (bop, e1, e2)) when BinaryOperator.has_inverse bop ->
      CondReif (Expr.binary (BinaryOperator.invert bop) e1 e2)
    | c -> CondNot c

  let cand e1 e2 =
    match e1, e2 with
    | False, _
    | _, False -> False
    | True, e
    | e, True -> e
    | e1, e2 -> CondAnd (e1, e2)


  let cor e1 e2 =
    match e1, e2 with
    | True, _
    | _, True -> True
    | False, e
    | e, False -> e
    | e1, e2 -> CondOr (e1, e2)

  let creified e = CondReif e

  let rec equal c1 c2 =
    match c1, c2 with
    | CondReif e1, CondReif e2 -> Expr.equal e1 e2
    | CondNot c1, CondNot c2 -> equal c1 c2
    | CondAnd(lc1, rc1), CondAnd(lc2, rc2)
    | CondOr(lc1, rc1), CondOr(lc2, rc2) ->
      equal lc1 lc2 && equal rc1 rc2
    | True, True
    | False, False -> true
    | _, _ -> false

  let rec is_symbolic = function
    | CondReif e -> Expr.is_symbolic e
    | CondNot c -> is_symbolic c
    | CondOr (c1, c2) | CondAnd (c1, c2) ->
      is_symbolic c1 || is_symbolic c2
    | _ -> false
  let rec general_vars on_e = function
    | CondReif e -> on_e e
    | CondNot c -> general_vars on_e c
    | CondOr (c1, c2)
    | CondAnd (c1, c2) ->
      Basic_types.String.Set.union (general_vars on_e c1) (general_vars on_e c2)
    | True | False -> Basic_types.String.Set.empty

  let variables = general_vars (Expr.variables)
  let temporaries = general_vars (Expr.temporaries)
end
and Expr:  sig
  include Sigs.Eq with type t = Dba.expr
  include Sigs.Printable with type t := Dba.expr
  val var : string -> Basic_types.BitSize.t -> Dba.vartag option -> t

  val flag : ?bits:Basic_types.BitSize.t -> string -> t

  val temporary : string -> Basic_types.BitSize.t -> t
  val constant : ?region:Dba.region -> Bitvector.t -> t
  val zeros : int -> t
  val one : t
  val ones : int -> t
  val zero : t

  val binary: Dba.binary_op -> t -> t -> t
  val add                : t -> t -> t
  val sub                : t -> t -> t
  val umul               : t -> t -> t
  val smul               : t -> t -> t
  val smod               : t -> t -> t
  val umod               : t -> t -> t
  val udiv               : t -> t -> t
  val sdiv               : t -> t -> t
  val append             : t -> t -> t
  val eq                 : t -> t -> t
  val diff               : t -> t -> t
  val ule                : t -> t -> t
  val sle                : t -> t -> t
  val ult                : t -> t -> t
  val slt                : t -> t -> t
  val uge                : t -> t -> t
  val sge                : t -> t -> t
  val ugt                : t -> t -> t
  val sgt                : t -> t -> t

  val unary : Dba.unary_op -> t -> t
  val uminus             : t -> t

  val sext : t -> Basic_types.BitSize.t -> t
  val uext : t -> Basic_types.BitSize.t -> t

  include Sigs.Bitwise with type t := t
  val shift_left  : t -> t -> t
  val shift_right : t -> t -> t
  val shift_right_signed : t -> t -> t

  val rotate_left  : t -> t -> t
  val rotate_right : t -> t -> t

  val ite : Dba.cond -> t -> t -> t
  val restrict : t -> int -> int -> t
  val restrict_to_bit : t -> int -> t
  val load : Basic_types.ByteSize.t -> Dba.endianness -> t -> t

  val bool_false : t
  val bool_true : t

  val temp : Basic_types.BitSize.t -> t
  val is_symbolic : t -> bool
  val of_lvalue : Dba.lhs -> t
  val is_zero : t -> bool
  val is_one : t -> bool
  val is_max : t -> bool
  val size_of : t -> int

  val variables : t -> Basic_types.String.Set.t
  val temporaries : t -> Basic_types.String.Set.t
end = struct
  type t = Dba.expr

  let rec size_of = function
    | Dba.ExprCst(_, b) -> Bitvector.size_of b
    | Dba.ExprExtU(_, size)
    | Dba.ExprExtS(_, size)
    | Dba.ExprVar(_, size, _) -> size
    | Dba.ExprLoad(bytesize, _, _) -> 8 * bytesize
    | Dba.ExprIte(_, e, _)
    | Dba.ExprUnary(_, e) -> size_of e
    | Dba.ExprBinary(bop, e1, e2) ->
       begin match bop with
       | Concat -> size_of e1 + size_of e2
       | Eq | Diff | LeqU | LtU | GeqU | GtU | LeqS | LtS | GeqS | GtS -> 1
       | Plus | Minus | MultU | MultS
         | DivU | DivS | ModU | ModS | Or
         | And | Xor | LShift | RShiftU | RShiftS | LeftRotate | RightRotate
         -> size_of e1
       end
    | Dba.ExprRestrict(_, i, j) -> j - i + 1
    | Dba.ExprAlternative _ -> failwith "size_of ExprAlternative"



  let var name nbits vtagopt =
    let sz = Basic_types.BitSize.to_int nbits in
    Dba.ExprVar (name, sz, vtagopt)

  let flag ?(bits=Basic_types.BitSize.bits1) flagname =
    var flagname bits (Some (Flag FlgUnspecified))

  let temporary tempname nbits = var tempname nbits (Some Temp)

  let constant ?(region=`Constant) bv = ExprCst (region, bv)

  let zeros length = constant (Bitvector.zeros length)

  let zero = constant (Bitvector.zero)

  let ones length = constant (Bitvector.ones length)
  let one = constant (Bitvector.one)

  let binary op e1 e2 =
    Dba.ExprBinary (op, e1, e2)

  let pp = Dba_printer.Ascii.pp_expr

  let symmetric_binary op e1 e2 =
    assert (size_of e1 = size_of e2);
    binary op e1 e2

  let append = binary Dba.Concat
  let shift_left = binary Dba.LShift
  let shift_right = binary Dba.RShiftU
  let shift_right_signed = binary Dba.RShiftS
  let rotate_left = binary Dba.LeftRotate
  let rotate_right = binary Dba.RightRotate


  let add  = symmetric_binary Dba.Plus
  let sub  = symmetric_binary Dba.Minus
  let umul = symmetric_binary Dba.MultU
  let smul = symmetric_binary Dba.MultS
  let smod = symmetric_binary Dba.ModS
  let umod = symmetric_binary Dba.ModU
  let udiv = symmetric_binary Dba.DivU
  let sdiv = symmetric_binary Dba.DivS
  let logor  = symmetric_binary Dba.Or
  let logxor = symmetric_binary Dba.Xor
  let logand = symmetric_binary Dba.And
  let eq = symmetric_binary Dba.Eq
  let diff = symmetric_binary Dba.Diff
  let ule = symmetric_binary Dba.LeqU
  let sle = symmetric_binary Dba.LeqS
  let ult = symmetric_binary Dba.LtU
  let slt = symmetric_binary Dba.LtS
  let uge = symmetric_binary Dba.GeqU
  let sge = symmetric_binary Dba.GeqS
  let ugt = symmetric_binary Dba.GtU
  let sgt = symmetric_binary Dba.GtS

  let unary op e = Dba.ExprUnary (op, e)

  let lognot = unary Dba.Not
  let uminus = unary Dba.UMinus


  let sext e bitsize = Dba.ExprExtS(e, Basic_types.BitSize.to_int bitsize)
  let uext e bitsize = Dba.ExprExtU(e, Basic_types.BitSize.to_int bitsize)


  let ite condition then_expr else_expr =
    Dba.ExprIte (condition, then_expr, else_expr)

  let restrict e off1 off2 =
    assert (off2 >= off1 && off1 >= 0);
    (* Probably should check later on or now that off1 & off2 are within the
     * bounds of the size of e *)
    Dba.ExprRestrict (e, off1, off2)

  let restrict_to_bit e off = restrict e off off

  let temp size =
    let name = Format.asprintf "temp%a" Basic_types.BitSize.pp size in
    var name size (Some Temp)

  let bool_true = one
  let bool_false = zero

  let load size endianness e =
    Dba.ExprLoad(Basic_types.ByteSize.to_int size, endianness, e)

  let rec equal e1 e2 =
    match e1, e2 with
    | ExprVar (n1, sz1, _),
      ExprVar (n2, sz2, _) -> n1 = n2 && sz1 = sz2
    | ExprLoad (sz1, en1, e1),
      ExprLoad (sz2, en2, e2) -> sz1 = sz2 && en1 = en2 && equal e1 e2
    | ExprCst (r1, bv1), ExprCst (r2, bv2) ->
      Region.compare r1 r2 = 0 && Bitvector.equal bv1 bv2
    | ExprUnary (unop1, e1), ExprUnary (unop2, e2) ->
      unop1 = unop2 && equal e1 e2
    | ExprBinary (binop1, lexpr1, rexpr1),
      ExprBinary (binop2, lexpr2, rexpr2) ->
      binop1 = binop2 && equal lexpr1 lexpr2 && equal rexpr1 rexpr2
    | ExprRestrict (e1, o11, o12),
      ExprRestrict (e2, o21, o22) -> equal e1 e2 && o11 = o21 && o12 = o22
    | ExprExtU (e1, sz1), ExprExtU (e2, sz2)
    | ExprExtS (e1, sz1), ExprExtS (e2, sz2) ->
      equal e1 e2 && sz1 = sz2
    | ExprIte (c1, e11, e12),
      ExprIte (c2, e21, e22) ->
      Condition.equal c1 c2 && equal e11 e21 && equal e12 e22
    | ExprAlternative (e_list1, tag1), ExprAlternative (e_list2, tag2) ->
      let rec aux elist1 elist2 =
        match elist1, elist2 with
        | [], [] -> true
        | e1 :: e1s, e2 :: e2s -> equal e1 e2 && aux e1s e2s
        | [], _
        | _, [] -> false
      in Tag.equal tag1 tag2 && aux e_list1 e_list2
    | _, _ -> false


  let rec is_symbolic = function
    | ExprVar(_, _, _) -> true
    | ExprLoad(_, _, _) -> true
    | ExprUnary(_, e1) -> is_symbolic e1
    | ExprBinary(_, e1, e2) ->
      is_symbolic e1 || is_symbolic e2
    | ExprRestrict(e1, _, _) -> is_symbolic e1
    | ExprExtU(e1, _) | ExprExtS(e1, _) -> is_symbolic e1
    | ExprIte(c, e1, e2) ->
      Condition.is_symbolic c || is_symbolic e1 || is_symbolic e2
    | ExprAlternative(x :: _, _) -> is_symbolic x
    | _ -> false

  let of_lvalue = function
  | LhsVar (x, sz, tag) -> ExprVar(x, sz, tag)
  | LhsVarRestrict (x, sz, o1, o2) ->
    ExprRestrict (ExprVar (x, sz, None), o1, o2)
  | LhsStore (sz, endiannness, e) ->
    ExprLoad (sz, endiannness, e)

  let is_zero = function
    | ExprCst (`Constant, bv) -> Bitvector.is_zeros bv
    | _ -> false

  let is_one = function
  | ExprCst (_, bv) -> Bitvector.is_one bv
  | _ -> false

  let is_max = function
    | Dba.ExprCst (`Constant, bv) -> Bitvector.is_max_ubv bv
    | _ -> false


  let rec variables = function
    | Dba.ExprCst _ -> Basic_types.String.Set.empty
    | Dba.ExprRestrict(e, _, _)
    | Dba.ExprUnary(_, e)
    | Dba.ExprLoad(_, _, e)
    | Dba.ExprExtU(e, _)
    | Dba.ExprExtS(e, _) -> variables e
    | Dba.ExprVar(name, _, _) -> Basic_types.String.Set.singleton name
    | Dba.ExprBinary(_, e1, e2)
    | Dba.ExprIte(_, e1, e2) ->
      Basic_types.String.Set.union (variables e1) (variables e2)
    | Dba.ExprAlternative _ -> failwith "variables : ExprAlternative"

  let rec temporaries = function
    | Dba.ExprCst _ -> Basic_types.String.Set.empty
    | Dba.ExprRestrict(e, _, _)
    | Dba.ExprUnary(_, e)
    | Dba.ExprLoad(_, _, e)
    | Dba.ExprExtU(e, _)
    | Dba.ExprExtS(e, _) -> temporaries e
    | Dba.ExprVar(name, _, Some Temp) -> Basic_types.String.Set.singleton name
    | Dba.ExprVar _ -> Basic_types.String.Set.empty
    | Dba.ExprBinary(_, e1, e2)
    | Dba.ExprIte(_, e1, e2) ->
      Basic_types.String.Set.union (temporaries e1) (temporaries e2)
    | Dba.ExprAlternative _ -> failwith "temporaries : ExprAlternative"
end


module LValue = struct
  type t = lhs

  let var ~bitsize name tagopt =
    let bsz = Basic_types.BitSize.to_int bitsize in
    Dba.LhsVar(name, bsz, tagopt)

  let flag
      ?(bitsize=Basic_types.BitSize.bits1) ?(subflag=Dba.FlgUnspecified) flagname =
    var flagname ~bitsize (Some (Flag subflag))

  let temporary tempname bitsize = var tempname ~bitsize (Some Dba.Temp)

  let temp nbits =
    let name = Format.asprintf "temp%a" Basic_types.BitSize.pp nbits in
    temporary name nbits


  let restrict name sz lo hi =
    assert(lo <= hi);
    assert(lo >= 0);
    let bsz = Basic_types.BitSize.to_int sz in
    assert(hi < bsz); (* TODO? : create a pure variable if hi - lo + 1 = sz *)
    Dba.LhsVarRestrict(name, bsz, lo, hi)

  let restrict_to_bit name sz bit = restrict name sz bit bit

  let store nbytes endianness e =
    let sz = Basic_types.ByteSize.to_int nbytes in
    Dba.LhsStore(sz, endianness, e)

  let equal lv1 lv2 =
    match lv1, lv2 with
    | LhsVar (x1, sz1, _), LhsVar (x2, sz2, _) ->
      x1 = x2 && sz1 = sz2
    | LhsVarRestrict (x1, sz1, o11, o12), LhsVarRestrict (x2, sz2, o21, o22) ->
      x1 = x2 && sz1 = sz2 && o11 = o21 && o12 = o22
    | LhsStore (sz1, en1, e1), LhsStore (sz2, en2, e2) ->
      sz1 = sz2 && en1 = en2 && Expr.equal e1 e2
    | _, _ -> false


  let is_expr_translatable = function
    | Dba.ExprVar _
    | Dba.ExprLoad _
    | Dba.ExprRestrict(ExprVar _, _, _) -> true
    | Dba.ExprRestrict _
    | Dba.ExprCst _
    | Dba.ExprUnary _
    | Dba.ExprBinary _
    | Dba.ExprExtU _
    | Dba.ExprExtS _
    | Dba.ExprIte _
    | Dba.ExprAlternative _ -> false

  
  let of_expr = function
    | Dba.ExprVar(name, sz, tag) ->
      var name ~bitsize:(Basic_types.BitSize.create sz) tag
    | Dba.ExprLoad(size, endian, e) ->
      store (Basic_types.ByteSize.create size) endian e
    | Dba.ExprRestrict(ExprVar(name, sz, _), i, j) ->
      restrict name (Basic_types.BitSize.create sz) i j
    | Dba.ExprRestrict _
    | Dba.ExprCst _
    | Dba.ExprUnary _
    | Dba.ExprBinary _
    | Dba.ExprExtU _
    | Dba.ExprExtS _
    | Dba.ExprIte _
    | Dba.ExprAlternative _ as e->
      let msg =
        Format.asprintf "LValue.of_expr : Cannot create lvalue from expression %a"
          Dba_printer.Ascii.pp_expr e in
      failwith msg


  module Map = Map.Make (
    struct
      type t = Dba.lhs
      let compare  = Pervasives.compare
    end
  )

  let bitsize lval =
    let sz =
      match lval with
      | LhsVar(_, size, _) -> size
      | LhsStore(size, _, _) -> 8 * size
      | LhsVarRestrict(_, _, loff, roff) -> roff - loff + 1
    in Basic_types.BitSize.create sz

  let unsafe_bitsize lval = bitsize lval |> Basic_types.BitSize.to_int

  let _pp = Dba_printer.Ascii.pp_lhs

  let name_of = function
    | LhsVar (name, _, _)
    | LhsVarRestrict (name, _, _, _) -> Some name
    | LhsStore _ -> None

  let is_temporary = function
    | LhsVar (_, _, Some Temp) -> true
    | LhsVar _
    | LhsVarRestrict _
    | LhsStore _ -> false

  let is_flag = function
    | LhsVar (_, _, Some (Flag _)) -> true
    | LhsVar _
    | LhsVarRestrict _
    | LhsStore _ -> false

  let variables = function
    | LhsVar (name, _, _)
    | LhsVarRestrict (name, _, _, _) -> Basic_types.String.Set.singleton name
    | LhsStore (_, _, e) -> Expr.variables e

  let temporaries = function
    | LhsVar (name, _, Some Temp) -> Basic_types.String.Set.singleton name
    | LhsVar _
    | LhsVarRestrict _ -> Basic_types.String.Set.empty
    (* Restrict cannot be applied to temporaries : check that! *)
    | LhsStore (_, _, e) -> Expr.temporaries e

  let resize size = function
    | LhsVar (vname, _sz, tag) -> var vname ~bitsize:size tag
    | LhsVarRestrict(vname, _sz, lo, hi) -> restrict vname size lo hi
    | LhsStore (_sz, endianness, e) -> store size endianness e
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
  include Basic_types.MapSetMaker(ComparableAddressStack)
  let pp fmt (caddr, call_stack, n) =
    Format.fprintf fmt "@[<hov 2>%a@,<%a>@,(%d)@]"
      Dba_printer.Ascii.pp_code_address caddr
      Call_stack.pp call_stack
      n
end


type 'a dbainstrmap = (Dba.instruction * 'a option) Caddress.Map.t


module Declarations = struct
  type t = (Dba.size * Dba.vartag option) Basic_types.String.Map.t

  open Basic_types.String.Map
  let of_list declarations =
    List.fold_left
      (fun smap (id, size, opttags) -> add id (size, opttags) smap)
      empty declarations
end


module Jump_target = struct
  type t = Dba.jump_target

  let inner n = Dba.JInner n
  let outer a = Dba.JOuter a

  let outer_jumps = function
    | JInner _ -> Virtual_address.Set.empty
    | JOuter a -> Virtual_address.Set.singleton (Virtual_address.of_code_address a)
end


type ('a, 'b) defuse = {
  defs: 'a;
  uses: 'b;
}

module Instruction = struct
  type t = Dba.instruction

  let assign lval rval nid = Dba.IkAssign(lval, rval, nid)

  let static_jump ?(tag=None) jt = Dba.IkSJump (jt, tag)

  let static_inner_jump ?(tag=None) n =
    static_jump (Jump_target.inner n) ~tag

  let call ~return_address jt =
    let tag = Some (Call return_address) in
    static_jump ~tag jt

  let dynamic_jump ?(tag=None) e = Dba.IkDJump (e, tag)

  let stop state = Dba.IkStop state

  let ite c goto nid = Dba.IkIf (c, goto, nid)

  let undefined lv nid = Dba.IkUndef (lv, nid)

  let non_deterministic lv r nid = Dba.IkNondet (lv, r, nid)

  let malloc lv e nid = Dba.IkMalloc (lv, e, nid)

  let free e id = Dba.IkFree (e, id)

  let iassert c nid = Dba.IkAssert (c, nid)

  let assume c nid = Dba.IkAssume (c, nid)

  let non_deterministic_assume lvs c id = Dba.IkNondetAssume (lvs, c, id)

  let print args id = Dba.IkPrint (args, id)

  let set_successor i id =
    match i with
    | IkAssign (lv, e, _) -> assign lv e id
    | IkIf (c, jt, _) -> ite c jt id
    | IkAssert (c, _) -> iassert c id
    | IkAssume (c, _) -> assume c id
    | IkNondetAssume (lvs, c, _) -> non_deterministic_assume lvs c id
    | IkNondet (lv, r, _) -> non_deterministic lv r id
    | IkUndef (lv, _) -> undefined lv id
    | IkMalloc (lv, e, _) -> malloc lv e id
    | IkFree (e, _) -> free e id
    | IkPrint (ds, _) -> print ds id
    | IkStop _
    | IkSJump _
    | IkDJump _ -> i


  let generic_reset_successors p f instr =
    let new_id id = if p id then f id else id in
    let new_jt = function
      | JOuter _ as jt -> jt
      | JInner id -> Jump_target.inner (new_id id)
    in
    match instr with
    | IkAssign (lv, e, id) -> assign lv e (new_id id)
    | IkIf (c, jt, id) -> ite c (new_jt jt) (new_id id)
    | IkAssert (c, id) -> iassert c (new_id id)
    | IkAssume (c, id) -> assume c (new_id id)
    | IkNondetAssume (lvs, c, id) -> non_deterministic_assume lvs c (new_id id)
    | IkNondet (lv, r, id) -> non_deterministic lv r (new_id id)
    | IkUndef (lv, id) -> undefined lv (new_id id)
    | IkMalloc (lv, e, id) -> malloc lv e (new_id id)
    | IkFree (e, id) -> free e (new_id id)
    | IkPrint (ds, id) -> print ds (new_id id)
    | IkSJump (jt, tag) -> static_jump ~tag (new_jt jt)
    | IkStop _
    | IkDJump _ -> instr


  let reset_successor ~src_id ~dst_id instr =
    generic_reset_successors ((=) src_id) (fun _ -> dst_id) instr


  let successors = function
    | IkAssign (_, _, i)
    | IkAssert (_, i)
    | IkAssume (_, i)
    | IkNondetAssume (_, _, i)
    | IkNondet (_, _, i)
    | IkUndef (_, i)
    | IkMalloc (_, _, i)
    | IkFree (_, i)
    | IkPrint (_, i)  -> [JInner i]
    | IkStop _ -> []
    | IkSJump (jt, _) -> [jt]
    | IkIf (_, jt, i) -> [jt; JInner i]
    | IkDJump _ -> []


  let no_defs uses = { defs = Basic_types.String.Set.empty; uses}

  let variables = function
    | IkMalloc (lv, e, _)
    | IkAssign (lv, e, _) ->
      { defs = LValue.variables lv;
        uses = Expr.variables e;}
    | IkPrint _
    | IkStop _
    | IkSJump _ -> no_defs Basic_types.String.Set.empty

    | IkFree (e, _)
    | IkDJump (e, _) -> no_defs (Expr.variables e)
    | IkIf (c, _, _)
    | IkAssert (c, _)
    | IkAssume (c, _) -> no_defs (Condition.variables c)
    | IkNondetAssume (lvals, c, _) ->
      { defs =
          List.fold_left
            (fun s lv -> Basic_types.String.Set.union s (LValue.variables lv))
            Basic_types.String.Set.empty lvals;
        uses = Condition.variables c;
      }
    | IkNondet (lval, _, _)
    | IkUndef (lval, _) ->
      { defs = LValue.variables lval; uses = Basic_types.String.Set.empty; }


  let temporaries = function
    | IkMalloc (lv, e, _)
    | IkAssign (lv, e, _) ->
      { defs = LValue.temporaries lv;
        uses = Expr.temporaries e;
      }
    | IkPrint _
    | IkStop _
    | IkSJump _ -> { defs = Basic_types.String.Set.empty;
                     uses = Basic_types.String.Set.empty; }
    | IkFree (e, _)
    | IkDJump (e, _) ->
      { defs = Basic_types.String.Set.empty;
        uses = Expr.temporaries e; }
    | IkIf (c, _, _)
    | IkAssert (c, _)
    | IkAssume (c, _) ->
      { defs = Basic_types.String.Set.empty;
        uses = Condition.temporaries c; }
    | IkNondetAssume (lvals, c, _) ->
      { defs =
          List.fold_left
            (fun s lv -> Basic_types.String.Set.union s (LValue.temporaries lv))
            Basic_types.String.Set.empty
            lvals;
        uses = Condition.temporaries c; }
    | IkNondet (lval, _, _)
    | IkUndef (lval, _) -> { defs = LValue.temporaries lval;
                             uses = Basic_types.String.Set.empty; }


  let outer_jumps instr =
    match instr with
    | IkAssign _
    | IkStop _
    | IkAssert _
    | IkAssume _
    | IkNondetAssume _
    | IkNondet _
    | IkUndef _
    | IkFree _
    | IkMalloc _
    | IkDJump (_, (Some Return | None))
    | IkPrint _ -> Virtual_address.Set.empty
    | IkDJump (_, Some Call a) ->
      Virtual_address.(Set.singleton (of_code_address a))
    | IkSJump (jt, Some Call a) ->
      let open Virtual_address in
      Set.add (of_code_address a) (Jump_target.outer_jumps jt)
    | IkSJump (jt, (Some Return  | None))
    | IkIf (_, jt, _) ->  Jump_target.outer_jumps jt

  let is_call = function
    | IkSJump (_, Some Call _)
    | IkDJump (_, Some Call _) -> true
    | _ -> false
end

module Statement = struct
  type t = {
    location : Caddress.t;
    instruction : Instruction.t
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

module Block = struct
  type t = Dba.instruction array

  let init = Array.init
  let empty = Array.make 0 (Instruction.stop None)
  let singleton instruction = Array.make 1 instruction

  let is_empty t = Array.length t = 0
  let length = Array.length
  let get t n = t.(n)

  let start _t = 0

  let of_list = Array.of_list
  let to_list = Array.to_list
  let iter = Array.iter
  let iteri = Array.iteri
  let map = Array.map
  let mapi = Array.mapi
  let fold_left = Array.fold_left

  exception Done
  let for_all p a =
    try iter (fun e -> if not (p e) then raise Done) a; true
    with Done -> false

  let copy t = init (length t) (get t)

  let pp ppf t =
    let open Format in
    fprintf ppf "@[<v 0>";
    iteri
      (fun n instr ->
         fprintf ppf "@[<h>%2d: %a@]@ " n Dba_printer.Ascii.pp_instruction instr) t;
    fprintf ppf "@]"

  let to_dbainstrs t address =
    let base = Caddress.block_start_of_int (address:>int) in
    let l = to_list t in
    List.mapi (fun i e -> Statement.create (Caddress.reid base i) e) l

  let no_inner_reference instr = function
    | Dba.JOuter _ -> true
    | (Dba.JInner _) as jt ->
      not (List.mem jt (Instruction.successors instr))

  let no_block_inner_references t n =
    let jt = Jump_target.inner n in
    for_all (fun instr -> no_inner_reference instr jt) t

  let remove t n =
    Format.printf "@[<v 0>Removing %d@ %a@]@." n pp t;
    assert (n < length t);
    assert (no_block_inner_references t n);
    init
      (length t - 1)
      (fun i -> if i < n then t.(i) else
          let p id = id >= n in
          let f id = id - 1 in
          Instruction.generic_reset_successors p f t.(i + 1))

  let outer_jumps =
    fold_left
      (fun hwset instr ->
         let jset = Instruction.outer_jumps instr in
         Virtual_address.Set.union hwset jset)
      Virtual_address.Set.empty

  let callees =
    fold_left
      (fun hwset dinstr ->
         match dinstr with
         | IkSJump (JOuter dst, (Some (Call _))) ->
           (* Only this pattern marks a call instruction of which we know the
              target *)
           let a = Virtual_address.of_code_address dst in
           Virtual_address.Set.add a hwset
         | _ -> hwset
      ) Virtual_address.Set.empty


  module Check = struct
    let inner_jump_inside_bound t label =
      label >= 0 && label < length t

    let get_inner_jumps =
      let aux acc = function
        | IkSJump (JInner id, _)
        | IkIf (_, JInner id, _) -> id :: acc
        | IkIf _
        | IkSJump _
        | IkDJump _
        | IkAssign _
        | IkStop _
        | IkAssert _
        | IkAssume _
        | IkNondetAssume _
        | IkNondet _
        | IkUndef _
        | IkMalloc _
        | IkFree _
        | IkPrint _ -> acc
      in fold_left aux []

    let has_inbound_inner_jumps t =
      get_inner_jumps t
      |> List.for_all (inner_jump_inside_bound t)


    exception Undeclared_Variable of string * Dba.instruction

    let no_undeclared_variables decls t =
      let no_undeclared_at_instr i =
        let du = Instruction.variables i in
        let vset = Basic_types.String.Set.union du.uses du.defs in
        try
          Basic_types.String.Set.iter
            (fun vname ->
               if not (Basic_types.String.Map.mem vname decls) then
                 raise (Undeclared_Variable (vname, i))
            ) vset;
          true
        with
        | Undeclared_Variable (vname, instr) ->
          Logger.fatal
            "Undeclared variable %s at instruction %a"
            vname Dba_printer.Ascii.pp_instruction instr;
          false
      in for_all no_undeclared_at_instr t


    exception Temporaries_undefined of
        Basic_types.String.Set.t * Dba.instruction

    let no_temporary_leak t =
      (* set of assigned temporaries *)
      let t_assigned = Basic_types.String.Set.empty in
      let no_temporary_leak_at_instr assigned i =
        let du = Instruction.temporaries i in
        let diff = Basic_types.String.Set.diff du.uses assigned in
        if not (Basic_types.String.Set.is_empty diff) then
          raise (Temporaries_undefined (diff, i))
        else Basic_types.String.Set.union assigned du.defs
      in
      try
        ignore (fold_left no_temporary_leak_at_instr t_assigned t);
        true
      with
      | Temporaries_undefined (tset, instr) ->
        Logger.fatal
          "@[<h>Temporaries %a were previously \
           undefined but used at instruction %a@]"
          (fun ppf set ->
             Basic_types.String.Set.iter
               (fun s -> Format.fprintf ppf "%s;@ " s)
               set)
          tset
          Dba_printer.Ascii.pp_instruction instr;
        false
  end
end


type read_perm = Read of bool
type write_perm = Write of bool
type exec_perm =  Exec of bool
type permissions = Dba.cond * (read_perm * write_perm * exec_perm)

type 'a program = {
  start_address : Dba.address;
  declarations : Declarations.t;
  permissions: permissions list Region.Map.t * Dba.cond Rights.t;
  initializations : Dba.instruction list;
  instructions : 'a dbainstrmap;
}
