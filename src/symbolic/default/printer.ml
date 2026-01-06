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

let byte_size = Natural.to_int Basic_types.Constants.bytesize

(* utils *)
let pp_int_as_bv ppf x = function
  | 1 -> Format.fprintf ppf "#b%d" (x land 1)
  | 4 -> Format.fprintf ppf "#x%01x" (x land 0xf)
  | 8 -> Format.fprintf ppf "#x%02x" (x land 0xff)
  | 12 -> Format.fprintf ppf "#x%03x" (x land 0xfff)
  | 16 -> Format.fprintf ppf "#x%04x" (x land 0xffff)
  | 20 -> Format.fprintf ppf "#x%05x" (x land 0xfffff)
  | 24 -> Format.fprintf ppf "#x%06x" (x land 0xffffff)
  | 28 -> Format.fprintf ppf "#x%07x" (x land 0xfffffff)
  | 32 ->
      Format.fprintf ppf "#x%08x"
        (Nativeint.to_int (Nativeint.logand (Nativeint.of_int x) 0xffffffffn))
  | 64 when x >= 100000000 -> Format.fprintf ppf "#x%016x" x
  | sz when x < 0 ->
      Format.fprintf ppf "(_ bv%a %d)" Z.pp_print
        (Z.extract (Z.of_int x) 0 sz)
        sz
  | sz -> Format.fprintf ppf "(_ bv%d %d)" x sz

let pp_bv ppf value size =
  try pp_int_as_bv ppf (Z.to_int value) size
  with Z.Overflow ->
    Format.fprintf ppf "(_ bv%a %d)" Z.pp_print
      (if Z.lt value Z.zero then Z.extract value 0 size else value)
      size

open Types

type term = string

type access = Select of term * int | Store of term * int
and def = Bl of Expr.t | Bv of Expr.t | Ax of Memory.t | Decl of string

and t = {
  fvariables : Expr.t StTbl.t;
  farrays : Memory.symbol StTbl.t;
  mutable id : Suid.t;
  bl_cons : string BvTbl.t;
  bv_cons : string BvTbl.t;
  ax_cons : string AxTbl.t;
  ax_root : Memory.symbol AxTbl.t;
  ordered_defs : def Queue.t;
  ordered_mem : access Queue.t AxTbl.t;
  debug : name:string -> label:string -> string;
}

let create ?(debug = fun ~name ~label:_ -> name) ~next_id () =
  let bv_cons = BvTbl.create 128 and bl_cons = BvTbl.create 32 in
  BvTbl.add bl_cons Expr.zero "false";
  BvTbl.add bv_cons Expr.zero "#b0";
  BvTbl.add bl_cons Expr.one "true";
  BvTbl.add bv_cons Expr.one "#b1";
  {
    fvariables = StTbl.create 16;
    farrays = StTbl.create 4;
    id = next_id;
    bl_cons;
    bv_cons;
    ax_cons = AxTbl.create 64;
    ax_root = AxTbl.create 64;
    ordered_defs = Queue.create ();
    ordered_mem = AxTbl.create 4;
    debug;
  }

let copy
    {
      fvariables;
      farrays;
      id;
      bl_cons;
      bv_cons;
      ax_cons;
      ax_root;
      ordered_defs;
      ordered_mem;
      debug;
    } =
  let ordered_mem =
    AxTbl.fold
      (fun ax history tbl ->
        AxTbl.add tbl ax (Queue.copy history);
        tbl)
      ordered_mem
      (AxTbl.create (AxTbl.length ordered_mem))
  in
  {
    fvariables = StTbl.copy fvariables;
    farrays = StTbl.copy farrays;
    id;
    bl_cons = BvTbl.copy bl_cons;
    bv_cons = BvTbl.copy bv_cons;
    ax_cons = AxTbl.copy ax_cons;
    ax_root;
    ordered_defs = Queue.copy ordered_defs;
    ordered_mem;
    debug;
  }

let pp_int_as_offset size ppf i = pp_bv ppf i size

let once = ""
and evicted = "\\"

let rec visit_bl ctx bl =
  match BvTbl.find ctx.bl_cons bl with
  | exception Not_found -> visit_and_mark_bl ctx bl BvTbl.add once
  | label ->
      if label == once then (
        let name = Suid.to_string ctx.id in
        ctx.id <- Suid.incr ctx.id;
        BvTbl.replace ctx.bl_cons bl name)
      else if label == evicted then (
        let name = Suid.to_string ctx.id in
        ctx.id <- Suid.incr ctx.id;
        visit_and_mark_bl ctx bl BvTbl.replace name)

and visit_and_mark_bl ctx (bl : Expr.t) set label =
  match bl with
  | Cst _ -> ()
  | Load _ (* cannot be a bl<1> *) -> assert false
  | Unary { f = Not; x; _ } ->
      set ctx.bl_cons bl label;
      visit_bl ctx x;
      Queue.push (Bl bl) ctx.ordered_defs
  | Binary { f = And | Or | Xor; x; y; _ } ->
      set ctx.bl_cons bl label;
      visit_bl ctx x;
      visit_bl ctx y;
      Queue.push (Bl bl) ctx.ordered_defs
  | Binary
      { f = Eq | Diff | Uge | Ule | Ugt | Ult | Sge | Sle | Sgt | Slt; x; y; _ }
    ->
      set ctx.bl_cons bl label;
      visit_bv ctx x;
      visit_bv ctx y;
      Queue.push (Bl bl) ctx.ordered_defs
  | Ite { c; t; e; _ } ->
      set ctx.bl_cons bl label;
      visit_bl ctx c;
      visit_bl ctx t;
      visit_bl ctx e;
      Queue.push (Bl bl) ctx.ordered_defs
  | Var _ | Unary _ | Binary _ ->
      set ctx.bl_cons bl label;
      visit_bv ctx bl;
      Queue.push (Bl bl) ctx.ordered_defs

and visit_bv ctx bv =
  match BvTbl.find ctx.bv_cons bv with
  | exception Not_found -> visit_and_mark_bv ctx bv BvTbl.add once
  | label ->
      if label == once then (
        let name = Suid.to_string ctx.id in
        ctx.id <- Suid.incr ctx.id;
        BvTbl.replace ctx.bv_cons bv name)
      else if label == evicted then (
        let name = Suid.to_string ctx.id in
        ctx.id <- Suid.incr ctx.id;
        visit_and_mark_bv ctx bv BvTbl.replace name)

and visit_and_mark_bv ctx bv set label' =
  match bv with
  | Var { name; size; label; _ } ->
      StTbl.add ctx.fvariables name bv;
      let name = ctx.debug ~name ~label in
      BvTbl.add ctx.bv_cons bv name;
      Queue.push
        (Decl (Format.sprintf "(declare-fun %s () (_ BitVec %d))" name size))
        ctx.ordered_defs
  | Load { len; addr; label; _ } ->
      let name = Suid.to_string ctx.id in
      ctx.id <- Suid.incr ctx.id;
      BvTbl.add ctx.bv_cons bv name;
      visit_bv ctx addr;
      visit_bv ctx addr;
      visit_ax ctx label;
      if len > 1 then visit_ax ctx label;
      Queue.push (Bv bv) ctx.ordered_defs;
      let (Symbol _ as root) = AxTbl.find ctx.ax_root label in
      let ordered_mem = AxTbl.find ctx.ordered_mem root in
      Queue.push (Select (BvTbl.find ctx.bv_cons addr, len)) ordered_mem
  | Cst _ ->
      set ctx.bv_cons bv label';
      Queue.push (Bv bv) ctx.ordered_defs
  | Unary { x; _ } ->
      set ctx.bv_cons bv label';
      visit_bv ctx x;
      Queue.push (Bv bv) ctx.ordered_defs
  | Binary { f = Eq | Diff | Uge | Ule | Ugt | Ult | Sge | Sle | Sgt | Slt; _ }
    ->
      set ctx.bv_cons bv label';
      visit_bl ctx bv;
      Queue.push (Bv bv) ctx.ordered_defs
  | Binary
      { f = Rol | Ror; x; y = (Load _ | Unary _ | Binary _ | Ite _) as y; _ } ->
      set ctx.bv_cons bv label';
      visit_bv ctx x;
      visit_bv ctx x;
      visit_bv ctx y;
      visit_bv ctx y;
      Queue.push (Bv bv) ctx.ordered_defs
  | Binary { x; y; _ } ->
      set ctx.bv_cons bv label';
      visit_bv ctx x;
      visit_bv ctx y;
      Queue.push (Bv bv) ctx.ordered_defs
  | Ite { c; t; e; _ } ->
      set ctx.bv_cons bv label';
      visit_bl ctx c;
      visit_bv ctx t;
      visit_bv ctx e;
      Queue.push (Bv bv) ctx.ordered_defs

and visit_ax ctx (ax : Memory.t) =
  match AxTbl.find ctx.ax_cons ax with
  | exception Not_found -> visit_and_mark_ax ctx ax AxTbl.add once
  | label ->
      if label == once then (
        let name = Suid.to_string ctx.id in
        ctx.id <- Suid.incr ctx.id;
        AxTbl.replace ctx.ax_cons ax name)
      else if label == evicted then (
        let name = Suid.to_string ctx.id in
        ctx.id <- Suid.incr ctx.id;
        visit_and_mark_ax ctx ax AxTbl.replace name)

and visit_and_mark_ax ctx (ax : Memory.t) set label =
  match ax with
  | Symbol { name; index; _ } as ax ->
      StTbl.add ctx.farrays name ax;
      AxTbl.add ctx.ax_cons ax name;
      AxTbl.add ctx.ax_root ax ax;
      AxTbl.add ctx.ordered_mem ax (Queue.create ());
      Queue.push
        (Decl
           (Format.sprintf
              "(declare-fun %s () (Array (_ BitVec %d) (_ BitVec %d)))" name
              index byte_size))
        ctx.ordered_defs
  | Layer { addr; store; over; _ } ->
      set ctx.ax_cons ax label;
      (match addr with
      | Cst _ -> ()
      | _ ->
          visit_bv ctx addr;
          visit_bv ctx addr);
      Store.iter
        (fun _ chunk ->
          if Chunk.is_term chunk then (
            let bv = Chunk.unsafe_to_term chunk in
            visit_bv ctx bv;
            if Expr.sizeof bv <> 8 then visit_bv ctx bv))
        store;
      visit_ax ctx over;
      let (Symbol { index; _ } as root) = AxTbl.find ctx.ax_root over in
      AxTbl.add ctx.ax_root ax root;
      Queue.push (Ax ax) ctx.ordered_defs;
      let ordered_mem = AxTbl.find ctx.ordered_mem root in
      let index =
        match addr with
        | Cst _ -> Format.asprintf "%a" (pp_int_as_offset index)
        | _ ->
            let addr = BvTbl.find ctx.bv_cons addr in
            Format.asprintf "(bvadd %s %a)" addr (pp_int_as_offset index)
      in
      let lazy_memory =
        match (addr, over) with Cst _, Symbol _ -> true | _ -> false
      in
      Store.iter
        (fun i chunk ->
          if Chunk.is_term chunk || not lazy_memory then
            Queue.push (Store (index i, Chunk.len chunk)) ordered_mem)
        store

let pp_unop ppf (op : Term.unary Term.operator) =
  match op with
  | Not -> Format.pp_print_string ppf "bvnot"
  | Minus -> Format.pp_print_string ppf "bvneg"
  | Uext n -> Format.fprintf ppf "(_ zero_extend %d)" n
  | Sext n -> Format.fprintf ppf "(_ sign_extend %d)" n
  | Restrict { Interval.hi; lo } -> Format.fprintf ppf "(_ extract %d %d)" hi lo

let pp_binop =
  let string_of_binop (op : Term.binary Term.operator) =
    match op with
    | Plus -> "bvadd"
    | Minus -> "bvsub"
    | Mul -> "bvmul"
    | Udiv -> "bvudiv"
    | Sdiv -> "bvsdiv"
    | Urem -> "bvurem"
    | Srem -> "bvsrem"
    | Or -> "bvor"
    | And -> "bvand"
    | Xor -> "bvxor"
    | Concat -> "concat"
    | Lsl -> "bvshl"
    | Lsr -> "bvlshr"
    | Asr -> "bvashr"
    | Rol -> "rotate_left"
    | Ror -> "rotate_right"
    | Eq -> "="
    | Diff -> assert false
    | Ule -> "bvule"
    | Ult -> "bvult"
    | Uge -> "bvuge"
    | Ugt -> "bvugt"
    | Sle -> "bvsle"
    | Slt -> "bvslt"
    | Sge -> "bvsge"
    | Sgt -> "bvsgt"
  in
  fun ppf f -> Format.pp_print_string ppf (string_of_binop f)

let rec print_bl ctx ppf bl =
  try
    let name = BvTbl.find ctx.bl_cons bl in
    if name == once || name == evicted then print_bl_no_cons ctx ppf bl
    else Format.pp_print_string ppf name
  with Not_found ->
    Format.pp_print_string ppf "(= ";
    Format.pp_print_space ppf ();
    print_bv ctx ppf bl;
    Format.pp_print_string ppf " #b1)"

and print_bl_no_cons ctx ppf bl =
  match bl with
  | Cst _ (* true and false should already be in the cache *)
  | Load _ (* cannot be a bl<1> *) ->
      assert false
  | Unary { f = Not; x; _ } ->
      Format.pp_print_string ppf "(not";
      Format.pp_print_space ppf ();
      print_bl ctx ppf x;
      Format.pp_print_char ppf ')'
  | Binary { f = (And | Or | Xor) as f; x; y; _ } ->
      Format.pp_print_char ppf '(';
      (Format.pp_print_string ppf
      @@
      match f with
      | And -> "and"
      | Or -> "or"
      | Xor -> "xor"
      | _ -> assert false);
      Format.pp_print_space ppf ();
      print_bl ctx ppf x;
      Format.pp_print_space ppf ();
      print_bl ctx ppf y;
      Format.pp_print_char ppf ')'
  | Binary { f = Diff; x; y; _ } ->
      Format.pp_print_string ppf "(not";
      Format.pp_print_space ppf ();
      Format.pp_print_string ppf "(=";
      Format.pp_print_space ppf ();
      print_bv ctx ppf x;
      Format.pp_print_space ppf ();
      print_bv ctx ppf y;
      Format.pp_print_string ppf "))"
  | Binary
      { f = (Eq | Uge | Ule | Ugt | Ult | Sge | Sle | Sgt | Slt) as f; x; y; _ }
    ->
      Format.pp_print_char ppf '(';
      pp_binop ppf f;
      Format.pp_print_space ppf ();
      print_bv ctx ppf x;
      Format.pp_print_space ppf ();
      print_bv ctx ppf y;
      Format.pp_print_char ppf ')'
  | Ite { c; t; e; _ } ->
      Format.pp_print_string ppf "(ite";
      Format.pp_print_space ppf ();
      print_bl ctx ppf c;
      Format.pp_print_space ppf ();
      print_bl ctx ppf t;
      Format.pp_print_space ppf ();
      print_bl ctx ppf e;
      Format.pp_print_char ppf ')'
  | Var _ | Unary _ | Binary _ ->
      Format.pp_print_string ppf "(=";
      Format.pp_print_space ppf ();
      print_bv ctx ppf bl;
      Format.pp_print_space ppf ();
      Format.pp_print_string ppf "#b1)"

and print_bv ctx ppf bv =
  let name = BvTbl.find ctx.bv_cons bv in
  if name == once || name == evicted then print_bv_no_cons ctx ppf bv
  else Format.pp_print_string ppf name

and print_bv_no_cons ctx ppf bv =
  match bv with
  | Var { name; _ } -> Format.pp_print_string ppf name
  | Load { len = 1; addr; label; _ } ->
      Format.pp_print_string ppf "(select";
      Format.pp_print_space ppf ();
      print_ax ctx ppf label;
      Format.pp_print_space ppf ();
      print_bv ctx ppf addr;
      Format.pp_print_char ppf ')'
  | Load { len; dir; addr; label; _ } ->
      Format.pp_print_string ppf "(concat";
      print_multi_select dir ppf len
        (AxTbl.find ctx.ax_cons label)
        (BvTbl.find ctx.bv_cons addr)
        (Expr.sizeof addr);
      Format.pp_print_char ppf ')'
  | Cst bv ->
      let size = Bv.size_of bv and value = Bv.value_of bv in
      pp_bv ppf value size
  | Unary { f; x; _ } ->
      Format.pp_print_char ppf '(';
      pp_unop ppf f;
      Format.pp_print_space ppf ();
      print_bv ctx ppf x;
      Format.pp_print_char ppf ')'
  | Binary { f = Eq | Uge | Ule | Ugt | Ult | Sge | Sle | Sgt | Slt; _ } ->
      Format.pp_print_string ppf "(ite";
      Format.pp_print_space ppf ();
      print_bl ctx ppf bv;
      Format.pp_print_space ppf ();
      Format.pp_print_string ppf "#b1";
      Format.pp_print_space ppf ();
      Format.pp_print_string ppf "#b0)"
  | Binary { f = Diff; x; y; _ } ->
      Format.pp_print_string ppf "(ite (=";
      Format.pp_print_space ppf ();
      print_bv ctx ppf x;
      Format.pp_print_space ppf ();
      print_bv ctx ppf y;
      Format.pp_print_char ppf ')';
      Format.pp_print_space ppf ();
      Format.pp_print_string ppf "#b0";
      Format.pp_print_space ppf ();
      Format.pp_print_string ppf "#b1)"
  | Binary { f = (Rol | Ror) as f; x; y = Cst bv; _ } ->
      Format.pp_print_string ppf "((_";
      Format.pp_print_space ppf ();
      pp_binop ppf f;
      Format.pp_print_space ppf ();
      Z.pp_print ppf (Bv.value_of bv);
      Format.pp_print_char ppf ')';
      Format.pp_print_space ppf ();
      print_bv ctx ppf x;
      Format.pp_print_char ppf ')'
  | Binary { f = (Rol | Ror) as f; x; y; _ } ->
      Format.pp_print_string ppf "(bvor";
      Format.pp_print_space ppf ();
      Format.pp_print_char ppf '(';
      pp_binop ppf (match f with Rol -> Lsl | Ror -> Lsr | _ -> assert false);
      Format.pp_print_space ppf ();
      Format.pp_print_string ppf (BvTbl.find ctx.bv_cons x);
      Format.pp_print_space ppf ();
      Format.pp_print_string ppf (BvTbl.find ctx.bv_cons y);
      Format.pp_print_char ppf ')';
      Format.pp_print_space ppf ();
      Format.pp_print_char ppf '(';
      pp_binop ppf (match f with Rol -> Lsr | Ror -> Lsl | _ -> assert false);
      Format.pp_print_space ppf ();
      Format.pp_print_string ppf (BvTbl.find ctx.bv_cons x);
      Format.pp_print_space ppf ();
      Format.pp_print_string ppf "(bvsub";
      Format.pp_print_space ppf ();
      pp_int_as_bv ppf (Expr.sizeof x) (Expr.sizeof x);
      Format.pp_print_space ppf ();
      Format.pp_print_string ppf (BvTbl.find ctx.bv_cons y);
      Format.pp_print_string ppf ")))"
  | Binary { f; x; y; _ } ->
      Format.pp_print_char ppf '(';
      pp_binop ppf f;
      Format.pp_print_space ppf ();
      print_bv ctx ppf x;
      Format.pp_print_space ppf ();
      print_bv ctx ppf y;
      Format.pp_print_char ppf ')'
  | Ite { c; t; e; _ } ->
      Format.pp_print_string ppf "(ite";
      Format.pp_print_space ppf ();
      print_bl ctx ppf c;
      Format.pp_print_space ppf ();
      print_bv ctx ppf t;
      Format.pp_print_space ppf ();
      print_bv ctx ppf e;
      Format.pp_print_char ppf ')'

and print_ax ctx ppf ax =
  let name = AxTbl.find ctx.ax_cons ax in
  if name == once || name == evicted then print_ax_no_cons ctx ppf ax
  else Format.pp_print_string ppf name

and print_ax_no_cons ctx ppf (ax : Memory.t) =
  match ax with
  | Symbol _ -> assert false
  | Layer { addr; store; over; _ } ->
      let idx_size = Expr.sizeof addr in
      let lazy_memory =
        match (addr, over) with Cst _, Symbol _ -> true | _ -> false
      in
      Store.iter
        (fun _ chunk ->
          if Chunk.is_term chunk || not lazy_memory then
            for _ = 1 to Chunk.len chunk do
              Format.pp_print_string ppf "(store";
              Format.pp_print_space ppf ()
            done)
        store;
      print_ax ctx ppf over;
      let rebase, idx =
        match addr with
        | Cst _ -> (false, "")
        | _ -> (true, BvTbl.find ctx.bv_cons addr)
      in
      let rec unroll_store idx_size lo i bv =
        Format.pp_print_space ppf ();
        if rebase then
          if Z.equal Z.zero i then (
            Format.pp_print_string ppf idx;
            Format.pp_print_space ppf ())
          else (
            Format.pp_print_string ppf "(bvadd";
            Format.pp_print_space ppf ();
            Format.pp_print_string ppf idx;
            Format.pp_print_space ppf ();
            pp_bv ppf i idx_size;
            Format.pp_print_char ppf ')')
        else pp_bv ppf i idx_size;
        Format.pp_print_space ppf ();
        let size = Expr.sizeof bv in
        if size > 8 then (
          Format.fprintf ppf "((_ extract %d %d)" (lo + 7) lo;
          Format.pp_print_space ppf ();
          print_bv ctx ppf bv;
          Format.pp_print_string ppf "))";
          let lo' = lo + 8 in
          if lo' < size then unroll_store idx_size lo' (Z.succ i) bv)
        else (
          (match bv with
          | Cst bv -> pp_int_as_bv ppf (Bitvector.to_uint bv) 8
          | _ -> print_bv ctx ppf bv);
          Format.pp_print_char ppf ')')
      in
      if lazy_memory then
        Store.iter
          (fun i chunk ->
            if Chunk.is_term chunk then
              unroll_store idx_size 0 i (Chunk.unsafe_to_term chunk))
          store
      else Store.iter_term (unroll_store idx_size 0) store

and print_multi_select =
  let rec print_multi_select_le ppf len ax bv size =
    if len = 1 then Format.fprintf ppf " (select@ %s@ %s)" ax bv
    else
      let len = len - 1 in
      Format.fprintf ppf " (select@ %s@ (bvadd@ %s@ " ax bv;
      pp_int_as_bv ppf len size;
      Format.pp_print_string ppf "))";
      print_multi_select_le ppf len ax bv size
  in
  let rec print_multi_select_be i ppf len ax bv size =
    if i = 0 then (
      Format.fprintf ppf "@ (select@ %s@ %s)" ax bv;
      print_multi_select_be 1 ppf len ax bv size)
    else if i < len then (
      Format.fprintf ppf " (select@ %s@ (bvadd@ %s@ " ax bv;
      pp_int_as_bv ppf i size;
      Format.pp_print_string ppf "))";
      print_multi_select_be (i + 1) ppf len ax bv size)
  in
  function
  | LittleEndian -> print_multi_select_le | BigEndian -> print_multi_select_be 0

let pp_print_defs ppf ctx =
  Queue.iter
    (function
      | Bl bl ->
          let name = BvTbl.find ctx.bl_cons bl in
          if name == once then BvTbl.replace ctx.bl_cons bl evicted
          else if name == evicted then ()
          else (
            Format.fprintf ppf "@[<h>(define-fun %s () Bool " name;
            print_bl_no_cons ctx ppf bl;
            Format.fprintf ppf ")@]@ ")
      | Bv bv ->
          let name = BvTbl.find ctx.bv_cons bv in
          if name == once then BvTbl.replace ctx.bv_cons bv evicted
          else if name == evicted then ()
          else (
            Format.fprintf ppf "@[<h>(define-fun %s () (_ BitVec %d) " name
              (Expr.sizeof bv);
            print_bv_no_cons ctx ppf bv;
            Format.fprintf ppf ")@]@ ")
      | Ax ax ->
          let name = AxTbl.find ctx.ax_cons ax in
          if name == once then AxTbl.replace ctx.ax_cons ax evicted
          else if name == evicted then ()
          else
            let (Symbol { index; _ }) = AxTbl.find ctx.ax_root ax in
            Format.fprintf ppf
              "@[<h>(define-fun %s () (Array (_ BitVec %d) (_ BitVec %d)) " name
              index byte_size;
            print_ax_no_cons ctx ppf ax;
            Format.fprintf ppf ")@]@ "
      | Decl dl ->
          Format.pp_print_string ppf dl;
          Format.pp_print_space ppf ())
    ctx.ordered_defs

let pp_flush_defs ppf ctx =
  pp_print_defs ppf ctx;
  Queue.clear ctx.ordered_defs

let flush_x find visit ctx ppf x =
  let name =
    match find ctx x with
    | exception Not_found ->
        visit ctx x;
        visit ctx x;
        find ctx x
    | name ->
        if name == once || name == evicted then (
          visit ctx x;
          find ctx x)
        else name
  in
  pp_flush_defs ppf ctx;
  name

let pp_flush_bl = flush_x (fun ctx bl -> BvTbl.find ctx.bl_cons bl) visit_bl
let pp_flush_bv = flush_x (fun ctx bv -> BvTbl.find ctx.bv_cons bv) visit_bv
let pp_print_bl = print_bl
let pp_print_bv = print_bv
let pp_print_ax = print_ax
let iter_free_variables f ctx = StTbl.iter f ctx.fvariables
let iter_free_arrays f ctx = StTbl.iter f ctx.farrays

let fold_array_accesses f ctx ar x =
  match AxTbl.find ctx.ordered_mem ar with
  | exception Not_found -> x
  | history -> Queue.fold f x history

let array_accesses_count ctx ar =
  match AxTbl.find ctx.ordered_mem ar with
  | exception Not_found -> 0
  | history -> Queue.length history
