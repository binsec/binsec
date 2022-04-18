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

let byte_size = Natural.to_int Basic_types.Constants.bytesize

module Session = struct
  let n = ref 0

  type state = Dead | Assert | Sat | Unsat

  type session = {
    solver : Prover.t;
    mutable state : state;
    pid : Subprocess.t;
    stdin : out_channel;
    stdout : in_channel;
    stderr : in_channel;
    stdlog : out_channel option;
    formatter : Format.formatter;
  }

  let close t = ignore @@ Subprocess.close t.pid

  let start ?stdlog timeout solver =
    Sse_options.Logger.debug "Openning session %d" !n;
    incr n;
    let cmd = Prover.command timeout solver in
    let pid = Subprocess.spawn ~pdeathsig:Sys.sigkill cmd in
    let stdin = Subprocess.stdin pid
    and stdout = Subprocess.stdout pid
    and stderr = Subprocess.stderr pid in
    let formatter =
      match stdlog with
      | None -> Format.formatter_of_out_channel stdin
      | Some stdlog ->
          let output str pos len =
            output_substring stdlog str pos len;
            output_substring stdin str pos len
          and flush () =
            flush stdlog;
            flush stdin
          in
          Format.make_formatter output flush
    in
    Format.fprintf formatter
      "@[<v 0>(set-option :print-success false)@ (set-info :smt-lib-version \
       2.5)@ (set-logic QF_ABV)@ @]";
    { solver; state = Assert; pid; stdin; stdout; stderr; stdlog; formatter }

  type status = SAT | UNSAT | UNKNOWN

  let check_sat t =
    Format.fprintf t.formatter "(check-sat)@.";
    match input_line t.stdout with
    | "sat" ->
        t.state <- Sat;
        SAT
    | "unsat" ->
        t.state <- Unsat;
        UNSAT
    | "unknown" ->
        t.state <- Assert;
        UNKNOWN
    | s -> Sse_options.Logger.fatal "Solver returned: %s" s
    | exception End_of_file ->
        t.state <- Dead;
        UNKNOWN

  let value_of_constant cst =
    let open Smtlib in
    match cst with
    | CstBool false -> Z.zero
    | CstBool true -> Z.one
    | CstBinary b -> Z.of_string_base 2 b
    | CstDecimal d | CstDecimalSize (d, _) -> Z.of_string_base 10 d
    | CstHexadecimal x -> Z.of_string_base 16 x
    | CstNumeral _ | CstString _ ->
        Sse_options.Logger.fatal
          "Model construction: unexpected constant %a as bitvector value"
          Smtlib_pp.pp_spec_constant cst

  let get_value t pp x =
    assert (t.state = Sat);
    Format.fprintf t.formatter "(get-value (%a))@." pp x;
    let lexbuf = Lexing.from_channel t.stdout in
    let open Smtlib in
    match Smtlib_parser.ivalue Smtlib_lexer.token lexbuf with
    | [ (_, { term_desc = TermSpecConstant cst; _ }) ] -> value_of_constant cst
    | _ -> assert false

  let put t pp x =
    pp t.formatter x;
    Format.pp_print_space t.formatter ();
    t.state <- Assert
end

(* utils *)
let pp_int_as_bv ppf x = function
  | 1 -> Format.fprintf ppf "#b%d" x
  | 4 -> Format.fprintf ppf "#x%01x" x
  | 8 -> Format.fprintf ppf "#x%02x" x
  | 12 -> Format.fprintf ppf "#x%03x" x
  | 16 -> Format.fprintf ppf "#x%04x" x
  | 20 -> Format.fprintf ppf "#x%05x" x
  | 24 -> Format.fprintf ppf "#x%06x" x
  | 28 -> Format.fprintf ppf "#x%07x" x
  | 32 -> Format.fprintf ppf "#x%08x" x
  | sz -> Format.fprintf ppf "(_ bv%d %d)" x sz

let pp_bv ppf value size =
  try pp_int_as_bv ppf (Z.to_int value) size
  with Z.Overflow -> Format.fprintf ppf "(_ bv%a %d)" Z.pp_print value size

module Solver () : Solver_sig.S = struct
  type result = Sat | Unsat | Unknown

  type memory = unit

  type term = string * int

  type access = Select of term * int | Store of term

  open Sexpr

  module Context = struct
    type t = {
      mutable id : Suid.t;
      bv_decl : unit BvTbl.t;
      bl_cons : string BvTbl.t;
      bv_cons : string BvTbl.t;
      ax_cons : string AxTbl.t;
      ordered_defs : def Queue.t;
      ordered_mem : access Queue.t;
    }

    and def = Bl of Expr.t | Bv of Expr.t | Ax of Memory.t

    let create id =
      {
        id;
        bv_decl = BvTbl.create 16;
        bl_cons = BvTbl.create 32;
        bv_cons = BvTbl.create 128;
        ax_cons = AxTbl.create 64;
        ordered_defs = Queue.create ();
        ordered_mem = Queue.create ();
      }
  end

  open Context

  let pp_int_as_offset ppf i = pp_bv ppf i (Kernel_options.Machine.word_size ())

  let once = ""

  let rec visit_bl ctx bl =
    try
      if BvTbl.find ctx.bl_cons bl == once then (
        let name = Suid.to_string ctx.id in
        ctx.id <- Suid.incr ctx.id;
        BvTbl.replace ctx.bl_cons bl name)
    with Not_found -> (
      match bl with
      | Cst _ -> ()
      | Load _ (* cannot be a bl<1> *) -> assert false
      | Unary { f = Not; x; _ } ->
          BvTbl.add ctx.bl_cons bl once;
          visit_bl ctx x;
          Queue.push (Bl bl) ctx.ordered_defs
      | Binary { f = And | Or; x; y; _ } ->
          BvTbl.add ctx.bl_cons bl once;
          visit_bl ctx x;
          visit_bl ctx y;
          Queue.push (Bl bl) ctx.ordered_defs
      | Binary
          {
            f = Eq | Diff | Uge | Ule | Ugt | Ult | Sge | Sle | Sgt | Slt;
            x;
            y;
            _;
          } ->
          BvTbl.add ctx.bl_cons bl once;
          visit_bv ctx x;
          visit_bv ctx y;
          Queue.push (Bl bl) ctx.ordered_defs
      | Ite { c; t; e; _ } ->
          BvTbl.add ctx.bl_cons bl once;
          visit_bl ctx c;
          visit_bl ctx t;
          visit_bl ctx e;
          Queue.push (Bl bl) ctx.ordered_defs
      | Var _ | Unary _ | Binary _ -> visit_bv ctx bl)

  and visit_bv ctx bv =
    try
      if BvTbl.find ctx.bv_cons bv == once then (
        let name = Suid.to_string ctx.id in
        ctx.id <- Suid.incr ctx.id;
        BvTbl.replace ctx.bv_cons bv name)
    with Not_found -> (
      match bv with
      | Var { name; size; _ } ->
          BvTbl.add ctx.bv_cons bv name;
          if size = 1 then
            BvTbl.add ctx.bl_cons bv (Printf.sprintf "(= %s #b1)" name);
          BvTbl.add ctx.bv_decl bv ()
      | Load { len; addr; label; _ } ->
          BvTbl.add ctx.bv_cons bv once;
          visit_bv ctx addr;
          visit_bv ctx addr;
          visit_ax ctx label;
          if len > 1 then visit_ax ctx label;
          Queue.push (Bv bv) ctx.ordered_defs;
          Queue.push
            (Select ((BvTbl.find ctx.bv_cons addr, Expr.sizeof addr), len))
            ctx.ordered_mem
      | Cst _ ->
          BvTbl.add ctx.bv_cons bv once;
          Queue.push (Bv bv) ctx.ordered_defs
      | Unary { x; _ } ->
          BvTbl.add ctx.bv_cons bv once;
          visit_bv ctx x;
          Queue.push (Bv bv) ctx.ordered_defs
      | Binary
          { f = Eq | Diff | Uge | Ule | Ugt | Ult | Sge | Sle | Sgt | Slt; _ }
        ->
          BvTbl.add ctx.bv_cons bv once;
          visit_bl ctx bv;
          Queue.push (Bv bv) ctx.ordered_defs
      | Binary
          {
            f = Rol | Ror;
            x;
            y = (Load _ | Unary _ | Binary _ | Ite _) as y;
            _;
          } ->
          BvTbl.add ctx.bv_cons bv once;
          visit_bv ctx x;
          visit_bv ctx x;
          visit_bv ctx y;
          visit_bv ctx y;
          Queue.push (Bv bv) ctx.ordered_defs
      | Binary { x; y; _ } ->
          BvTbl.add ctx.bv_cons bv once;
          visit_bv ctx x;
          visit_bv ctx y;
          Queue.push (Bv bv) ctx.ordered_defs
      | Ite { c; t; e; _ } ->
          BvTbl.add ctx.bv_cons bv once;
          visit_bl ctx c;
          visit_bv ctx t;
          visit_bv ctx e;
          Queue.push (Bv bv) ctx.ordered_defs)

  and visit_ax ctx ax =
    try
      if AxTbl.find ctx.ax_cons ax == once then (
        let name = Suid.to_string ctx.id in
        ctx.id <- Suid.incr ctx.id;
        AxTbl.replace ctx.ax_cons ax name)
    with Not_found -> (
      match ax with
      | Memory.Unknown -> AxTbl.add ctx.ax_cons ax Suid.(to_string zero)
      | Memory.Source { over; _ } ->
          AxTbl.add ctx.ax_cons ax once;
          visit_ax ctx over;
          Queue.push (Ax ax) ctx.ordered_defs
      | Memory.Layer { addr; bytes; over; _ } ->
          AxTbl.add ctx.ax_cons ax once;
          visit_bv ctx addr;
          visit_bv ctx addr;
          BiMap.iter (fun _ bv -> visit_bv ctx bv) bytes;
          visit_ax ctx over;
          Queue.push (Ax ax) ctx.ordered_defs;
          let index = BvTbl.find ctx.bv_cons addr in
          BiMap.iter
            (fun i _ ->
              let index =
                Format.asprintf "(bvadd %s %a)" index pp_int_as_offset i
              in
              Queue.push (Store (index, Expr.sizeof addr)) ctx.ordered_mem)
            bytes)

  let pp_unop ppf (op : Term.unary Term.operator) =
    match op with
    | Not -> Format.pp_print_string ppf "bvnot"
    | Minus -> Format.pp_print_string ppf "bvneg"
    | Uext n -> Format.fprintf ppf "(_ zero_extend %d)" n
    | Sext n -> Format.fprintf ppf "(_ sign_extend %d)" n
    | Restrict { Interval.hi; lo } ->
        Format.fprintf ppf "(_ extract %d %d)" hi lo

  let pp_binop =
    let string_of_binop (op : Term.binary Term.operator) =
      match op with
      | Plus -> "bvadd"
      | Minus -> "bvsub"
      | Mul -> "bvmul"
      | Udiv -> "bvudiv"
      | Sdiv -> "bvsdiv"
      | Umod -> "bvurem"
      | Smod -> "bvsrem"
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
    let name = BvTbl.find ctx.bl_cons bl in
    if name == once then print_bl_no_cons ctx ppf bl
    else Format.pp_print_string ppf name

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
    | Binary { f = (And | Or) as f; x; y; _ } ->
        Format.pp_print_char ppf '(';
        (Format.pp_print_string ppf
        @@ match f with And -> "and" | Or -> "or" | _ -> assert false);
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
        {
          f = (Eq | Uge | Ule | Ugt | Ult | Sge | Sle | Sgt | Slt) as f;
          x;
          y;
          _;
        } ->
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
    if name == once then print_bv_no_cons ctx ppf bv
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
    | Binary { f = Diff; _ } ->
        Format.pp_print_string ppf "(ite";
        Format.pp_print_space ppf ();
        print_bl ctx ppf bv;
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
        pp_binop ppf
          (match f with Rol -> Lsl | Ror -> Lsr | _ -> assert false);
        Format.pp_print_space ppf ();
        Format.pp_print_string ppf (BvTbl.find ctx.bv_cons x);
        Format.pp_print_space ppf ();
        Format.pp_print_string ppf (BvTbl.find ctx.bv_cons y);
        Format.pp_print_char ppf ')';
        Format.pp_print_space ppf ();
        Format.pp_print_char ppf '(';
        pp_binop ppf
          (match f with Rol -> Lsr | Ror -> Lsl | _ -> assert false);
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
    if name == once then print_ax_no_cons ctx ppf ax
    else Format.pp_print_string ppf name

  and print_ax_no_cons ctx ppf ax =
    match ax with
    | Memory.Unknown -> Suid.pp ppf Suid.zero
    | Memory.Source { over; _ } -> print_ax ctx ppf over
    | Memory.Layer { addr; bytes; over; pop = 1; _ } ->
        Format.pp_print_string ppf "(store";
        Format.pp_print_space ppf ();
        print_ax ctx ppf over;
        Format.pp_print_space ppf ();
        print_bv ctx ppf addr;
        Format.pp_print_space ppf ();
        print_bv ctx ppf (snd (BiMap.choose bytes));
        Format.pp_print_char ppf ')'
    | Memory.Layer { addr; bytes; over; pop; _ } ->
        for _ = 1 to pop do
          Format.pp_print_string ppf "(store";
          Format.pp_print_space ppf ()
        done;
        print_ax ctx ppf over;
        let addr_space = Expr.sizeof addr
        and idx = BvTbl.find ctx.bv_cons addr in
        BiMap.iter
          (fun i bv ->
            Format.pp_print_space ppf ();
            if Z.zero = i then (
              Format.pp_print_string ppf idx;
              Format.pp_print_space ppf ())
            else (
              Format.pp_print_string ppf "(bvadd";
              Format.pp_print_space ppf ();
              Format.pp_print_string ppf idx;
              Format.pp_print_space ppf ();
              pp_bv ppf i addr_space;
              Format.pp_print_char ppf ')';
              Format.pp_print_space ppf ());
            print_bv ctx ppf bv;
            Format.pp_print_char ppf ')')
          bytes

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
    | LittleEndian -> print_multi_select_le
    | BigEndian -> print_multi_select_be 0

  let put ctx ppf constraints =
    Format.pp_open_vbox ppf 0;
    (* init *)
    BvTbl.add ctx.bl_cons Expr.zero "false";
    BvTbl.add ctx.bv_cons Expr.zero "#b0";
    BvTbl.add ctx.bl_cons Expr.one "true";
    BvTbl.add ctx.bv_cons Expr.one "#b1";
    (* visit assertions *)
    List.iter (visit_bl ctx) constraints;
    (* print declarations *)
    BvTbl.iter
      (fun bv _ ->
        match bv with
        | Var { name; size; _ } ->
            Format.fprintf ppf "(declare-const %s (_ BitVec %d))@ " name size
        | _ -> assert false)
      ctx.bv_decl;
    (if Queue.is_empty ctx.ordered_mem = false then
     let addr_space = Kernel_options.Machine.word_size () in
     Format.fprintf ppf
       "(declare-const %a (Array (_ BitVec %d) (_ BitVec %d)))@ " Suid.pp
       Suid.zero addr_space byte_size);
    (* print assertions *)
    Format.pp_open_hovbox ppf 0;
    Queue.iter
      (function
        | Bl bl ->
            let name = BvTbl.find ctx.bl_cons bl in
            if name != once then (
              Format.fprintf ppf "(define-fun %s () Bool " name;
              print_bl_no_cons ctx ppf bl;
              Format.fprintf ppf ")@ ")
        | Bv bv ->
            let name = BvTbl.find ctx.bv_cons bv in
            if name != once then (
              Format.fprintf ppf "(define-fun %s () (_ BitVec %d) " name
                (Expr.sizeof bv);
              print_bv_no_cons ctx ppf bv;
              Format.fprintf ppf ")@ ")
        | Ax ax ->
            let name = AxTbl.find ctx.ax_cons ax in
            if name != once then (
              Format.fprintf ppf
                "(define-fun %s () (Array (_ BitVec %d) (_ BitVec %d)) " name
                (Kernel_options.Machine.word_size ())
                byte_size;
              print_ax_no_cons ctx ppf ax;
              Format.fprintf ppf ")@ "))
      ctx.ordered_defs;
    List.iter
      (fun bl ->
        Format.pp_print_string ppf "(assert ";
        print_bl ctx ppf bl;
        Format.pp_print_char ppf ')';
        Format.pp_print_space ppf ())
      constraints;
    Format.pp_close_box ppf ();
    Format.pp_close_box ppf ()

  let session =
    Session.start (* ~stdlog:stderr *)
      (int_of_float (Sse_options.Timeout.get ()))
      (Formula_options.Solver.get ())

  let ctx = ref None

  let bind fid e constraints =
    let ctx =
      let x = create fid in
      ctx := Some x;
      x
    in
    visit_bv ctx e;
    visit_bv ctx e;
    put ctx session.formatter constraints;
    (BvTbl.find ctx.bv_cons e, Expr.sizeof e)

  let put fid constraints =
    ctx := Some (create fid);
    put (Option.get !ctx) session.formatter constraints

  let get e = (BvTbl.find (Option.get !ctx).bv_cons e, Expr.sizeof e)

  let add bl =
    let ctx = Option.get !ctx in
    Session.put session
      (fun ppf bl -> Format.fprintf ppf "(assert %a)" (print_bl ctx) bl)
      bl

  let neq (e, s) x =
    Session.put session
      (fun ppf () ->
        Format.fprintf ppf "(assert (not (= %s " e;
        pp_bv ppf x s;
        Format.pp_print_string ppf ")))")
      ()

  let get_memory () = ((), (Option.get !ctx).ordered_mem)

  let get_at () (x, _) =
    Session.get_value session
      (fun ppf -> Format.fprintf ppf "(select %s %s)" Suid.(to_string zero))
      x

  let get_value (x, _) = Session.get_value session Format.pp_print_string x

  let succ (x, s) = (Format.asprintf "(bvadd %s %a)" x pp_int_as_offset Z.one, s)

  let check_sat () =
    match Session.check_sat session with
    | UNKNOWN -> Unknown
    | UNSAT -> Unsat
    | SAT -> Sat

  let close () = Session.close session
end
