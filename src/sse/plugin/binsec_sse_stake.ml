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

module StrMap = Basic_types.String.Map
open Types
open Ir

(* --- Expr utils --- *)
let rec pp_dba_rec ?(flag = 0) (_ : unit) (ppf : Format.formatter) = function
  | Dba.Expr.Var { size; name; _ } ->
      if 2 land flag = 0 then Format.fprintf ppf "%s" name
      else Format.fprintf ppf "%s<%d>" name size
  | Dba.Expr.Load (size, _, e, _) ->
      Format.fprintf ppf "%@[%a,%d]" (pp_parenthesis ~flag ()) e size
  | Dba.Expr.Cst bit ->
      if 1 land flag = 0 then
        Format.fprintf ppf "%a" Bitvector.pp_hex_or_bin bit
      else
        (* remove ambiguity on bitvector size in some cases *)
        Format.fprintf ppf "%a<%d>" Bitvector.pp_hex_or_bin bit
          (Bitvector.size_of bit)
  | Dba.Expr.Unary (op, e) -> Format.fprintf ppf "%a" (pp_dba_uop ~flag e) op
  | Dba.Expr.Binary (op, e, e') ->
      Format.fprintf ppf "%a" (pp_dba_bop ~flag e e') op
  | Dba.Expr.Ite (e, e', e'') ->
      Format.fprintf ppf "%a ? %a : %a" (pp_parenthesis ~flag ()) e
        (pp_parenthesis ~flag ()) e' (pp_parenthesis ~flag ()) e''

and pp_parenthesis ?(flag = 0) (_ : unit) (ppf : Format.formatter) = function
  | e -> (
      match e with
      | Dba.Expr.Var _ | Dba.Expr.Load _ | Dba.Expr.Cst _ ->
          pp_dba_rec ~flag () ppf e
      | e -> Format.fprintf ppf "(%a)" (pp_dba_rec ~flag ()) e)

and pp_dba_uop ?(flag = 0) (e : Dba.Expr.t) (ppf : Format.formatter) = function
  | Dba.Unary_op.Not -> Format.fprintf ppf "! %a" (pp_parenthesis ~flag ()) e
  | Dba.Unary_op.Sext size ->
      Format.fprintf ppf "sext%d %a" size (pp_parenthesis ~flag ()) e
  | Dba.Unary_op.Uext size ->
      Format.fprintf ppf "uext%d %a" size (pp_parenthesis ~flag ()) e
  | Dba.Unary_op.Restrict inter ->
      Format.fprintf ppf "%a{%d..%d}" (pp_parenthesis ~flag ()) e inter.hi
        inter.lo
  | Dba.Unary_op.UMinus -> Format.fprintf ppf "- %a" (pp_parenthesis ~flag ()) e

and pp_dba_bop ?(flag = 0) (e : Dba.Expr.t) (e' : Dba.Expr.t)
    (ppf : Format.formatter) = function
  | Plus ->
      Format.fprintf ppf "%a + %a" (pp_parenthesis ~flag ()) e
        (pp_parenthesis ~flag ()) e'
  | Minus ->
      Format.fprintf ppf "%a - %a" (pp_parenthesis ~flag ()) e
        (pp_parenthesis ~flag ()) e'
  | Mult ->
      Format.fprintf ppf "%a * %a" (pp_parenthesis ~flag ()) e
        (pp_parenthesis ~flag ()) e'
  | DivU ->
      Format.fprintf ppf "%a /u %a" (pp_parenthesis ~flag ()) e
        (pp_parenthesis ~flag ()) e'
  | RemU ->
      Format.fprintf ppf "%a %%u %a" (pp_parenthesis ~flag ()) e
        (pp_parenthesis ~flag ()) e'
  | DivS ->
      Format.fprintf ppf "%a /s %a" (pp_parenthesis ~flag ()) e
        (pp_parenthesis ~flag ()) e'
  | RemS ->
      Format.fprintf ppf "%a %%s %a" (pp_parenthesis ~flag ()) e
        (pp_parenthesis ~flag ()) e'
  | Or ->
      Format.fprintf ppf "%a | %a" (pp_parenthesis ~flag ()) e
        (pp_parenthesis ~flag ()) e'
  | And ->
      Format.fprintf ppf "%a & %a" (pp_parenthesis ~flag ()) e
        (pp_parenthesis ~flag ()) e'
  | Xor ->
      Format.fprintf ppf "%a ^ %a" (pp_parenthesis ~flag ()) e
        (pp_parenthesis ~flag ()) e'
  | Concat ->
      Format.fprintf ppf "%a :: %a" (pp_parenthesis ~flag ()) e
        (pp_parenthesis ~flag:0 ())
        e'
  | LShift ->
      Format.fprintf ppf "%a lsl %a" (pp_parenthesis ~flag ()) e
        (pp_parenthesis ~flag ()) e'
  | RShiftU ->
      Format.fprintf ppf "%a lsr %a" (pp_parenthesis ~flag ()) e
        (pp_parenthesis ~flag ()) e'
  | RShiftS ->
      Format.fprintf ppf "%a asr %a" (pp_parenthesis ~flag ()) e
        (pp_parenthesis ~flag ()) e'
  | LeftRotate ->
      Format.fprintf ppf "%a rol %a" (pp_parenthesis ~flag ()) e
        (pp_parenthesis ~flag ()) e'
  | RightRotate ->
      Format.fprintf ppf "%a ror %a" (pp_parenthesis ~flag ()) e
        (pp_parenthesis ~flag ()) e'
  | Eq ->
      Format.fprintf ppf "%a = %a" (pp_parenthesis ~flag ()) e
        (pp_parenthesis ~flag ()) e'
  | Diff ->
      Format.fprintf ppf "%a <> %a" (pp_parenthesis ~flag ()) e
        (pp_parenthesis ~flag ()) e'
  | LeqU ->
      Format.fprintf ppf "%a <=u %a" (pp_parenthesis ~flag ()) e
        (pp_parenthesis ~flag ()) e'
  | LtU ->
      Format.fprintf ppf "%a <u %a" (pp_parenthesis ~flag ()) e
        (pp_parenthesis ~flag ()) e'
  | GeqU ->
      Format.fprintf ppf "%a >=u %a" (pp_parenthesis ~flag ()) e
        (pp_parenthesis ~flag ()) e'
  | GtU ->
      Format.fprintf ppf "%a >u %a" (pp_parenthesis ~flag ()) e
        (pp_parenthesis ~flag ()) e'
  | LeqS ->
      Format.fprintf ppf "%a <=s %a" (pp_parenthesis ~flag ()) e
        (pp_parenthesis ~flag ()) e'
  | LtS ->
      Format.fprintf ppf "%a <s %a" (pp_parenthesis ~flag ()) e
        (pp_parenthesis ~flag ()) e'
  | GeqS ->
      Format.fprintf ppf "%a >=s %a" (pp_parenthesis ~flag ()) e
        (pp_parenthesis ~flag ()) e'
  | GtS ->
      Format.fprintf ppf "%a >s %a" (pp_parenthesis ~flag ()) e
        (pp_parenthesis ~flag ()) e'

let pp_dba = pp_dba_rec ()
let shortname = "stake"

module Logger = Logger.Sub (struct
  let name = shortname
end)

type watchpoint = Rvalue | Load | Address | Test | Target

module Watchpoint = struct
  type t = watchpoint

  let to_string = function
    | Rvalue -> "rvalue"
    | Load -> "load"
    | Address -> "address"
    | Test -> "test"
    | Target -> "jump-target"

  let of_string = function
    | "rvalue" -> Rvalue
    | "load" -> Load
    | "address" -> Address
    | "test" -> Test
    | "jump-target" -> Target
    | _ -> raise (Invalid_argument "of_string")

  let pp ppf t = Format.pp_print_string ppf (to_string t)
end

type mode = Ignore | Fix | Check

module type OPTIONS = sig
  val rval : mode
  val load : mode
  val addr : mode
  val test : mode
  val target : mode
  val reg_init : bool
  val mem_init : bool
end

module Builtin
    (O : OPTIONS)
    (E : ENGINE with type Path.value = Symbolic.Default.Expr.t) :
  EXTENSIONS with type path = E.Path.t = struct
  module Path = E.Path
  module Value = Path.Value

  type path = Path.t
  type loc = Var of Dba.Var.t | Mem of int * Machine.endianness * Dba.Expr.t

  type builtin +=
    | Init of loc
    | Fix of Dba.Expr.t * watchpoint
    | Check of Dba.Expr.t * watchpoint

  let instrumentation_routine : Revision.t -> unit =
    let visit_expr : Revision.t -> Revision.vertex -> Dba.Expr.t -> unit =
     fun graph vertex expr ->
      if O.reg_init then
        Dba_types.Var.Set.iter
          (fun ({ info; _ } as var) ->
            match info with
            | Register | Flag ->
                Revision.insert_before graph vertex (Builtin (Init (Var var)))
            | _ -> ())
          (Dba_types.Expr.collect_variables expr Dba_types.Var.Set.empty)
    in
    let instrument :
        Revision.t -> Revision.vertex -> Dba.Expr.t -> watchpoint -> unit =
     fun graph vertex expr watchpoint ->
      let opt =
        match watchpoint with
        | Rvalue -> O.rval
        | Load -> O.load
        | Address -> O.addr
        | Test -> O.test
        | Target -> O.target
      in
      match opt with
      | Ignore -> ()
      | Fix ->
          Revision.insert_before graph vertex (Builtin (Fix (expr, watchpoint)))
      | Check ->
          Revision.insert_before graph vertex
            (Builtin (Check (expr, watchpoint)))
    in
    fun graph ->
      Revision.iter_new_vertex
        (fun vertex ->
          match Revision.node graph vertex with
          | Fallthrough { kind = Assign { rval; _ }; _ } ->
              visit_expr graph vertex rval;
              instrument graph vertex rval Rvalue
          | Fallthrough { kind = Load { var = { size; _ }; dir; addr; _ }; _ }
            ->
              visit_expr graph vertex addr;
              instrument graph vertex addr Address;
              if O.mem_init then
                Revision.insert_before graph vertex
                  (Builtin (Init (Mem (size / 8, dir, addr))));
              let load = Dba.Expr.load (Size.Byte.create (size / 8)) dir addr in
              instrument graph vertex load Load
          | Fallthrough { kind = Store { addr; rval; _ }; _ } ->
              visit_expr graph vertex rval;
              visit_expr graph vertex addr;
              instrument graph vertex rval Rvalue;
              instrument graph vertex addr Address
          | Fallthrough { kind = Assume test; _ }
          | Fallthrough { kind = Assert test; _ }
          | Branch { test; _ } ->
              visit_expr graph vertex test;
              instrument graph vertex test Test
          | Terminator { kind = Jump { target; _ }; _ } ->
              visit_expr graph vertex target;
              instrument graph vertex target Target
          | _ -> ())
        graph

  let wordsize = Machine.ISA.word_size E.isa

  let full_mask : unit Zmap.t =
    Zmap.singleton ~lo:Z.zero ~hi:(Z.extract Z.minus_one 0 wordsize) ()

  let slice : Bitvector.t -> int -> Image.buffer Zmap.t =
   fun addr size ->
    let lo = Bitvector.value_of addr in
    let hi = Z.add lo (Z.of_int (size - 1)) in
    let view = Zmap.singleton ~lo ~hi Image.Zero in
    Zmap.union_left
      (Zmap.substract ~crop:Image.crop_buffer E.image.content
         (Zmap.substract full_mask view))
      view

  let read : int -> Machine.endianness -> Bitvector.t -> Bitvector.t =
   fun size endianness addr ->
    let reader =
      Image.content_reader
        (Virtual_address.of_bitvector addr)
        (Z.of_int size) ~endianness (slice addr size)
    in
    Reader.Read.read reader size

  let hard_patch : Path.t -> Image.buffer Zmap.t -> unit =
   fun path content ->
    Zmap.iter
      (fun (Item { lo; hi; elt }) ->
        let cst = Bitvector.create lo wordsize in
        let addr = Value.constant cst in
        match (elt : Image.buffer) with
        | Zero ->
            Logger.debug ~level:1 "%a: Zeroing addresses [%a .. %a]"
              Virtual_address.pp (Path.pc path) Bitvector.pp_hex_or_bin cst
              Bitvector.pp_hex_or_bin
              (Bitvector.create hi wordsize);
            Path.memcpy_v path None ~addr
              (Z.to_int (Z.sub hi lo) + 1)
              (Bigarray.Array1.create Bigarray.int8_unsigned C_layout 0)
        | Data { offset; len; value } ->
            Logger.debug ~level:1 "%a: Loading addresses [%a .. %a] from file"
              Virtual_address.pp (Path.pc path) Bitvector.pp_hex_or_bin cst
              Bitvector.pp_hex_or_bin
              (Bitvector.create hi wordsize);
            Path.memcpy_v path None ~addr len
              (Bigarray.Array1.sub value offset len))
      content

  let soft_patch :
      int ->
      Machine.endianness ->
      Bitvector.t ->
      path ->
      Symbolic.Default.Memory.symbol ->
      unit =
   fun size endianness addr path (Symbol _ as root) ->
    let value = read size endianness addr in
    let load =
      Symbolic.Default.Expr.load size endianness (Value.constant addr) root
    in
    Logger.debug ~level:1 "%a: Soft patching %a with %a" Virtual_address.pp
      (Path.pc path) Bitvector.pp_hex_or_bin addr Bitvector.pp_hex_or_bin value;
    match Path.assume_v path (Value.binary Eq load (Value.constant value)) with
    | Some _ -> ()
    | None ->
        Logger.warning "%a: Soft patching %a with %a failed." Virtual_address.pp
          (Path.pc path) Bitvector.pp_hex_or_bin addr Bitvector.pp_hex_or_bin
          value
    | exception Symbolic.State.Unknown ->
        Logger.warning "%a: Soft patching %a with %a timed out."
          Virtual_address.pp (Path.pc path) Bitvector.pp_hex_or_bin addr
          Bitvector.pp_hex_or_bin value

  let rec patch : int -> Machine.endianness -> Bitvector.t -> path -> unit =
   fun size endianness addr path ->
    match Path.read_v path None ~addr:(Value.constant addr) size endianness with
    | Cst _ -> ()
    | Load { label = Symbol _; _ } -> hard_patch path (slice addr size)
    | Load { label; _ } ->
        soft_patch size endianness addr path
          (Symbolic.Default.Memory.base label)
    | _ -> if 1 < size then split_patch size endianness addr path

  and split_patch : int -> Machine.endianness -> Bitvector.t -> path -> unit =
   fun size endianness addr path ->
    patch 1 endianness addr path;
    if 1 < size then
      split_patch (size - 1) endianness (Bitvector.succ addr) path

  let init : Dba.Var.t -> path -> unit =
   fun ({ name; size; _ } as var) path ->
    match Path.State.lookup var (Path.state path) with
    | exception Symbolic.State.Undefined _ ->
        Logger.warning "%a: Initializing register %s to zero" Virtual_address.pp
          (Path.pc path) name;
        Path.assign_v path var (Value.constant (Bitvector.zeros size))
    | _ -> ()

  let fetch : int -> Machine.endianness -> Dba.Expr.t -> path -> unit =
   fun size dir addr path ->
    let value = Path.get_value path addr in
    if Path.is_symbolic_v path value then
      Logger.warning "%a: Failed to ensure %a is initialized" Virtual_address.pp
        (Path.pc path) pp_dba
        (Dba.Expr.load (Size.Byte.create size) dir addr)
    else
      let cst = Path.eval_v path value in
      patch size dir cst path

  let fix : Dba.Expr.t -> watchpoint -> path -> unit =
   fun expr watchpoint path ->
    let value = Path.get_value path expr in
    if Path.is_symbolic_v path value then (
      let cst = Path.eval_v path value in
      Logger.warning "%a: Enforce %a = %a (%a)" Virtual_address.pp
        (Path.pc path) pp_dba expr Bitvector.pp_hex_or_bin cst Watchpoint.pp
        watchpoint;
      ignore (Path.assume_v path (Value.binary Eq value (Value.constant cst))))

  let check : Dba.Expr.t -> watchpoint -> path -> path continuation =
   fun expr watchpoint path ->
    let value = Path.get_value path expr in
    let t1 = Unix.gettimeofday () in
    Logger.debug ~level:3 "Checking %a (%a)" pp_dba expr Watchpoint.pp
      watchpoint;
    let witness : trilean =
      match
        Path.check_sat_assuming_v path
          (Value.binary Diff value (Value.constant (Path.eval_v path value)))
      with
      | exception Symbolic.State.Unknown -> Unknown
      | None -> True
      | Some _ -> False
    in
    let t2 = Unix.gettimeofday () in
    Logger.debug ~level:3 "checked (time %fs)" (t2 -. t1);
    match witness with
    | True -> Return
    | Unknown ->
        Logger.error "%a: Potential non-deterministic expression %a (%a)"
          Virtual_address.pp (Path.pc path) pp_dba expr Watchpoint.pp watchpoint;
        Signal Unresolved_formula
    | False ->
        Logger.error "%a: Non-deterministic expression %a (%a)"
          Virtual_address.pp (Path.pc path) pp_dba expr Watchpoint.pp watchpoint;
        Signal Unsatisfiable_assumption

  let list =
    [
      Instrumentation_routine instrumentation_routine;
      Builtin_resolver
        (function
        | Init (Var var) -> Apply (init var)
        | Init (Mem (size, dir, addr)) -> Apply (fetch size dir addr)
        | Fix (expr, info) -> Apply (fix expr info)
        | Check (expr, info) -> Call (check expr info)
        | _ -> Unknown);
      Builtin_printer
        (fun ppf builtin ->
          match builtin with
          | Init (Var { name; _ }) ->
              Format.fprintf ppf "ensure init %s" name;
              true
          | Init (Mem (size, dir, addr)) ->
              Format.fprintf ppf "ensure init %a" pp_dba
                (Dba.Expr.load (Size.Byte.create size) dir addr);
              true
          | Fix (expr, info) ->
              Format.fprintf ppf "fix %a (%a)" pp_dba expr Watchpoint.pp info;
              true
          | Check (expr, info) ->
              Format.fprintf ppf "check %a (%a)" pp_dba expr Watchpoint.pp info;
              true
          | _ -> false);
    ]
end

module Plugin (O : OPTIONS) : PLUGIN = struct
  let name = shortname
  let fields _ = []

  let extensions :
      type a. (module ENGINE with type Path.t = a) -> a extension list =
   fun engine ->
    let module Engine = (val engine) in
    match Engine.Path.State.more Symbolic.State.ValueKind with
    | Some Symbolic.Default.Term ->
        let module Extensions = Builtin (O) (Engine) in
        Extensions.list
    | _ ->
        Logger.fatal "unable to use '%s' within the current symbolic engine"
          shortname
end
