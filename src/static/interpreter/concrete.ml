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

open Simulation

module Bv = Bitvector
module It = struct
  include Interval
  let l8 = {lo=0; hi=7}
end
module Var = Basic_types.String
module Addr = struct
  include Virtual_address
  let of_caddress = Dba_types.Caddress.to_virtual_address
end
module Caddr = Dba_types.Caddress

module Env = struct
  type t = {meta : Bv.t Var.Map.t; main : Bv.t Addr.Map.t}
  exception Ox8BADF00D of Dba.Expr.t

  module Load = struct
    let ox8BADF00D s m i =
      let sz = Size.Byte.create s in
      let ax = Machine.Word_size.get () |> Bv.create @@ Addr.to_bigint i in
      let ex = Dba.Expr.load sz m @@ Dba.Expr.constant ax in
      Ox8BADF00D ex
  end
  module Restrict = struct
    let ox8BADF00D n s =
      let ex = Dba.Expr.var n s None in
      Ox8BADF00D ex
  end

  (* helper functions *)
  let rec split succ i v s t =
    if s = 1 then Addr.Map.add i v t
    else split succ (succ i) (Bv.extract v It.{lo=8; hi=8*s-1}) (s - 1)
      @@ Addr.Map.add i (Bv.extract v It.l8) t
  let write e m i v s = match m with
    | Dba.BigEndian ->
      {e with main = split Addr.pred (i |> Addr.add_int @@ s - 1) v s e.main}
    | Dba.LittleEndian ->
      {e with main = split Addr.succ i v s e.main}
  let rec join succ i l s t =
    if s = 1 then Addr.Map.find i t :: l
    else join succ (succ i) (Addr.Map.find i t :: l) (s - 1) t
  let read e m i s = try match m with
    | Dba.BigEndian ->
      join Addr.pred (Addr.add_int s i) [] s e.main |> Bv.concat
    | Dba.LittleEndian -> join Addr.succ i [] s e.main |> Bv.concat
    with Not_found -> raise @@ Load.ox8BADF00D s m i

  let empty = {meta=Var.Map.empty; main=Addr.Map.empty}

  let eval_unop  = Dba.Unary_op.(function
      | UMinus     -> Bitvector.neg
      | Not        -> Bitvector.lognot
      | Sext s     -> fun b -> Bitvector.extend_signed b s
      | Uext s     -> fun b -> Bitvector.extend b s
      | Restrict i -> fun b -> Bitvector.extract b i)

  let eval_binop = Dba.Binary_op.(function
      | Plus        -> Bitvector.add
      | Minus       -> Bitvector.sub
      | Mult        -> Bitvector.mul
      | DivU        -> Bitvector.udiv
      | DivS        -> Bitvector.sdiv
      | ModU        -> Bitvector.umod
      | ModS        -> Bitvector.smod
      | Or          -> Bitvector.logor
      | And         -> Bitvector.logand
      | Xor         -> Bitvector.logxor
      | Concat      -> Bitvector.append
      | Eq          -> fun x y -> Bitvector.of_bool (Bitvector.equal x y)
      | Diff        -> fun x y -> Bitvector.of_bool (Bitvector.diff x y)
      | LeqU        -> fun x y -> Bitvector.of_bool (Bitvector.ule x y)
      | LtU         -> fun x y -> Bitvector.of_bool (Bitvector.ult x y)
      | GeqU        -> fun x y -> Bitvector.of_bool (Bitvector.uge x y)
      | GtU         -> fun x y -> Bitvector.of_bool (Bitvector.ugt x y)
      | LeqS        -> fun x y -> Bitvector.of_bool (Bitvector.sle x y)
      | LtS         -> fun x y -> Bitvector.of_bool (Bitvector.slt x y)
      | GeqS        -> fun x y -> Bitvector.of_bool (Bitvector.sge x y)
      | GtS         -> fun x y -> Bitvector.of_bool (Bitvector.sgt x y)
      | LShift      -> fun x y ->
          let s = Bigint.int_of_big_int (Bitvector.value_of y) in
          assert (s <= Bitvector.size_of x);
          Bitvector.shift_left x s
      | RShiftU     -> fun x y ->
          let s = Bigint.int_of_big_int (Bitvector.value_of y) in
          assert (s <= Bitvector.size_of x);
          Bitvector.shift_right x s
      | RShiftS     -> fun x y ->
          let s = Bigint.int_of_big_int (Bitvector.value_of y) in
          assert (s <= Bitvector.size_of x);
          Bitvector.shift_right_signed x s
      | LeftRotate  -> fun x y ->
          let s = Bigint.int_of_big_int (Bitvector.value_of y) in
          assert (s <= Bitvector.size_of x);
          Bitvector.rotate_left x s
      | RightRotate -> fun x y ->
          let s = Bigint.int_of_big_int (Bitvector.value_of y) in
          assert (s <= Bitvector.size_of x);
          Bitvector.rotate_right x s)

  let rec eval e = let open Dba.Expr in function
      | Cst (_, v)         -> v
      | Var (n, _, _) as v ->
        begin try Var.Map.find n e.meta
        with Not_found -> raise @@ Ox8BADF00D v end
      | Load (s, m, a)     ->
        read e m (Addr.of_bitvector @@ eval e a) s
      | Unary (o, a)       ->
        eval_unop o @@ eval e a
      | Binary (o, a, b)   ->
        eval e b |> eval_binop o @@ eval e a
      | Ite (c, t, f)      ->
        if Bv.is_zero @@ eval e c then eval e f else eval e t

  let assign e l v = match l with
    | Dba.LValue.Store (s, m, a) ->
      write e m (Addr.of_bitvector @@ eval e a) v s
    | Dba.LValue.Var (n, _, _) -> {e with meta = Var.Map.add n v e.meta}
    | Dba.LValue.Restrict (n, s, {It.lo; It.hi}) ->
      let v = match Var.Map.find n e.meta with
        | o when lo = 0 ->
          v |> Bv.append @@ Bv.extract o It.{lo=hi + 1; hi=Bv.size_of o - 1}
        | o when hi = Bv.size_of o - 1 ->
          Bv.append v @@ Bv.extract o It.{lo=0; hi=lo - 1}
        | o -> Bv.append (Bv.extract o It.{lo=hi + 1; hi=Bv.size_of o - 1})
          @@ Bv.append v (Bv.extract o It.{lo=0; hi=lo - 1})
        | exception Not_found -> raise @@ Restrict.ox8BADF00D n s in
      {e with meta = Var.Map.add n v e.meta}

  let kill e = function
    | Dba.LValue.Store _ -> Errors.not_yet_implemented "Undef on memory cell."
    | Dba.LValue.Var (n, _, _) | Dba.LValue.Restrict (n, _, _) ->
      {e with meta = Var.Map.remove n e.meta}

  let load_memory_file e filename =
    let map =
      let parser = Parser.patchmap and lexer = Lexer.token in
      Parse_utils.read_file ~parser ~lexer ~filename in
    { e with
      main = Virtual_address.Map.fold
          (fun a bs map ->
             let l = Binstream.length bs in
             let rec aux m i =
               if i < l then
                 let b = Binstream.get_byte_exn bs i in
                 let m' = Virtual_address.Map.add
                     (Virtual_address.add_int i a)
                     (Bitvector.create (Bigint.big_int_of_int b) 8)
                     m in
                 aux m' (succ i)
               else m
             in aux map 0)
          map e.main }

  let init e = function
    | Parse_helpers.Initialization.Assignment (lval, rval) ->
      eval e rval |> assign e lval
    | Parse_helpers.Initialization.MemLoad (addr, size) ->
      let img = Kernel_functions.get_img () in
      let addr = Virtual_address.of_int64 addr in
      let rec loop e s =
        if s = 0 then e
        else
          let addr = Virtual_address.add_int s addr in
          let byte = Virtual_address.to_bigint addr
                  |> Loader_utils.get_byte_at img |> Bv.of_int ~size:8 in
          let e = write e Dba.LittleEndian addr byte 1 in
          loop e @@ s - 1 in
      loop e size
    | Parse_helpers.Initialization.NondeterministicAssignment(lval, _) ->
      raise @@ Ox8BADF00D (Dba.LValue.to_expr lval)

  let load_init_file e filename =
    let parser = Parser.initialization and lexer = Lexer.token in
    Parse_utils.read_file ~parser ~lexer ~filename
    |> List.fold_left init e
end

module type Program = sig
  type t
  exception Ox8BADF00D of Dba.address

  val fetch : t -> Dba.address -> Dba.Instr.t
end

module Interpreter (P: Program) = struct
  exception AssertFailure of Dba.address
  exception EndOfTrace of Env.t

  let echo ppf = function
    | Dba.Exp e -> Dba_printer.Ascii.pp_bl_term ppf e
    | Dba.Str s -> Format.fprintf ppf "%s" s

  let step e a = let open Dba.Instr in function
    | Assign (l, x, n) -> Env.assign e l @@ Env.eval e x, Caddr.reid a n
    | Undef (l, n) -> Env.kill e l, Caddr.reid a n
    | SJump (Dba.JInner n, _) -> e, Caddr.reid a n
    | SJump (Dba.JOuter n, _) -> e, n
    | DJump (t, _) -> e, Caddr.block_start @@ Env.eval e t
    | If (c, Dba.JInner i, n) ->
      if Bv.is_zero @@ Env.eval e c then e, Caddr.reid a n
      else e, Caddr.reid a i
    | If (c, Dba.JOuter i, n) ->
      if Bv.is_zero @@ Env.eval e c then e, Caddr.reid a n
      else e, i
    | Stop _ -> raise @@ EndOfTrace e
    | Assert (c, n) ->
      if Bv.is_one @@ Env.eval e c then e, Caddr.reid a n
      else raise @@ AssertFailure a
    | Print (l, n) ->
      List.iter (fun p -> Logger.result "%a" echo p) l;
      e, Caddr.reid a n
    | _ -> Errors.not_yet_implemented "Non supported DBA instruction."

  let fetch = P.fetch
end

module Image = struct
  type t = unit
  exception Ox8BADF00D of Dba.address

  let fetch _ c =
    let d, _ = Disasm_core.decode @@ Addr.of_caddress c in
    match Dhunk.inst d.Instruction.dba_block c.Dba.id with
    | None -> raise @@ Ox8BADF00D c
    | Some i -> i
end

module Dba_program = struct
  type t = Dba.id Dba_types.program
  exception Ox8BADF00D of Dba.address

  let fetch t c =
    try Caddr.Map.find c t.Dba_types.instructions |> fst
    with Not_found -> raise @@ Ox8BADF00D c
end

module Instr_list = struct
  type t = Dba.Instr.t list ref
  exception Ox8BADF00D of Dba.address

  let init = ref

  let fetch t _ = match !t with
    | [] -> raise @@ Ox8BADF00D !Caddr.default_init
    | i :: r -> t := r; i
end

module Goal = struct
  let step t a =
    let a = Caddr.to_virtual_address a in
    List.fold_left
      (fun (cut, print) act ->
         if act.Action.address = a then
           match act.Action.goal with
           | Action.Cut -> true, print
           | Action.Enumerate (_, ex) -> cut, ex :: print
           | _ -> cut, print
         else cut, print)
      (false, []) t
end

module Img = struct
  include Interpreter (Image)

  let echo e ex =
    Logger.result "%a = %a"
      Dba_printer.Ascii.pp_bl_term ex
      Bv.pp @@ Env.eval e ex
  let rec loop e g a =
    try
      let cut, print = Goal.step g a in
      List.iter (fun ex -> echo e ex) print;
      if cut then e else
        let e, a = step e a @@ fetch () a in
        loop e g a
    with Instr_list.Ox8BADF00D _ | EndOfTrace _ -> e

  let run e g a =
   loop e g a
end

let run () = if Simulation.is_enabled () then try
      let env = match Simulation.MemoryFile.get_opt () with
        | None -> Env.empty
        | Some f -> Env.load_memory_file Env.empty f in
      let env = match Simulation.InitFile.get_opt () with
        | None -> env
        | Some f -> Env.load_init_file env f in
      let ep = Bv.of_string @@ Kernel_options.Entry_point.get () in
      let goals = Simulation.Goals.get () in
      ignore @@ Img.run env goals @@ Caddr.block_start ep
    with
    | Env.Ox8BADF00D ex ->
      Simulation.Logger.error "No concrete value for %a"
        Dba_printer.Ascii.pp_bl_term ex
    | Img.AssertFailure a ->
      Simulation.Logger.error "Assertion fail at %a" Caddr.pp_base a

let _ = Cli.Boot.enlist ~name:"sim2" ~f:run
