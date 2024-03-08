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

open Simulation
module Bv = Bitvector

module It = struct
  include Interval

  let l8 = { lo = 0; hi = 7 }
end

module Var = Basic_types.String

module Addr = struct
  include Virtual_address

  let of_caddress = Dba_types.Caddress.to_virtual_address
end

module Caddr = Dba_types.Caddress

module Env = struct
  type t = { meta : Bv.t Var.Map.t; main : Bv.t Addr.Map.t }

  exception Ox8BADF00D of Dba.Expr.t

  let pp ppf e =
    Format.fprintf ppf "@[<v>";
    Var.Map.iter
      (fun var bv ->
        Format.fprintf ppf "%s := %a@ " var Bitvector.pp_hex_or_bin bv)
      e.meta;
    Addr.Map.iter
      (fun addr byte ->
        Format.fprintf ppf "0x%08x := %a@ " (addr :> int) Bitvector.pp_hex byte)
      e.main;
    Format.fprintf ppf "@]"

  module Load = struct
    let ox8BADF00D s m i =
      let sz = Size.Byte.create s in
      let ax =
        Kernel_options.Machine.word_size () |> Bv.create @@ Addr.to_bigint i
      in
      let ex = Dba.Expr.load sz m @@ Dba.Expr.constant ax in
      Ox8BADF00D ex
  end

  module Restrict = struct
    let ox8BADF00D n s =
      let ex = Dba.Expr.var n s in
      Ox8BADF00D ex
  end

  (* helper functions *)
  let rec split succ i v s t =
    if s = 1 then Addr.Map.add i v t
    else
      split succ (succ i) (Bv.extract v It.{ lo = 8; hi = (8 * s) - 1 }) (s - 1)
      @@ Addr.Map.add i (Bv.extract v It.l8) t

  let write e m i v s =
    match m with
    | Machine.BigEndian ->
        {
          e with
          main = split Addr.pred (i |> Addr.add_int @@ (s - 1)) v s e.main;
        }
    | Machine.LittleEndian -> { e with main = split Addr.succ i v s e.main }

  let rec join succ i l s t =
    if s = 1 then Addr.Map.find i t :: l
    else join succ (succ i) (Addr.Map.find i t :: l) (s - 1) t

  let read e m i s =
    try
      match m with
      | Machine.BigEndian ->
          join Addr.pred (Addr.add_int (s - 1) i) [] s e.main |> Bv.concat
      | Machine.LittleEndian -> join Addr.succ i [] s e.main |> Bv.concat
    with Not_found -> raise @@ Load.ox8BADF00D s m i

  let empty = { meta = Var.Map.empty; main = Addr.Map.empty }

  let eval_unop =
    Dba.Unary_op.(
      function
      | UMinus -> Bitvector.neg
      | Not -> Bitvector.lognot
      | Sext s -> fun b -> Bitvector.extend_signed b s
      | Uext s -> fun b -> Bitvector.extend b s
      | Restrict i -> fun b -> Bitvector.extract b i)

  let eval_binop =
    Dba.Binary_op.(
      function
      | Plus -> Bitvector.add
      | Minus -> Bitvector.sub
      | Mult -> Bitvector.mul
      | DivU -> Bitvector.udiv
      | DivS -> Bitvector.sdiv
      | ModU -> Bitvector.umod
      | ModS -> Bitvector.smod
      | Or -> Bitvector.logor
      | And -> Bitvector.logand
      | Xor -> Bitvector.logxor
      | Concat -> Bitvector.append
      | Eq -> fun x y -> Bitvector.of_bool (Bitvector.equal x y)
      | Diff -> fun x y -> Bitvector.of_bool (Bitvector.diff x y)
      | LeqU -> fun x y -> Bitvector.of_bool (Bitvector.ule x y)
      | LtU -> fun x y -> Bitvector.of_bool (Bitvector.ult x y)
      | GeqU -> fun x y -> Bitvector.of_bool (Bitvector.uge x y)
      | GtU -> fun x y -> Bitvector.of_bool (Bitvector.ugt x y)
      | LeqS -> fun x y -> Bitvector.of_bool (Bitvector.sle x y)
      | LtS -> fun x y -> Bitvector.of_bool (Bitvector.slt x y)
      | GeqS -> fun x y -> Bitvector.of_bool (Bitvector.sge x y)
      | GtS -> fun x y -> Bitvector.of_bool (Bitvector.sgt x y)
      | LShift ->
          fun x y ->
            let s = Z.to_int (Bitvector.value_of y) in
            assert (s <= Bitvector.size_of x);
            Bitvector.shift_left x s
      | RShiftU ->
          fun x y ->
            let s = Z.to_int (Bitvector.value_of y) in
            assert (s <= Bitvector.size_of x);
            Bitvector.shift_right x s
      | RShiftS ->
          fun x y ->
            let s = Z.to_int (Bitvector.value_of y) in
            assert (s <= Bitvector.size_of x);
            Bitvector.shift_right_signed x s
      | LeftRotate ->
          fun x y ->
            let s = Z.to_int (Bitvector.value_of y) in
            assert (s <= Bitvector.size_of x);
            Bitvector.rotate_left x s
      | RightRotate ->
          fun x y ->
            let s = Z.to_int (Bitvector.value_of y) in
            assert (s <= Bitvector.size_of x);
            Bitvector.rotate_right x s)

  let rec eval e =
    let open Dba.Expr in
    function
    | Cst v -> v
    | Var { name; _ } as v -> (
        try Var.Map.find name e.meta with Not_found -> raise @@ Ox8BADF00D v)
    | Load (s, m, a, None) -> read e m (Addr.of_bitvector @@ eval e a) s
    | Load _ -> assert false
    | Unary (o, a) -> eval_unop o @@ eval e a
    | Binary (o, a, b) -> eval e b |> eval_binop o @@ eval e a
    | Ite (c, t, f) -> if Bv.is_zero @@ eval e c then eval e f else eval e t

  let assign e l v =
    match l with
    | Dba.LValue.Store (s, m, a, None) ->
        write e m (Addr.of_bitvector @@ eval e a) v s
    | Dba.LValue.Store _ -> assert false
    | Dba.LValue.Var va -> { e with meta = Var.Map.add va.name v e.meta }
    | Dba.(LValue.Restrict ({ name = n; size = s; _ }, { It.lo; It.hi })) ->
        let v =
          match Var.Map.find n e.meta with
          | o when lo = 0 ->
              v
              |> Bv.append
                 @@ Bv.extract o It.{ lo = hi + 1; hi = Bv.size_of o - 1 }
          | o when hi = Bv.size_of o - 1 ->
              Bv.append v @@ Bv.extract o It.{ lo = 0; hi = lo - 1 }
          | o ->
              Bv.append (Bv.extract o It.{ lo = hi + 1; hi = Bv.size_of o - 1 })
              @@ Bv.append v (Bv.extract o It.{ lo = 0; hi = lo - 1 })
          | exception Not_found -> raise @@ Restrict.ox8BADF00D n s
        in
        { e with meta = Var.Map.add n v e.meta }

  let kill e = function
    | Dba.LValue.Store _ -> Errors.not_yet_implemented "Undef on memory cell."
    | Dba.LValue.Var v | Dba.LValue.Restrict (v, _) ->
        { e with meta = Var.Map.remove v.name e.meta }

  let load_memory_file e filename =
    let map =
      let parser = Parser.patchmap and lexer = Lexer.token in
      Parse_utils.read_file ~parser ~lexer ~filename
    in
    {
      e with
      main =
        Virtual_address.Map.fold
          (fun a bs map ->
            let l = Binstream.length bs in
            let rec aux m i =
              if i < l then
                let b = Binstream.get_byte_exn bs i in
                let m' =
                  Virtual_address.Map.add
                    (Virtual_address.add_int i a)
                    (Bitvector.create (Z.of_int b) 8)
                    m
                in
                aux m' (succ i)
              else m
            in
            aux map 0)
          map e.main;
    }

  let init e init_directive =
    let module I = Parse_helpers.Initialization in
    match init_directive.I.operation with
    | I.Universal _ -> e
    | I.Assignment (lval, I.Singleton rval, _) -> eval e rval |> assign e lval
    | I.Assignment (lval, _, _) -> raise @@ Ox8BADF00D (Dba.LValue.to_expr lval)
    | I.Mem_load (addr, size) ->
        let img = Kernel_functions.get_img () in
        let wsize = Kernel_options.Machine.word_size () in
        let rec loop e s =
          if s = 0 then e
          else
            let bv_off = Bitvector.of_int ~size:wsize (s - 1) in
            let addr =
              Dba_utils.Expr.eval_from_img (Kernel_functions.get_img ()) addr
            in
            let addr = Bitvector.add bv_off addr in
            let byte = Loader_utils.get_byte_at img addr |> Bv.of_int ~size:8 in
            let e =
              let addr = Virtual_address.of_bitvector addr in
              write e Machine.LittleEndian addr byte 1
            in
            loop e @@ (s - 1)
        in
        loop e size
    | I.Assumption e -> raise @@ Ox8BADF00D e

  let load_init_file e filename =
    let parser = Parser.initialization and lexer = Lexer.token in
    Parse_utils.read_file ~parser ~lexer ~filename |> List.fold_left init e
end

module type Program = sig
  type t

  exception Ox8BADF00D of Dba.address

  val fetch : t -> Dba.address -> Dba.Instr.t
end

module Interpreter (P : Program) = struct
  exception AssertFailure of Dba.address
  exception EndOfTrace of Env.t

  let step e a =
    let open Dba.Instr in
    function
    | Assign (l, x, n) -> (Env.assign e l @@ Env.eval e x, Caddr.reid a n)
    | Undef (l, n) -> (Env.kill e l, Caddr.reid a n)
    | SJump (Dba.JInner n, _) -> (e, Caddr.reid a n)
    | SJump (Dba.JOuter n, _) -> (e, n)
    | DJump (t, _) ->
        (e, Caddr.block_start @@ Virtual_address.of_bitvector @@ Env.eval e t)
    | If (c, Dba.JInner i, n) ->
        if Bv.is_zero @@ Env.eval e c then (e, Caddr.reid a n)
        else (e, Caddr.reid a i)
    | If (c, Dba.JOuter i, n) ->
        if Bv.is_zero @@ Env.eval e c then (e, Caddr.reid a n) else (e, i)
    | Stop _ -> raise @@ EndOfTrace e
    | Assert (c, n) ->
        if Bv.is_one @@ Env.eval e c then (e, Caddr.reid a n)
        else raise @@ AssertFailure a
    | _ -> Errors.not_yet_implemented "Non supported DBA instruction."

  let fetch = P.fetch
end

module Image = struct
  type t = unit

  exception Ox8BADF00D of Dba.address

  type cache = { mutable addr : Virtual_address.t; mutable dhunk : Dhunk.t }

  let cache = { addr = Virtual_address.create 0; dhunk = Dhunk.empty }

  let fetch _ c =
    if Caddr.base_value c <> cache.addr then (
      let i, _ = Disasm_core.decode @@ Addr.of_caddress c in
      cache.addr <- Caddr.base_value c;
      cache.dhunk <- i.Instruction.dba_block);
    match Dhunk.inst cache.dhunk c.Dba.id with
    | None -> raise @@ Ox8BADF00D c
    | Some i -> i
end

module Dba_program = struct
  type t = Dba_types.program

  exception Ox8BADF00D of Dba.address

  let fetch t c =
    try Caddr.Map.find c t.Dba_types.instructions
    with Not_found -> raise @@ Ox8BADF00D c
end

module Instr_list = struct
  type t = Dba.Instr.t list ref

  exception Ox8BADF00D of Dba.address

  let init = ref

  let fetch t _ =
    match !t with
    | [] -> raise @@ Ox8BADF00D !Caddr.default_init
    | i :: r ->
        t := r;
        i
end

module Goal = struct
  let address = Directive.addr

  type action = Continue | Cut

  let echo e ex =
    Logger.result "%a = %a" Dba_printer.Ascii.pp_bl_term ex Bv.pp_hex_or_bin
    @@ Env.eval e ex

  let step t e a =
    if a.Dba.id = 0 then
      let a = Caddr.to_virtual_address a in
      List.fold_left
        (fun action act ->
          let addr = address act in
          if addr = a then
            match Directive.directive act with
            | Directive.Cut g when not (Bv.is_zero (Env.eval e g)) -> Cut
            | Directive.Enumerate (_, ex) ->
                echo e ex;
                action
            | _ -> action
          else action)
        Continue t
    else Continue
end

module Img = struct
  include Interpreter (Image)

  let rec loop e g a =
    Simulation.Logger.debug "Step %a:%d;" Caddr.pp_base a a.Dba.id;
    match Goal.step g e a with
    | Goal.Cut -> e
    | Goal.Continue ->
        let e, a = step e a @@ fetch () a in
        loop e g a

  let run e g a = try loop e g a with EndOfTrace e -> e
end

let run () =
  if Simulation.is_enabled () then
    try
      let env =
        match Simulation.MemoryFile.get_opt () with
        | None -> Env.empty
        | Some f -> Env.load_memory_file Env.empty f
      in
      let env =
        match Simulation.InitFile.get_opt () with
        | None -> env
        | Some f -> Env.load_init_file env f
      in
      let entrypoint =
        match Kernel_functions.get_ep () with
        | Some v ->
            assert ((v :> int) <> 0);
            v
        | None -> Simulation.Logger.fatal "Entrypoint is not set or not valid"
      in
      Simulation.Logger.debug "initial env: %a" Env.pp env;
      let goals = Simulation.Directives.get () in
      ignore @@ Img.run env goals @@ Caddr.block_start entrypoint
    with
    | Env.Ox8BADF00D ex ->
        Simulation.Logger.error "No concrete value for %a"
          Dba_printer.Ascii.pp_bl_term ex
    | Img.AssertFailure a ->
        Simulation.Logger.error "Assertion fail at %a" Caddr.pp_base a

let _ = Cli.Boot.enlist ~name:"sim2" ~f:run
