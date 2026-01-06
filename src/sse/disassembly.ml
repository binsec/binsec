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

module IntTbl = Basic_types.Integers.Int.Htbl
open Binsec_sse_loader.Disassembly

type 'a hook = 'a Binsec_sse_loader.Disassembly.hook =
  | Fetch : (Virtual_address.t -> Ir.Graph.t option) hook
  | Decode : (Virtual_address.t -> int Reader.t -> Ir.Graph.t option) hook
  | Disasm : (Instruction.t -> Ir.Graph.t option) hook
  | Rewrite : (Ir.Graph.t -> unit) hook

(** Information to be used by optimization. *)
type 'a knowledge = 'a Binsec_sse_loader.Disassembly.knowledge =
  | May_read : Dba_types.Var.Set.t option knowledge
  | Must_write : Dba_types.Var.Set.t knowledge

module Callback = Callback
module Revision = Revision

type 'a config = 'a Compiler.config
type 'a cache = 'a Compiler.cache
type nonrec 'a fiber = ([ `All ], 'a) Types.fiber
type nonrec 'a t = { config : 'a config; fibers : 'a cache; segment : t }

let create :
    Callback.t ->
    decoder:(int Reader.t -> Virtual_address.t -> Instruction.t) ->
    Virtual_address.t ->
    Virtual_address.t Reader.t ->
    Z.t ->
    'a config ->
    'a t =
 fun callback ~decoder base reader size config ->
  {
    config;
    fibers = IntTbl.create 16;
    segment = create callback ~decoder base reader size;
  }

let create_small :
    Callback.t ->
    decoder:(int Reader.t -> Virtual_address.t -> Instruction.t) ->
    Virtual_address.t ->
    int Reader.t ->
    int ->
    'a config ->
    'a t =
 fun callback ~decoder base reader size config ->
  {
    config;
    fibers = IntTbl.create 16;
    segment = create_small callback ~decoder base reader size;
  }

let apply :
    'a t ->
    Virtual_address.t ->
    (Binsec_sse_loader.Disassembly.t -> Virtual_address.t -> Ir.View.vertex) ->
    'a fiber =
 fun { config; fibers; segment } addr f ->
  let vertex = f segment addr in
  try IntTbl.find fibers vertex
  with Not_found ->
    let code =
      Compiler.create config ~killset:(killset segment) ~fibers (graph segment)
    in
    Compiler.get code vertex

let fetch_no_link : 'a t -> Virtual_address.t -> 'a fiber =
 fun env addr -> apply env addr fetch_no_link

let disassemble_from : 'a t -> Virtual_address.t -> 'a fiber =
 fun env addr -> apply env addr disassemble_from

let address : 'a t -> Virtual_address.t = fun { segment; _ } -> address segment
let graph : 'a t -> Ir.View.t = fun { segment; _ } -> graph segment
let callback : 'a t -> Callback.t = fun { segment; _ } -> callback segment

let killset : 'a t -> Ir.Graph.vertex -> Dba_types.Var.Set.t =
 fun { segment; _ } vertex -> killset segment vertex
