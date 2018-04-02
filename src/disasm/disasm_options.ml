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




let simplification_levels = ["prog";"fun";"seq";"fun-no-inline";
                             "seq-no-inline";"fun-no-sum";
                             "seq-no-sum"]

let simpl_fun = ref false
let simpl_sequence = ref false
let simpl_inline_calls = ref true
let simpl_no_summaries = ref false
let simpl = ref false


let simplification_cli_handler =
  Arg.Symbol (
    simplification_levels,
    (fun s ->
       match (String.lowercase s) with
       | "prog" -> simpl :=  true
       | "fun" -> simpl := true; simpl_fun := true
       | "seq" -> simpl := true; simpl_sequence := true
       | "fun-no-inline" ->
         simpl := true;
         simpl_fun := true;
         simpl_inline_calls := false
       | "seq-no-inline" ->
         simpl := true;
         simpl_sequence := true;
         simpl_inline_calls := false
       | "fun-no-sum" ->
         simpl := true;
         simpl_fun := true;
         simpl_no_summaries := true
       | "seq-no-sum" ->
         simpl := true;
         simpl_sequence := true;
         simpl_no_summaries := true
       | _ ->
         assert false)
  )


module DisassemblyMode = struct
  type t =
    | Recursive | Linear | Linear_byte_wise | ExtendedLinear

  let assoc_tbl = [
    "rec", Recursive;
    "linear", Linear;
    "bytelinear", Linear_byte_wise;
    "extlinear", ExtendedLinear;
  ]

  let values = List.map fst assoc_tbl
  
  let set, get  =
    let mode = ref Recursive in
    (fun s -> mode := List.assoc (String.lowercase s) assoc_tbl),
    (fun () -> !mode)

  let set_recursive () = set "rec"
  let set_linear () = set "linear"
  let set_byte_wise_linear () = set "bytelinear"
  let set_extended_linear () = set "reclinear"

  let cli_handler = Arg.Symbol(values, set)
end


module DbaOutputFile = struct
  include Parameters.Builder.String(
    struct
      let name = "o-dba"
      let default = "out.dba"
      let doc = Format.sprintf " Set DBA instructions output file [%s]" default
    end)
end


module OpcodeOutputFile =
  Parameters.Builder.OptionalString(
    struct
      let name = "dump"
      let doc = " Set opcodes output file [stdout]"
    end)


module NoLoaderMode =
  Parameters.Builder.False(
  struct
    let name = "no-loader"
    let doc = "Do not use loader and starts at 0x0"
  end
  )

module IgnoreUnhandledInstructions =
  Parameters.Builder.True (
  struct
    let name = "ignore-unhandled"
    let doc = "Skip unknown instructions"
  end
  )

module ProtectedMode = Parameters.Builder.False (
  struct
    let name = "protected-mode"
    let doc = "Activate protected mode memory addressing (using segment selectors)"
  end
  )

module ShowInstructionCount =
  Parameters.Builder.False (
      struct
        let name = "show-instruction-count"
        let doc = "Show a summary of encountered instructions"
      end
  )

module Sections =
  Parameters.Builder.StringSet (
      struct
        let name = "sections"
        let doc = "Disassemble (linear) given comma separated list of sections"
      end
    )
  
let is_ignored_segment, mark_ignored_segment =
  let h = Hashtbl.create 6 in
  let segments = ([X86Types.FS; X86Types.GS; X86Types.CS; X86Types.SS; X86Types.DS; X86Types.ES;]) in
  (* all segments ignored by default *)
  List.iter (fun seg -> Hashtbl.add h seg true) segments;
  (fun sreg -> Hashtbl.find h sreg),
  (fun string ->
     let segment =
       match String.lowercase string with
       | "fs" -> Some X86Types.FS
       | "gs" -> Some X86Types.GS
       | "cs" -> Some X86Types.CS
       | "ss" -> Some X86Types.SS
       | "ds" -> Some X86Types.DS
       | "es" -> Some X86Types.ES
       | _ -> Logger.info "Ignoring unknown segment %s" string; None
     in
     match segment with
     | Some segment -> Hashtbl.replace h segment false
     | None -> ()
  )

let mark_ignored_segments s =
  List.iter mark_ignored_segment (Str.split (Str.regexp  ",") s)

let set_file, get_file =
  let file = ref None in
  (fun s ->
     (*     assert(Sys.file_exists s); *)
     file := Some s
  ),
  (fun () ->
     match !file with
     | None -> ""
     | Some f -> f
  )


module ArmDecoder = Parameters.Builder.String (
  struct
    let name = "arm-decoder"
    let default = "armsec/bin/unisim-armsec-0.8.0 arm"
    let doc = Format.sprintf " ARM decoder command [%s]" default
  end
  )

module Logger = Logger.Make(struct let name = "disasm" end)
