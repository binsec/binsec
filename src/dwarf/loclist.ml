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

open Basic_types.Integers
module Map = Basic_types.Integers.Int.Map

type entry =
  | Entry of {
      offset : int;
      addresses : Virtual_address.t Interval.t;
      expr : Expr.t;
    }
  | Eol of int

type t = entry list

let pp =
  let columns =
    [|
      Prettytbl.Column.default;
      Prettytbl.Column.default;
      Prettytbl.Column.default;
      Prettytbl.Column.default;
    |]
  in
  fun ppf (isa, l) ->
    Format.pp_open_vbox ppf 0;
    let t = Prettytbl.make columns in
    Prettytbl.append t [| "Offset"; "Begin"; "End"; "Expression" |];
    List.iter
      (function
        | Entry { offset; addresses = { lo; hi }; expr } ->
            Prettytbl.append t
              [|
                Format.sprintf "%08x" offset;
                Utils.addr_to_string isa lo;
                Utils.addr_to_string isa hi;
                Format.asprintf "%a" Expr.pp expr;
              |]
        | Eol offset ->
            Prettytbl.append t
              [| Format.sprintf "%08x" offset; "<End of list>"; ""; "" |])
      l;
    Prettytbl.pp ppf t;
    Format.pp_close_box ppf ()

let load img : t Map.t =
  let isa = Loader.Img.arch img in
  match Loader_utils.find_section_by_name ".debug_loc" img with
  | exception Not_found -> Map.empty
  | section ->
      let bits =
        match Machine.ISA.bits isa with
        | `x32 -> `x32
        | `x64 -> `x64
        | _ -> assert false
      in
      let at = (Loader.Section.pos section).raw in
      let length = (Loader.Section.size section).raw in
      let cursor = Reader.Read.sub (Loader.Img.cursor ~at img) length in
      let rec loop isa rev_entries cursor =
        if Reader.at_end cursor then rev_entries
        else
          let offset = Reader.get_pos cursor in
          let lo = Utils.read_addr isa cursor in
          if Utils.is_max_addr isa lo then
            Logger.fatal "Unsuported base address selection entry"
          else
            let hi = Utils.read_addr isa cursor in
            if
              Virtual_address.equal lo Virtual_address.zero
              && Virtual_address.equal hi Virtual_address.zero
            then loop isa (Eol offset :: rev_entries) cursor
            else
              let size = Uint16.to_int (Reader.Read.u16 cursor) in

              let expr = Expr.load isa bits size cursor in
              loop isa
                (Entry { offset; addresses = { lo; hi }; expr } :: rev_entries)
                cursor
      in
      let rev_entries = loop isa [] cursor in
      let rec loop rev_entries entries links =
        match rev_entries with
        | [] -> links
        | (Entry { offset; _ } as e) :: rev_entries
        | (Eol offset as e) :: rev_entries ->
            let entries = e :: entries in
            loop rev_entries entries (Map.add offset entries links)
      in
      loop rev_entries [] Map.empty
