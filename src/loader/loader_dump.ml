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

open Loader_types

module Section = struct
  type t = {
    flag: int;
    name: string;
    pos: int map;
    size: int map;
    binstream: Binstream.t      (* Or reader? *)
  }

  let has_flag n t = match n with
    | Read ->  (t.flag land 1) == 1
    | Write -> (t.flag land 2) == 2
    | Exec ->  (t.flag land 4) == 4
  ;;

  type header = unit
  let header _ = ()
  let size t = t.size
  let pos  t = t.pos
  let flag t = t.flag
  let name t = t.name

end
module Symbol = struct

  type t = unit
  type header = unit
  let name _ = assert false
  let value _ = assert false
  let header _ = assert false
end

module Img = struct

  type header = unit
  type t = {
    sections: Section.t list;
    arch: Machine.t;
    entry: int;
  }

  let sections {sections; _} = Array.of_list sections
  let symbols _ = [||]
  let arch t = t.arch
  let header _ = ()
  let entry t = t.entry

  let cursor ?at:_ _ = assert false

  let pp _ _ = assert false
end


let check_magic _ = assert false
let read_address img i =
  let sections = img.Img.sections in
  let exception Ret of int in
  try sections |> List.iter (fun sec ->
      let open Section in
      let min = sec.pos.virt in
      let max = min + sec.size.virt in
      if min <= i && i < max
      then
        let idx = i - min in
        match Binstream.get_byte sec.binstream idx
        with | Some x -> raise (Ret x)
             | None -> raise (Ret 0)
      else ()
    );
    raise Not_found
  with Ret x -> x
;;

let read_offset _ = assert false
let load_file _ = assert false
let load_file_descr _ = assert false
let load _ = assert false

module Offset = Loader_buf.Make
    (struct
      type t = Img.t
      let get _t _i = assert false
      let dim _i = assert false
    end)

module Address = Loader_buf.Make
    (struct
      type t = Img.t
      let get t i = read_address t i
      let dim _ = max_int
    end)

let add_section ~flag ~name ~pos ~size binstream img =
  let pos = {raw=0;virt=pos} in
  let size = {raw=Binstream.length binstream; virt=size} in
  let sec = {Section.flag=flag;name=name;pos;size;binstream} in
  {img with Img.sections = sec::img.Img.sections}

let initial_img ~entry ~arch = {
  Img.entry=entry;Img.arch=arch;Img.sections=[]
}
