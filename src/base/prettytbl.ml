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

type alignment = L | C | R

module Column = struct
  type t = {align: alignment;
            min_length: int; max_length: int;
            left_border: string; right_border: string}

  let default =
    { align=L; min_length=0; max_length=max_int;
      left_border=""; right_border="" }

  let make ?(min_length=0) ?(max_length=max_int)
        ?(left_border="") ?(right_border="")
        ?(align=L) () =
    { align; min_length; max_length; left_border; right_border }

  let pp ppf t k s =
    Format.pp_print_string ppf t.left_border;
    let l = String.length s in
    begin
      if k < l then begin
          for i = 0 to k - 4 do
            Format.pp_print_char ppf (String.get s i)
          done;
          for _ = 1 to min 3 k do
            Format.pp_print_char ppf '.'
          done;
        end else
        match t.align with
        | L -> Format.pp_print_string ppf s;
               for _ = 1 to k - l do
                 Format.pp_print_char ppf ' ';
               done
        | C -> for _ = 1 to (k - l) / 2 do
                 Format.pp_print_char ppf ' ';
               done;
               Format.pp_print_string ppf s;
               for _ = 1 to (k - l + 1) / 2 do
                 Format.pp_print_char ppf ' ';
               done
        | R -> for _ = 1 to k - l do
                 Format.pp_print_char ppf ' ';
               done;
               Format.pp_print_string ppf s
    end;
    Format.fprintf ppf "%s" t.right_border;
end

type t = {header: Column.t array;
          sizes: int array;
          mutable next: int;
          mutable rows: string array option array}

let make header =
  if Array.length header = 0 then raise @@ Invalid_argument "empty table"
  else
    let sizes = Array.map (fun c -> c.Column.min_length) header in
    let rows = Array.make 64 None in
    {header; sizes; next=0; rows}

let appendi t row =
  let length = Array.length t.rows in
  if t.next = length then
    t.rows <- Array.init (2 * length)
                (fun i -> if i < length then t.rows.(i) else None);
  t.rows.(t.next) <- row;
  t.next <- t.next + 1

let append t data =
  if Array.length data <> Array.length t.header then
    raise @@ Invalid_argument "column lengths mismatch";
  Array.iteri (fun i s ->
      let l = String.length s in
      if t.sizes.(i) < l && l <= t.header.(i).Column.max_length then
        t.sizes.(i) <- l) data;
  appendi t (Some data)

let pp ppf t =
  let pp_row row =
    Format.pp_open_hovbox ppf 0;
    let l = Array.length t.header - 1 in
    for c = 0 to l - 1 do
      Column.pp ppf t.header.(c) t.sizes.(c) row.(c);
      Format.pp_print_space ppf ()
    done;
    Column.pp ppf t.header.(l) t.sizes.(l) row.(l);
    Format.pp_close_box ppf () in
  Format.pp_open_vbox ppf 0;
  for r = 0 to t.next - 2 do
    pp_row @@ Utils.unsafe_get_opt t.rows.(r);
    Format.pp_force_newline ppf ()
  done;
  pp_row @@  Utils.unsafe_get_opt t.rows.(t.next - 1);
  Format.pp_close_box ppf ()


