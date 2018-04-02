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

let load (filename:string): string =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = Bytes.create n  in
  Pervasives.really_input ic s 0 n;
  close_in ic;
  Bytes.unsafe_to_string s

let copy (input:string) (output:string): unit =
  let s = load input in
  let ic = open_out output in
  output_string ic s;
  close_out ic

let readlines (filename:string): string list =
  let fd = open_in filename in
  let lines = ref [] in
  try
    while true do
      let line = input_line fd in
      lines := line :: !lines
    done;
    assert false
  with End_of_file ->
    begin
      close_in fd;
      List.rev !lines
    end
