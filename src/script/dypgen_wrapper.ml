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

let () =
  match Sys.argv with
  | [| _; dyp_meta; zarith_meta; binsec_meta; dypgen; src |] ->
      let dyp_lib = Filename.dirname dyp_meta
      and zarith_lib = Filename.dirname zarith_meta
      and binsec_lib = Filename.dirname binsec_meta in
      Unix.execv dypgen
        [|
          dypgen;
          "--ocamlc";
          Format.sprintf
            "-I . -I %s -I %s -I %s -I %s/base -I %s/kernel/dba -I %s/kernel \
             -I %s/script/ast -open Binsec_kernel -open Binsec_script_ast \
             -rectypes"
            dyp_lib zarith_lib binsec_lib binsec_lib binsec_lib binsec_lib
            binsec_lib;
          src;
        |]
  | _ ->
      failwith
        (Format.sprintf
           "usage: %s %%{lib:dyp:META} %%{lib:zarith:META} %%{lib:binsec:META} \
            %%{bin:dypgen} %%{src}"
           (Array.get Sys.argv 0))
