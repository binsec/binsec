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

open Format

let pp_list ?(pre="") ?(post="") ?(sep="; ") pp fmt l =
  let rec pp_aux fmt = function
    | [] -> assert false
    | [e] -> fprintf fmt "%a" pp e
    | e :: es -> fprintf fmt "%a%s%a" pp e sep pp_aux es
  in
  if l = [] then
    fprintf fmt ""
  else
    fprintf fmt "%s%a%s" pre pp_aux l post

let pp_as_string to_string fmt element =
  Format.fprintf fmt "%s" (to_string element)

let pp_opt_as_string to_string fmt = function
  | None -> fprintf fmt "None"
  | Some element ->
    pp_as_string to_string fmt element

let pp_opt pp ppf = function
  | None -> fprintf ppf "none"
  | Some v -> fprintf ppf "%a" pp v 

let string_from_pp pp_fun element =
  Format.asprintf "%a" pp_fun element

let pp_dba_prelude ?(flat_memory=false) ppf () =
  let default_value = if flat_memory then "0x7fffee28" else "(stack, 0x7fffee28)" in     
  Format.fprintf ppf
    "@[<v 0>\
     # configuration@ \
     \\entry_point : (0x00000000, 0)@ \
     \\addr : 32@ \
     \\endianness : little@ \
      @ \
     #declaration@ \
     var CF : 1 <FLAG>@ \
     var DF : 1 <FLAG>@ \
     var ZF : 1 <FLAG>@ \
     var OF : 1 <FLAG>@ \
     var SF : 1 <FLAG>@ \
     var AF : 1 <FLAG>@ \
     var PF : 1 <FLAG>@ \
     var NOP : 1@ \
      @ \
     var eax : 32@ \
     var ecx : 32@ \
     var edx : 32@ \
     var ebx : 32@ \
     var esp : 32@ \
     var ebp : 32@ \
     var esi : 32@ \
     var edi : 32@ \
      @ \
     var mm0 : 64@ \
     var mm1 : 64@ \
     var mm2 : 64@ \
     var mm3 : 64@ \
     var mm4 : 64@ \
     var mm5 : 64@ \
     var mm6 : 64@ \
     var mm7 : 64@ \
      @ \
     var st0 : 80@ \
     var st1 : 80@ \
     var st2 : 80@ \
     var st3 : 80@ \
     var st4 : 80@ \
     var st5 : 80@ \
     var st6 : 80@ \
     var st7 : 80@ \
      @ \
     var xmm0 : 128@ \
     var xmm1 : 128@ \
     var xmm2 : 128@ \
     var xmm3 : 128@ \
     var xmm4 : 128@ \
     var xmm5 : 128@ \
     var xmm6 : 128@ \
     var xmm7 : 128@ \
      @ \
     var res8 : 8@ \
     var res16 : 16@ \
     var res32 : 32@ \
     var res64 : 64@ \
      @ \
     var temp8 : 8@ \
     var temp16 : 16@ \
     var temp32 : 32@ \
     var temp40 : 40@ \
     var temp48 : 48@ \
     var temp56 : 56@ \
     var temp64 : 64@ \
     var temp72 : 72@ \
     var temp80 : 80@ \
     var temp88 : 88@ \
     var temp96 : 96@ \
     var temp104 : 104@ \
     var temp112 : 112@ \
     var temp120 : 120@ \
     var temp128 : 128@ \
     var temp256 : 256@ \
      @ \
     var temp9 : 9@ \
     var temp17 : 17@ \
     var temp33 : 33@ \
      @ \
     var temp : 32@ \
      @ \
     var cpt : 16@ \
     var cpt : 32@ \
      @ \
     #initialisation@ \
      @ \
     CF := 0@ \
     DF := 0@ \
     SF := 0@ \
     ebp := %s@ \
     esp := %s@ \
     eax := 0<32>@ \
     ebx := 0<32>@ \
     ecx := 0<32>@ \
     edx := 0<32>@ \
     edi := 0<32>@ \
     esi := 0<32>@ \
     @]@."
    default_value
    default_value


let pp_byte ?(prefixed=false) ppf n =
  assert (n <= 255 && n >= 0); (* Check it is indeed a 8-bits byte *)
  Format.fprintf ppf "%s%2x"
    (if prefixed then "0x" else "")
    n

let pp_to_file ~filename pp value =
  let oc = open_out filename in
  let ppf = Format.formatter_of_out_channel oc in
  fprintf ppf "%a@?" pp value;
  close_out oc
