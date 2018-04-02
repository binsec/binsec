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
    
type color = Graphics.color

let rgb = Graphics.rgb
              
let pp_with_prefix prefix ppf color =
  fprintf ppf "%s%6x" prefix color
    
let pp ppf color = pp_with_prefix "#" ppf color

module FlatUI = struct
  
  (* See https://www.materialui.co/flatuicolors *)
  let turquoise    = rgb 26  188 156
  let greensea     = rgb 22  160 133
  let emerland     = rgb 46  204 113
  let nephritis    = rgb 39  174 96
  let peterriver   = rgb 52  152 219
  let belizehole   = rgb 41  128 185
  let amethyst     = rgb 155 89  182
  let wisteria     = rgb 142 68  173
  let wetasphalt   = rgb 52  73  94
  let midnightblue = rgb 44  62  80
  let sunflower    = rgb 241 196 15
  and orange       = rgb 243 156 18
  and carrot       = rgb 230 126 34 
  and pumpkin      = rgb 211 84  0 
  and alizarin     = rgb 231 76  60
  and pomegranate  = rgb 192 57  43 
  and clouds       = rgb 236 240 241 
  and silver       = rgb 189 195 199 
  and concrete     = rgb 149 165 166
  and asbestos     = rgb 127 140 141
      
end
