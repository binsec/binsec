(**************************************************************************)
(*  This file is part of Binsec.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
(*    VERIMAG                                                             *)
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
(**************************************************************************)

type file_descr = int64
type file_name = string

(* file descri * file name-> list of input *)
type fd = (file_descr, file_name) Hashtbl.t

let (tbl:fd) = Hashtbl.create 20

let reset () = Hashtbl.reset tbl

(* if fd exist, remove it *)
let add_fd fd name = 
  Logger.debug "Size fd %d, new file %s" (Hashtbl.length tbl) name;         
  Hashtbl.replace tbl fd name;;

let list_fd () = 
  Logger.debug "Size fd %d" (Hashtbl.length tbl);
  Hashtbl.fold (fun key vals l -> (key,vals) :: l) tbl [];; 

let get_name fd = 
  try 
    Some (Hashtbl.find tbl fd)
  with
  | Not_found -> None

