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

(* For now, only handle list of read / mmap *)

(* **)
type read_map = (int,string) Hashtbl.t * int


(* symb input read * symb input mmap *) 
type symb_file = read_map * string list

(* file descri * file name-> list of input *)
type symb_files = (int64, symb_file)Hashtbl.t

let init () : symb_files = Hashtbl.create 20

let init_tbl () : symb_file = ((Hashtbl.create 20),0),[]

let add_read (tbl:symb_files) fd (l:string list) =
    let ((elems_read,offset),elems_mmap) = 
        try
            Hashtbl.find tbl fd
        with
            Not_found -> init_tbl ()
    in
    Logger.debug "Add input from read %Lx %d (off %d)\n" fd (List.length l) offset;
    List.iteri (fun i x-> Hashtbl.add elems_read (i+offset) x) l ;
    Hashtbl.replace tbl fd ((elems_read,offset+(List.length l)),elems_mmap)
        
let move_offset tbl fd off =
    let ((elems_read,_),elems_mmap) = 
        try
            Hashtbl.find tbl fd
        with
            Not_found -> init_tbl ()
    in
    Hashtbl.replace tbl fd ((elems_read,off),elems_mmap)

let add_mmap tbl fd l =
    let (elems_read,_) = 
        try
            Hashtbl.find tbl fd
        with
            Not_found -> init_tbl ()
    in Hashtbl.replace tbl fd (elems_read,l)

let get_vals_read tbl =
    Logger.debug "Size vals read %d" (Hashtbl.length tbl) ;
    Hashtbl.fold (fun k v acc -> (k,v)::acc) tbl []

let get_vals_mmap l =
    List.mapi (fun i x -> (i,x) ) l

let find_aliases_from_sf sf = 
    let (read,_),mmap = sf in
    let rec f i acc size_max=
        if(i<size_max) then
                try
                       let alias = Hashtbl.find read i,List.nth mmap i in
                       f (i+1) (alias::acc) size_max
                with
                       Not_found -> f (i+1) acc size_max
        else acc
    in
    f 0 [] (List.length mmap)


let get_aliases (tbl:symb_files) = 
        Hashtbl.fold (fun _k v acc -> (find_aliases_from_sf v) @ acc) tbl []

let get_vals (tbl:symb_files) fd =
   let ((elems_read,offset),elems_mmap) = 
    try
        Hashtbl.find tbl fd
    with
        Not_found -> init_tbl ()
   in
   if(offset>(List.length elems_mmap)) then (get_vals_read elems_read) else (get_vals_mmap elems_mmap)

let update_file name vals =
    Logger.debug "Update %s vals %d" name (List.length vals);
    let oc = open_out_gen [Open_wronly ; Open_binary] 777 name in
    List.iter (fun (i,x) -> seek_out oc i; output_byte oc x) vals;
    close_out oc
