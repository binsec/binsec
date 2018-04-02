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


let parse_call_stack s =
    let cs = Str.split (Str.regexp "-") s in
    List.rev (List.map (fun str -> Scanf.sscanf str "%x:%d" (fun x _ -> Int64.of_int x)) cs)

let add_score h a b s call_stack =
    let call_stack = parse_call_stack call_stack in
    let () = Printf.printf "Add score %d\n" s in
    try
        let prev = Hashtbl.find h (a,call_stack) in
        Hashtbl.replace h (a,call_stack) ((b,s)::prev)
    with Not_found ->  Hashtbl.add h (a,call_stack) ([b,s]);;
  
let add_score_file f h = 
   let chan = open_in f in
   try
     while true; do
       let line = input_line chan in    
       Scanf.sscanf line "0x%Lx,0x%Lx,%d,%s" (fun a b s cs -> add_score h a b s cs) ;
     done; 
   with End_of_file ->
     close_in chan ;;

let parse_event event = 
    let addr,cs = Scanf.sscanf event "%[^_]_%s" (fun x y -> x,y) in
    let addr = Scanf.sscanf addr "%Lx" (fun x -> x ) in
    let cs = parse_call_stack cs in
    addr,cs

let parse_line_event l =  
    let elem  = Str.split (Str.regexp ",") l in
    List.map (fun x -> parse_event x) elem

let add_list_event f list_alloc list_free list_use =
    let chan = open_in f in
    let () = 
        try
            let line_alloc = Scanf.sscanf (input_line chan) "%s" (fun x -> x) in
            list_alloc := parse_line_event line_alloc 
        with _ -> () 
    in
    let () = 
        try
            let line_free = Scanf.sscanf (input_line chan) "%s" (fun x -> x) in
            list_free := parse_line_event line_free 
        with _ -> () 
    in
    let () = 
        try
            let line_use = Scanf.sscanf (input_line chan) "%s" (fun x -> x) in
            list_use := parse_line_event line_use 
        with _ -> () 
    in
    close_in chan
