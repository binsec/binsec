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

(* A block is a small CFG consisting of DBAs
     Its "address" is an index number
     Its contents is a DBA instruction
     Its symbols are not defined.
     For now we will add integers. A better solution might be tags
*)
module A = struct
  type t = int
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = (=)
  let of_int n = n
end

module I = struct
  type t = Dba.Instr.t
  let hash = Hashtbl.hash
  let equal = (=)
  let of_instr i = i
end

module S = A

module C = struct
  module C_dot = struct
    include Cfg.Make(A)(I)(S)
    let graph_attributes _g = []
    let default_vertex_attributes _ = [`Shape `Box]
    let vertex_name v =
      Format.asprintf "\"%d: %a\""
        (V.addr v)
        (Print_utils.pp_opt Dba_printer.Ascii.pp_instruction) (V.inst v)

    let vertex_attributes _v = []
    let get_subgraph _ = None
    let default_edge_attributes _ = []
    let edge_attributes _ = []
  end
  include C_dot
  module D = Graph.Graphviz.Dot(C_dot)
end

module DI = Dba_types.Instruction

type t = C.C_dot.t

module Node = struct
  type t = C.vertex
  let id = C.V.addr
  let inst = C.V.inst
end


(* pred and succ are aliases *)
let pred = C.pred
let succ = C.succ

let empty = C.create 0

let stop =
  let g = C.create 1 in
  C.add_inst g (A.of_int 0) (Dba.Instr.stop (Some Dba.OK));
  g

let node g n =
 C.mem_vertex_a g (A.of_int n)

let inst g n =
  match node g n with
  | None -> None
  | Some v -> C.V.inst v

let add g idx instr =
  C.add_inst g (A.of_int idx) (I.of_instr instr)

let init n (f:int -> Dba.Instr.t) =
  let g = C.create n in
  for i = 0 to n - 1 do
    add g i (f i);
  done;
  g

let singleton instruction =
  let g = C.create 1 in
  add g 0 instruction;
  g

let length g = C.nb_vertex g
let is_empty g = length g = 0

let start g =
  match C.mem_vertex_a g 0 with
  | None -> assert false
  | Some v -> v

let beginning g = node g 0
let beginning_inst g = inst g 0


let fold f acc g =
  C.fold_vertex
    (fun v acc ->
       match C.V.inst v with
       | None -> acc
       | Some i -> f acc i
    ) g acc

let of_list l =
  let len = List.length l in
  let g = C.create len in
  List.iteri
    (fun idx instr ->
       (* Add the node for the instruction *)
       add g idx instr;
       (* Add edges to its successors *)
       DI.successors instr
       |> List.iter
         (function
           | Dba.JInner ji -> C.add_edge_a g idx ji
           | Dba.JOuter _ -> ()
         )
    ) l;
  g

let of_labelled_list l =
  let len = List.length l in
  let g = C.create len in
  List.iteri
    (fun idx (i,instr) ->
       assert(idx == i);
       (* Add the node for the instruction *)
       add g idx instr;
       (* Add edges to its successors *)
       DI.successors instr
       |> List.iter
         (function
           | Dba.JInner ji -> C.add_edge_a g idx ji
           | Dba.JOuter _ -> ()
         )
    ) l;
  g


let flatten g =
  C.fold_vertex
    (fun v acc ->
       match C.V.inst v with
       | None -> acc
       | Some i ->
         (C.V.addr v, i) :: acc)
    g []
  |> List.sort (fun (idx1, _) (idx2, _) -> compare idx1 idx2)


let to_list g = flatten g |> List.map snd

let iteri ~f g =
  C.iter_vertex
    (fun v ->
       let a = C.V.addr v in
       match C.V.inst v with
       | None -> ()
       | Some i -> f a i) g

let iter ~f g = iteri ~f:(fun _ inst -> f inst) g
let copy g = C.copy g
let mapi ~f g =
  let g' = copy g in
  iteri ~f:(fun n i -> add g' n (f n i)) g';
  g'

exception Done
let for_all p g =
  try
    iter ~f:(fun i -> if not (p i) then raise Done) g;
    true
  with Done -> false

let vertex_list g =
  C.fold_vertex (fun v l -> v :: l) g []
  |> List.sort (fun x y -> Pervasives.compare (C.V.addr x) (C.V.addr y))

let pp_succs node_addr ppf succ_nodes =
  let direct_succ = node_addr + 1 in
  let succ_addrs = List_utils.filter_map ((<>) direct_succ) C.V.addr succ_nodes in
  match  succ_addrs with
  | [] -> ()
  | succs ->
    let open Format in
    pp_open_hbox ppf ();
    pp_print_string ppf "; goto ";
    Print_utils.pp_list ~sep:";" pp_print_int ppf succs;
    pp_close_box ppf ()


let _pp ppf t =
  let open Format in
  let g_l = vertex_list t in
  fprintf ppf "@[<v 0>";
  List.iter (
    fun v ->
      let open Dba.Instr in
      match C.V.inst v with
      | None -> ()
      | Some (DJump _ | SJump _ | If _ as inst) ->
        let addr = C.V.addr v in
        fprintf ppf "@[<h>%2d: %a@]@ "
          addr Dba_printer.Ascii.pp_instruction inst
      | Some inst ->
        let addr = C.V.addr v in
        let next_addrs = succ t v in
        fprintf ppf "@[<h>%2d: %a%a@]@ "
          addr Dba_printer.Ascii.pp_instruction inst
          (pp_succs addr) next_addrs
  ) g_l;
  fprintf ppf "@]"

let pp ppf t =
  let open Format in
  fprintf ppf "@[<v 0>";
  flatten t |> List.iter (fun (addr,instr) ->
      fprintf ppf "@[<h>%2d: %a@]@ "
        addr (Dba_printer.Ascii.pp_instruction_maybe_goto ~current_id:addr) instr);
  fprintf ppf "@]"

;;

let to_stmts t (address:Virtual_address.t) =
  let base = Dba_types.Caddress.block_start_of_int (address:>int) in
  let l = to_list t in
  List.mapi (fun i e ->
      Dba_types.Statement.create (Dba_types.Caddress.reid base i) e) l

let no_inner_reference instr = function
  | Dba.JOuter _ -> true
  | (Dba.JInner _) as jt ->
    not (List.mem jt (DI.successors instr))

let _no_block_inner_references t n =
  let jt = Dba.Jump_target.inner n in
  for_all (fun instr -> no_inner_reference instr jt) t

(* let remove t n =
 *   Format.printf "@[<v 0>Removing %d@ %a@]@." n pp t;
 *   assert (n < length t);
 *   assert (no_block_inner_references t n);
 *   init
 *     (length t - 1)
 *     (fun i ->
 *       if i < n then get t i |> unsafe_get_op else
 *         let p id = id >= n in
 *         let f id = id - 1 in
 *         DI.generic_reset_successors ~p ~f (get t (i + 1))) *)

let outer_jumps =
  fold
    (fun hwset instr ->
       let jset = DI.outer_jumps instr in
       Virtual_address.Set.union hwset jset)
    Virtual_address.Set.empty

let callees =
  fold
    (fun hwset dinstr ->
       let open Dba in
       match dinstr with
       | Instr.SJump (JOuter dst, (Some (Call _))) ->
         (* Only this pattern marks a call instruction of which we know the
              target *)
         let a = Dba_types.Caddress.to_virtual_address dst in
         Virtual_address.Set.add a hwset
       | _ -> hwset
    ) Virtual_address.Set.empty


module Logger = Logger.Make (struct let name = "hunk" end)
(* TODO: take possible failures into account *)
let export_to_file g =
  let filename = Filename.temp_file "dba" ".dot" in
  Logger.debug ~level:4 "Exporting graph to file %s" filename;
  let oc = Pervasives.open_out_bin filename in
  C.D.output_graph oc g;
  close_out oc;
  filename

let view ~viewer filename =
  let svg_filename = Filename.chop_extension filename ^ ".svg" in
  Logger.debug ~level:4 "Exporting graph to SVG file %s" svg_filename;
  let cmd = Printf.sprintf "dot -T svg %s > %s" filename svg_filename in
  ignore (Sys.command cmd);
  let view_cmd = Printf.sprintf "%s %s" viewer svg_filename in
  ignore(Sys.command view_cmd)

let export_and_view ?(cmd="firefox") g =
  export_to_file g |> view ~viewer:cmd

(* [is_return] actually checks if the graph is a linear suite of instruction
   terminated by a return jump.
*)
let is_return g =
  let rec aux node =
    match C.V.inst node with
    | None -> false
    | Some dinst ->
      DI.is_return dinst
      ||
        match C.succ g node with
        | [v] -> aux v
        | _ -> false
  in match C.mem_vertex_a g (A.of_int 0) with
  | None -> false
  | Some v -> aux v


exception Has_indirect_jump
let has_indirect_jump g =
  try
    C.iter_vertex
      (fun v ->
         match C.V.inst v with
         | None -> ()
         | Some (Dba.Instr.DJump _) -> raise Has_indirect_jump
         | Some _ -> ()
      ) g;
    false
  with
  | Has_indirect_jump -> true


module Check = struct
  let inner_jump_inside_bound t label =
    label >= 0 && label < length t

  let get_inner_jumps =
    let open Dba in
    let aux acc = function
      | Instr.SJump (JInner id, _)
      | Instr.If (_, JInner id, _) -> id :: acc
      | Instr.If _
      | Instr.SJump _
      | Instr.DJump _
      | Instr.Assign _
      | Instr.Stop _
      | Instr.Assert _
      | Instr.Assume _
      | Instr.NondetAssume _
      | Instr.Nondet _
      | Instr.Undef _
      | Instr.Malloc _
      | Instr.Free _
      | Instr.Print _ -> acc
    in fold aux []

  let has_inbound_inner_jumps t =
    get_inner_jumps t
    |> List.for_all (inner_jump_inside_bound t)


  exception Undeclared_Variable of string * Dba.Instr.t

  let no_undeclared_variables decls t =
    let no_undeclared_at_instr i =
      let du = DI.variables i in
      let vset =
        let open Dba_types in
        Basic_types.String.Set.union du.uses du.defs in
      try
        Basic_types.String.Set.iter
          (fun vname ->
             if not (Basic_types.String.Map.mem vname decls) then
               raise (Undeclared_Variable (vname, i))
          ) vset;
        true
      with
      | Undeclared_Variable (vname, instr) ->
        Logger.fatal
          "Undeclared variable %s at instruction %a"
          vname Dba_printer.Ascii.pp_instruction instr
    in for_all no_undeclared_at_instr t


  exception Temporaries_undefined of Basic_types.String.Set.t * Dba.Instr.t


  module N =
    C.Fixpoint(
    struct

      type data = Basic_types.String.Set.t
      let direction = Cfg.Forward
      let join = Basic_types.String.Set.union
      let equal = Basic_types.String.Set.equal
      let analyze e d =
        let src = C.E.src e in
        match C.V.inst src with
        | None -> d
        | Some i ->
          join (DI.temporaries i).Dba_types.defs d
    end
    )

  let no_temporary_leak g =
    let module Strg = Basic_types.String in
    let init v =
      match C.V.inst v with
      | None -> Strg.Set.empty
      | Some i -> (DI.temporaries i).Dba_types.defs
    in
    let f = N.analyze init g in
    Logger.debug ~level:6
      "@[<v 0>%a@]"
      (fun ppf g ->
         C.iter_vertex
           (fun v ->
             Format.fprintf ppf "%d: %a [%a]@ "
               (C.V.addr v)
               (Print_utils.pp_opt Dba_printer.Ascii.pp_instruction) (C.V.inst v)
               (fun ppf s ->
                 Strg.Set.iter
                   (fun name -> Format.fprintf ppf "%s; " name)
                   s) (f v)
           ) g
      ) g;
    try
      C.iter_vertex
        (fun v ->
           match C.V.inst v with
           | None -> ()
           | Some inst ->
             let du = DI.temporaries inst in
             let defined = f v in
             let undefined_temporaries =
               Strg.Set.diff du.Dba_types.uses defined in
             if not (Strg.Set.is_empty undefined_temporaries) then
               raise (Temporaries_undefined (undefined_temporaries, inst))
        ) g;
      true
    with
    | Temporaries_undefined (tset, instr) ->
      export_and_view g;
      Logger.fatal
        "@[<h>Temporaries %a were previously \
         undefined but used at instruction %a@]"
        (fun ppf set ->
           Basic_types.String.Set.iter
             (fun s -> Format.fprintf ppf "%s;@ " s)
             set)
        tset
        Dba_printer.Ascii.pp_instruction instr


end


module Simplify = struct

  let remove_gotos g =
    C.iter_vertex
      (fun v ->
         let open Dba in
         Logger.debug "Checking out instruction %d" (C.V.addr v);
         match C.V.inst v with
         | Some (Instr.SJump (JInner _, _) as just_i) ->
           begin
             match C.succ g v, C.pred g v with
             | [s], [p] ->
               Logger.debug "Removing %a"
                 Dba_printer.Ascii.pp_instruction just_i;
               C.add_edge g p s;
               C.remove_edge g p v;
               C.remove_edge g v s;
               (* Patch the predeccessor hard coded DBA successor *)
               begin
                 match C.V.inst p with
                 | None -> ()
                 | Some i ->
                   let dst_id = C.V.addr s in
                   let src_id = C.V.addr v in
                   let instr =
                     DI.reset_successor ~src_id ~ dst_id i in
                   C.add_inst g (C.V.addr p) instr
               end;
               C.remove_vertex g v
             | [_], [] ->
               (* The case where a goto has no predecessors should not happen
               *)
               assert false
             | [], [_] ->
               (* The case where a goto has no successors makes no sense *)
               assert false
             | _, _ -> ()
           end
         | Some _ | None -> ()
      ) g


  let run g = remove_gotos g
end



type conditional = {
    condition : Dba.Expr.t;
    consequent : Virtual_address.t;
    alternative : Virtual_address.t;
}

let conditional g =
  if length g <> 2 then None
  else begin
    match beginning_inst g with
    | None -> None
    | Some Dba.Instr.If (condition, Dba.JOuter consequent, _) ->
       begin
         match C.succ g (beginning g |> Utils.unsafe_get_opt) with
         | []
         | _ :: _ :: _ -> None
         | [v] ->
            match C.V.inst v with
            | None -> None
            | Some Dba.Instr.SJump (Dba.JOuter alternative, _) ->
               let open Dba_types.Caddress in
               Some { condition;
                      consequent = to_virtual_address consequent;
                      alternative = to_virtual_address alternative;
                 }
            | Some _ -> None
       end
    | Some _ -> None
    end
;;
