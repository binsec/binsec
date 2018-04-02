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

open Dba_types

let is_mem_call a = Caddress.Set.mem (Caddress.reid a 0)

let rec get_violated_rets infos acc =
  let open Analysis_config_piqi.Callret_analysis_results_ret_data in
  match infos with
  | [] -> acc
  | ret :: tl ->
    match ret.status with
    | `ok -> get_violated_rets tl acc
    | `viol -> get_violated_rets tl (ret :: acc)
      (* let has_returned_once = *)
      (*        List.fold_left (fun acc label -> acc || label = `has_returned) false ret.labels in *)
      (* if not (has_returned_once)  then *)
      (*        ret :: acc *)
      (* else acc *)


let rec get_regular_rets infos acc =
  let open Analysis_config_piqi.Callret_analysis_results_ret_data in
  match infos with
  | [] -> acc
  | ret :: tl ->
    match ret.status with
    | `ok -> get_regular_rets tl (ret :: acc)
    | `viol -> get_regular_rets tl acc


open Format
let pp_jumpmap ppf jumpmap =
  fprintf ppf "@[<hov 0>";
  Caddress.Map.iter
    (fun addr addr_list ->
       printf "%a -> %a;@ "
         Dba_printer.Ascii.pp_code_address addr
         (fun ppf l ->
            List.iter
              (fun a -> fprintf ppf "%a" Dba_printer.Ascii.pp_code_address a) l)
         addr_list
    ) jumpmap;
  fprintf ppf "@]"


let pp_violated_returns ppf violated_rets =
  let open Analysis_config_piqi.Callret_analysis_results_ret_data in
  fprintf ppf "@[<hov 6>Violated rets:@ ";
  List.iter (fun ret ->
      let bv = Bitvector.create (Bigint.big_int_of_int64 ret.ret_addr) 32 in
      fprintf ppf "%a,@"  Bitvector.pp_hex bv)
    violated_rets;
  fprintf ppf "@]"


let pp_violated_calls ppf violated_calls =
  fprintf ppf "@[<hov 6>violated calls:@ ";
  Caddress.Set.iter
    (fun a -> fprintf ppf "%a, " Dba_printer.Ascii.pp_code_address a)
    violated_calls;
  fprintf ppf "@]"


let _get_violated_and_regular_calls jumpMap =
  if !Options.violated_call_ret_file = ""
  then Caddress.Set.empty, Caddress.Set.empty, jumpMap
  else
    let raw = File_utils.load !Options.violated_call_ret_file in
    let buf = Piqirun.init_from_string raw in
    let infos = Analysis_config_piqi.parse_callret_analysis_results buf in
    (* Renvoie un Callret_analysis_results (voir analysis_config_piqi.ml) *)
    let values = infos.Analysis_config_piqi.Callret_analysis_results.values in
    let violated_rets = get_violated_rets values [] in
    let regular_rets = get_regular_rets values [] in
    let viol_call_addrs acc rets =
      let open Analysis_config_piqi.Callret_analysis_results_call_data in
      List.fold_left
        (fun acc call ->
           let bv = Bitvector.create (Bigint.big_int_of_int64 call.addr) 32 in
           let a = Caddress.block_start bv in
           if call.status = `viol
           then Caddress.Set.add a acc else acc)
        acc rets.Analysis_config_piqi.Callret_analysis_results_ret_data.calls
    in
    let regular_call_addrs acc rets =
      let open Analysis_config_piqi.Callret_analysis_results_call_data in
      List.fold_left
        (fun acc call ->
           let a = Bitvector.create (Bigint.big_int_of_int64 call.addr) 32
                   |> Caddress.block_start in
           Caddress.Set.add a acc)
        acc rets.Analysis_config_piqi.Callret_analysis_results_ret_data.calls
    in

    let violated_calls =
      List.fold_left viol_call_addrs Caddress.Set.empty violated_rets
    in
    let regular_calls =
      List.fold_left regular_call_addrs Caddress.Set.empty regular_rets
    in
    let open Analysis_config_piqi.Callret_analysis_results_ret_data in
    let jumpMap =
      List.fold_left (fun acc ret ->
          let a = Bitvector.create (Bigint.big_int_of_int64 ret.ret_addr) 32
                |> Caddress.block_start in
          let addr_list = try Caddress.Map.find a acc with Not_found -> [] in
          let addr_list =
            List.fold_left (fun acc a ->
                let a =
                  Bitvector.create (Bigint.big_int_of_int64 a) 32
                  |> Caddress.block_start
                in a :: acc) addr_list ret.returnsites
          in
          let a =
            Bitvector.create (Bigint.big_int_of_int64 ret.ret_addr) 32
            |> Caddress.block_start
          in Caddress.Map.add a addr_list acc) jumpMap violated_rets
    in
    Logger.result "@[<v 0>%a@ %a@ %a@]"
      pp_jumpmap jumpMap
      pp_violated_returns violated_rets
      pp_violated_calls violated_calls;
    violated_calls, regular_calls, jumpMap
