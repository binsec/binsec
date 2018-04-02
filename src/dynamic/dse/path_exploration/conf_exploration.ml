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

open Options

type conf_t = string * int

let counter_conf =
  let count = ref (-1) in
  fun () ->
    incr count;
    !count

let get_conf_name conf = fst conf
let get_conf_id   conf = snd conf

let build_analysis_configuration tracefile conf =
    let conf = get_conf_name conf in
    let options = Piqirun_ext.make_options ~json_omit_missing_fields:true () in
    let config_raw = File_utils.load conf in
    { configuration = Config_piqi_ext.parse_configuration ~opts:options config_raw `json;
      trace_file    = tracefile ;
      trace_input   = Chunked(open_in_bin tracefile, true) 
    }

