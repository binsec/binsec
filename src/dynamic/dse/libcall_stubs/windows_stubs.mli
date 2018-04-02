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

open Libcall_types
open Libcall_piqi

module GetModuleHandle_call: LibCall
  with type data_t = getmodulehandle_t
   and type pol_t = getmodulehandle_pol

module GetProcAddress_call: LibCall
  with type data_t = getprocaddress_t
   and type pol_t = getprocaddress_pol

module Getmainargs_call: LibCall
  with type data_t = getmainargs_t
   and type pol_t = getmainargs_pol

module Gethostname_call: LibCall
  with type data_t = gethostname_t
   and type pol_t = gethostname_pol

