(**************************************************************************)
(*  This file is part of Binsec.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2017                                               *)
(*    VERIMAG                                                             *)
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
(**************************************************************************)

open Libcall_types
open Libcall_piqi

(** Libcall stubs *)

(** Atoi stub  
 
    Pol: src: (addr: action,value: action), ret: action 
    
    Rules: No specific limitation
 *)
module Atoi_call: LibCall
  with type data_t = atoi_t
   and type pol_t = atoi_pol

(**
  Printf stub

  Pol: format: (addr: action,value: action), ret: action 

  Rules: no specific limitation
  *)
module Printf_call: LibCall
  with type data_t = printf_t
   and type pol_t = printf_pol

(**
  Malloc stub

  Pol: size: action, ret: action 

  Rules: no specific limitation
  *)
module Malloc_call: LibCall
  with type data_t = malloc_t
   and type pol_t = malloc_pol

module Strcpy_call: LibCall
  with type data_t = strcpy_t
   and type pol_t = strcpy_pol

(**
  Generic stub

  Pol: ret: action

  Rules: no specific limitation

  Info: Conc = conc eax. Symb = symb eax, other = do nthing
  *)
module Generic_call: LibCall
  with type data_t = generic_t
   and type pol_t = generic_pol

(**
 Mmap stub: Create new symbolic inputs
 
 Pol: addr: action, length: action, prot: action, flags: action, fd: action, offset: action, ret: (addr: action, value: action)
 
 Rules: Conc ret.addr, do nothing with other parameters 
 
 Notes:  Inputs (of size) created in ret.addr, created only in path_exploration
 *)   
module Mmap_call: LibCall
  with type data_t = mmap_t
   and type pol_t = mmap_pol

(**
  Read stub: Create new symbolic inputs
 
  Pol: fd: action, buf: action, counter: action, ret: action

  Rules: No specific limitation
 
 Notes:  Inputs (of size min(counter,ret)) created in buf, created only in path_exploration
 *)
module Read_call: LibCall
  with type data_t = read_t
   and type pol_t = read_pol

(**
  Fread stub: Create new symbolic inputs
 
  Pol: ptr: action, size: action, nmemb: action, stream: action, ret: action

  Rules: No specific limitation
 
 Notes:  Inputs (of size min(size*nmemb,ret)) created in ptr, created only in path_exploration
 *)
module Fread_call: LibCall
  with type data_t = fread_t
   and type pol_t = fread_pol



(**
  Open stub

  Pol: pathname (addr: action, value: action), flags: action, mode: action, ret: action

  Rules: Do nothing with parameters

  Notes: Automatically keeps ret -> pathname.
  Used during path exploration to assign inputs from a fd -> filename.
 *)
module Open_stub_call: LibCall
  with type data_t = open_t
   and type pol_t = open_pol

(**
    Lseek stub: stub of lseek
     
    Pol: fd: action, offset: action, whence: action, ret: action
 
    Rules: No specific limitation
          
    Notes:  Move offset of a file descripter in path exploration
 *)
module Lseek_call: LibCall
  with type data_t = lseek_t
     and type pol_t = lseek_pol


(**
  Fscanf stub: Create new symbolic inputs (experimental)
 
  Pol: stream: action, elems: action, ret: action
 
  Rules: Do nothing with parameters
 
  Notes:  Inputs (following format) created. Only %d are implemented for now. Created only in path_exploration
  
  Still experimental
 *)
module Fscanf_call: LibCall
  with type data_t = fscanf_t
   and type pol_t = fscanf_pol 

(**
  Free stub 
 
  Pol: ptr: action

  Rules: No specific limitation
  *)
module Free_call: LibCall
  with type data_t = free_t
   and type pol_t = free_pol

(**
  Strchr stub 

  Pol: sc: (addr: action, value: action); c: (addr: action, value:action), ret: action

  Rules: All actions = logic
  *)
module Strchr_call: LibCall
  with type data_t = strchr_t
   and type pol_t = strchr_pol

(**
  Strchr stub 

  Pol: sc: (addr: action, value: action); c: (addr: action, value:action), ret: action
 
  Rules: All actions = logic

  (strrchr same as strchr, but returns the last occurence, not the first)
  *)
module Strrchr_call: LibCall
  with type data_t = strchr_t
   and type pol_t = strchr_pol

(**
  Strcmp stub 

  Pol: src: (addr: action, value: action); dst: (addr: action, value:action), ret: action

  Rules: All actions = logic

  Notes:  compare elements up to the max of the concrete size of the two paramaters
  *)
module Strcmp_call: LibCall
  with type data_t = strcmp_t
   and type pol_t = strcmp_pol

(**
  Strncmp stub 

  Pol: src: (addr: action, value: action); dst: (addr: action, value:action), ret: action

  Rules: All actions = logic

  Notes:  compare elements up to the max of the concrete size of the two paramaters
  *)
module Strncmp_call: LibCall
  with type data_t = strncmp_t
   and type pol_t = strncmp_pol



(**
 *   Memcmp stub 
 *
 *     Pol: src: (addr: action, value: action); dst: (addr: action, value:action), ret: action
 *
 *       Rules: All actions = logic
 *
 *         Notes:  compare elements up to the max of the concrete size of the two paramaters
 *           *)
module Memcmp_call: LibCall
  with type data_t = memcmp_t
     and type pol_t = memcmp_pol




module Strncpy_call: LibCall
  with type data_t = strncpy_t
   and type pol_t = strncpy_pol

(**
  Ctype_b_loc stub

  Pol: table: (addr: action, value: action), ret: action

  Rules: table.addr  = conc, table.value = conc, ret = logic

  Note: function added by gcc
  *)
module Ctype_b_loc_call: LibCall
  with type data_t = ctype_b_loc_t
   and type pol_t = ctype_b_loc_pol

module Memcpy_call: LibCall
  with type data_t = memcpy_t
   and type pol_t = memcpy_pol

(**
  Realloc stub
 
  Pol: ptr: action, size: action, ret: action

  Rules: ret: conc

  Note: Conc ret, do nothing with param

  if ret != ptr != null -> copy content of *ptr in *ret
  *)
module Realloc_call: LibCall
  with type data_t = realloc_t
   and type pol_t = realloc_pol

(** 
  Memset stub

  Pol: s: (addr: action, value: action), c: action, size: action, ret: action

  Note: if s.value = logic -> implicit concretization of the size
  *)
module Memset_call: LibCall
  with type data_t = memset_t
   and type pol_t = memset_pol

module Fgetc_call: LibCall
  with type data_t = fgetc_t
   and type pol_t = fgetc_pol

module Fstat_call: LibCall
  with type data_t = fstat_t
   and type pol_t = fstat_pol

module Fxstat64_call: LibCall
  with type data_t = fxstat64_t
     and type pol_t = fxstat64_pol

module Qsort_call: LibCall
  with type data_t = qsort_t
   and type pol_t = qsort_pol

module Bsearch_call: LibCall
  with type data_t = bsearch_t
   and type pol_t = bsearch_pol 

