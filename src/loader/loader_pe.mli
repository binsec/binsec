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

open Loader_types

type file_header = private {
  machine                 : u16;
  number_of_sections      : u16;
  time_date_stamp         : u32;
  pointer_to_symbol_table : u32;
  number_of_symbols       : u32;
  size_of_optional_header : u16;
  characteristics         : u16;
}

type standard_fields = private {
  magic                      : u16;
  size_of_code               : u32;
  size_of_initialized_data   : u32;
  size_of_uninitialized_data : u32;
  address_of_entry_point     : u32;
  base_of_code               : u32;
  base_of_data        : u32 option;
}

type windows_fields = private {
  image_base              : u64;
  section_alignement      : u32;
  file_alignement         : u32;
  size_of_image           : u32;
  size_of_headers         : u32;
  checksum                : u32;
  subsystem               : u16;
  dll_characteristics     : u16;
  size_of_stack_reserve   : u64;
  size_of_stack_commit    : u64;
  size_of_heap_reserve    : u64;
  size_of_heap_commit     : u64;
  number_of_rva_and_sizes : u32;
}

type data_directory = {
  virtual_address : u32;
  size            : u32;
}

type data_directories = {
  export_directory       : data_directory;
  import_directory       : data_directory;
  resource_directory     : data_directory;
  exception_directory    : data_directory;
  security_directory     : data_directory;
  basereloc_directory    : data_directory;
  debug_directory        : data_directory;
  globalptr_directory    : data_directory;
  tls_directory          : data_directory;
  load_config_directory  : data_directory;
  bound_import_directory : data_directory;
  iat_directory          : data_directory;
  delay_import_directory : data_directory;
  clr_header_directory   : data_directory;
}

type optional_header = private {
  standard_fields  : standard_fields;
  windows_fields   : windows_fields;
  data_directories : data_directories;
}

type section = private {
  section_name     : string;
  virtual_size        : u32;
  virtual_address     : u32;
  size_of_raw_data    : u32;
  pointer_to_raw_data : u32;
  characteristics     : u32;
}

type symbol = private {
  symbol_name         : string;
  value                 : u32;
  section_number        : u16;
  storage_class         : u8;
  number_of_aux_symbols : u8;
}

include Loader_sigs.S
  with type Section.header = section
   and type Symbol.header  = symbol
   and type Img.header     = file_header * optional_header
