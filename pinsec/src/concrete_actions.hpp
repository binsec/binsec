/**************************************************************************/
/*  This file is part of Binsec.                                          */
/*                                                                        */
/*  Copyright (C) 2016-2017                                               */
/*    VERIMAG                                                             */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  you can redistribute it and/or modify it under the terms of the GNU   */
/*  Lesser General Public License as published by the Free Software       */
/*  Foundation, version 2.1.                                              */
/*                                                                        */
/*  It is distributed in the hope that it will be useful,                 */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         */
/*  GNU Lesser General Public License for more details.                   */
/*                                                                        */
/*  See the GNU Lesser General Public License version 2.1                 */
/*  for more details (enclosed in the file licenses/LGPLv2.1).            */
/**************************************************************************/

#include <vector>
#include "pin.H"

#include "types/protobuf/common.pb.h"

void set_register_value(CONTEXT*  ctx, REG reg, common::register_value_t val);

ADDRINT get_register_value(CONTEXT* ctx, REG reg);

void record_one_register(THREADID threadid, REG reg, CONTEXT * ctx, bool isread);

void record_registers(THREADID threadid, std::vector<REG> regs, CONTEXT * ctx, bool isread);

void record_memory(THREADID threadid, VOID* ea, UINT32 size, bool isread);

void read_memory(VOID* ea,  char* dst,  UINT32 size);

string read_memory_string(VOID* ea);
string read_n_memory_string(VOID* ea,int sz);

void patch_memory(VOID* ea, UINT32 size, char* value);

void record_wave_number(THREADID threadid, ADDRINT addr, int size);
void record_argv(THREADID threadid, CONTEXT * ctx);
