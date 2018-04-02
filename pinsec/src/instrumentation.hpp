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

#ifndef INSTRUMENTATION_H
#define INSTRUMENTATION_H

#include <pin.H>
#include <list>
#include <set>
#include <ctime>

#include "types/types.hpp"
#include "types/protobuf/config.pb.h"
#include "callback.hpp"

VOID Image_instrumentation(IMG img, VOID* data);

VOID Instruction_instrumentation(INS ins, VOID *v);

VOID Fini_instrumentation(INT32 code, VOID *v);

VOID Function_instrumentation(RTN rtn,VOID *v);

thread_data_t* get_tls(THREADID threadid);

//Global variables
extern TLS_KEY tls_key;
extern PIN_LOCK lock;
extern INT32 num_threads;

//Config related
extern ADDRINT start_address;
extern ADDRINT stop_address;
extern bool with_callmap;
extern bool strict_skip;
extern bool auto_skip;
extern bool trace_waves;
extern bool start_at_eip;
extern bool server_enabled;
extern int  current_wave;
extern string function_to_instrument;
extern std::set<ADDRINT> skip_addr;
extern std::set<ADDRINT> breakpoints;
extern instruction_map_t instruction_map;
extern configuration::configuration configuration_file;

extern std::map<ADDRINT, unsigned int> instruction_exec_counter;

extern libcall_policy_name_map_t libcall_policy_name_map;
extern libcall_policy_address_map_t libcall_policy_address_map;
extern syscall_policy_map_t syscall_policy_map;
extern input_map_t inputs;
extern wave_map_t wave_mapping;
extern std::time_t timeout;
extern std::time_t start_timestamp;

#endif
