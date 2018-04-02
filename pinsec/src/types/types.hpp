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

#ifndef TYPES_H
#define TYPES_H

#include<map>
#include<vector>
#include<list>
#include <algorithm>
#include <unordered_map>

#include <boost/variant.hpp>

#include "instruction.hpp"
#include "protobuf/syscall.pb.h"
#include "protobuf/libcall.pb.h"
#include "protobuf/config.pb.h"
#include "protobuf/common.pb.h"
#include "protobuf/trace.pb.h"


//Internal description of the current concrete infos of an instruction (because manipulating protobuf at this level is boring)
struct instruction_infos_t {
    bool     isempty;
    THREADID threadid;
    ADDRINT  address;
    string   opcode;
    std::vector<trace_format::ins_con_info_t*> concinfos;
};


//Class that holds the thread specific informations
class thread_data_t
{
  public:
    thread_data_t() {
        is_tracing=false;
        pending_call=false;
        resume_addr=0;
        tmp_writeaddr=0;
        tmp_writesize=0;
        ins_infos.isempty = true;
        ins_infos.concinfos.reserve(20);
        parameters.reserve(10);
		callbackafter_called = true;
		re_execute = false;
		suspended = false;
    }
    bool    is_tracing;             //Either we should trace or not
    bool    pending_call;           //Either or not we are looking ahead for call infos
	bool	callbackafter_called;   //Used to hold if the callback after has been called (due to bug sometimes it is not)
	bool    re_execute;             //Set to true when re-executing an instruction
	bool    suspended;				//Set to true when a breakpoint is reached
    std::vector<ADDRINT> parameters;//Temporary function parameters when there is no callmap
    ADDRINT resume_addr;            //Resume address when we are not tracing
    const libcall_types::libcall_pol* current_policy;
    ADDRINT* tmp_writeaddr;         //Temporary write address (only avaible in the callback BEFORE and we need it in the AFTER)
    UINT32   tmp_writesize;         //Temporary write size (idem)
    SYSCALL_STANDARD cur_sys_std;   //Current syscall (because need this in the callback AFTER)
    ADDRINT  cur_sys_num;           //Current syscall number (idem)
    instruction_infos_t ins_infos;  //Current instruction (see above)
	ADDRINT previous_addr;			//Previous address (used only because of a *bug* in pintool)
};

using instruction_map_t             = std::unordered_map<ADDRINT, Instruction>;
using libcall_policy_name_map_t     = std::map<std::string, const libcall_types::libcall_pol*>;
using libcall_policy_address_map_t  = std::map<ADDRINT, const libcall_types::libcall_pol*>;
using syscall_policy_map_t          = std::map<uint32_t, const syscall_types::syscall_pol*>;
using wave_map_t                    = std::map<ADDRINT, int>;

//---- Types for inputs ----
struct reg_input_t {
  REG                      reg;
  common::register_value_t value;
  common::action           action;
};

struct mem_input_t {
  ADDRINT       addr;
  unsigned int  size;
  string        value;
  common::action action;
};

struct indirect_input_t {
  REG           reg;
  unsigned int  size;
  string        value;
  common::action action;
};
using input_t = boost::variant<reg_input_t, mem_input_t,indirect_input_t>;
enum { REG_INPUT = 0, MEM_INPUT = 1, INDIRECT_INPUT=2};


using input_pos_t = std::tuple<ADDRINT, configuration::input_t::when_t, unsigned int>;
using input_map_t = std::map< input_pos_t, std::list<input_t> >;
//------------------------

#endif // TYPES_H
