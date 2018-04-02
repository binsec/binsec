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

#include<iostream>
#include<list>
#include<map>
#include<set>
#include<ctime>
#include<math.h>

#include <algorithm>
#include <fstream>

#include <pin.H>

#include "types/protobuf/config.pb.h"
#include "types/instruction.hpp"
//#include "types/types.hpp"
#include "callback.hpp"
#include "inout/trace.hpp"
#include "server/server.hpp"

#include "lib/json2pb/json2pb.h"
#include "instrumentation.hpp"

PIN_LOCK lock;              //Use by Pin for thread
INT32    num_threads = 0;   //Hold the number of threads
TLS_KEY  tls_key;           //Storage for thread, managed by Pin

//-- Config related
ADDRINT start_address          = ADDRINT{0};            //Address where to start (set by main.cpp)
ADDRINT stop_address           = ADDRINT{0};            //Address where to stop  (set by main.cpp)
string  function_to_instrument = "";                    //Function name that should be instrumented
bool    with_callmap           = false;                 //Hold if a callmap was provided (set by main.cpp)
bool	strict_skip            = true;                  //Hold wheter or not we will try to apply a libpolicy on skips
bool    auto_skip              = false;
bool    server_enabled         = false;					//True if the server is running
bool    trace_waves            = false;
bool    start_at_eip           = false;
bool	fini_called			   = false;
int     current_wave           = 0;
std::set<ADDRINT> skip_addr    = std::set<ADDRINT>();   //Set of addresses to skip (set by main.cpp)
std::set<ADDRINT> breakpoints  = std::set<ADDRINT>();
std::time_t timeout            = 0;                     //Timeout
std::time_t start_timestamp    = 0;


std::map<ADDRINT, unsigned int> instruction_exec_counter = std::map<ADDRINT, unsigned int>{};

configuration::configuration configuration_file;        //Configuration file (filled by json2pb)

libcall_policy_name_map_t    libcall_policy_name_map    = libcall_policy_name_map_t();      //Map policies with names: name->policy     (used when no callmap provided)
libcall_policy_address_map_t libcall_policy_address_map = libcall_policy_address_map_t();   //Map policies with address: address-> policy (used when callmap provided)
syscall_policy_map_t         syscall_policy_map         = syscall_policy_map_t();           //Map of syscalls: int->policy(syscall)
instruction_map_t            instruction_map            = instruction_map_t();              //Map of instructions: ADDR->instruction
input_map_t                  inputs                     = input_map_t();                    //Map for inputs <ADDR,BEFORE/AFTER> -> input list
wave_map_t                   wave_mapping               = wave_map_t();                     //Map of addr->int where int is the wave layer that wrote at that addr
//========================================================================


// function to access thread-specific data
thread_data_t* get_tls(THREADID threadid) {
    thread_data_t* tdata = static_cast<thread_data_t*>(PIN_GetThreadData(tls_key, threadid));
    return tdata;
}


VOID Instruction_instrumentation(INS ins, VOID *v) {

    ADDRINT addr = INS_Address(ins);
    Instruction inst = Instruction(ins);
    instruction_map[addr] = inst;

    // trick: we set the initial value for the counter as the max value for unsigned int type,
    // when it is updated (i.e. incremented the first time), then it becomes 0
    instruction_exec_counter[addr] = std::numeric_limits<unsigned int>::max();

    INS_InsertCall(ins, IPOINT_BEFORE, (AFUNPTR)callback_instruction_before, //Insert the main callback common to all instructions
                   IARG_INST_PTR,
                   IARG_CONTEXT,
                   IARG_THREAD_ID,
                   IARG_CALL_ORDER, CALL_ORDER_FIRST,
                   IARG_END);

    //Set various callbacks with the current instruction manipulate memory data in read/write
    bool standard_mem_op = INS_IsStandardMemop(ins);
    if ((inst.flags & IS_MEMORY_READ) && standard_mem_op) {
        INS_InsertCall(ins, IPOINT_BEFORE, (AFUNPTR) callback_instruction_memory,
                       IARG_THREAD_ID, IARG_CONTEXT, IARG_MEMORYREAD_EA, IARG_MEMORYREAD_SIZE, IARG_BOOL, true, IARG_CALL_ORDER, CALL_ORDER_FIRST+1, IARG_END);
    }
    if (INS_HasMemoryRead2(ins) && standard_mem_op) {
        INS_InsertCall(ins, IPOINT_BEFORE, (AFUNPTR) callback_instruction_memory,
                       IARG_THREAD_ID, IARG_CONTEXT, IARG_MEMORYREAD2_EA, IARG_MEMORYREAD_SIZE, IARG_BOOL, true, IARG_CALL_ORDER, CALL_ORDER_FIRST+2, IARG_END);
    }

    if ((inst.flags & IS_MEMORY_WRITE) && standard_mem_op) {
        INS_InsertCall(ins, IPOINT_BEFORE, (AFUNPTR)callback_instruction_memory,
            IARG_THREAD_ID, IARG_CONTEXT, IARG_MEMORYWRITE_EA, IARG_MEMORYWRITE_SIZE, IARG_BOOL, false, IARG_CALL_ORDER, CALL_ORDER_FIRST+3, IARG_END);
    }


    if(INS_IsBranchOrCall(ins)) {

        if (INS_IsDirectCall(ins)) {
            ADDRINT called_addr = INS_DirectBranchOrCallTargetAddress(ins);
            for(int i = 0; i < configuration_file.fun_skips_size(); i++) { //Potentially add a skip on the current call if it jumps to an address listed in skip-auto
                uint64_t current_addr = configuration_file.fun_skips(i);
                if(current_addr == called_addr) {
                    skip_addr.insert(addr);
                    cout << "Skip(auto) add addr:" << hexstr(addr) << endl;
                }
            }
        }

        INS_InsertCall(ins, IPOINT_TAKEN_BRANCH, (AFUNPTR)callback_instruction_after, //If the instruction is a branching one use the IPOINT_TAKEN_BRANCH
                       IARG_INST_PTR,
                       IARG_CONTEXT,
                       IARG_THREAD_ID,
                       IARG_FUNCRET_EXITPOINT_VALUE,
                       IARG_BOOL, true,
                       IARG_CALL_ORDER, CALL_ORDER_LAST,
                       IARG_END);
    }
    if(INS_HasFallThrough(ins)) {

        INS_InsertCall(ins, IPOINT_AFTER, (AFUNPTR)callback_instruction_after,         //Otherwise use the IPOINT_AFTER
                       IARG_INST_PTR,
                       IARG_CONTEXT,
                       IARG_THREAD_ID,
                       IARG_FUNCRET_EXITPOINT_VALUE,
                       IARG_BOOL, false,
                       IARG_CALL_ORDER, CALL_ORDER_LAST,
                       IARG_END);
    }
}



VOID Function_instrumentation(RTN rtn, VOID* data) {
  RTN_Open(rtn);

  RTN_InsertCall(rtn, IPOINT_BEFORE, (AFUNPTR)callback_routine_before, //Insert a callback on the entry of a function will all the parameters
                 IARG_THREAD_ID,
                 IARG_CONTEXT,
                 IARG_INST_PTR,
                 IARG_ADDRINT, RTN_Name(rtn).c_str(),
                 IARG_FUNCARG_ENTRYPOINT_VALUE, 0,
                 IARG_FUNCARG_ENTRYPOINT_VALUE, 1,
                 IARG_FUNCARG_ENTRYPOINT_VALUE, 2,
                 IARG_FUNCARG_ENTRYPOINT_VALUE, 3,
                 IARG_FUNCARG_ENTRYPOINT_VALUE, 4,
                 IARG_FUNCARG_ENTRYPOINT_VALUE, 5,
                 IARG_FUNCARG_ENTRYPOINT_VALUE, 6,
                 IARG_FUNCARG_ENTRYPOINT_VALUE, 7,
                 IARG_FUNCARG_ENTRYPOINT_VALUE, 8,
                 IARG_FUNCARG_ENTRYPOINT_VALUE, 9,
                 IARG_END);

  RTN_InsertCall(rtn, IPOINT_AFTER, (AFUNPTR)callback_routine_after, IARG_THREAD_ID, IARG_CONTEXT, IARG_ADDRINT, RTN_Name(rtn).c_str(), IARG_END);
  RTN_Close(rtn);
}



VOID Image_instrumentation(IMG img, VOID* data) {

  cout << "Image load:" << IMG_Name(img) <<  endl;

	if (start_at_eip && IMG_IsMainExecutable(img)) {
		cout << "Entrypoint:" << hex << IMG_Entry(img) << endl;
		start_address = IMG_Entry(img);
	}

    for (SEC sec = IMG_SecHead(img); SEC_Valid(sec); sec = SEC_Next(sec)) {

      for (RTN rtn = SEC_RtnHead(sec); RTN_Valid(rtn); rtn = RTN_Next(rtn)) {
        RTN_Open(rtn);
        string name = RTN_Name(rtn);
        if(function_to_instrument == name && function_to_instrument != "") { //When loading an image if it contains the function to instrument set the start, stop accordingly
          start_address = RTN_Address(rtn);
          stop_address = INS_Address(RTN_InsTail(rtn));
          cout << "start:" << hexstr(start_address) << ", stop:" <<hexstr(stop_address) << endl;
        }
        RTN_Close(rtn);
      }

    }
}


VOID Fini_instrumentation(INT32 code, VOID *v) {
    clock_t t = clock() - start_timestamp;
    int seconds = static_cast<int>(((float)t)/CLOCKS_PER_SEC);
	if (!fini_called) {
		fini_called = true;
        printf("Seconds:%d\n", seconds);
        cout << "Fini_instrumentation in " << int(seconds/60) <<"m" << dec << (seconds % 60) <<"s" << endl;
		finalize_trace_file();
		cout << "Trace finalized" << endl;
		if (server_enabled) {
			server_enabled = false;
			//send_message("END", "EMPTY", true); //Calling it apparently does not work on windows
			shutdown_server();
		}
		cout << "End instrumentation.." << endl;
	}
	else
		cout << "Fini already called.." << endl;
}
