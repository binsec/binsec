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

#include <pin.H>
#include <iostream>
#include <fstream>
#include<map>


#include "instrumentation.hpp"
#include "callback.hpp"
#include "types/protobuf/config.pb.h"
#include "types/protobuf/libcall.pb.h"
#include "types/protobuf/syscall.pb.h"
#include "types/protobuf/common.pb.h"
#include "lib/json2pb/json2pb.h"
#include "inout/trace.hpp"
#include "utils.hpp"
#include "server/server.hpp"
#include "call_infos.hpp"

using namespace std;
using namespace common;

//--------- Arguments -----------
KNOB<string>    function_param  (KNOB_MODE_WRITEONCE, "pintool", "function",  "",      "function name to instrument (replace start, stop)");
KNOB<string>    config_filename (KNOB_MODE_WRITEONCE, "pintool", "config",    "",      "configuration file");
KNOB<string>    trace_filename  (KNOB_MODE_WRITEONCE, "pintool", "out",       "out.tr","trace output file");
KNOB<int>       chunk_size      (KNOB_MODE_WRITEONCE, "pintool", "chunk-size","10000", "size of chunks in the trace");
KNOB<uint32_t>  trace_length_knob(KNOB_MODE_WRITEONCE,"pintool", "maxlength", "500000","length of trace");
KNOB<uint32_t>  timeout_knob    (KNOB_MODE_WRITEONCE, "pintool", "timeout",   "0",      "timeout in seconds");
KNOB<BOOL>      skip_strict_knob(KNOB_MODE_WRITEONCE, "pintool", "apply-pol-skip","0",  "search to apply libpolicy on skips");
KNOB<BOOL>      auto_skip_knob  (KNOB_MODE_WRITEONCE, "pintool", "auto-skip", "0",      "Automatically skip calls/jumps in external libraries");
KNOB<BOOL>      trace_wave_knob (KNOB_MODE_WRITEONCE, "pintool", "trace-wave", "0",     "Track waves of self-modifications");
KNOB<BOOL>      start_eip_knob  (KNOB_MODE_WRITEONCE, "pintool", "start-entrypoint", "0", "Start tracing at the entry point of the program");
KNOB<BOOL>      server_knob     (KNOB_MODE_WRITEONCE, "pintool", "server",    "0",      "enable server mode");
KNOB<string>    port_knob       (KNOB_MODE_WRITEONCE, "pintool", "port",      "5555",   "server port");

//-------------------------------

/* Fonction that read the configuration file and convert it into protobuf using json2pb */
bool load_configuration_file(string name) {
    ifstream is (name);
    if(!is) {
        cout << "Configuration file not found" << endl;
        return false;
    }
    is.seekg (0, is.end);
    int length = is.tellg();
    is.seekg (0, is.beg);
    char * buffer = new char [length]; //Create the buffer of the exact size
    is.read (buffer,length);
    is.close();

    json2pb(configuration_file, buffer, length); //Convert json to pb2 here

    if(configuration_file.has_start())          //Look for a start address
        start_address = configuration_file.start();
    if(configuration_file.has_stop())           //Look for a stop address
        stop_address = configuration_file.stop();

    for(int i = 0; i < configuration_file.call_skips_size(); i++) {
        uint64_t addr = configuration_file.call_skips(i);
        cout << "Skip add addr:" << hexstr(addr) << endl;
        skip_addr.insert(addr);
    }

	for (int i = 0; i < configuration_file.breakpoints_size(); i++) {
		uint64_t bp = configuration_file.breakpoints(i);
		cout << "Breakpoint:" << hexstr(bp) << endl;
		breakpoints.insert(bp);
	}
	if (configuration_file.breakpoints_size() != 0 && !server_knob.Value()) {
		cout << "Breakpoints are ignored when server mode disabled" << endl;
	}

    with_callmap = configuration_file.call_map_size() != 0;

    //Put the libcalls policies in a map
    for(int i = 0; i < configuration_file.libcalls_size(); i++) {
        const libcall_types::libcall_pol* policy_entry = &configuration_file.libcalls(i);
        std::string name = policy_entry->name();
        libcall_policy_name_map[name] = policy_entry;
    }

    //Put the syscalls in a map
    for(int i = 0; i < configuration_file.syscalls_size(); i++) {
        const syscall_types::syscall_pol* policy_entry = &configuration_file.syscalls(i);
        uint32_t id = policy_entry->id();
        syscall_policy_map[id] = policy_entry;
    }

    //Create the map ADDRINT->policy
    for(int i = 0; i < configuration_file.call_map_size(); i++) {
        uint64_t address = configuration_file.call_map(i).address();
        std::string polname = configuration_file.call_map(i).name();
        if(libcall_policy_name_map.find(polname) != libcall_policy_name_map.end())
            libcall_policy_address_map[address] = libcall_policy_name_map[polname];
        else {
            //cout << "No policy found so create a generic one. for("<< polname << ")" << endl;
			libcall_types::libcall_pol* lib = new libcall_types::libcall_pol();
			lib->set_ident(libcall_types::GENERIC);
			lib->set_action(common::SKIP);
			lib->set_name(polname);
            libcall_policy_address_map[address] = lib;
        }
    }


    //Create the two maps for inputs injection
    for (int i = 0; i < configuration_file.inputs_size(); i++)  {
        configuration::input_t input = configuration_file.inputs(i);
        if(input.action() != DEFAULT && input.action() != SYMB) { //Solely PATCH and CONC input are used by Pin (others by Binsec)
            std::list<input_t> current_elts;
            auto key = std::make_tuple(input.address(),input.when(),input.iteration());

            if(inputs.find(key) != inputs.end())
                current_elts = inputs[key];
            else
                current_elts = std::list<input_t>();

            if(input.typeid_() == configuration::input_t::REG) {
                REG reg = get_reg_from_name(input.reg().name());
                reg_input_t reg_i = { reg, input.reg().value(), input.action() };
                current_elts.push_front(reg_i);
                inputs[key] = current_elts;
            }
            else if(input.typeid_() == configuration::input_t::MEM) {
                ADDRINT addr = input.mem().addr();
                mem_input_t mem = { addr , input.mem().value().length(), input.mem().value(), input.action() };
                current_elts.push_front(mem);
                inputs[key] = current_elts;
            }
            else if(input.typeid_() == configuration::input_t::INDIRECT) {
                auto pin_reg = get_reg_from_name(input.indirect().name());
                auto indirect_item = indirect_input_t{pin_reg, input.indirect().value().length(), input.indirect().value(), input.action()};
                current_elts.push_front(indirect_item);
                inputs[key] = current_elts;
            }
        }
    }

    return true;
    //cout << "Message:" << configuration_file.DebugString().c_str() << endl;
    //cout << "JSON:" << pb2json(configuration_file).c_str() << endl;
}



int main(int argc, char* argv[]) {

    PIN_InitSymbols();

    if(PIN_Init(argc, argv)) {
        cerr << KNOB_BASE::StringKnobSummary() << endl;
		return 1;
    }

    initialize_lookup_map();

    strict_skip = !skip_strict_knob.Value();
    auto_skip = auto_skip_knob.Value();
    trace_waves = trace_wave_knob.Value();
	start_at_eip = start_eip_knob.Value();

    if(config_filename.Value() != "") {
        if(!load_configuration_file(config_filename.Value())) //Parse configuration file
            return 1;
    }

    if(function_param.Value() != "") {
        function_to_instrument = function_param.Value();    //Set the function name to instrument if provided
    }

    if(timeout_knob.Value() != 0)
        timeout = std::time(nullptr)+ timeout_knob.Value(); //Set the timeout if provided

    if(server_knob.Value())  {  //Initialize the serve if the option is set
		if (initialize_server(port_knob)) {
			cout << "Server initialized" << endl;
			server_enabled = true;
		}
        else {
            cout << "Failed to initialize server" << endl;
            return 1;
        }
    }

    // Initialize the lock
    PIN_InitLock(&lock);

    // Obtain  a key for TLS storage.
    tls_key = PIN_CreateThreadDataKey(0);                   //Create a storage to store thread datas

    //Initialize the trace file
    initialize_trace_file(trace_filename.Value(), chunk_size.Value(), trace_length_knob.Value()); //Initialize the trace file

    //Add instrumentation functions on IMG, RTN, INS
    IMG_AddInstrumentFunction(Image_instrumentation, 0);
    RTN_AddInstrumentFunction(Function_instrumentation, 0);
    INS_AddInstrumentFunction(Instruction_instrumentation, 0);

    //Add Syscall callbacks
    PIN_AddSyscallEntryFunction(Enter_syscall, 0);
    PIN_AddSyscallExitFunction(Exit_syscall, 0);

    //Add threads callbacks
    PIN_AddThreadStartFunction(ThreadStart, 0);
    PIN_AddThreadFiniFunction(ThreadFini, 0);

    PIN_AddFiniFunction(Fini_instrumentation, 0);

    start_timestamp = clock(); // Get the first start execution of the program
    PIN_StartProgram();
    return 0;
}

