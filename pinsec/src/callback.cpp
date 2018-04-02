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

#include <iostream>
#include "instrumentation.hpp"
#include <string.h>
#include <algorithm>
#include <ctime>
#include <unordered_map>
#include <cassert>
#include <boost/algorithm/string/predicate.hpp>

#include "inout/trace.hpp"
#include "types/protobuf/common.pb.h"
#include "types/protobuf/libcall.pb.h"
#include "concrete_actions.hpp"
#include "call_infos.hpp"
#include "syscall_infos.hpp"
#include "server/server.hpp"

int icount = 0;
libcall_types::libcall_pol* default_policy = new libcall_types::libcall_pol();
char opc_buffer[30];

// Fonction to checks for arbitrary value retrieval or patching values
void check_inputs_operations(THREADID threadid, ADDRINT addr, CONTEXT* ctx, configuration::input_t::when_t when, ADDRINT next) {
    auto ins_counter = instruction_exec_counter[addr];
    auto ident = std::make_tuple(addr, when, ins_counter);     //Values are in a map index by a tuple <addr,[BEFORE/AFTER], iteration> for a quick access to the right entries in log(n)

    if(inputs.find(ident) != inputs.end()) {    //Keep going only if there is inputs to process
        bool to_reboot = false;                 //Boolean that will be set to true if something has been patched and so the instruction should be re-instrumented (to take changes in consideration)

        while(!inputs[ident].empty()) {         //All inputs are processed and poped from the list (so for know an input can only be used once)
            input_t i = inputs[ident].front();  //Retrieve the input
            inputs[ident].pop_front();          //Pop it from the list
            switch(i.which()) {
                case REG_INPUT: {
                    reg_input_t reg_i = boost::get<reg_input_t>(i);
                    if(reg_i.action == common::CONC) {              //If CONC only retrieve the value of the reg and put it in concrete infos
                        record_one_register(threadid, reg_i.reg, ctx, true);
                        cout << "reg(" << REG_StringShort(reg_i.reg) << ")";
                    }
                    else if(reg_i.action == common::PATCH) {       //If PATCH, patch the register value and activate the reboot flag
                        set_register_value(ctx, reg_i.reg, reg_i.value);
                        to_reboot = true;
                    }
                    break;
                }
                case MEM_INPUT: {
                    mem_input_t mem_i = boost::get<mem_input_t>(i);
                    if(mem_i.action == common::CONC) {                  //If CONC of memory
                        cout <<" Mem:[" << hex << mem_i.addr << "]:";
                        record_memory(threadid, (void*)mem_i.addr, mem_i.size, true); //Put the memory value in concrete infos
                    }
                    else if(mem_i.action == common::PATCH) {            //If PATCH
                        char arr[100];
                        assert(mem_i.size < 100);
                        unsigned int idx = 0;
                        string s = mem_i.value;
                        for (unsigned int i = 0; i < mem_i.value.size(); i++) { //Dirty: Assume the value is in hex and iterate every two chars of the string
                            arr[idx] = s[i];                                       //Put the long in the allocated array
                            idx++;
                        }
                        patch_memory((void*)mem_i.addr, mem_i.size, arr);             //Patch the value directly. TODO: Patch using PIN_SafeCopy
                    }
                    break;
                }
                case INDIRECT_INPUT: {
                    indirect_input_t reg_i = boost::get<indirect_input_t>(i);
                    ADDRINT addr = get_register_value(ctx, reg_i.reg);
                    cout << "Indirect parch, eax " << hex << addr << endl;
                    mem_input_t mem_i = {addr,reg_i.size , reg_i.value,reg_i.action} ;
                   /* if(mem_i.action == common::CONC) {                  //If CONC of memory
                        cout <<" Mem:[" << hex << mem_i.addr << "]:";
                        record_memory(threadid, (void*)mem_i.addr, mem_i.size, true); //Put the memory value in concrete infos
                    }
                    else*/
                    if(mem_i.action == common::PATCH) {            //If PATCH
                        char arr[100];
                        assert(mem_i.size < 100);
                        unsigned int idx = 0;
                        string s = mem_i.value;
                        for (unsigned int i = 0; i < mem_i.value.size(); i++) { //Dirty: Assume the value is in hex and iterate every two chars of the string
                          arr[idx] = s[i];                                       //Put the long in the allocated array
                          idx++;
                        }
                        cout << "Will patch " << hex << mem_i.addr << " Size " << mem_i.size <<" val2" << mem_i.value.size() <<" Val " << arr << endl;
                        patch_memory((void*)mem_i.addr, mem_i.size, arr);             //Patch the value directly. TODO: Patch using PIN_SafeCopy
                    }
                    break;
                }
            }
        }
        if(inputs[ident].empty()) {
            inputs.erase(ident);  //Delete the entry from the map
        }
        if(to_reboot) {           //If to_reboot bool activated reinstrument the current instruction
            PIN_SetContextReg(ctx, REG_INST_PTR, next);
            PIN_ExecuteAt(ctx);
        }
    }
}

BOOL is_jumping_library(ADDRINT addr) {
    PIN_LockClient();
    IMG img = IMG_FindByAddress(addr);
    RTN rtn = RTN_FindByAddress(addr);
    string sec_name = "";
    if (RTN_Valid(rtn)) {
        RTN_Open(rtn);
        SEC sec = RTN_Sec(rtn);
        sec_name = SEC_Name(sec);
        RTN_Close(rtn);
    }
    PIN_UnlockClient();
    if(IMG_Valid(img)) {
        //cout << hex << addr << " is into " << IMG_Name(img) << endl;
        return !IMG_IsMainExecutable(img) || sec_name == ".plt";
    }
    else {
        //cout << hex << addr << "is an invalid image !" << endl;
        return false;
    }
 }
//======================================================



// =============== Instructions callbacks ==============
VOID callback_instruction_after(ADDRINT addr, CONTEXT * ctx, THREADID threadid, ADDRINT ret, bool is_branch);

//Instruction BEFORE callback called FIRST!
VOID callback_instruction_before(ADDRINT addr, CONTEXT * ctx, THREADID threadid) {
    thread_data_t* tdata = get_tls(threadid);

    Instruction const& ins = instruction_map[addr];
    //ADDRINT origin_addr = (ADDRINT)PIN_GetContextReg( ctx, REG_INST_PTR);

    if (std::time(nullptr) >= timeout && timeout != 0) {  //If timeout over stop tracing
      cout << "Timeout exceeded..." << endl;
      if (server_enabled) {
          Fini_instrumentation(0, nullptr);
      }
      PIN_ExitApplication(0);
    }

    if(!tdata->is_tracing && (addr == start_address || addr == tdata->resume_addr)) { //Reenable tracing if we reach start address or a resume address
        if(addr == start_address)
            cout << "===== Start tracing =====" << endl;
        tdata->is_tracing = true;
        tdata->resume_addr = 0;
	//record_argv(threadid,ctx); TODO: add as option
    }

    check_messages(ctx, threadid, tdata->suspended); //Check if there is sever message pendings

    if(tdata->is_tracing) {

        instruction_exec_counter[addr]++;
        //cout << endl;
        if(!tdata->ins_infos.isempty) {            //If there is pending concrete infos pending(of the previous instruction). Commit them to the trace to start collecting new ones
            if (!tdata->callbackafter_called){
                cout << "Call the callback_after manually because it has apparently not been done" << endl;
                ADDRINT eax = (ADDRINT)PIN_GetContextReg(ctx, LEVEL_BASE::REG_EAX); //Assume exit point value was eax
                callback_instruction_after(tdata->previous_addr, ctx, threadid, eax, false);
            }
            add_concrete_infos_trace(tdata->ins_infos, true);
            tdata->ins_infos.isempty = true;
            tdata->ins_infos.concinfos.clear();
        }

        //cout << dec << icount << ":[" << hex << addr << "] " << ins->disassembly << endl;
        icount++;

		if (breakpoints.find(addr) != breakpoints.end() && server_enabled && !tdata->re_execute) {
			tdata->re_execute = true;
			flush_current_chunk();
			cout << "Breakpoint reached:" << hex << addr << endl;
			tdata->suspended = true;
			send_message("BP_REACHED", std::to_string(addr), false);
			while (check_messages(ctx, threadid, true)) {}
		}
	
        check_inputs_operations(threadid, addr, ctx, configuration::input_t::BEFORE, addr); //First thing being done because it can potentially reboot the instruction instrumentation

		if (trace_waves) { //If wave tracing activated
            //int size = INS_Size(ins);
            bool of_higher_layer = false;
            for (unsigned int x=addr; x < addr+ins.opcode_size; x++) { //Loop because some bytes might have been patched not necessarily the whole instr
                if (wave_mapping.find(x) != wave_mapping.end()) {//if we find the target_addr in the wave mapping cache
                    of_higher_layer = of_higher_layer || (wave_mapping[x] == current_wave + 1);
                }
            }
            //If of wave+1 update the instruction in case it has been patched
            if (of_higher_layer) { //Jumping in the wave+1
                current_wave++;
                cout << "============== New Wave n°" << current_wave << " ==============" << endl;
                add_metadata_wave(current_wave);
            }
		}

        //Reset all necessary boolean, and update info of the current instruction
        tdata->ins_infos.isempty = false;
        tdata->ins_infos.threadid = threadid;
        tdata->ins_infos.address =  addr;
        read_memory((void*)addr, opc_buffer, ins.opcode_size);
        tdata->ins_infos.opcode = string(opc_buffer, ins.opcode_size);
        record_wave_number(threadid, addr, ins.opcode_size);
        record_registers(threadid, ins.src_registers, ctx, true); //Collect registers READ values
    }
	tdata->callbackafter_called = false;
	tdata->previous_addr = addr;
	tdata->re_execute = false;
}

//Instruction BEFORE callback called SECOND(and potentially multiple times for the same instr) ONLY if the instruction read values in memory
VOID callback_instruction_memory(THREADID threadid, CONTEXT* ctx, ADDRINT * mem_addr, UINT32 size, bool isread) {
    thread_data_t* tdata = get_tls(threadid);
    if(tdata->is_tracing) {     //Collect data only if we are tracing
        if(!isread) {           //If the instruction write in memory remember the size and address being written because they will not be available in the AFTER callback
            thread_data_t* tdata = get_tls(threadid);
            tdata->tmp_writeaddr = mem_addr;
            tdata->tmp_writesize = size;
        }
        else {
            record_memory(threadid, mem_addr, size, true); //If read collect the data
        }
    }

    //Update wave_map with the memory writes
    if(trace_waves && !isread) {
        for(UINT32 i = (ADDRINT)mem_addr; i < ((ADDRINT)mem_addr)+size; i++) {
            wave_mapping[i] = current_wave+1;
        }
    }
}

//Instruction AFTER callback called by every instruction
VOID callback_instruction_after(ADDRINT addr, CONTEXT * ctx, THREADID threadid, ADDRINT ret, bool is_branch) {
    Instruction const& ins = instruction_map[addr];
    thread_data_t* tdata = get_tls(threadid);
	tdata->callbackafter_called = true;
    ADDRINT target_addr = (ADDRINT)PIN_GetContextReg( ctx, REG_INST_PTR); //Retrieve the target address

    if(!tdata->is_tracing && (ins.flags & IS_RET) && target_addr == tdata->resume_addr) { //If we are not tracing and return on a "callsite", call the ret dispatcher
        dispatch_ret(threadid, ctx, ret);
    }

    if(tdata->is_tracing) {

        record_registers(threadid, ins.dst_registers, ctx, false); //Collect registers written
        //Set the next address concrrete info
        trace_format::ins_con_info_t* info = new trace_format::ins_con_info_t();
        info->set_typeid_(trace_format::ins_con_info_t::NEXT_ADDRESS);
        info->set_next_address(target_addr);
        //common::address_t* nextaddrinfos = info->mutable_next_address();
        //set_address_t(nextaddrinfos, sizeof(ADDRINT), target_addr);
        tdata->ins_infos.concinfos.push_back(info);
        //----------

        if(ins.flags & IS_MEMORY_WRITE) {
            record_memory(threadid, tdata->tmp_writeaddr, tdata->tmp_writesize, false); //Collect memory written (if there is)
        }

        if(ins.flags & IS_CALL) {
            tdata->resume_addr = ins.next_address;
        }

        if(skip_addr.find(addr) != skip_addr.end()) { //Check if the current address should then be skipped
            //We should skip
            cout << "Will skip: " << hex << addr << endl;
            tdata->is_tracing = false;
            tdata->pending_call = !strict_skip; //If its a call we will want to put the return value
            tdata->parameters.clear();
         }

        if((ins.flags & (IS_CALL | IS_BRANCH))) {  //If it is a call and a callmap is provided check if there is a policy telling to skip this call
            if(with_callmap) {
                auto it = libcall_policy_address_map.find(addr);
                if(it != libcall_policy_address_map.end()) {    //If there is a policy
                    const libcall_types::libcall_pol* pol = libcall_policy_address_map[addr];
                    if(pol->action() == common::tracing_action::SKIP) { //If we should skip it
                        tdata->current_policy = pol;
//                        tdata->pending_call = true;
                        tdata->is_tracing = false;
                        ADDRINT esp = get_register_value(ctx, LEVEL_BASE::REG_ESP);
//                        ADDRINT arg1;
//                        read_memory((void *)(esp+(sizeof(ADDRINT))), (char *)&arg1, sizeof(ADDRINT));
                        ADDRINT args[10];
                        for(int i=0; i < 10; i++) { //We are intentionally also putting the callsite as args
                            read_memory((void *)(esp+(i*sizeof(ADDRINT))), (char *)&args[i], sizeof(ADDRINT));
                        }
                        cout << "Match policy " << pol->name() << endl;
                        dispatch_call(threadid, ctx, addr, (char *)pol->name().c_str(), args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9]);
                    }
                }
                // TODO: else if we auto skip does not get in anyway ?
            }
            else if(auto_skip && is_jumping_library(target_addr)) {
                cout << hex << addr << " jump in library [do not trace]" << endl;
                tdata->current_policy = default_policy;
                tdata->is_tracing = false;
                tdata->pending_call = true;
            }
        }
    }

    if(!tdata->is_tracing && target_addr == tdata->resume_addr) { //If the the target address is the resumme_addr re-enable the tracing
        tdata->is_tracing = true;
        tdata->resume_addr = 0;
    }

    if(addr == stop_address) {      //If the current address is the stop address, stop the instrumentation
        tdata->is_tracing = false;
        if(!tdata->ins_infos.isempty) {
            add_concrete_infos_trace(tdata->ins_infos, true); //Add all infos already collected for the current instruction
            tdata->ins_infos.isempty = true;
        }
        cout << "Stop address reached" << endl;
		//Fini_instrumentation(0, nullptr);
        //PIN_ExitApplication(0);
    }

    check_inputs_operations(threadid, addr, ctx, configuration::input_t::AFTER, target_addr); //Afterall check if there are inputs for patching or collecting
}
// ================================================================= //



// =================== Routine callbacks ===================//

VOID callback_routine_before(THREADID threadid, CONTEXT * ctx, ADDRINT addr, CHAR * name,
                             ADDRINT arg0, ADDRINT arg1, ADDRINT arg2, ADDRINT arg3, ADDRINT arg4,
                             ADDRINT arg5, ADDRINT arg6, ADDRINT arg7, ADDRINT arg8, ADDRINT arg9) {

    thread_data_t* tdata = get_tls(threadid);

    if(tdata->is_tracing)
        cout << "Enter fun:" << name << endl;

    if(!tdata->is_tracing && tdata->pending_call && !with_callmap) { //If we are waiting for call infos
        if(tdata->parameters.empty()) {
            tdata->parameters.push_back(arg0); tdata->parameters.push_back(arg1); tdata->parameters.push_back(arg2); tdata->parameters.push_back(arg3);
            tdata->parameters.push_back(arg4); tdata->parameters.push_back(arg5); tdata->parameters.push_back(arg6); tdata->parameters.push_back(arg7);
            tdata->parameters.push_back(arg8); tdata->parameters.push_back(arg9);
        }
        bool domatch = false;
        for(auto const& i: libcall_policy_name_map) {       //Iterate the map of NAME->Policy to find if one match
            if(boost::algorithm::ends_with(name, i.first)) {//function name match
                cout << "Match found:" << i.first << endl;
                tdata->current_policy = i.second;           //Set the policy (the will be processed below)
                domatch = true;
            }
  
        }
        if(domatch || !boost::algorithm::starts_with(name, ".")) { //If there is match
                                                        //Or we are hit a real function (Pintool use to trigger the routine for calls in ".plt" before jump in the real fun)
            dispatch_call(threadid, ctx, addr, name, tdata->parameters[0], tdata->parameters[1], tdata->parameters[2],
                            tdata->parameters[3], tdata->parameters[4], tdata->parameters[5], tdata->parameters[6],
                            tdata->parameters[7], tdata->parameters[8], tdata->parameters[9]);
            tdata->pending_call = false;
            tdata->parameters.clear();
        }
    }
}

VOID callback_routine_after(THREADID threadid, CONTEXT * ctx, CHAR * name) {
    //Not used but never know..
    //cout << "Exit fun:" << tmp << endl;
}
//=============================================================//



//===================== Syscall callbacks =====================
VOID Enter_syscall(THREADID threadid, CONTEXT* ctxt, SYSCALL_STANDARD syscall_std, VOID *v) {
    thread_data_t* tdata = get_tls(threadid);

	ADDRINT num = PIN_GetSyscallNumber(ctxt, syscall_std);
	
    //cout << "Syscall:" << dec << num << endl;

	if (is_exit_syscall(num, syscall_std) && server_enabled) {
		cout << "Will get in the syscall exit.." << endl;
		flush_current_chunk();
		send_message("END", "EMPTY", false);
		Fini_instrumentation(0, nullptr);
	}

	if(tdata->is_tracing) {
        tdata->cur_sys_std = syscall_std;
		tdata->cur_sys_num = num;

		cout << "Syscall:" << num << endl;

        dispatch_syscall_entry(PIN_GetContextReg(ctxt, REG_INST_PTR),
                               PIN_GetSyscallNumber(ctxt, syscall_std),
                               syscall_std,
                               PIN_GetSyscallArgument(ctxt, syscall_std, 0),
                               PIN_GetSyscallArgument(ctxt, syscall_std, 1),
                               PIN_GetSyscallArgument(ctxt, syscall_std, 2),
                               PIN_GetSyscallArgument(ctxt, syscall_std, 3),
                               PIN_GetSyscallArgument(ctxt, syscall_std, 4),
                               PIN_GetSyscallArgument(ctxt, syscall_std, 5));
    }
}

VOID Exit_syscall(THREADID threadid, CONTEXT* ctxt, SYSCALL_STANDARD syscall_std, VOID *v) {
    thread_data_t* tdata = get_tls(threadid);

	if(tdata->is_tracing) {

		cout << "Syscall exit:" << tdata->cur_sys_num << endl;

        dispatch_syscall_ret(tdata->cur_sys_num,
                             tdata->cur_sys_std,
                             PIN_GetSyscallReturn(ctxt, syscall_std));
    }
}
//================================================================//


//====================== Threads callbacks =======================//
VOID ThreadStart(THREADID threadid, CONTEXT *ctxt, INT32 flags, VOID *v) {
    PIN_GetLock(&lock, threadid+1);
    num_threads ++;
    PIN_ReleaseLock(&lock);

    thread_data_t* tdata = new thread_data_t();

    if(function_to_instrument == "" && start_address == 0) //If no function nor address to start was provided instrument all
        tdata->is_tracing = true;
    else
        tdata->is_tracing = false;

    PIN_SetThreadData(tls_key, tdata, threadid);
    cout << "--> Thread n°" << threadid << "  [start]" << endl;

}

VOID ThreadFini(THREADID threadid, const CONTEXT *ctxt, INT32 code, VOID *v) {
	cout << "---> Thread n°" << threadid << " [stop(" << code << ")]" << endl;
	PIN_GetLock(&lock, threadid+1);
    num_threads --;
    PIN_ReleaseLock(&lock);
	thread_data_t* tdata = get_tls(threadid);
	if (!tdata->ins_infos.isempty) { //Flush last instruction if any
		cout << "Flush last instr of thread:" << threadid << endl;
        add_concrete_infos_trace(tdata->ins_infos, false);
	}
}
//=====================================================================//
