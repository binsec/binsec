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
#include<iomanip>
#include<vector>
#include "pin.H"
#include "types/types.hpp"
#include "types/protobuf/trace.pb.h"
#include "instrumentation.hpp"
#include "inout/trace.hpp"
#include "inout/printer.hpp"
#include "utils.hpp"

using namespace common;
using namespace trace_format;

//Patch the given register if the given value
void set_register_value(CONTEXT*  ctx, REG reg, register_value_t val) {
    ADDRINT patchvalue=0;
    switch(val.typeid_()) {
        case INVALID_SIZE:
            cout << "Invalid address cannot patch" << endl;
            break;
        case BIT8:
            patchvalue = val.value_8();
            break;
        case BIT16:
            patchvalue = val.value_16();
            break;
        case BIT32:
            patchvalue = val.value_32();
            break;
        case BIT64:
            patchvalue = val.value_64();
            break;
        default:
            cout << "Patching register greater than 64 bits not yet supported" << endl;
            return;
    }
    PIN_SetContextReg(ctx, reg, patchvalue);

    /*
    if(REG_Size(reg) >= val.length()) {  //both in bytes so ok
        cout << REG_StringShort(reg) << " patchingnot supported (larger than an ADDRINT)" << endl;
        //TODO:See PIN_SetContextRegval and REGVAL type
    }
    else {
        PIN_SetContextReg(ctx, reg, *reinterpret_cast<ADDRINT *>(val.c_str()));  //TODO: Check if ok !
    }
    */
}

//Function that return the given register value
ADDRINT get_register_value(CONTEXT* ctx, REG reg) {
    unsigned int regsize = REG_Size(reg);
    if(regsize > sizeof(ADDRINT)) {
        cout << REG_StringShort(reg) << " not supported larger than an ADDRINT" << endl;
        return ADDRINT{0};
    }
    else {
        if(REG_is_fpst(reg) || REG_is_xmm_ymm_zmm(reg)) {
            cout << REG_StringShort(reg) << " ignored";
            return ADDRINT{0};
        }
        else {
            if(regsize == sizeof(ADDRINT))
                return PIN_GetContextReg(ctx, reg);
            else {
                ADDRINT val;
                PIN_GetContextRegval(ctx, reg, reinterpret_cast<UINT8*>(&val));
                return val;
            }
        }
    }
}


//Function that retrieve a register value and put it in concrete infos
void record_one_register(THREADID threadid, REG reg, CONTEXT * ctx, bool isread) {
    thread_data_t* tdata = get_tls(threadid);

	if (REG_is_any_x87(reg) || REG_is_xmm_ymm_zmm(reg)) {
		cout << "Register: " << REG_StringShort(reg) << " ignored" << endl;
	}
	else {
		unsigned int size = REG_Size(reg);
		ADDRINT value = get_register_value(ctx, reg);

		ins_con_info_t* info = new ins_con_info_t();
		info->set_typeid_(isread ? ins_con_info_t::REGREAD : ins_con_info_t::REGWRITE);
        common::register_t* concreg = isread ? info->mutable_read_register(): info->mutable_write_register();
		concreg->set_name(REG_StringShort(reg));
        set_register_value_t(concreg->mutable_value(), size, value);
		tdata->ins_infos.concinfos.push_back(info);
	}
}


//Retrieve multiples registers and put them in concrete infos
void record_registers(THREADID threadid, std::vector<REG> regs, CONTEXT * ctx, bool isread) {
    for(REG reg: regs) {
        record_one_register(threadid, reg, ctx, isread);
    }
}

//Read a string in memory (terminated by '\0'
string read_memory_string(VOID* ea) {
    string s = "";

    //assert((ADDRINT)ea > 50 && "You're very likely reading improper memory, better to stop");

    char c = '\0';
    for(int i=0; i < 1000; i++) { //string limited to 1000 char
        PIN_SafeCopy(&c, (void*)(((ADDRINT)ea)+i), 1);
        if(c == '\0') {
            break;
        }
        s.push_back(c);
    }
    return s;
}

//Read a string in memory (terminated by '\0'
string read_n_memory_string(VOID* ea,int sz) {
    string s = "";
    char c = '\0';
    for(int i=0; i < sz; i++) { //string limited to 1000 char
        PIN_SafeCopy(&c, (void*)(((ADDRINT)ea)+i), 1);
        s.push_back(c);
    }
    return s;
}

//Read the memory at the given address for the given size and put it in dst
void read_memory(VOID* ea, char* dst, UINT32 size) {
    PIN_SafeCopy(dst, ea, size);
    //for (UINT32 i = 0; i < size; i++) {
    //    dst[i] = static_cast<UINT32>(static_cast<UINT8*>(ea)[i]);
    //}
}

//Retrieve a memory value and put it in concrete infos
void record_memory(THREADID threadid, VOID* ea, UINT32 size, bool isread) {
    thread_data_t* tdata = get_tls(threadid);
    ins_con_info_t* info = new ins_con_info_t();
    info->set_typeid_(isread ? ins_con_info_t::MEMLOAD : ins_con_info_t::MEMSTORE);
    memory_t* concmem = isread ? info->mutable_load_memory() : info->mutable_store_memory();
    concmem->set_addr((ADDRINT)ea);

    char buff[256] = "";
    if(size > 256) {
      std::runtime_error("Cannot record more than 128 bytes (well yes but not now..) ");
    }
    PIN_SafeCopy(&buff, ea, size);
    concmem->set_value(string(buff,size));
    tdata->ins_infos.concinfos.push_back(info);
}


//Patch the memory with the given value
void patch_memory(VOID* ea, UINT32 size, char* value) {
    EXCEPTION_INFO excp_info;
    size_t size_copied = PIN_SafeCopyEx(ea, value, size, &excp_info);
    if (size_copied != size) {
      cout << "error: " << PIN_ExceptionToString(&excp_info);
    }
    else {
      cout << "[after patching]:" << hex << (ADDRINT)ea;
    }

}

void record_wave_number(THREADID threadid, ADDRINT addr, int size) {
    if(trace_waves) {
        int wave = 0;
        for (unsigned int x=addr; x < addr+size; x++) {
            if (wave_mapping.find(x) != wave_mapping.end()) {
                wave = wave >= wave_mapping[x] ? wave : wave_mapping[x];
            }
        }
        thread_data_t* tdata = get_tls(threadid);
        ins_con_info_t* info = new ins_con_info_t();
        info->set_typeid_(ins_con_info_t::WAVE);
        info->set_wave(wave);
        tdata->ins_infos.concinfos.push_back(info);
    }
}

void record_argv(THREADID threadid, CONTEXT * ctx) {

    	thread_data_t* tdata = get_tls(threadid);
	size_t i=1;
	REG reg=get_reg_from_name("esp");

	ADDRINT addr_argc = get_register_value(ctx, reg)+4;
	ADDRINT addr_argv = get_register_value(ctx, reg)+8;

	string argc_as_string = read_n_memory_string((void*)addr_argc,4);
  	ADDRINT argc = (argc_as_string[0]&0xff) + ((argc_as_string[1]&0xff)<<8) + ((argc_as_string[2]&0xff)<<16) + ((argc_as_string[3]&0xff)<<24);

	ins_con_info_t* info_argc = new ins_con_info_t();
	info_argc->set_typeid_(ins_con_info_t::MEMLOAD);
	memory_t* concmem_argc = info_argc->mutable_load_memory();
	concmem_argc->set_addr((ADDRINT)addr_argc);
	concmem_argc->set_value(argc_as_string);
	tdata->ins_infos.concinfos.push_back(info_argc);
	
	string argv_as_string = read_n_memory_string((void*)addr_argv,4);
  	ADDRINT argv = (argv_as_string[0]&0xff) + ((argv_as_string[1]&0xff)<<8) + ((argv_as_string[2]&0xff)<<16) + ((argv_as_string[3]&0xff)<<24);

	ins_con_info_t* info_argv = new ins_con_info_t();
	info_argv->set_typeid_(ins_con_info_t::MEMLOAD);
	memory_t* concmem_argv = info_argv->mutable_load_memory();
	concmem_argv->set_addr((ADDRINT)addr_argv);
	concmem_argv->set_value(argv_as_string);
	tdata->ins_infos.concinfos.push_back(info_argv);

	for(i=1;i<argc;i++){ // start at 1; we ignore argv[0]
	    ADDRINT addr_argvv = argv+i*4;
	    string argvv_as_string = read_n_memory_string((void*)addr_argvv,4);
	
            ins_con_info_t* info_argvv = new ins_con_info_t();
       	    info_argvv->set_typeid_(ins_con_info_t::MEMLOAD);
            memory_t* concmem_argvv = info_argvv->mutable_load_memory();
      	    concmem_argvv->set_addr((ADDRINT)addr_argvv);
      	    concmem_argvv->set_value(argvv_as_string);
      	    tdata->ins_infos.concinfos.push_back(info_argvv);

            ADDRINT argvv = (argvv_as_string[0]&0xff) + ((argvv_as_string[1]&0xff)<<8) + ((argvv_as_string[2]&0xff)<<16) + ((argvv_as_string[3]&0xff)<<24);
	    ins_con_info_t* info = new ins_con_info_t();
	    info->set_typeid_(ins_con_info_t::MEMLOAD);
	    memory_t* concmem = info->mutable_load_memory();
	    concmem->set_addr((ADDRINT)argvv);
            string buff=read_memory_string((void*)argvv);
	    concmem->set_value(buff);
	    tdata->ins_infos.concinfos.push_back(info);
	}

}
