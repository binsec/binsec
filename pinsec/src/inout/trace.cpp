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

#include <fstream>
#include<iostream>

#include <pin.H>

#include "../types/types.hpp"
#include "../types/protobuf/trace.pb.h"
#include "printer.hpp"
#include "../server/server.hpp"
#include "../instrumentation.hpp"

using namespace trace_format;
using namespace common;

static chunk_t       current_chunk = chunk_t();  //Protobuf chunk that will hold the pending instructions
static std::ofstream output_stream;              //Trace output stream
static uint32_t      trace_length  = 0;          //Current trace length
int                  chunk_length  = 1000;       //Chunk length
uint32_t             maxlength     = 0;          //Max size of the trace
bool                 header_written = false;

void write_trace_header() {
  header_t trace_header = header_t();
  switch (sizeof(ADDRINT)) {
  case 4:
    trace_header.set_architecture(header_t::X86);
    trace_header.set_address_size(BIT32);
    break;
  case 8:
    trace_header.set_architecture(header_t::X86_64);
    trace_header.set_address_size(BIT64);
    break;
  }

  if (server_enabled) {
	  string data = trace_header.SerializeAsString();
	  cout << "Trace_header ready wait client" << endl;
	  send_message("TRACE_HEADER", data, true);
  }
  else {
	  int header_size = trace_header.ByteSize();
	  output_stream.write(reinterpret_cast<const char*>(&header_size), sizeof(int));
	  trace_header.SerializeToOstream(&output_stream);
  }
  cout << "Trace header written" << endl;
}


//Write the current chunk in the trace file
void flush_current_chunk() {
	if (!header_written) {
		write_trace_header();
		header_written = true;
	}

	int size = current_chunk.body_size();

	if (size != 0) {
		cout << "flush " << dec << size << " instructions" << endl;
		if (server_enabled) {
			string data = string();
			current_chunk.SerializeToString(&data);
			send_message("TRACE_CHUNK", data, false);
		}
		else {
			int chunk_size = current_chunk.ByteSize();       //Get the size of the chunk (because every chunk is preceded by it size)
			output_stream.write(reinterpret_cast<const char*>(&chunk_size), sizeof(int));
			current_chunk.SerializeToOstream(&output_stream);//Directly serialize the chunk to the output stream
		}

		current_chunk.Clear();                           //Clear the chunk
	}
}



void set_register_value_t(register_value_t* addr, UINT size, ADDRINT value) {
    switch (size) {
    case 1:
      addr->set_typeid_(BIT8);
      addr->set_value_8(value);
      break;

    case 2:
        addr->set_typeid_(BIT16);
        addr->set_value_16(value);
      break;

    case 4:
        addr->set_typeid_(BIT32);
        addr->set_value_32(value);
      break;

    case 8:
        addr->set_typeid_(BIT64);
        addr->set_value_64(value);
      break;

    default:// TODO: change ADDRINT value, by a void* or char*
      cout << "[register_value_t value not set, size:" << size << "]" << endl;
      addr->set_typeid_(INVALID_SIZE);
      addr->set_value_64(value); //TODO: Really needed ?
      break;
    }
}

ADDRINT register_value_t_to_addrint(const register_value_t addr) {
    switch (addr.typeid_()) {
      case BIT8:
        return (ADDRINT)addr.value_8();
      case BIT16:
        return (ADDRINT)addr.value_16();
      case BIT32:
        return (ADDRINT)addr.value_32();
      case BIT64:
        return (ADDRINT)addr.value_64();
      default:
        std::runtime_error("Cannot convert register_value_t to ADDRINT");
    }
    return 0; //Unreachable
}

void finalize_trace_file();

//Receive some concrete informations and put them in the current chunk
void add_concrete_infos_trace(instruction_infos_t infos, bool exit_on_maxlength) {

	if (trace_length >= maxlength && maxlength != 0) {
		cout << "trace length already exceeded" << endl;
		return;
	}

    //Create a body of type instruction
    body_t* body = current_chunk.add_body();
    body->set_typeid_(body_t::INSTRUCTION);
    instruction_t* instr = body->mutable_instruction();
    instr->set_thread_id(infos.threadid);
    instr->set_address(infos.address);
    instr->set_opcode(infos.opcode);
    //----------

    //Fill it with all the concrete infos
    for(ins_con_info_t* i: infos.concinfos) {
        instr->mutable_concrete_infos()->AddAllocated(i);
    }

    //Flush the chunk if we reach the max size
    if(current_chunk.body_size() >= chunk_length) {
        flush_current_chunk();
    }

    trace_length ++;
    if(trace_length >= maxlength && maxlength != 0) {
        cout << "Max trace length exceeded, stop." << endl;
        //current_chunk.clear_body();
		if (server_enabled) {
			Fini_instrumentation(0, nullptr);
		}
        if(exit_on_maxlength) { //False when function called by ThreadFini where calling exit is forbidden
            PIN_ExitApplication(0);
            cout << "After exit application called !" << endl;
        }
     }
}

void add_metadata_wave(int wave_number) {
    //Create a body of type metadata
    body_t* body = current_chunk.add_body();
    body->set_typeid_(body_t::METADATA);
    metadata_t* meta = body->mutable_metadata();
    meta->set_typeid_(metadata_t::WAVE_TYPE);
    meta->set_wave_metadata(wave_number);
    //----------

    //Flush the chunk if we reach the max size
    if(current_chunk.body_size() >= chunk_length) {
        flush_current_chunk();
    }
}

//Initialize all the field and the output stream
void initialize_trace_file(const std::string filename, int chunk_size, int max_length) {
  try {
    chunk_length = chunk_size;
    maxlength = max_length;
    output_stream.open(filename.c_str(), std::ofstream::out | std::ofstream::binary | std::ofstream::trunc);
    //write_trace_header();
    current_chunk.Clear();
  }
  catch (const std::exception& expt) {
    cout << expt.what() << endl;
    PIN_ExitProcess(1);
  }
}


void finalize_trace_file() {
  try {
    flush_current_chunk();
    cout << "trace length: " << trace_length << " instructions" << endl;
    output_stream.close();

    google::protobuf::ShutdownProtobufLibrary();// free internal objects of protobuf
  }
  catch (const std::exception& expt) {
    cout << expt.what() << endl;
    PIN_ExitProcess(1);
  }
}
