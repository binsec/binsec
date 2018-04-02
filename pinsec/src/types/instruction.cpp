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

/* CEA + IMAG */
#include "instruction.hpp"
#include "../concrete_actions.hpp"
#include "types.hpp"

string get_englobing_fun_name(ADDRINT addr) {
  PIN_LockClient();
  RTN routine = RTN_FindByAddress(addr);
  PIN_UnlockClient();

  if (RTN_Valid(routine)) {
    string routine_mangled_name = PIN_UndecorateSymbolName(RTN_Name(routine), UNDECORATION_NAME_ONLY);
    return routine_mangled_name;
  }
  else return "";
}

Instruction::Instruction(const INS& ins) {
  //this->address     = INS_Address(ins);
  this->next_address= INS_NextAddress(ins);
  this->opcode_size = INS_Size(ins);

//  char opc[15]; //Do not put opcode_size because MSVC does not support variable size array on stack
//  read_memory((void*)address, opc, opcode_size);
//  this->opcode      = string(opc, opcode_size);

  // including image, routine
  //auto img                = IMG_FindByAddress(this->address);
  //this->including_image   = IMG_Valid(img) ? IMG_Name(img) : "";
  //this->including_routine_name = get_englobing_fun_name(this->address);

  //this->has_fall_through = INS_HasFallThrough(ins);
  unsigned char flg = 0;
  flg = flg | (INS_IsCall(ins) ? IS_CALL : 0);//  this->is_call    = INS_IsCall(ins);
  flg = flg | (INS_IsBranch(ins) ? IS_BRANCH : 0); //this->is_branch  = INS_IsBranch(ins);
  //this->is_indirect= INS_IsDirectBranchOrCall(ins);
  flg = flg | (INS_IsRet(ins) ? IS_RET : 0); //this->is_ret     = INS_IsRet(ins);
  //this->is_syscall = INS_IsSyscall(ins);
  flg = flg | (INS_IsMemoryRead(ins) ? IS_MEMORY_READ : 0); //this->is_memory_read    = INS_IsMemoryRead(ins);
  flg = flg | (INS_IsMemoryWrite(ins) ? IS_MEMORY_WRITE : 0);// this->is_memory_write   = INS_IsMemoryWrite(ins);
  //this->has_memory_read_2 = INS_HasMemoryRead2(ins);
  this->flags = flg;


  // read registers
  UINT32 read_reg_number = INS_MaxNumRRegs(ins);
  for (UINT32 reg_id = 0; reg_id < read_reg_number; ++reg_id) {
    this->src_registers.push_back(INS_RegR(ins, reg_id));
  }

  // written registers
  UINT32 written_reg_number = INS_MaxNumWRegs(ins);
  for (UINT32 reg_id = 0; reg_id < written_reg_number; ++reg_id) {
    this->dst_registers.push_back(INS_RegW(ins, reg_id));
  }

}
