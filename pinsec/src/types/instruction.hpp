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

/* CEA + IMAG
*/

#ifndef INSTRUCTION_H
#define INSTRUCTION_H

#include <pin.H>
extern "C" {
#include "xed-interface.h"
}

#include <string>
#include <vector>

//constexpr Instruction def = {

//};

#define IS_CALL 0x1
#define IS_BRANCH 0x2
#define IS_RET 0x4
#define IS_MEMORY_READ 0x8
#define IS_MEMORY_WRITE 0x10

struct Instruction {
    Instruction() = default;
    Instruction(const INS& ins);

    //ADDRINT       address;
    ADDRINT       next_address;
    unsigned char opcode_size;
    //std::string opcode;

    //Flags
    unsigned char flags;

//    //bool has_fall_through;
//    bool is_call;
//    bool is_branch;
//    //bool is_indirect;
//    //bool is_syscall;
//    bool is_ret;
//    bool is_memory_read; //Keep even if unused
//    bool is_memory_write;
//    //bool has_memory_read_2;

    std::vector<REG> src_registers;
    std::vector<REG> dst_registers;
};


#endif
















