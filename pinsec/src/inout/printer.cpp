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

#include "../types/protobuf/trace.pb.h"

using namespace common;
using namespace trace_format;
using namespace std;

void print_register_value_t(common::register_value_t addr) {
    switch(addr.typeid_()) {
        case BIT8:
            cout << hex << addr.value_8() << "(8)";
            break;
        case BIT16:
            cout << hex << addr.value_16() << "(16)";
            break;
        case BIT32:
            cout << hex << addr.value_32() << "(32)";
            break;
        case BIT64:
            cout << hex << addr.value_64() << "(64)";
            break;
        case BIT80:
            cout << addr.value_80() << "(80)";
            break;
        case BIT128:
            cout << addr.value_128() << "(128)";
            break;
        case BIT256:
            cout << addr.value_256() << "(256)";
            break;
        default:
            cout << "DEFAULT";
    }
}

void print_register_t(common::register_t reg, bool isread) {
    cout << " " << reg.name() << (isread ? "=" : "<-");
    print_register_value_t(reg.value());
}

void print_memory_t(memory_t mem, bool isread) {
    cout << " [" << hex << mem.addr() << "]" << (isread ? ":" : "<-") << mem.value();
}
