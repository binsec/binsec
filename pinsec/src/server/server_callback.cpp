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

#include "pin.H"
#include "../types/types.hpp"
#include "../instrumentation.hpp"

using namespace std;

bool dispatch_messages(CONTEXT* ctx, THREADID thread_id, string cmd, string message) {
	cout << "############## Received [" << cmd.c_str() << "]:" << message.c_str()<< endl;
	if (cmd == "PATCH_ZF") {
		ADDRINT eflags = PIN_GetContextReg(ctx, REG_EFLAGS);
		int val = message == "1" ? 64 : 0;
		cout << "Will patch ZF:" << dec << (eflags & 64) << " with:" << (64 & val) << endl;
		PIN_SetContextReg(ctx, REG_EFLAGS, eflags | val);
		PIN_ExecuteAt(ctx);
	}
	else if (cmd == "RESUME") {
		thread_data_t* tdata = get_tls(thread_id);
		tdata->suspended = false;
		cout << "Will resume" << endl;
		return false;
	}

	return true;
}