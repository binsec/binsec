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

VOID callback_routine_before(THREADID threadid, CONTEXT * ctx, ADDRINT addr, CHAR * name,
                             ADDRINT arg0, ADDRINT arg1, ADDRINT arg2, ADDRINT arg3, ADDRINT arg4,
                             ADDRINT arg5, ADDRINT arg6, ADDRINT arg7, ADDRINT arg8, ADDRINT arg9);
VOID callback_routine_after(THREADID threadid, CONTEXT * ctx, char * name);

VOID callback_instruction_memory(THREADID threadid, CONTEXT* ctx, ADDRINT* mem_addr, UINT32 size, bool isread);
VOID callback_instruction_before(ADDRINT addr, CONTEXT * ctx, THREADID threadid);
VOID callback_instruction_after(ADDRINT addr, CONTEXT * ctx, THREADID threadid, ADDRINT ret, bool is_branch);

VOID Enter_syscall(THREADID thread_id, CONTEXT* ctxt, SYSCALL_STANDARD syscall_std, VOID *v);
VOID Exit_syscall(THREADID thread_id, CONTEXT* ctxt, SYSCALL_STANDARD syscall_std, VOID *v);

VOID ThreadStart(THREADID threadid, CONTEXT *ctxt, INT32 flags, VOID *v);
VOID ThreadFini(THREADID threadid, const CONTEXT *ctxt, INT32 code, VOID *v);
