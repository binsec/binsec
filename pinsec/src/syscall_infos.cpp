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

#if defined(TARGET_MAC)
#include <sys/syscall.h>
#elif !defined(TARGET_WINDOWS)
#include <syscall.h>
#endif

VOID dispatch_syscall_entry_windows(ADDRINT ip, ADDRINT num, SYSCALL_STANDARD sys_std, ADDRINT arg0, ADDRINT arg1, ADDRINT arg2, ADDRINT arg3, ADDRINT arg4, ADDRINT arg5) {
	cout << "Enter syscall:" << dec << num << endl;
}


VOID dispatch_syscall_entry_linux(ADDRINT ip, ADDRINT num, SYSCALL_STANDARD sys_std, ADDRINT arg0, ADDRINT arg1, ADDRINT arg2, ADDRINT arg3, ADDRINT arg4, ADDRINT arg5) {
#if defined(TARGET_LINUX)
	switch(num) {
        case SYS_restart_syscall: { cout << "SYS_restart_syscall" << endl; break; }
        case SYS_exit: { cout << "SYS_exit" << endl; break; }
        case SYS_fork: { cout << "SYS_fork" << endl; break; }
        case SYS_read: { cout << "SYS_read" << endl; break; }
        case SYS_write: { cout << "SYS_write" << endl; break; }
        case SYS_open: { cout << "SYS_open" << endl; break; }
        case SYS_close: { cout << "SYS_close" << endl; break; }
        case SYS_waitpid: { cout << "SYS_waitpid" << endl; break; }
        case SYS_creat: { cout << "SYS_creat" << endl; break; }
        case SYS_link: { cout << "SYS_link" << endl; break; }
        case SYS_unlink: { cout << "SYS_unlink" << endl; break; }
        default:
            cout << "Unknown syscall:" << num << endl;
    }
#endif
}


VOID dispatch_syscall_ret_linux(ADDRINT num, SYSCALL_STANDARD sys_std, ADDRINT ret) {
#if defined(TARGET_LINUX)
	switch(num) {
        case SYS_restart_syscall: { cout << "ret SYS_restart_syscall" << endl; break; }
        case SYS_exit: { cout << "ret SYS_exit" << endl; break; }
        case SYS_fork: { cout << "ret SYS_fork" << endl; break; }
        case SYS_read: { cout << "ret SYS_read" << endl; break; }
        case SYS_write: { cout << "ret SYS_write" << endl; break; }
        case SYS_open: { cout << "ret SYS_open" << endl; break; }
        case SYS_close: { cout << "ret SYS_close" << endl; break; }
        case SYS_waitpid: { cout << "ret SYS_waitpid" << endl; break; }
        case SYS_creat: { cout << "ret SYS_creat" << endl; break; }
        case SYS_link: { cout << "ret SYS_link" << endl; break; }
        case SYS_unlink: { cout << "ret SYS_unlink" << endl; break; }
        default:
            cout << "Unknown syscall ret:" << num << endl;
    }
#endif
}

VOID dispatch_syscall_ret_windows(ADDRINT num, SYSCALL_STANDARD sys_std, ADDRINT ret) {

}


bool is_exit_syscall(ADDRINT value, SYSCALL_STANDARD sys_std){
	ADDRINT exit_value=0;
	switch (sys_std) {
	case SYSCALL_STANDARD_IA32_LINUX:
	case SYSCALL_STANDARD_IA32E_LINUX:
		exit_value=1;
		break;
	case SYSCALL_STANDARD_IA32_MAC:
	case SYSCALL_STANDARD_IA32E_MAC:
		cout << "Unsupported mac" << endl;
		break;
	case SYSCALL_STANDARD_IA32E_WINDOWS_FAST:
	case SYSCALL_STANDARD_IA32_WINDOWS_FAST:
	case SYSCALL_STANDARD_IA32_WINDOWS_ALT:
	case SYSCALL_STANDARD_WINDOWS_INT:
		exit_value = 371;
		break;
	case SYSCALL_STANDARD_WOW64:
		exit_value = 41; //True up to Windows7
		break;
	case SYSCALL_STANDARD_INVALID:
		cout << "Invalid syscall" << endl;
	}
	return value == exit_value;
}



// =========================== MAIN DISPATCHERS =========================== //
VOID dispatch_syscall_entry(ADDRINT ip, ADDRINT num, SYSCALL_STANDARD sys_std, ADDRINT arg0, ADDRINT arg1, ADDRINT arg2, ADDRINT arg3, ADDRINT arg4, ADDRINT arg5) {
    switch(sys_std) {
        case SYSCALL_STANDARD_IA32_LINUX:
        case SYSCALL_STANDARD_IA32E_LINUX:
            dispatch_syscall_entry_linux(ip, num, sys_std, arg0, arg1, arg2, arg3, arg4, arg5);
            break;
        case SYSCALL_STANDARD_IA32_MAC:
        case SYSCALL_STANDARD_IA32E_MAC:
            cout << "Unsupported mac" << endl;
            break;
        case SYSCALL_STANDARD_IA32E_WINDOWS_FAST:
        case SYSCALL_STANDARD_IA32_WINDOWS_FAST:
        case SYSCALL_STANDARD_IA32_WINDOWS_ALT:
        case SYSCALL_STANDARD_WINDOWS_INT:
        case SYSCALL_STANDARD_WOW64:
            dispatch_syscall_entry_windows(ip, num, sys_std, arg0, arg1, arg2, arg3, arg4, arg5);
            break;
        case SYSCALL_STANDARD_INVALID:
            cout << "Invalid syscall" << endl;
    }
}

VOID dispatch_syscall_ret(ADDRINT num, SYSCALL_STANDARD sys_std, ADDRINT ret) {
    switch(sys_std) {
        case SYSCALL_STANDARD_IA32_LINUX:
        case SYSCALL_STANDARD_IA32E_LINUX:
            dispatch_syscall_ret_linux(num, sys_std, ret);
            break;
        case SYSCALL_STANDARD_IA32_MAC:
        case SYSCALL_STANDARD_IA32E_MAC:
            cout << "Unsupported mac" << endl;
            break;
        case SYSCALL_STANDARD_IA32E_WINDOWS_FAST:
        case SYSCALL_STANDARD_IA32_WINDOWS_FAST:
        case SYSCALL_STANDARD_IA32_WINDOWS_ALT:
        case SYSCALL_STANDARD_WINDOWS_INT:
        case SYSCALL_STANDARD_WOW64:
            dispatch_syscall_ret_windows(num, sys_std, ret);
            break;
        case SYSCALL_STANDARD_INVALID:
            cout << "Invalid syscall" << endl;
    }
}
// =========================================================================== //
