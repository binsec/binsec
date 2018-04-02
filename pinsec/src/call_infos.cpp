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
#include "pin.H"
#include "types/protobuf/libcall.pb.h"
#include "instrumentation.hpp"
#include "concrete_actions.hpp"
#include "inout/trace.hpp"
#include "utils.hpp"


#include <iomanip>

using namespace libcall_types;
using namespace trace_format;

#define BUFFSIZE 10000

char buff[BUFFSIZE];

std::map<string, int> lookup_map = std::map<string, int>();

void initialize_lookup_map() {
    lookup_map["printf"] = PRINTF;
    lookup_map["atoi"] = ATOI;
    lookup_map["_IO_printf"] = PRINTF;
    lookup_map["malloc"] = MALLOC;
    lookup_map["__libc_malloc"] = MALLOC;
    lookup_map["_IO_printf"] = PRINTF;
    lookup_map["GetModuleHandleA"] = GETMODULEHANDLE;
    lookup_map["GetProcAddress"] = GETPROCADDRESS;
    lookup_map["__getmainargs"] = GETMAINARGS;
    lookup_map["gethostname"] = GETHOSTNAME;
    lookup_map["free"] = FREE;
    lookup_map["__libc_free"] = FREE;
    lookup_map["memcpy"] = MEMCPY;
    lookup_map["memset"] = MEMSET;
    lookup_map["fgetc"] = FGETC;
    lookup_map["_IO_getc"] = FGETC;
}


// =============================== Library dispatchers ================================ //

// --------- ctype_b_loc ----------- //
void dispatch_call_ctype_b_loc()
{
  return ;
}

void dispatch_ret_ctype_b_loc(ctype_b_loc_t* container, ADDRINT ret) {
  string addr_as_string = read_n_memory_string((void*)ret,4);
  ADDRINT addr = (addr_as_string[0]&0xff) + ((addr_as_string[1]&0xff)<<8) + ((addr_as_string[2]&0xff)<<16) + ((addr_as_string[3]&0xff)<<24);
  cout << " cbyte " << std::hex << addr << "\n";
  string table = read_n_memory_string((void*) (addr-0x100),0x200);
  container->mutable_table()->set_value(table);
  container->mutable_table()->set_addr(addr);
  container->set_ret(ret);
  return ;
}

// --------- Fscanf ----------- //

// return the number of %3d  //
size_t parse_scanf_format(string format) //TODO: modify the function
{
//  return format.find("%d");
  int i = 0;
  size_t off;
  off = format.find("%3d");
  while(off != std::string::npos){
    i++;
    off = format.find("%3d",off+3); // +3 : size of %3d
    if(i>10) break;
  }

  return i;
}

void dispatch_call_fscanf(fscanf_t* container, ADDRINT arg1, ADDRINT arg2, ADDRINT arg3, ADDRINT arg4, ADDRINT arg5, ADDRINT arg6, ADDRINT arg7, ADDRINT arg8, ADDRINT arg9)
{
  size_t i;
  fscanf_elem_t *elem;
  container->set_stream(arg1);
  string format = read_memory_string((void*)arg2);
  cout << std::hex << arg2 << "\n";
  //cout << "format " << format << "\n";
  size_t number_int = parse_scanf_format(format);
  cout << "Number int " << number_int << "\n" ;
  for(i=0;i<number_int;i++) {
    //elem = new fscanf_elem_t();
    elem = container -> add_elems();
    elem->set_type(fscanf_enum::d);
    switch(i){
        case 0 : elem->set_addr(arg3); break;
        case 1 : elem->set_addr(arg4); break;
        case 2 : elem->set_addr(arg5); break;
        case 3 : elem->set_addr(arg6); break;
        case 4 : elem->set_addr(arg7); break;
        case 5 : elem->set_addr(arg8); break;
        case 6 : elem->set_addr(arg9); break;
        default: break;
    }
  }
  return ;
}

void dispatch_ret_fscanf(fscanf_t* container, ADDRINT ret) 
{
  container->set_ret(ret);
  return ;
}


// --------- Free ----------- //
void dispatch_call_free(free_t* container, ADDRINT arg1)
{
  container->set_ptr(arg1);
  return ;
}

void dispatch_ret_free(free_t* container, ADDRINT ret) 
{
  return ;
}



// --------- Exit ----------- //
void dispatch_call_exit()
{
  return ;
}

void dispatch_ret_exit(exit_t* container) 
{
  return ;
}

// --------- Onlyret ----------- //
void dispatch_call_generic()
{
  return;
}

void dispatch_ret_generic(generic_t* container, ADDRINT ret) {
  cout << "\nRet : " << ret << " \n";
  container->set_ret(ret);
}

//------------- read -------------//
void dispatch_call_read(read_t* container, ADDRINT arg1, ADDRINT arg2, ADDRINT arg3) {
  cout << "\nRead " << arg1 << " " << arg2 << " "  << arg3  <<"\n";
  container->set_fd(arg1);
  container->mutable_buf()->set_addr(arg2);
  container->set_count(arg3);
}

void dispatch_ret_read(read_t* container, ADDRINT ret) {
  string buf = read_n_memory_string((void*)container->mutable_buf()->addr(),ret); 
  container->mutable_buf()->set_value(buf);
  container->set_ret(ret);
}

//------------- open -------------//
void dispatch_call_open(open_t* container, ADDRINT arg1, ADDRINT arg2, ADDRINT arg3) {
  string txt = read_memory_string((void*)arg1);
  cout << "\nOpen " << txt << " " << arg2 << " "  << arg3 ;
  container->mutable_pathname()->set_addr(arg1);
  container->mutable_pathname()->set_value(txt);
  container->set_flags(arg2);
  container->set_mode(arg3);
}

void dispatch_ret_open(open_t* container, ADDRINT ret) {
  container->set_ret(ret);
  cout << "ret " << ret << "\n" ;
}

//------------ strchr ----------//
void dispatch_call_strchr(strchr_t* container, ADDRINT arg1, ADDRINT arg2) {
  string txt = read_memory_string((void*)arg1);
  container->set_s(arg1);
  container->set_c(arg2);
  container->set_size_max(txt.length());
  cout << " Strchr " << arg1 << " " << arg2 << " size : " << txt.length() << "\n";
}

void dispatch_ret_strchr(strchr_t* container, ADDRINT ret) {
  container->set_ret(ret);
}

//------------ strcmp ----------//
void dispatch_call_strcmp(strcmp_t* container, ADDRINT arg1, ADDRINT arg2) {
  string txt1 = read_memory_string((void*)arg1);
  string txt2 = read_memory_string((void*)arg2);
  container->set_s1(arg1);
  container->set_s2(arg2);
  container->set_size_max_s1(txt1.length());
  container->set_size_max_s2(txt2.length());
  cout << "\nStrcmp " << arg1 << " size " << txt1.length() << ",  " << arg2 << " size " << txt2.length() <<"\n";
}

void dispatch_ret_strcmp(strcmp_t* container, ADDRINT ret) {
  container->set_ret(ret);
}

//------------ strncmp ----------//
void dispatch_call_strncmp(strncmp_t* container, ADDRINT arg1, ADDRINT arg2, ADDRINT arg3) {
  container->set_s1(arg1);
  container->set_s2(arg2);
  container->set_n(arg3);
  cout << "\nStrncmp \n";
}

void dispatch_ret_strncmp(strncmp_t* container, ADDRINT ret) {
  container->set_ret(ret);
}

//------------ memcmp ----------//
void dispatch_call_memcmp(memcmp_t* container, ADDRINT arg1, ADDRINT arg2, ADDRINT arg3) {
  container->set_s1(arg1);
  container->set_s2(arg2);
  container->set_n(arg3);
  cout << "\nMemcmp\n";
}

void dispatch_ret_memcmp(memcmp_t* container, ADDRINT ret) {
  container->set_ret(ret);
}


//------------ Strncpy ----------//
void dispatch_call_strncpy(strncpy_t* container, ADDRINT arg1, ADDRINT arg2, ADDRINT arg3) {
  container->set_dst(arg1);
  container->set_src(arg2);
  container->set_n(arg3);
  cout << "\n----------------> Strncpy " << arg1 << " " << arg2 << " size : " << arg3 << "\n";
}

void dispatch_ret_strncpy(strncpy_t* container, ADDRINT ret) {
  container->set_ret(ret);
}

//------------ Fstat ----------//
void dispatch_call_fstat(fstat_t* container, ADDRINT arg1, ADDRINT arg2) {
  container->set_fd(arg1);
  container->mutable_buf()->set_addr(arg2);
}

void dispatch_ret_fstat(fstat_t* container, ADDRINT ret) {
  string buf = read_n_memory_string((void*)container->mutable_buf()->addr(),88); /*88 = sizeof(struct stat) in int32 */ 
  container->mutable_buf()->set_value(buf);
  container->set_ret(ret);
}

//------------ Fxstat64 ----------//
void dispatch_call_fxstat64(fxstat64_t* container, ADDRINT arg1, ADDRINT arg2, ADDRINT arg3) {
   container->set_vers(arg1);
   container->set_fd(arg2);
   container->mutable_buf()->set_addr(arg3);
}

void dispatch_ret_fxstat64(fxstat64_t* container, ADDRINT ret) {
   string buf = read_n_memory_string((void*)container->mutable_buf()->addr(),88); /*88 = sizeof(struct stat) in int32 */ 
   container->mutable_buf()->set_value(buf);
   container->set_ret(ret);
}

//------------ Realloc ----------//
void dispatch_call_realloc(realloc_t* container, ADDRINT arg1, ADDRINT arg2) {
  container->set_ptr(arg1);
  container->set_size(arg2);
}

void dispatch_ret_realloc(realloc_t* container, ADDRINT ret) {
  container->set_ret(ret);
}

//------------ MMAP ----------//
void dispatch_call_mmap(mmap_t* container, ADDRINT arg1, ADDRINT arg2,ADDRINT arg3, ADDRINT arg4,  ADDRINT arg5, ADDRINT arg6) {
  container->set_addr(arg1);
  container->set_length(arg2);
  container->set_prot(arg3);
  container->set_flags(arg4);
  container->set_fd(arg5);
  container->set_offset(arg6);
}

void dispatch_ret_mmap(mmap_t* container, ADDRINT ret) {
  cout << "Mmap " << std::hex << ret << endl ;
  container->mutable_ret()->set_addr(ret);
  string val = read_n_memory_string((void*)ret,container->length());
  container->mutable_ret()->set_value(val);
}

//------------ QSORT ----------//
void dispatch_call_qsort(qsort_t* container, ADDRINT arg1, ADDRINT arg2,ADDRINT arg3, ADDRINT arg4) {
  container->mutable_base()->set_addr(arg1);
  container->set_nmemb(arg2);
  container->set_size(arg3);
  container->set_compare(arg4);
  cout << "Qsort base: " << std::hex << arg1 << "size " << arg2 << " * " << arg3 << endl ;
}

void dispatch_ret_qsort(qsort_t* container) {
  string val = read_n_memory_string((void*)container->mutable_base()->addr(),container->nmemb() * container->size());
  container->mutable_base()->set_value(val);
}

//------------ Bsearch ----------//
void dispatch_call_bsearch(bsearch_t* container, ADDRINT arg1, ADDRINT arg2,ADDRINT arg3, ADDRINT arg4, ADDRINT arg5) {
  container->mutable_key()->set_addr(arg1);
  string val_key = read_n_memory_string((void*)arg1, arg4);
  container->mutable_key()->set_value(val_key);

  container->mutable_base()->set_addr(arg2);
  string val_base = read_n_memory_string((void*)arg2, arg3*arg4);
  container->mutable_base()->set_value(val_base);

  container->set_nmemb(arg3);
  container->set_size(arg4);
  container->set_compare(arg5);
  cout << "Bsearch key: " << std::hex << arg1 << " base "<< hex << arg2 << " nmemb " << arg2 << " size " << arg3 << endl ;
}

void dispatch_ret_bsearch(bsearch_t* container, ADDRINT ret) {
  container->mutable_ret()->set_addr(ret);
  string val_ret = read_n_memory_string((void*)ret, container->size());
  container->mutable_ret()->set_value(val_ret);
}

//------------ Lseek ----------//
void dispatch_call_lseek(lseek_t* container, ADDRINT arg1, ADDRINT arg2,ADDRINT arg3) {
  container->set_fd(arg1);
  container->set_offset(arg2);
  container->set_whence(arg3);
  cout << "Lseek: fd " << std::hex << arg1 << " offset "<< hex << arg2 << " whence  " << arg3 << endl ;
}

void dispatch_ret_lseek(lseek_t* container, ADDRINT ret) {
  container->set_ret(ret);
}


//------------ FREAD ----------//
void dispatch_call_fread(fread_t* container, ADDRINT arg1, ADDRINT arg2,ADDRINT arg3, ADDRINT arg4) {
  container->mutable_ptr()->set_addr(arg1);
  container->set_size(arg2);
  container->set_nmemb(arg3);
  container->set_stream(arg4);
  cout << "Fread: ptr " << std::hex << arg1 << " size "<< hex << arg2 << " nmemb " << arg3 << " FILE " << hex << arg4 <<endl ;
}

void dispatch_ret_fread(fread_t* container, ADDRINT ret) {
  string buf = read_n_memory_string((void*)container->mutable_ptr()->addr(),ret*container->size()); 
  container->mutable_ptr()->set_value(buf);
  container->set_ret(ret);
}




//---------- Printf ------------//
void dispatch_call_printf(printf_t* container,
                          ADDRINT arg0, ADDRINT arg1, ADDRINT arg2, ADDRINT arg3, ADDRINT arg4,
                          ADDRINT arg5, ADDRINT arg6, ADDRINT arg7, ADDRINT arg8, ADDRINT arg9) {
  container->mutable_format()->set_addr(arg1);
  string format = read_memory_string((void*)arg1);
  container->mutable_format()->set_value(format);
}

void dispatch_ret_printf(printf_t* container, ADDRINT ret) {
  container->set_ret(ret);
}

//------------ Malloc -------------//
void dispatch_call_malloc(malloc_t* container, ADDRINT arg1) {
  cout << "Malloc: "  << arg1 << "\n";
  container->set_size(arg1);
}

void dispatch_ret_malloc(malloc_t* container, ADDRINT ret) {
  container->set_ret(ret);
}

//------------ Atoi --------------//
void dispatch_call_atoi(atoi_t* container, ADDRINT arg1) {
  container->mutable_src()->set_addr(arg1);
  string cnt = read_memory_string((void*)arg1);
  container->mutable_src()->set_value(cnt);
}

void dispatch_ret_atoi(atoi_t* container, ADDRINT ret) {
  container->set_ret(ret);
}

//------------- Strcpy -------------//
void dispatch_call_strcpy(strcpy_t* container, ADDRINT arg1, ADDRINT arg2) {
  container->mutable_src()->set_addr(arg2);
  string src_cnt = read_memory_string((void*) arg2);
  container->mutable_src()->set_value(src_cnt);

  container->mutable_dst()->set_addr(arg1);
  string dst_cnt = read_memory_string((void*) arg1);
  container->mutable_dst()->set_value(dst_cnt);
}

void dispatch_ret_strcpy(strcpy_t* container, ADDRINT ret) {
  container->set_ret(ret);
}

//------------ GetModuleHandle -------------//
void dispatch_call_getmodulehandle(getmodulehandle_t* container, ADDRINT arg1) {
    container->mutable_module_name()->set_addr(arg1);
    string module_name = read_memory_string((void*) arg1);
    container->mutable_module_name()->set_value(module_name);
}

void dispatch_ret_getmodulehandle(getmodulehandle_t* container, ADDRINT ret) {
    container->set_ret(ret);
}

//------------- GetProcAddres -------------//
void dispatch_call_getprocaddress(getprocaddress_t* container, ADDRINT arg1, ADDRINT arg2) {
    container->set_hmodule(arg1);
    container->mutable_proc_name()->set_addr(arg2);
    string proc_name = read_memory_string((void*) arg2);
    container->mutable_proc_name()->set_value(proc_name);
}

void dispatch_ret_getprocaddress(getprocaddress_t* container, ADDRINT ret) {
    container->set_ret(ret);
}

//--------------- _getmainargs ------------//
void dispatch_call_getmainargs(getmainargs_t* container, ADDRINT arg1, ADDRINT arg2, ADDRINT arg3, ADDRINT arg4, ADDRINT arg5) {
    container->mutable_argc()->set_addr(arg1);
    container->mutable_argv()->set_addr(arg2);
    container->mutable_env()->set_addr(arg3);
    container->set_dowildcard(arg4);
    container->mutable_startinfo()->set_addr(arg5);
}

void dispatch_ret_getmainargs(getmainargs_t* container, ADDRINT ret) {
  char* outBuf[sizeof(ADDRINT)];
  memcpy(outBuf, (const void *)container->mutable_argc()->addr(), sizeof(ADDRINT));
  string s = string((const char*)outBuf, sizeof(ADDRINT));
  container->mutable_argc()->set_value(s);
  memcpy(outBuf, (const void *)container->mutable_argv()->addr(), sizeof(ADDRINT));
  s = string((const char*)outBuf, sizeof(ADDRINT));
  container->mutable_argv()->set_value(s);
  memcpy(outBuf, (const void *)container->mutable_env()->addr(), sizeof(ADDRINT));
  s = string((const char*)outBuf, sizeof(ADDRINT));
  container->mutable_env()->set_value(s);
  memcpy(outBuf, (const void *)container->mutable_startinfo()->addr(), sizeof(ADDRINT));
  s = string((const char*)outBuf, sizeof(ADDRINT));
  container->mutable_startinfo()->set_value(s);
    container->set_ret(ret);
}

//--------------- gethostname ------------//
void dispatch_call_gethostname(gethostname_t* container, ADDRINT arg1, ADDRINT arg2) {
    string name = read_memory_string((void*) arg1);
    container->mutable_name()->set_addr(arg1);
    container->mutable_name()->set_value(name);
    container->set_namelen(arg2);
}

void dispatch_ret_gethostname(gethostname_t* container, ADDRINT ret) {
    string name = read_memory_string((void *)container->mutable_name()->addr());
    container->mutable_name()->set_value(name);
    container->set_ret(ret);
}


//---------------- memcpy ------------//
void dispatch_call_memcpy(memcpy_t* container, ADDRINT dest, ADDRINT src, ADDRINT size) {
    container->mutable_dest()->set_addr(dest);
    container->mutable_src()->set_addr(src);
    if (size > BUFFSIZE)
        cout << "WARNING: memcpy size > BUFFSIZE :" << size << endl;
    container->set_size(size);
}

void dispatch_ret_memcpy(memcpy_t* container, ADDRINT ret) {
    void* src = (void *)(ADDRINT)container->mutable_src()->addr();
    void* dest = (void *)(ADDRINT)container->mutable_dest()->addr();
    int size = container->size();
    read_memory(src, buff, size);
    container->mutable_src()->set_value(string((const char*)buff, size));
    read_memory(dest, buff, size);
    container->mutable_dest()->set_value(string((const char*)buff, size));
    container->set_ret(ret);
}

//----------------- memset ---------------//
void dispatch_call_memset(memset_t* container, ADDRINT s_addr, ADDRINT c, ADDRINT size) {
    container->mutable_s()->set_addr(s_addr);
    container->set_c(c);
    if (size > BUFFSIZE)
        cout << "WARNING: memset size > BUFFSIZE :" << size << endl;
    container->set_size(size);
}

void dispatch_ret_memset(memset_t* container, ADDRINT ret) {
/*    read_memory((void *)(ADDRINT)container->mutable_s()->addr(), buff, container->size());
    container->mutable_s()->set_value(string((const char*)buff, container->size()));*/
    string val = read_n_memory_string((void*)container->mutable_s()->addr(),container->size());
    container->mutable_s()->set_value(val);
    container->set_ret(ret);
}

//--------------- fgetc --------------//
void dispatch_call_fgetc(fgetc_t* container, ADDRINT arg1) {
    container->set_stream(arg1);
}

void dispatch_ret_fgetc(fgetc_t* container, ADDRINT ret) {
    container->set_ret(ret);
}
// ================================================================================== //


// ======================== MAIN DISPATCHERS ======================== //

//Fonction that will call the right library call depending in the policy ident field
VOID dispatch_call(THREADID threadid, CONTEXT * ctx, ADDRINT addr, CHAR * name,
                   ADDRINT arg0, ADDRINT arg1, ADDRINT arg2, ADDRINT arg3, ADDRINT arg4,
                   ADDRINT arg5, ADDRINT arg6, ADDRINT arg7, ADDRINT arg8, ADDRINT arg9) {
  thread_data_t* tdata = get_tls(threadid);

//  cout << "\nIn distpach call "  << addr << " : " << name << "\n";


  //Creation of the new ins_conc_t
  ins_con_info_t* info = new ins_con_info_t();
  info->set_typeid_(ins_con_info_t::CALL);
  libcall_t* call = info->mutable_call();
  call->set_func_addr((UINT64)addr);
  call->set_func_name(string(name));
  call->set_is_traced(false);
  //-----------------------


  int ident = tdata->current_policy->ident();

  if (ident == GENERIC) {
      if (lookup_map.find(name) != lookup_map.end()) {
          cout << "Lookup map found match:" << name << endl;
          ident = lookup_map[name];
      }
  }

  switch(ident) {
      case PRINTF: {
          call->set_ident(PRINTF);
          dispatch_call_printf(call->mutable_printf(), arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
          break;
      }
      case MALLOC: {
          call->set_ident(MALLOC);
          dispatch_call_malloc(call->mutable_malloc(), arg1);
          break;
      }
      case ATOI: {
          call->set_ident(ATOI);
          dispatch_call_atoi(call->mutable_atoi(), arg1);
          break;
      }
      case STRCPY: {
          call->set_ident(STRCPY);
          dispatch_call_strcpy(call->mutable_strcpy(), arg1, arg2);
          break;
      }   
      case STRNCPY: {
          call->set_ident(STRNCPY);
          dispatch_call_strncpy(call->mutable_strncpy(), arg1, arg2,arg3);
          break;
      }
      case FSTAT: {
          call->set_ident(FSTAT);
          dispatch_call_fstat(call->mutable_fstat(), arg1, arg2);
          break;
      }
      case FXSTAT64: {
          call->set_ident(FXSTAT64);
          dispatch_call_fxstat64(call->mutable_fxstat64(), arg1, arg2, arg3);
          break;
      }
      case REALLOC: {
          call->set_ident(REALLOC);
          dispatch_call_realloc(call->mutable_realloc(), arg1, arg2);
          break;
      }
      case MMAP: {
          call->set_ident(MMAP);
          dispatch_call_mmap(call->mutable_mmap(), arg1, arg2, arg3, arg4, arg5, arg6);
          break;
      }
      case QSORT: {
          call->set_ident(QSORT);
          dispatch_call_qsort(call->mutable_qsort(), arg1, arg2, arg3, arg4);
          break;
      }
      case BSEARCH: {
          call->set_ident(BSEARCH);
          dispatch_call_bsearch(call->mutable_bsearch(), arg1, arg2, arg3, arg4, arg5);
          break;
      }
      case OPEN_STUB: {
          call->set_ident(OPEN_STUB);
          dispatch_call_open(call->mutable_open_stub(), arg1, arg2, arg3);
          break;
      }
      case GENERIC: {
          cout << "no match found use GENERIC for:" << name << endl;
          call->set_ident(GENERIC);
          dispatch_call_generic();
          break;
      }
      case READ : {
          call->set_ident(READ);
          dispatch_call_read(call->mutable_read(), arg1, arg2,arg3);
          break;
      }
      case STRCHR : {
          call->set_ident(STRCHR);
          dispatch_call_strchr(call->mutable_strchr(), arg1, arg2);
          break;
      }
      /* STRRCHR needs the same values as STRCHR */
      case STRRCHR : {
          call->set_ident(STRRCHR);
          dispatch_call_strchr(call->mutable_strchr(), arg1, arg2);
          break;
      }
      case STRCMP : {
          call->set_ident(STRCMP);
          dispatch_call_strcmp(call->mutable_strcmp(), arg1, arg2);
          break;
      }
      case STRNCMP : {
          call->set_ident(STRNCMP);
          dispatch_call_strncmp(call->mutable_strncmp(), arg1, arg2, arg3);
          break;
      }
      case MEMCMP : {
          call->set_ident(MEMCMP);
          dispatch_call_memcmp(call->mutable_memcmp(), arg1, arg2, arg3);
          break;
      }
      case CTYPE_B_LOC: {
          call->set_ident(CTYPE_B_LOC);
          dispatch_call_ctype_b_loc();
          break;
      }
      case EXIT : {
          call->set_ident(EXIT);
          dispatch_call_exit();
          break;
      }
      case LSEEK : {
          call->set_ident(LSEEK);
          dispatch_call_lseek(call->mutable_lseek(), arg1, arg2, arg3);
          break;
      }
      case FREAD : {
          call->set_ident(FREAD);
          dispatch_call_fread(call->mutable_fread(), arg1, arg2, arg3,arg4);
          break;
      }
      case GETMODULEHANDLE: {
          call->set_ident(GETMODULEHANDLE);
          dispatch_call_getmodulehandle(call->mutable_getmodulehandle(), arg0); //TODO:Verifie that it is the arg0 instead of arg1
          break;
      }
      case GETPROCADDRESS:{
          call->set_ident(GETPROCADDRESS);
          dispatch_call_getprocaddress(call->mutable_getprocaddress(), arg0, arg1);
          break;
      }
      case GETMAINARGS:{
          call->set_ident(GETMAINARGS);
          dispatch_call_getmainargs(call->mutable_getmainargs(), arg0, arg1, arg2, arg3, arg4);
          break;
      }
      case GETHOSTNAME:{
          call->set_ident(GETHOSTNAME);
          dispatch_call_gethostname(call->mutable_gethostname(), arg0, arg1);
          break;
      }
      case FREE: {
        call->set_ident(FREE);
        dispatch_call_free(call->mutable_free(), arg1);
        break;
      }
      case MEMCPY: {
        call->set_ident(MEMCPY);
        dispatch_call_memcpy(call->mutable_memcpy(), arg1, arg2, arg3);
        break;
      }
      case MEMSET: {
        call->set_ident(MEMSET);
        dispatch_call_memset(call->mutable_memset(), arg1, arg2, arg3);
        break;
      }
      case FGETC:{
          call->set_ident(FGETC);
          dispatch_call_fgetc(call->mutable_fgetc(), arg1);
          break;
      }
      case FSCANF:{
          call->set_ident(FSCANF);
          dispatch_call_fscanf(call->mutable_fscanf(),arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
          break;
      }
      default:
          cout << "Fun not recognized [use GENERIC]" << endl;
          call->set_ident(GENERIC);
          break;
  }

  tdata->ins_infos.concinfos.push_back(info);
}

//Same than previous function but dispatch for ret
VOID dispatch_ret(THREADID threadid, CONTEXT * ctx, ADDRINT ret) {
  thread_data_t* tdata = get_tls(threadid);

  //Search for the libcall_t that should already be in the list of current conc_infos
  libcall_t* call = nullptr;
  for(ins_con_info_t* info: tdata->ins_infos.concinfos) {
    if(info->typeid_() ==  ins_con_info_t::CALL) {
      call = info->mutable_call();
    }
  }
  if(call == nullptr) {
    //cout << "libcall_t not found in concretes infos";
    return;
  }

  switch(call->ident()) {
      case PRINTF: { dispatch_ret_printf(call->mutable_printf(), ret); break; }
      case MALLOC: { dispatch_ret_malloc(call->mutable_malloc(), ret); break; }
      case ATOI:   { dispatch_ret_atoi(call->mutable_atoi(), ret);  break; }
      case STRCPY: { dispatch_ret_strcpy(call->mutable_strcpy(), ret); break; }
      case STRNCPY: { dispatch_ret_strncpy(call->mutable_strncpy(), ret); break; }
      case FSTAT: { dispatch_ret_fstat(call->mutable_fstat(), ret); break; }
      case FXSTAT64: { dispatch_ret_fxstat64(call->mutable_fxstat64(), ret); break; }
      case REALLOC: { dispatch_ret_realloc(call->mutable_realloc(), ret); break; }
      case MMAP: { dispatch_ret_mmap(call->mutable_mmap(), ret); break; }
      case QSORT: { dispatch_ret_qsort(call->mutable_qsort()); break; }
      case BSEARCH: { dispatch_ret_bsearch(call->mutable_bsearch(),ret); break; }
      case GENERIC:{ dispatch_ret_generic(call->mutable_generic(),ret);break;}
//      case IGNORE: { dispatch_ret_ignore(call->mutable_ignore()); break;}
      case READ:   { dispatch_ret_read(call->mutable_read(),ret);break;}
      case OPEN_STUB:   { dispatch_ret_open(call->mutable_open_stub(),ret);break;}
      case STRCHR: { dispatch_ret_strchr(call->mutable_strchr(),ret);break;}
      case STRRCHR: { dispatch_ret_strchr(call->mutable_strchr(),ret);break;}
      case STRCMP: { dispatch_ret_strcmp(call->mutable_strcmp(),ret);break;}
      case STRNCMP: { dispatch_ret_strncmp(call->mutable_strncmp(),ret);break;}
      case MEMCMP: { dispatch_ret_memcmp(call->mutable_memcmp(),ret);break;}
      case FREE: { dispatch_ret_free(call->mutable_free(),ret);break;}
      case CTYPE_B_LOC: { dispatch_ret_ctype_b_loc(call->mutable_ctype_b_loc(),ret);break;}
      case EXIT:   { dispatch_ret_exit(call->mutable_exit());break;}
      case LSEEK:   { dispatch_ret_lseek(call->mutable_lseek(),ret );break;}
      case FREAD:   { dispatch_ret_fread(call->mutable_fread(),ret );break;}

      case GETMODULEHANDLE: { dispatch_ret_getmodulehandle(call->mutable_getmodulehandle(), ret); break; }
      case GETPROCADDRESS: { dispatch_ret_getprocaddress(call->mutable_getprocaddress(), ret); break; }
      case GETMAINARGS: { dispatch_ret_getmainargs(call->mutable_getmainargs(), ret); break;}
      case GETHOSTNAME: { dispatch_ret_gethostname(call->mutable_gethostname(), ret); break;}
      case MEMCPY: {dispatch_ret_memcpy(call->mutable_memcpy(), ret); break; }
      case MEMSET: {dispatch_ret_memset(call->mutable_memset(), ret); break; }
      case FGETC: {dispatch_ret_fgetc(call->mutable_fgetc(), ret); break; }
      case FSCANF: {dispatch_ret_fscanf(call->mutable_fscanf(), ret); break; }
      default:
          cout << "NOT RECOGNISED";
  }
}
