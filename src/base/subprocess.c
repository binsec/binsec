/**************************************************************************/
/*  This file is part of BINSEC.                                          */
/*                                                                        */
/*  Copyright (C) 2016-2024                                               */
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
/*                                                                        */
/**************************************************************************/

#ifndef CAML_INTERNALS
#define CAML_INTERNALS
#endif
#include <caml/mlvalues.h>
#include <caml/signals.h>

#ifdef __linux__

#include <unistd.h>
#include <sys/prctl.h>
#include <errno.h>

CAMLprim intnat
native_subprocess_set_pdeathsig (intnat sig)
{
  int status = prctl(PR_SET_PDEATHSIG, caml_convert_signal_number(sig));
  return status | ((getppid() != 1) - 1);
}

#else

CAMLprim intnat
native_subprocess_set_pdeathsig (intnat sig)
{
  return 0;
}

#endif

CAMLprim value
caml_subprocess_set_pdeathsig (value vsig)
{
  return Val_long(native_subprocess_set_pdeathsig(Long_val(vsig)));
}
