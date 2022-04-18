/**************************************************************************/
/*  This file is part of BINSEC.                                          */
/*                                                                        */
/*  Copyright (C) 2016-2019                                               */
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

#include <stdint.h>
#include <caml/mlvalues.h>
#include <caml/hash.h>

CAMLprim value cstubs_hash_fold_int(value st, value i)
{
  return Val_long(caml_hash_mix_intnat(Long_val(st), Long_val(i)));
}

CAMLprim intnat cstubs_hash_fold_int_untagged(intnat st, intnat i)
{
  return caml_hash_mix_intnat(st, i);
}

CAMLprim value cstubs_hash_fold_string(value st, value v)
{
  return Val_long(caml_hash_mix_string(Long_val(st), v));
}

CAMLprim intnat cstubs_hash_fold_string_untagged(intnat st, value v)
{
  return caml_hash_mix_string(st, v);
}
