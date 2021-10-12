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
