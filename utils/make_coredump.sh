#!/usr/bin/env bash

set -x

snapshot="$1"
shift
if [[ $snapshot = -h ]] || [[ -z $snapshot ]]; then
  echo "$0: usage: $0 name_of_core_file_to_create command arg1 arg2 ..."
  echo "creates a core file with all libs to run the command loaded"
  echo "actually runs code from the executable: only works for the current architecture."
  echo "also assumes glibc loader"
  exit 1
fi

exe="$1"
if [[ ! -f "$exe" ]]; then
  echo "$exe is not a file, aborting"
  exit 1
fi

case "$(file --dereference "$exe")" in
  *ELF\ 32-bit*)
    get_main='**((int*)($esp+4))'
    ;;
  *ELF\ 64-bit*)
    get_main='*$rdi'
    ;;
  *)
    echo "could not guess if file is 32 or 64bit"
    exit 1
    ;;
esac

tmpfile="$(mktemp)"
trap 'rm -f $tmpfile' EXIT

cat > "$tmpfile" <<EOF
set interactive-mode off
set pagination off
set breakpoint pending on
set env LD_BIND_NOW=1
set env GLIBC_TUNABLES=glibc.cpu.hwcaps=-AVX512VL,-AVX512F
break __libc_start_main
run
# $get_main contains the address of main, even if main is not a symbol
break $get_main
continue
# we reached main
generate-core-file $snapshot
kill
quit
EOF


exec gdb -n -batch -x $tmpfile --args "$@"
