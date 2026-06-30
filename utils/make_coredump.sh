#!/usr/bin/env bash

gdb=${GDB:-gdb}
self=$(basename $0)

short_options=hq
help_options="-h"
quiet_options+="-q"

getopt --test > /dev/null
if [[ $? -eq 4 ]]; then
  help_options+=", --help"
  quiet_options+=", --quiet"
  long_options="--longoptions=help,quiet"
else
  long_options=""
fi

function show_usage () {
  echo "USAGE:"
  echo ""
  echo -e "\t$self [options] core-file-path executable"
  echo -e "\t$self [options] core-file-path [--] executable [arg1 arg2 ...]"
  echo ""
  echo "Creates a core file with all libs to run the command loaded."
  echo "/!\ It assumes glibc loader to break at the main entrypoint."
  echo "Runs code from the executable (either natively or with qemu - if available)."
  echo ""
  echo "You can set the gdb executable with the environment variable 'GDB'."
  echo -e "e.g:\tGDB=gdb-multiarch make_coredump.sh core.snapshot /bin/ls"
  echo ""
  echo "OPTIONS:"
  echo -e "\t$help_options\tDisplay this help message."
  echo -e "\t$quiet_options\tActivate silent mode."
}

args=$(getopt --options=$short_options $long_options --name "$self" -- "$@")
if [[ $? -ne 0 ]]; then
  show_usage
  exit 1
fi

eval set -- $args

help_required=false
quiet_mode=false
while true; do
  case "$1" in
    -h|--help)
      help_required=true
      shift
      ;;
    -q|--quiet)
      quiet_mode=true
      shift
      ;;
    --)
      shift
      break
      ;;
  esac
done

if [[ "$help_required" = true ]]; then
  echo "This is a BINSEC utility script to generate coredump."
  show_usage
  exit 0
fi

snapshot="$1"
shift

if [[ -z "$snapshot" ]]; then
  echo "$self: missing output path 'core-file-path', aborting."
  show_usage
  exit 1
fi

exe="$1"

if [[ -z "$exe" ]]; then
  echo "$self: receive no file to analyze, aborting."
  show_usage
  exit 1
elif [[ ! -f "$exe" ]]; then
  echo "$self: $exe is not a file, aborting."
  show_usage
  exit 1
fi

batch="--batch"
if [[ "$quiet_mode" = true ]]; then
  batch+="-silent"
else
  set -x
fi

x86_32=false
arch=$(file --brief --dereference "$exe" | cut -d, -f2 | tail -c +2)
case "$arch" in
  "Intel 80386" | "Intel i386")
    x86_32=true
    main='*((int*)($esp+4))'
    tunables="-AVX512VL,-AVX512F"
    ;;
  "x86-64")
    main='$rdi'
    tunables="-AVX512VL,-AVX512F"
    ;;
  "ARM")
    main='$r0'
    tunables=""
    ;;
  "ARM aarch64")
    main='$x0'
    tunables=""
    ;;
  "64-bit PowerPC or cisco 7500")
    main='*((long*)($r8+8))'
    tunables=""
    ;;
  "UCB RISC-V")
    main='$a0'
    tunables=""
    ;;
  *)
    echo "$self: Architecture $arch is not supported by this script."
    exit 1
    ;;
esac

host=$(file --brief --dereference "/bin/env" | cut -d, -f2 | tail -c +2)

tmpfile="$(mktemp)"
trap 'rm -f $tmpfile' EXIT

cat > "$tmpfile" <<EOF
# $main contains the address of main, even if main is not a symbol
set \$main = $main
break *\$main
continue
if (\$pc != \$main)
  set \$pc = \$main
end
# we reached main
generate-core-file $snapshot
kill
quit
EOF

echo 0x3f > /proc/self/coredump_filter

if [ "$arch" != "$host" ] && ([ "$x86_32" = false ] || [ "$host" != "x86-64" ]); then
    case "$arch" in
      "Intel 80386" | "Intel i386")
        qemu="qemu-i386"
        ;;
      "x86-64")
        qemu="qemu-x86_64"
        ;;
      "ARM")
        qemu="qemu-arm"
        ;;
      "ARM aarch64")
        qemu="qemu-aarch64"
        ;;
      "64-bit PowerPC or cisco 7500")
        qemu="qemu-ppc64"
        ;;
      "UCB RISC-V")
        if [ "$(file --brief --dereference "$exe" | cut -d, -f1 | cut -d' ' -f2)" = "32-bit" ]; then
            qemu="qemu-riscv32"
        else
            qemu="qemu-riscv64"
        fi
        ;;
    esac
    port=$(comm -23 <(seq 49152 65535 | sort) <(ss -Htan | awk '{print $4}' | cut -d':' -f2 | sort -u) | shuf | head -n 1)
    $qemu -E "LD_BIND_NOW=1" -E "GLIBC_TUNABLES=glibc.cpu.hwcaps=$tunables" -g $port $@ &
    $gdb -n $batch \
      -iex "set interactive-mode off" \
      -iex "set pagination off" \
      -iex "set breakpoint pending on" \
      -ex "target remote:$port" \
      -ex "break __libc_start_main" \
      -ex "continue" \
      -x $tmpfile $exe
else
    $gdb -n $batch \
      -iex "set interactive-mode off" \
      -iex "set pagination off" \
      -iex "set env LD_BIND_NOW=1" \
      -iex "set env GLIBC_TUNABLES=glibc.cpu.hwcaps=$tunables" \
      -iex "set breakpoint pending on" \
      -ex "break __libc_start_main" \
      -ex "run" \
      -x $tmpfile --args $@
fi
