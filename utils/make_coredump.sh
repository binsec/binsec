#!/usr/bin/env bash

gdb=${GDB:-gdb}

set -x

snapshot="$1"
shift
if [[ $snapshot = -h ]] || [[ -z $snapshot ]]; then
  echo "$0: usage: $0 name_of_core_file_to_create command arg1 arg2 ..."
  echo "creates a core file with all libs to run the command loaded"
  echo "runs code from the executable (either natively or with qemu)"
  echo "also assumes glibc loader"
  echo "you can set the gdb executable with the environment variable 'GDB'"
  echo "e.g. GDB=gdb-multiarch make_coredump.sh core.snapshot /bin/ls" 
  exit 1
fi

exe="$1"
if [[ ! -f "$exe" ]]; then
  echo "$exe is not a file, aborting"
  exit 1
fi

arch=$(file --brief --dereference "$exe" | cut -d, -f2 | tail -c +2)

case "$arch" in
  "Intel 80386")
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
    echo "Architecture $arch is not supported by this script."
    exit 1
    ;;
esac

host=$(file --brief --dereference "/usr/bin/env" | cut -d, -f2 | tail -c +2)

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

if [ "$arch" != "$host" ] && ([ "$arch" != "Intel 80386" ] || [ "$host" != "x86-64" ]); then
    case "$arch" in
      "Intel 80386")
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
    $gdb -n -batch \
      -iex "set interactive-mode off" \
      -iex "set pagination off" \
      -iex "set breakpoint pending on" \
      -ex "target remote:$port" \
      -ex "break __libc_start_main" \
      -ex "continue" \
      -x $tmpfile $exe
else
    $gdb -n -batch \
      -iex "set interactive-mode off" \
      -iex "set pagination off" \
      -iex "set env LD_BIND_NOW=1" \
      -iex "set env GLIBC_TUNABLES=glibc.cpu.hwcaps=$tunables" \
      -iex "set breakpoint pending on" \
      -ex "break __libc_start_main" \
      -ex "run" \
      -x $tmpfile --args "$@"
fi