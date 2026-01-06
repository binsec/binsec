#!/usr/bin/env bash

# Check dependencies
if ! command -v eu-readelf > /dev/null; then
  echo "[error] ... Usage of \`eu-readelf\` needed, please install the package \`elfutils\` for the continuation."
  exit 1
fi

set -x

snapshot="$1"
archive="$2"
if [[ $snapshot = -h ]] || [[ -z $snapshot ]] || [[ -z $archive ]]; then
  echo "$0: usage: $0 name_of_core_file name_of_the_archive"
  echo "creates an archive of the files used by the core"
  exit 1
fi

core_file=$(eu-readelf $snapshot --notes | sed '0,/CORE[ 0-9]*FILE/d')
n=$(echo -n "$core_file" | head --lines 1 | grep -e "files:" | awk '{print $1}')
files=$(echo -n "$core_file" | sed -n "2,$((n + 1))p" | awk '{print $4}' | sort | uniq | sed 's|^/||g')

if [[ $files =  "" ]]; then
  echo "[log] ... No data to archive, exit."
  exit 0
fi

for f in $files; do
  if [ -f "/$f.debug" ]; then
    files+=" $f.debug"
  else
    buildid=$(eu-readelf --notes /$f | grep -e "Build ID:" | awk '{print $3}')
    entry="usr/lib/debug/.build-id/$(echo -n $buildid | head -c 2)/$(echo -n $buildid | tail -c +3).debug"
    if [ -f "/$entry" ]; then
      files+=" $entry"
    fi
  fi
done

tar -czf $archive -C / $files