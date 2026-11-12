#!/usr/bin/env bash
# Gather libhosi and every library it needs (except the system's) into one directory, and make
# each of them look for the others next to itself only.  The directory can then be moved anywhere.
#
#   scripts/bundle-libhosi.sh PATH/TO/libhosi.{dylib,so} OUT_DIR
#
# macOS: otool / install_name_tool / codesign.  Linux: ldd / patchelf.
set -euo pipefail

lib=${1:?usage: $0 libhosi OUT_DIR}
out=${2:?usage: $0 libhosi OUT_DIR}
mkdir -p "$out"
out=$(cd "$out" && pwd)
cp -f "$lib" "$out/$(basename "$lib")"
chmod u+w "$out/$(basename "$lib")"

case "$(uname -s)" in
  Darwin)
    todo=("$lib")
    i=0
    while [ "$i" -lt "${#todo[@]}" ]; do
      f=${todo[$i]}; i=$((i + 1))
      rpaths=$(otool -l "$f" | awk '/cmd LC_RPATH/ {r = 1} r && /^ *path / {print $2; r = 0}')
      # `|| true`: a library that needs nothing from @rpath has no such line
      for dep in $(otool -L "$f" | awk 'NR > 1 {print $1}' | grep '^@rpath/' || true); do
        name=${dep#@rpath/}
        [ -e "$out/$name" ] && continue
        found=""
        while IFS= read -r d; do
          d=${d//@loader_path/$(dirname "$f")}
          if [ -n "$d" ] && [ -e "$d/$name" ]; then found="$d/$name"; break; fi
        done <<<"$rpaths"
        [ -n "$found" ] || { echo "bundle: cannot find $dep (wanted by $f)" >&2; exit 1; }
        cp -f "$found" "$out/$name"; chmod u+w "$out/$name"
        todo+=("$found")
      done
    done
    for f in "$out"/*.dylib; do
      otool -l "$f" | awk '/cmd LC_RPATH/ {r = 1} r && /^ *path / {print $2; r = 0}' |
        while IFS= read -r r; do install_name_tool -delete_rpath "$r" "$f"; done
      install_name_tool -add_rpath @loader_path "$f"
      codesign -f -s - "$f" 2>/dev/null   # changing a library voids its signature; arm64 wants one
    done
    ;;
  Linux)
    command -v patchelf >/dev/null || { echo "bundle: patchelf is needed" >&2; exit 1; }
    system='^(linux-vdso|ld-linux.*|libc|libm|libdl|libpthread|librt|libutil|libresolv|libgcc_s)\.so'
    todo=("$lib")
    i=0
    while [ "$i" -lt "${#todo[@]}" ]; do
      f=${todo[$i]}; i=$((i + 1))
      for dep in $(ldd "$f" | awk '/=>/ && $3 ~ /^\// {print $3}'); do
        name=$(basename "$dep")
        if [[ $name =~ $system ]] || [ -e "$out/$name" ]; then continue; fi
        cp -f "$dep" "$out/$name"; chmod u+w "$out/$name"
        todo+=("$dep")
      done
    done
    for f in "$out"/*.so*; do patchelf --set-rpath '$ORIGIN' "$f"; done
    ;;
  *) echo "bundle: $(uname -s) is not handled" >&2; exit 1 ;;
esac
echo "bundled $(ls "$out" | wc -l | tr -d ' ') libraries into $out"
