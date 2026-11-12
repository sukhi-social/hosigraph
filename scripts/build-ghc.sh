#!/usr/bin/env bash
# Build without cabal (handy when Hackage is unreachable): needs only ghc + gcc and the
# packages that ship with GHC / distributions (aeson, QuickCheck for the tests).
#
#   scripts/build-ghc.sh app    -> build/hosigraph-demo
#   scripts/build-ghc.sh test   -> build/hosigraph-test
#   (both need BRIDGE_DIR=/path/to/native-bridge and ENGINE_DIR=/path/to/the-engine)
set -euo pipefail
cd "$(dirname "$0")/.."
# native-bridge and the engine it wraps are separate packages and live wherever you keep them: say where.
bridge="${BRIDGE_DIR:-}"; engine="${ENGINE_DIR:-}"
if [ ! -f "$bridge/src/Native/Engine.hs" ] || [ ! -f "$engine/cbits/dlmdb/mdb.c" ]; then
  echo "set BRIDGE_DIR to native-bridge and ENGINE_DIR to the engine package it wraps, e.g." >&2
  echo "  BRIDGE_DIR=../native-bridge ENGINE_DIR=/path/to/the-engine $0 ${1:-app}" >&2
  exit 1
fi
mkdir -p build/obj build/c
cc_obj() {  # source -> object, rebuilt only when the source is newer
  local src="$1" out="build/c/$(basename "${1%.c}").o"
  if [ ! -f "$out" ] || [ "$src" -nt "$out" ]; then gcc -O2 -fPIC -pthread -c "$src" -o "$out"; fi
}
cc_obj "$engine/cbits/dlmdb/mdb.c"
cc_obj "$engine/cbits/dlmdb/midl.c"
cc_obj cbits/hosi_sync.c
common=(-isrc "-i$bridge/src" "-i$engine/src" -Wall -Wno-name-shadowing -O1 -threaded -rtsopts -outputdir build/obj
        build/c/mdb.o build/c/midl.o build/c/hosi_sync.o -optl-pthread)
case "$target" in
  app)  ghc "${common[@]}" -iapp  app/Main.hs   -o build/hosigraph-demo ;;
  test) ghc "${common[@]}" -itest test/Spec.hs  -o build/hosigraph-test ;;
  *) echo "usage: $0 [app|test]"; exit 1 ;;
esac
