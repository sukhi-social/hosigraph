#!/usr/bin/env bash
# Build without cabal (handy when Hackage is unreachable): needs only ghc + gcc and the
# packages that ship with GHC / distributions (aeson, QuickCheck for the tests).
# takka, the Datalog engine, is in this repository (takka/), so there is nothing else to find.
#
#   scripts/build-ghc.sh app    -> build/hosigraph-demo
#   scripts/build-ghc.sh test   -> build/hosigraph-test
set -euo pipefail
cd "$(dirname "$0")/.."
target="${1:-app}"
mkdir -p build/obj build/c
cc_obj() {  # source -> object, rebuilt only when the source is newer
  local src="$1" out="build/c/$(basename "${1%.c}").o"
  if [ ! -f "$out" ] || [ "$src" -nt "$out" ]; then gcc -O2 -fPIC -pthread -c "$src" -o "$out"; fi
}
cc_obj takka/cbits/dlmdb/mdb.c
cc_obj takka/cbits/dlmdb/midl.c
cc_obj cbits/hosi_sync.c
common=(-isrc -itakka/src -Wall -Wno-name-shadowing -O1 -threaded -rtsopts -outputdir build/obj
        build/c/mdb.o build/c/midl.o build/c/hosi_sync.o -optl-pthread)
case "$target" in
  app)  ghc "${common[@]}" -iapp  app/Main.hs   -o build/hosigraph-demo ;;
  test) ghc "${common[@]}" -itest test/Spec.hs  -o build/hosigraph-test ;;
  *) echo "usage: $0 [app|test]"; exit 1 ;;
esac
