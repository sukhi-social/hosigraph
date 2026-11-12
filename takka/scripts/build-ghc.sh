#!/usr/bin/env bash
# Build without cabal (handy when Hackage is unreachable): needs only ghc + gcc
# plus the GHC packages: aeson text bytestring containers (all ship with Linux distros).
#
#   scripts/build-ghc.sh app      -> build/takka-demo
#   scripts/build-ghc.sh test     -> build/takka-test   (needs QuickCheck)
#   scripts/build-ghc.sh bench    -> build/takka-bench
#   scripts/build-ghc.sh fuzz     -> build/dlmdb-fuzz   (needs QuickCheck; see docs/dlmdb-findings.md)
set -euo pipefail
cd "$(dirname "$0")/.."
target="${1:-app}"
mkdir -p build/obj build/c
for f in mdb midl; do
  if [ ! -f build/c/$f.o ] || [ cbits/dlmdb/$f.c -nt build/c/$f.o ]; then
    gcc -O2 -fPIC -pthread -c cbits/dlmdb/$f.c -o build/c/$f.o
  fi
done
common=(-isrc -Wall -Wno-name-shadowing -O1 -threaded -rtsopts -outputdir build/obj
        build/c/mdb.o build/c/midl.o -optl-pthread)
case "$target" in
  app)  ghc "${common[@]}" -iapp app/Main.hs  -o build/takka-demo ;;
  test) ghc "${common[@]}" -itest test/Spec.hs -o build/takka-test ;;
  bench) ghc "${common[@]}" -ibench bench/Bench.hs -o build/takka-bench ;;
  fuzz) ghc "${common[@]}" -ibench bench/FuzzDlmdb.hs -o build/dlmdb-fuzz ;;
  *) echo "usage: $0 [app|test|bench|fuzz]"; exit 1 ;;
esac
