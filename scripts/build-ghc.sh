#!/usr/bin/env bash
# SPDX-License-Identifier: BlueOak-1.0.0
# Build without cabal (handy when Hackage is unreachable): needs only ghc and the packages that
# ship with GHC / distributions (aeson, QuickCheck for the tests). There is no C to compile.
# takka (values, EDN, Datalog syntax) is in this repository (takka/), so there is nothing else to find.
#
#   scripts/build-ghc.sh app    -> build/hosigraph-demo
#   scripts/build-ghc.sh test   -> build/hosigraph-test
set -euo pipefail
cd "$(dirname "$0")/.."
target="${1:-app}"
mkdir -p build/obj
common=(-isrc -itakka/src -Wall -Wno-name-shadowing -O1 -threaded -rtsopts -outputdir build/obj)
case "$target" in
  app)  ghc "${common[@]}" -iapp  app/Main.hs   -o build/hosigraph-demo ;;
  test) ghc "${common[@]}" -itest test/Spec.hs  -o build/hosigraph-test ;;
  *) echo "usage: $0 [app|test]"; exit 1 ;;
esac
