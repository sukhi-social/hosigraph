#!/usr/bin/env bash
# Build the musl bundle.  Run inside an Alpine container, from the root of the repository:
#
#   docker run --rm -v "$PWD:/w" -w /w alpine:3.20 \
#     sh -c 'apk add --no-cache bash && exec bash hosigraph-ex/scripts/ci-musl-bundle.sh ASSET.tar.gz'
#
# (see .github/workflows/precompile-hosi-ex.yml).  What the glibc jobs do step by step is done here
# in one script, because the steps have to run inside the container, where the GitHub actions
# (which want a glibc node) cannot.  Alpine 3.20 has Erlang/OTP 26, the oldest NIF API we ship for.
#
# The cabal store and ghcup live in /root/.cabal and /root/.ghcup; mount them to keep them.
set -euo pipefail

asset=${1:?usage: $0 ASSET.tar.gz}
GHC_VERSION=${GHC_VERSION:-9.10.3}
CABAL_VERSION=${CABAL_VERSION:-3.14.2.0}

apk add --no-cache build-base curl git xz perl patchelf \
  gmp-dev libffi-dev ncurses-dev zlib-dev \
  erlang erlang-dev elixir >/dev/null

# ghcup chooses the Alpine (musl, dynamically linked) GHC by itself
export PATH="/root/.ghcup/bin:/root/.cabal/bin:$PATH"
if ! command -v ghc >/dev/null || [ "$(ghc --numeric-version)" != "$GHC_VERSION" ]; then
  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org |
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 BOOTSTRAP_HASKELL_ADJUST_BASHRC=0 sh
  ghcup install ghc "$GHC_VERSION" --set
  ghcup install cabal "$CABAL_VERSION" --set
fi
ghc --version; cabal --version; erl -noshell -eval 'io:format("OTP ~s, NIF ~s~n", [erlang:system_info(otp_release), erlang:system_info(nif_version)]), halt().'

cabal update
cabal build hosigraph-abi:flib:hosi

lib=$(find dist-newstyle -type f -name 'libhosi.so*' | head -n 1)
echo "library: $lib"
mkdir -p hosigraph-ex/priv
hosigraph-ex/scripts/bundle-libhosi.sh "$lib" "$PWD/hosigraph-ex/priv"

(cd hosigraph-ex &&
  HOSI_BUILD=source HOSI_LIB_DIR="$PWD/priv" HOSI_INCLUDE_DIR="$PWD/../hosigraph-abi/gen/include" mix compile)
tar -czf "$asset" -C hosigraph-ex/priv .

# Try what will be shipped, where nothing of the build can be found
try=$(mktemp -d)
cp -R hosigraph-ex/mix.exs hosigraph-ex/c_src hosigraph-ex/support hosigraph-ex/lib hosigraph-ex/test "$try/"
mkdir "$try/priv"
tar -xzf "$asset" -C "$try/priv"
mv dist-newstyle dist-newstyle.aside
(cd "$try" && ldd priv/hosi_nif.so && HOSI_BUILD=none mix test)
mv dist-newstyle.aside dist-newstyle
