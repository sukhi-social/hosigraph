#!/usr/bin/env bash
# SPDX-License-Identifier: BlueOak-1.0.0
# Cut a release of the hosigraph library: check that the tree says what the tag will say, run the
# tests as CI does, make the annotated tag, and push it only when asked.
#
#   scripts/release.sh                 check only: lists what is wrong, changes nothing
#   scripts/release.sh --test          ... and the tests of CI (needs HOSIGRAPH_DATALEVIN_JAR, below)
#   scripts/release.sh --tag           ... and make the tag v<version> here, on HEAD
#   scripts/release.sh --push          ... and push the tag (implies --tag; HEAD has to be on origin/main)
#   scripts/release.sh [options] 0.2.0 the version is then checked against the cabal file's
#
# The version is the one in hosigraph.cabal: 0.2.0.0 is tagged v0.2.0, and 0.2.0.1 is v0.2.0.1. The
# tag has to say what the cabal file says, as the tag `hosi-ex-v*` has to say what hosigraph-ex/mix.exs
# says (.github/workflows/precompile-hosi-ex.yml). A tag `v*` starts no workflow; only `hosi-ex-v*` does,
# and that one is for the Elixir package, with its own version.
#
# What is checked, and why:
#   * nothing is uncommitted: the tag names a commit, so the disk has to be that commit;
#   * CHANGELOG.md has a section for this version, and it does not say "(not released)";
#   * the tag is not made yet, here or on the remote;
#   * HEAD is on origin/main (after a fetch). A tag on a commit the remote does not have would take
#     that commit, and the ones behind it, to the remote without a review. For --push this stops the
#     release; otherwise it is a warning, because making the tag here first is fine.
#
# --test runs what .github/workflows/test.yml runs (keep them in step). The tests that ask for Datalevin
# print that they skipped it when there is none, and a release is not tried with a skipped khana, so
# the jar is needed: HOSIGRAPH_DATALEVIN_JAR (and HOSIGRAPH_JAVA, default `java`). The jar has to be the
# one CI pins, by its sha256.
#
# The tag is made with git, not jj: jj cannot make an annotated tag. It follows the git settings of the
# one who runs it (tag.gpgSign included). In a jj checkout HEAD is the parent of the working copy, that
# is, the last commit that was made.
set -euo pipefail
cd "$(dirname "$0")/.."

remote=origin
branch=main
do_test=0 do_tag=0 do_push=0 given=""

while [ $# -gt 0 ]; do
  case "$1" in
    --test) do_test=1 ;;
    --tag)  do_tag=1 ;;
    --push) do_tag=1; do_push=1 ;;
    -h|--help) sed -n '2,/^set -euo/p' "$0" | sed '$d' | sed 's/^# \{0,1\}//'; exit 0 ;;
    -*) echo "unknown option: $1 (see the head of this file)" >&2; exit 2 ;;
    *)  given="$1" ;;
  esac
  shift
done

problems=() warnings=()
problem() { problems+=("$1"); }
warning() { warnings+=("$1"); }

# ---- the version: the cabal file is the one that says it
version=$(sed -n 's/^version: *//p' hosigraph.cabal | head -n 1)
[ -n "$version" ] || { echo "no version in hosigraph.cabal" >&2; exit 2; }
if [[ "$version" =~ ^[0-9]+\.[0-9]+\.[0-9]+\.0$ ]]; then tagver=${version%.0}; else tagver=$version; fi
tag="v$tagver"
if [ -n "$given" ] && [ "$given" != "$tagver" ] && [ "$given" != "$version" ]; then
  problem "you said $given, but hosigraph.cabal says $version (the tag would be $tag)"
fi

# ---- the tree
if [ -n "$(git status --porcelain)" ]; then
  problem "there are uncommitted changes ($(git status --porcelain | wc -l | tr -d ' ') files): commit them first"
fi
head=$(git rev-parse HEAD)

# ---- the changelog
if ! grep -q -E "^## ${version//./\\.}( |\$)" CHANGELOG.md; then
  problem "CHANGELOG.md has no section '## $version'"
elif grep -q -E "^## ${version//./\\.} \(not released\)" CHANGELOG.md; then
  problem "CHANGELOG.md still says '## $version (not released)': put the date there"
fi

# ---- the tag
if git rev-parse -q --verify "refs/tags/$tag" >/dev/null; then
  problem "the tag $tag is made already, here"
fi
if git ls-remote --exit-code --tags "$remote" "refs/tags/$tag" >/dev/null 2>&1; then
  problem "the tag $tag exists on $remote"
fi
git fetch --quiet "$remote" "$branch" 2>/dev/null || warning "could not fetch $remote/$branch: what follows about it may be old"

# ---- is HEAD on the remote's main
if git rev-parse -q --verify "refs/remotes/$remote/$branch" >/dev/null; then
  if ! git merge-base --is-ancestor "$head" "refs/remotes/$remote/$branch"; then
    msg="HEAD ${head:0:8} is not on $remote/$branch yet: the tag would name a commit the remote does not have"
    if [ "$do_push" = 1 ]; then problem "$msg"; else warning "$msg (push it, or merge it, before the tag is pushed)"; fi
  fi
else
  problem "no $remote/$branch here: git fetch $remote"
fi

# ---- report what is wrong before any slow step
echo "version $version -> tag $tag on ${head:0:8} ($(git log -1 --format=%s | cut -c1-60))"
for w in ${warnings[@]+"${warnings[@]}"}; do echo "warning: $w"; done
if [ ${#problems[@]} -gt 0 ]; then
  for p in "${problems[@]}"; do echo "PROBLEM: $p" >&2; done
  echo "not ready." >&2
  exit 1
fi

# ---- the tests, as CI runs them
if [ "$do_test" = 1 ]; then
  jar=${HOSIGRAPH_DATALEVIN_JAR:-}
  [ -f "$jar" ] || { echo "--test needs HOSIGRAPH_DATALEVIN_JAR: the standalone Datalevin jar CI uses (see the head of this file)" >&2; exit 1; }
  pinned=$(sed -n 's/^  DATALEVIN_JAR_SHA256: "\(.*\)"$/\1/p' .github/workflows/test.yml)
  actual=$( (sha256sum "$jar" 2>/dev/null || shasum -a 256 "$jar") | cut -d' ' -f1 )
  [ "$actual" = "$pinned" ] || { echo "the jar has sha256 $actual, CI pins $pinned" >&2; exit 1; }
  export HOSIGRAPH_DATALEVIN_JAR HOSIGRAPH_JAVA="${HOSIGRAPH_JAVA:-java}"

  echo "--> build"
  cabal build --enable-tests hosigraph takka setu \
    hosigraph-abi:lib:hosigraph-abi hosigraph-abi:exe:hosigraph-abi-gen hosigraph-abi:test:spec
  echo "--> test"
  cabal test hosigraph:spec setu:spec hosigraph-abi:spec --test-show-details=direct
  echo "--> are the generated files up to date?"
  cabal run hosigraph-abi:exe:hosigraph-abi-gen -- --check hosigraph-abi/gen
fi

# ---- the tag, annotated, with the changelog's section as its message
if [ "$do_tag" = 1 ]; then
  msgfile=$(mktemp)
  trap 'rm -f "$msgfile"' EXIT
  {
    echo "hosigraph $version"
    echo
    awk -v h="## $version" '
      index($0, h) == 1 && (length($0) == length(h) || substr($0, length(h) + 1, 1) == " ") { on = 1; next }
      on && /^## / { exit }
      on { print }
    ' CHANGELOG.md
  } > "$msgfile"
  git tag -a "$tag" -F "$msgfile" "$head"
  echo "made $tag on ${head:0:8}"
fi

# ---- the push, only when asked
if [ "$do_push" = 1 ]; then
  git push "$remote" "refs/tags/$tag"
  echo "pushed $tag to $remote"
elif [ "$do_tag" = 1 ]; then
  echo "to publish it: git push $remote refs/tags/$tag   (or: scripts/release.sh --push)"
else
  echo "ready. To make the tag: scripts/release.sh --tag"
fi
