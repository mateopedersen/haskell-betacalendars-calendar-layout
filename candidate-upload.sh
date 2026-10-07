#!/usr/bin/env bash
set -euo pipefail

package_dir="$(cd "$(dirname "$0")" && pwd)"
cd "$package_dir"

if ! grep -q '^source-repository head$' betacalendars-calendar-layout.cabal; then
  echo "Add the verified public source-repository metadata before uploading." >&2
  exit 2
fi

cabal check
cabal build all
cabal test all
cabal haddock all
cabal sdist

archive="$package_dir/dist-newstyle/sdist/betacalendars-calendar-layout-0.1.0.0.tar.gz"
if [[ ! -f "$archive" ]]; then
  echo "Expected source archive was not produced: $archive" >&2
  exit 3
fi

tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT
tar -xzf "$archive" -C "$tmpdir"
extracted="$(find "$tmpdir" -mindepth 1 -maxdepth 1 -type d -print -quit)"
if [[ -z "$extracted" ]]; then
  echo "Could not find the extracted source distribution." >&2
  exit 4
fi
(
  cd "$extracted"
  cabal build all
  cabal test all
  cabal haddock all
)

# Cabal's default upload mode creates a reviewable candidate.
cabal upload "$archive"
