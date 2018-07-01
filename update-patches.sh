#!/usr/bin/env nix-shell
#!nix-shell -p bash git -i bash

set -e

patchdir="$(dirname "$0")/patches"

if ! grep -q 'name.*habitica' package.json 2> /dev/null; then
  echo "You need to run this from within a Habitica git clone." >&2
  exit 1
fi

if [ -n "$1" ]; then
  baseRev="$1"
else
  baseRev="$(git describe --abbrev=0 --tags)"
fi

for rev in $(git rev-list "$baseRev...HEAD"); do
  filename="$(git show -s --format='%(trailers:only)' "$rev" \
    | sed -ne 's/^Filename: *//p')"
  if [ -z "$filename" ]; then
    echo "Unable to find patch file name for commit $rev." >&2
    exit 1
  fi
  git show "$rev" | tail -n +2 > "$patchdir/$filename"
done
