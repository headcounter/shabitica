#!/usr/bin/env nix-shell
#!nix-shell -p bash git -i bash

set -e

patchdir="$(dirname "$0")/patches"

if ! grep -q 'name.*habitica' package.json 2> /dev/null; then
  echo "You need to run this from within a Habitica git clone." >&2
  exit 1
fi

for rev in $(git rev-list "$(git describe --abbrev=0)...HEAD"); do
  filename="$(git show -s --format='%(trailers:only)' "$rev" \
    | sed -ne 's/^Filename: *//p')"
  if [ -z "$filename" ]; then
    echo "Unable to find patch file name for commit $rev." >&2
    exit 1
  fi
  git diff "$rev^!" > "$patchdir/$filename"
done
