#!/usr/bin/env nix-shell
#!nix-shell -p bash nix jq -i bash

set -e

if ! grep -q 'name.*habitica' package.json 2> /dev/null; then
  echo "You need to run this from within a Habitica git clone." >&2
  exit 1
fi

getKillFiles() {
  ( cd "$(dirname "$0")"
    nix-instantiate --eval --json -E '
      ((import <nixpkgs> {}).callPackages ../pkgs/shabitica {})
        .source.filesToKill
    '
  )
}

getKillFiles | jq -r '.[] | @sh "rm -r \(.)"' | $SHELL
