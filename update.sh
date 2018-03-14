#!/usr/bin/env nix-shell
#!nix-shell -p bash python3 curl.bin jq nodePackages.node2nix -i bash

set -e

get_latest_version() {
  curl -s https://api.github.com/repos/HabitRPG/habitica/git/refs/tags \
    | jq -r 'map(.ref | match("^refs/tags/v([0-9.]+)$") | .captures[0].string)
           | sort_by(split(".") | map(tonumber))[-1]'
}

get_package_json() {
  local srcpath="$1"

  local habitica="{\"habitica\": \"file:$srcpath\"}"
  local tools="\"gulp-cli\", \"mocha\", {\"node-pre-gyp\": \"^0.8.0\"}"
  echo "[$tools, \"google-fonts-offline\", $habitica]"
}

make_fetch_expr() {
  local version="$1"
  local sha256="${2:-0000000000000000000000000000000000000000000000000000}"

  local url="https://github.com/HabitRPG/habitica/archive/v$version.tar.gz"

  echo 'fetchzip {'
  echo '      name = "habitica-'"$version"'";'
  echo '      url = "'"$url"'";'
  echo '      sha256 = "'"$sha256"'";'
  echo '      extraPostFetch = '\'\'
  echo '        patch -p1 -d "$out" < ${../patches/strip-dependencies.patch}'
  echo '        patch -p1 -d "$out" < ${../patches/socket-activation.patch}'
  echo '      '\'\'';'
  echo -n '    }'
}

fetch_archive() {
  local version="$1"
  local sha256="$2"

  local fetchexpr="$(make_fetch_expr "$version" "$sha256")"
  local firstfetch="with import <nixpkgs> {}; $fetchexpr"
  (cd generated && nix-build --no-out-link -E "$firstfetch")
}

fetch_latest_hash() {
  local version="$(get_latest_version)"
  local output="$(fetch_archive "$version" 2>&1 || :)"
  local sha256="$(echo "$output" | sed -r -n \
    -e 's/^.*with sha256 hash '\''([^'\'']+)'\'' instead of.*/\1/p')"
  if [ -z "$sha256" ]; then
    echo "Couldn't fetch hash for Habitica version $version:" >&2
    echo "$output" >&2
    exit 1
  fi
  echo "$sha256:$version"
}

cd "$(dirname "$0")"
mkdir -p generated

sha_ver="$(fetch_latest_hash)"
sha256="${sha_ver%%:*}"
version="${sha_ver#*:}"

srcpath="$(fetch_archive "$version" "$sha256")"

node2nix -6 \
  -i <(get_package_json "$srcpath") \
  -o generated/node-packages.nix \
  --no-copy-node-env \
  -e '<nixpkgs/pkgs/development/node-packages/node-env.nix>' \
  -c generated/default.nix

sed -i -e 's,\.\./<nixpkgs,<nixpkgs,' \
  -e '/inherit/s/fetchurl/& fetchzip/' \
  generated/default.nix

sed -i -e 's/^{ *\(.*\) *}:$/{ \1, fetchzip }:/' \
  generated/node-packages.nix

python3 -c '
import sys
buf = open(sys.argv[1], "rb").read().replace(
  sys.argv[2].encode(), sys.argv[3].encode()
)
open(sys.argv[1], "wb").write(buf)
' generated/node-packages.nix "= $srcpath" \
  "= $(make_fetch_expr "$version" "$sha256")"
