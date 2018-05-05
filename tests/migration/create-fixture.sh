#!/usr/bin/env nix-shell
#!nix-shell --pure -p bash coreutils nix -i bash
#!nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/c505e5777143a35828494e2c1e9744940b224a5d.tar.gz

# This script creates a test database for Habitica version 4.38.0, which is the
# last version in our fork, which doesn't need migrations. The generated test
# database will then be used in our tests to apply all the migrations up until
# the latest version.

set -e
testDir="$(cd "$(dirname "$0")" && pwd)"
fixtureDir="$(nix-build --no-out-link "$testDir/create-fixture.nix" -A out)"
dest="$testDir/fixture"
test -e "$testDir"
[ ! -e "$dest" ] || rm -rf "$dest"
cp --no-preserve=all -r "$fixtureDir" "$dest"
cat "$(nix-build --no-out-link "$testDir/create-fixture.nix" -A spec)" \
  > "$testDir/spec.json"
