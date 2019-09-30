#!/usr/bin/env nix-shell
#!nix-shell pypi2nix.nix -i bash
cd "$(dirname "$0")"
exec pypi2nix -V python37 --no-emit-extra-build-inputs \
              -E libxml2 -E libxslt -r requirements.txt -s setuptools-scm
