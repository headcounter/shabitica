#!/usr/bin/env nix-shell
#!nix-shell pypi2nix.nix -i bash
cd "$(dirname "$0")"
exec pypi2nix -V 3.7 -E libxml2 -E libxslt \
              -r requirements.txt -s setuptools-scm
