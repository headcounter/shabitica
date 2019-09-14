{ pkgs, python }:

self: super: let
  inherit (pkgs) lib;

  addBuildDeps = deps: name: super.${name}.overrideAttrs (drv: {
    buildInputs = (drv.buildInputs or []) ++ deps;
  });

in lib.genAttrs [
  "importlib-metadata"
  "pluggy"
  "pytest"
  "pytest-xdist"
  "zipp"
] (addBuildDeps [ self.setuptools-scm ])
