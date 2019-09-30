{ pkgs, python }:

self: super: let
  inherit (pkgs) lib;

  addBuildDeps = deps: name: super.${name}.overrideAttrs (drv: {
    buildInputs = (drv.buildInputs or []) ++ deps;
  });

in {
  pytest-rerunfailures = super.pytest-rerunfailures.overrideAttrs (drv: {
    propagatedBuildInputs = (drv.propagatedBuildInputs or []) ++ [
      pkgs.python37Packages.setuptools
    ];
  });
} // lib.genAttrs [
  "apipkg" "execnet" "importlib-metadata" "pluggy" "py" "pytest"
  "pytest-base-url" "pytest-forked" "pytest-html" "pytest-metadata"
  "pytest-selenium" "pytest-variables" "pytest-xdist" "zipp"
] (addBuildDeps [ self.setuptools-scm ])
