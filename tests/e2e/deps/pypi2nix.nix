{ pkgs ? import <nixpkgs> {} }:

let
  pypi2nixSrc = pkgs.fetchFromGitHub {
    owner = "garbas";
    repo = "pypi2nix";
    rev = "628bc2b021dc9c445605904ddba8525ae4efa1e6";
    sha256 = "1vxp2brl1d36y2xrk21ifwc77hcishpq0m96jpcaq24n6j10gh90";
  };

  pypi2nix = (pkgs.callPackage pypi2nixSrc {
    inherit pkgs;
  }).overrideAttrs (drv: {
    patchPhase = (drv.patchPhase or "") + ''
      # This is to make sure that we don't have *any* of the dependencies that
      # are required during the actual update ending up *globally* in *all* of
      # the resulting python packages.
      sed -i -e '/pypi2nix\.stage3\.main/,/^ *)/ {
        s/\(extra_build_inputs=\)[^,]*/\1[]/g
      }' src/pypi2nix/cli.py
    '';
  });

in pkgs.mkShell { nativeBuildInputs = [ pypi2nix pkgs.bash pkgs.coreutils ]; }
