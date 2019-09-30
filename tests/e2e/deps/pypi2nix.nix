{ pkgs ? import <nixpkgs> {} }:

let
  pypi2nixSrc = pkgs.fetchFromGitHub {
    owner = "nix-community";
    repo = "pypi2nix";
    rev = "ea40bcf4afca0b4087a3f827d40c509df81f9592";
    sha256 = "0bnkb0h2hakgg08xvdfznpcfli61x2g5d40k7bnlmf2578n6jr7z";
  };

  pypi2nix = pkgs.callPackage pypi2nixSrc { inherit pkgs; };

in pkgs.mkShell { nativeBuildInputs = [ pypi2nix pkgs.bash pkgs.coreutils ]; }
