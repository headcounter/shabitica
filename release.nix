{ nixpkgs ? <nixpkgs> }:

let
  pkgs = import nixpkgs {};
  inherit (pkgs) lib;

  jobs = {
    tests = import ./tests.nix { inherit nixpkgs pkgs; };
  };

in jobs // {
  habitica = pkgs.releaseTools.channel {
    name = "habitica";
    constituents = lib.collect lib.isDerivation jobs;
    src = ./.;
  };
}
