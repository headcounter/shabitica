{ nixpkgs ? <nixpkgs> }:

let
  pkgs = import nixpkgs {};

in {
  tests = import ./tests.nix { inherit nixpkgs pkgs; };
}
