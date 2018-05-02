{ nixpkgs ? <nixpkgs>, pkgs ? import nixpkgs {} }:

let
  args = {
    inherit nixpkgs pkgs;
    inherit (pkgs) lib;
  };

in {
  upstream   = import ./upstream.nix   args;
  downstream = import ./downstream.nix args;
}
