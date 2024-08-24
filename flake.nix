{
  description = "shabitica flake";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/21.05";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    { self, flake-utils, ... }@inputs:
     flake-utils.lib.eachDefaultSystem (system:{
      nixosModules.default = (
        {
          config,
          lib,
          pkgs,
          ...
        }:
        {
          imports = [
            (import ./default.nix { pkgs = (import inputs.nixpkgs { system = system; }); inherit lib config;})
          ];
        }
      );

    });
}
