{
  description = "shabitica flake";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/21.05";

  outputs = { self, nixpkgs, ... }@inputs: {
    nixosModules = builtins.mapAttrs
      (system: pkgs: {
        default =
          { lib, config, ... }: {
            imports = [
              (import ./default.nix {
                inherit lib config;
                pkgs = import nixpkgs { inherit system; };
              })
            ];
          };
      })
      nixpkgs.legacyPackages;
  };
}
