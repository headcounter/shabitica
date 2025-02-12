{
  description = "shabitica flake";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/21.05";

  outputs = { nixpkgs, ... }: {
    nixosModules.default = { lib, config, ... }: {
      imports = [
        (import ./default.nix {
          inherit lib config;
          pkgs = import nixpkgs {
            inherit (config.nixpkgs.localSystem) system;
          };
        })
      ];
    };
  };
}
