{
  description = "shabitica flake";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/21.05";

  outputs = { nixpkgs, ... }: {
    nixosModules.default = { lib, config, ... }: {
      imports = [ ./modules ];

      shabitica.pinnedPkgs = {...}: import nixpkgs {
        inherit (config.nixpkgs.localSystem) system;
      };
    };
  };
}
