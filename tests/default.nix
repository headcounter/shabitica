{ nixpkgs ? <nixpkgs>, pkgs ? import nixpkgs {} }:

let
  args = {
    inherit nixpkgs pkgs;
    inherit (pkgs) lib;
  };

  callTest = path: let
    common = { config, lib, ... }: {
      imports = [ ../. ];
      networking.firewall.enable = false;
      habitica.hostName = lib.mkDefault config.networking.hostName;
      virtualisation.diskSize = 16384;
      virtualisation.memorySize = 1024;
    };
    testFun = import path (args // {
      inherit common;

      registerUser = username: hostname: let
        inherit (args) lib;
        mkPerlString = val: "'${lib.escape ["\\" "'"] val}'";
        listToCommand = lib.concatMapStringsSep " " lib.escapeShellArg;
        data = builtins.toJSON {
          inherit username;
          email = "${username}@example.org";
          password = "test";
          confirmPassword = "test";
        };
        url = "http://${hostname}/api/v3/user/auth/local/register";
      in mkPerlString (listToCommand [
        "curl" "-f" "-H" "Content-Type: application/json" "-d" data url
      ]);
    });
  in import "${nixpkgs}/nixos/tests/make-test.nix" testFun;

in {
  upstream = import ./upstream.nix args;

  allow-register-first = callTest ./allow-register-first.nix;
}
